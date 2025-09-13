//! # jprop
//! no-std parser for java `.properties` files that actually works.
#![no_std]
#![deny(
    clippy::correctness,
    clippy::perf,
    clippy::complexity,
    clippy::style,
    clippy::nursery,
    clippy::pedantic,
    clippy::clone_on_ref_ptr,
    clippy::decimal_literal_representation,
    clippy::float_cmp_const,
    clippy::missing_docs_in_private_items,
    clippy::multiple_inherent_impl,
    clippy::unwrap_used,
    clippy::cargo_common_metadata,
    clippy::used_underscore_binding
)]

extern crate alloc;
#[cfg(feature = "std")]
extern crate std;

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt::{Display, Formatter};
use core::marker::PhantomData;
use core::mem;

/// Marker struct for I/O which cannot fail.
/// Any Result or Enum variant that contains this type is unreachable.
///
/// It is, for example, used if the source or output is memory,
/// because reading/writing from/to memory cannot fail without panicking or crashing.
#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord, Hash, Default)]
pub struct InfallibleIO;

/// Error enum for Parsing characters
#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord, Hash)]
pub enum CharacterInputError<E> {
    UnexpectedEof,
    InvalidInput,
    InputError(E),
}

pub trait CharacterInput<E> {
    /// Read the next character from some input source
    ///
    /// # Return values
    /// 1. Ok(None) to signal EOF.
    /// 2. Ok(Some) to give the next to the parser.
    /// 3. Err is passed as is to the caller.
    /// # Errors
    /// Presumably IO Errors
    ///
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>>;
}

pub trait ByteInput<E> {
    /// Read the next byte from the stream or return None on eof.
    /// # Errors
    /// Presumably IO Errors.
    fn next_byte(&mut self) -> Result<Option<u8>, E>;
}

pub trait PropertyHandler {
    fn handle(&mut self, position: &ParserPosition, value: Element) -> bool;
}

/// The ISO-8859-1 codepage, byte to char.
/// `<https://en.wikipedia.org/wiki/ISO/IEC_8859-1>`
/// 0 char is used as a substitute for undefined.
static ISO_8859_1: [char; 256] = [
    //0x00-0x0F
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\t', '\n', '\0', '\0', '\r', '\0', '\0',
    //0x10-0x1F
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    //0x20-0x2F
    ' ', '!', '"', '#', '$', '%', '&', '\'', '(', ')', '*', '+', ',', '-', '.', '/',
    //0x30-0x3F
    '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', ':', ';', '<', '=', '>', '?',
    //0x40-0x4F
    '@', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O',
    //0x50-0x5F
    'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z', '[', '\\', ']', '^', '_',
    //0x60-0x6f
    '`', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o',
    //0x70-0x7F
    'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z', '{', '|', '}', '~', '\0',
    //0x80-0x8F
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    //0x90-0x9F
    '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0', '\0',
    //0xA0-0xAF
    '\u{00A0}', '¡', '¢', '£', '¤', '¥', '¦', '§', '¨', '©', 'ª', '«', '¬', '\u{00AD}', '®', '¯',
    //0xB0-0xBF
    '°', '±', '²', '³', '´', 'µ', '¶', '·', '¸', '¹', 'º', '»', '¼', '½', '¾', '¿',
    //0xC0-0xCF
    'À', 'Á', 'Â', 'Ã', 'Ä', 'Å', 'Æ', 'Ç', 'È', 'É', 'Ê', 'Ë', 'Ì', 'Í', 'Î', 'Ï',
    //0xD0-0xDF
    'Ð', 'Ñ', 'Ò', 'Ó', 'Ô', 'Õ', 'Ö', '×', 'Ø', 'Ù', 'Ú', 'Û', 'Ü', 'Ý', 'Þ', 'ß',
    //0xE0-0xEF
    'à', 'á', 'â', 'ã', 'ä', 'å', 'æ', 'ç', 'è', 'é', 'ê', 'ë', 'ì', 'í', 'î', 'ï',
    //0xF0-0xFF
    'ð', 'ñ', 'ò', 'ó', 'ô', 'õ', 'ö', '÷', 'ø', 'ù', 'ú', 'û', 'ü', 'ý', 'þ', 'ÿ',
];

/// UTF8 character input.
struct UTF8<'a, T: ByteInput<E>, E>(&'a mut T, bool, bool, PhantomData<E>);
impl<T: ByteInput<E>, E> CharacterInput<E> for UTF8<'_, T, E> {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>> {
        let mut buf = [0u8; 4];

        buf[0] = match self
            .0
            .next_byte()
            .map_err(CharacterInputError::InputError)?
        {
            None => return Ok(None), //EOF
            Some(d) => d,
        };

        let first = buf[0];
        let iso = ISO_8859_1[first as usize];
        if self.2 {
            if iso == '\0' {
                return Err(CharacterInputError::InvalidInput);
            }

            return Ok(Some(iso));
        }

        if iso == '\0' {
            self.1 = true;
        }

        if first & 0b1000_0000 == 0 {
            if first > b'~' {
                self.1 = true;
            }
            return Ok(Some(char::from(first)));
        }

        if first & 0b1100_0000 == 0b1000_0000 {
            if self.1 {
                //We have already read garbage in the past.
                return Err(CharacterInputError::InvalidInput);
            }

            //We are going to switch encoding to ISO-8859-1
            self.2 = true;
            return Ok(Some(iso));
        }

        //We can no longer switch to ISO-8859-1
        self.1 = true;

        let cnt = if first & 0b1110_0000 == 0b1100_0000 {
            2
        } else if first & 0b1111_0000 == 0b1110_0000 {
            3
        } else if first & 0b1111_1000 == 0b1111_0000 {
            4
        } else {
            return Err(CharacterInputError::InvalidInput);
        };

        for n in buf.iter_mut().take(cnt).skip(1) {
            *n = self
                .0
                .next_byte()
                .map_err(CharacterInputError::InputError)?
                .ok_or(CharacterInputError::UnexpectedEof)?;
        }

        core::str::from_utf8(&buf[0..cnt])
            .ok()
            .and_then(|e| e.chars().next())
            .map(Some)
            .ok_or_else(|| CharacterInputError::InvalidInput)
    }
}

/// ISO-8859-1 character input.
struct ISO88591<'a, T: ByteInput<E>, E>(&'a mut T, PhantomData<E>);

impl<T: ByteInput<E>, E> CharacterInput<E> for ISO88591<'_, T, E> {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>> {
        let byte = match self
            .0
            .next_byte()
            .map_err(CharacterInputError::InputError)?
        {
            None => return Ok(None), //EOF
            Some(d) => d,
        };

        let iso = ISO_8859_1[byte as usize];
        if iso == '\0' {
            return Err(CharacterInputError::InvalidInput);
        }

        Ok(Some(iso))
    }
}

/// Parser state machine states.
#[derive(Debug, Eq, PartialEq)]
enum State {
    /// Start of a line, no non-whitespace read yet.
    LineStart,
    /// We have just read \r.
    CarriageReturn,
    /// We are in the process of parsing a comment line. Meaning this line has no non whitespace before the first # or !
    Comment,
    /// We are parsing the key, as in the actual key characters.
    Key,
    /// We have parsed the keys and are now parsing whitespaces after the key. We are looking for the '=' more or less.
    KeyWhitespace,
    /// We have got the = after the key, so now we are ignoring all whitespaces until the value starts.
    BeginValue,
    /// We are parsing the value.
    Value,
    /// We are parsing an escape sequence in false=key, true=value.
    Escape(bool),
    /// We are currently ignoring whitespaces until we find a non-whitespace while parsing a multi line false=key, true=value
    MultiLineTrim(bool),
    /// We have parsed an escaped \r (like \ followed by the actual CR, not the r character) and are now checking for the \n.
    /// false=key, true=value
    EscapeCarriageReturn(bool),
    /// We are parsing a Unicode escape sequence in either false=key, true=value.
    Unicode(bool),
    /// We are expecting a '\' after parsing a utf-16 surrogate Unicode value.
    Unicode2ExpectBackslash(bool, u16),
    /// We are expecting a 'u' after parsing a utf-16 surrogate Unicode value.
    Unicode2ExpectU(bool, u16),
    /// Ware are parsing the second utf-16 character which is unicode escaped.
    Unicode2(bool, u16),
}

/// Element in a .properties file
#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Hash)]
pub enum Element {
    BlankLine,
    Comment(String),
    Value(String, String),
}

/// useful for iter over `Vec<(String, String)>` or `HashMap<String, String>` or its refs.
impl<K: alloc::string::ToString, V: alloc::string::ToString> From<(K, V)> for Element {
    fn from(value: (K, V)) -> Self {
        Self::Value(value.0.to_string(), value.1.to_string())
    }
}

impl<K: alloc::string::ToString, V: alloc::string::ToString> From<&(K, V)> for Element {
    fn from(value: &(K, V)) -> Self {
        Self::Value(value.0.to_string(), value.1.to_string())
    }
}

/// useful for iter over &Vec<ParsedValue>
impl From<&Self> for Element {
    fn from(value: &Self) -> Self {
        value.clone()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord, Hash, Default)]
pub struct ParserPosition {
    pub character_total: u64,
    pub character_in_line: u64,
    pub line: u64,
}

impl Display for ParserPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        f.write_fmt(format_args!(
            "line: {} pos: {}",
            self.line, self.character_in_line
        ))
    }
}

//Doesnt need to be public.
impl ParserPosition {
    /// Called after a new-line has been processed.
    fn next_line(&mut self) {
        self.line += 1;
        self.character_in_line = 0;
    }

    /// Called after a char has been read.
    fn next_char(&mut self) {
        self.character_in_line += 1;
        self.character_total += 1;
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord, Hash)]
pub enum ParserError<E> {
    /// We were in the middle of parsing something that has yet to be completed,
    /// For example, a Unicode escape sequence...
    UnexpectedEof,

    /// The `CharacterInput` decoded an invalid character, which is not a valid Unicode literal, or some other decoding error occurred.
    /// This is not an io error but indicates invalid data/corruption.
    InvalidInput(ParserPosition),

    /// Input io error E occurred.
    InputError(ParserPosition, E),
    /// a character after a \ is invalid.
    InvalidEscapeCharacter(ParserPosition, char),
    /// a \uXXXX escape sequence did not have one of the 'X' characters be a hexadecimal digit.
    InvalidUnicodeEscapeCharacter(ParserPosition, char),
    /// a \uXXXX escape sequence contained a 16-bit hexadecimal number, which is NOT a valid Unicode literal and not a utf-16 surrogate.
    InvalidUnicodeValue(ParserPosition, u16),
    /// a \uXXXX\uYYYY escape sequence contained an utf-16 surrogate in the first escape sequence, but the second one does not match the first.
    InvalidUnicodeSurrogateValue(ParserPosition, u16, u16),
}

impl<E: Display> Display for ParserError<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::UnexpectedEof => f.write_str("UnexpectedEof"),
            Self::InvalidInput(pos) => {
                f.write_str("InvalidInput(")?;
                Display::fmt(pos, f)?;
                f.write_str(")")
            }
            Self::InputError(pos, e) => {
                f.write_str("InputError(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            Self::InvalidEscapeCharacter(pos, e) => {
                f.write_str("InvalidEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            Self::InvalidUnicodeEscapeCharacter(pos, e) => {
                f.write_str("InvalidUnicodeEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            Self::InvalidUnicodeValue(pos, e) => {
                f.write_str("InvalidUnicodeEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            Self::InvalidUnicodeSurrogateValue(pos, s1, s2) => {
                f.write_str("InvalidUnicodeSurrogateValue(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(s1, f)?;
                f.write_str(", ")?;
                Display::fmt(s2, f)?;
                f.write_str(")")
            }
        }
    }
}

impl<T: Iterator<Item = char>> CharacterInput<InfallibleIO> for T {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<InfallibleIO>> {
        Ok(self.next())
    }
}

/// Input for a slice, tracks read position.
/// basically low budget `std::io::Cursor`
struct SliceInput<'a>(&'a [u8], usize);

impl ByteInput<InfallibleIO> for SliceInput<'_> {
    fn next_byte(&mut self) -> Result<Option<u8>, InfallibleIO> {
        if self.0.len() <= self.1 {
            return Ok(None);
        }

        let r = self.0[self.1];
        self.1 += 1;
        Ok(Some(r))
    }
}

impl<T: Fn(&ParserPosition, Element) -> bool> PropertyHandler for T {
    fn handle(&mut self, position: &ParserPosition, value: Element) -> bool {
        self(position, value)
    }
}

/// Property Handler for parsing Vec<ParsedValue>.
#[derive(Default, Debug)]
struct DocHandler(Vec<Element>);

impl PropertyHandler for DocHandler {
    fn handle(&mut self, _: &ParserPosition, value: Element) -> bool {
        self.0.push(value);
        true
    }
}

/// Property Handler for parsing `HashMap<String, String>` key value pairs.
#[cfg(feature = "std")]
#[derive(Default, Debug)]
struct MapHandler(std::collections::HashMap<String, String>);

#[cfg(feature = "std")]
impl PropertyHandler for MapHandler {
    fn handle(&mut self, _: &ParserPosition, value: Element) -> bool {
        if let Element::Value(key, value) = value {
            self.0.insert(key, value);
        }
        true
    }
}

/// Property Handler for parsing Vec<String, String> key value pairs.
#[derive(Default, Debug)]
struct VecHandler(Vec<(String, String)>);

impl PropertyHandler for VecHandler {
    fn handle(&mut self, _: &ParserPosition, value: Element) -> bool {
        if let Element::Value(key, value) = value {
            self.0.push((key, value));
        }
        true
    }
}

#[cfg(feature = "std")]
impl<T: std::io::Read> ByteInput<std::io::Error> for T {
    fn next_byte(&mut self) -> Result<Option<u8>, std::io::Error> {
        let mut buf = [0u8; 1];
        if self.read(&mut buf)? == 0 {
            return Ok(None);
        }

        Ok(Some(buf[0]))
    }
}

/// Read bytes from a source and treat them as a ISO-8859-1 String and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_iso_8859_1_to_doc<E>(
    source: &mut impl ByteInput<E>,
) -> Result<Vec<Element>, ParserError<E>> {
    let mut result = DocHandler::default();
    parse_iso_8859_1(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as a ISO-8859-1 String and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_iso_8859_1_to_vec<E>(
    source: &mut impl ByteInput<E>,
) -> Result<Vec<(String, String)>, ParserError<E>> {
    let mut result = VecHandler::default();
    parse_iso_8859_1(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as a ISO-8859-1 String and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_iso_8859_1_to_map<E>(
    source: &mut impl ByteInput<E>,
) -> Result<std::collections::HashMap<String, String>, ParserError<E>> {
    let mut result = MapHandler::default();
    parse_iso_8859_1(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as UTF-8 and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_iso_8859_1<E>(
    source: &mut impl ByteInput<E>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<E>> {
    let mut n = ISO88591(source, PhantomData);
    parse(&mut n, handler)
}

/// Read bytes from a source and treat them as UTF-8 and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_utf8_to_doc<E>(
    source: &mut impl ByteInput<E>,
) -> Result<Vec<Element>, ParserError<E>> {
    let mut result = DocHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as UTF-8 and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_utf8_to_vec<E>(
    source: &mut impl ByteInput<E>,
) -> Result<Vec<(String, String)>, ParserError<E>> {
    let mut result = VecHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as UTF-8 and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_utf8_to_map<E>(
    source: &mut impl ByteInput<E>,
) -> Result<std::collections::HashMap<String, String>, ParserError<E>> {
    let mut result = MapHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

/// Read bytes from a source and treat them as UTF-8 and then parse them as a properties file.
///
/// # Errors
/// * if the input source errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_utf8<E>(
    source: &mut impl ByteInput<E>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<E>> {
    let mut n = UTF8(source, false, false, PhantomData);
    parse(&mut n, handler)
}

/// Treat the bytes as a ISO-8859-1 string and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_iso_8859_1_to_doc(
    bytes: impl AsRef<[u8]>,
) -> Result<Vec<Element>, ParserError<InfallibleIO>> {
    let mut result = DocHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as a ISO-8859-1 string and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_iso_8859_1_to_vec(
    bytes: impl AsRef<[u8]>,
) -> Result<Vec<(String, String)>, ParserError<InfallibleIO>> {
    let mut result = VecHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as a ISO-8859-1 string and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_bytes_iso_8859_1_to_map(
    bytes: impl AsRef<[u8]>,
) -> Result<std::collections::HashMap<String, String>, ParserError<InfallibleIO>> {
    let mut result = MapHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as a ISO-8859-1 string and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_iso_8859_1(
    bytes: impl AsRef<[u8]>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<InfallibleIO>> {
    let n = bytes.as_ref();
    let mut sl = SliceInput(n, 0);
    let mut iso = ISO88591(&mut sl, PhantomData);
    parse(&mut iso, handler)
}

/// Treat the bytes as utf-8 and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_utf8_to_doc(
    bytes: impl AsRef<[u8]>,
) -> Result<Vec<Element>, ParserError<InfallibleIO>> {
    let mut result = DocHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as utf-8 and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_utf8_to_vec(
    bytes: impl AsRef<[u8]>,
) -> Result<Vec<(String, String)>, ParserError<InfallibleIO>> {
    let mut result = VecHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as utf-8 and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_bytes_utf8_to_map(
    bytes: impl AsRef<[u8]>,
) -> Result<std::collections::HashMap<String, String>, ParserError<InfallibleIO>> {
    let mut result = MapHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

/// Treat the bytes as utf-8 and parse them as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_bytes_utf8(
    bytes: impl AsRef<[u8]>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<InfallibleIO>> {
    let n = bytes.as_ref();
    let mut sli = SliceInput(n, 0);
    let mut utf = UTF8(&mut sli, false, false, PhantomData);
    parse(&mut utf, handler)
}

/// Parse the str as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_str_to_doc(str: impl AsRef<str>) -> Result<Vec<Element>, ParserError<InfallibleIO>> {
    let mut result = DocHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

/// Parse the chars as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_chars_to_doc<T: IntoChar>(
    source: impl IntoIterator<Item = T>,
) -> Result<Vec<Element>, ParserError<InfallibleIO>> {
    let mut result = DocHandler::default();
    parse_chars(source, &mut result)?;
    Ok(result.0)
}

/// Parse the str as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_str_to_vec(
    str: impl AsRef<str>,
) -> Result<Vec<(String, String)>, ParserError<InfallibleIO>> {
    let mut result = VecHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

/// Parse the chars as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
pub fn parse_chars_to_vec<T: IntoChar>(
    source: impl IntoIterator<Item = T>,
) -> Result<Vec<(String, String)>, ParserError<InfallibleIO>> {
    let mut result = VecHandler::default();
    parse_chars(source, &mut result)?;
    Ok(result.0)
}

/// Parse the str as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_str_to_map(
    str: impl AsRef<str>,
) -> Result<std::collections::HashMap<String, String>, ParserError<InfallibleIO>> {
    let mut result = MapHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

/// Parse the chars as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
#[cfg(feature = "std")]
pub fn parse_chars_to_map<T: IntoChar>(
    source: impl IntoIterator<Item = T>,
) -> Result<std::collections::HashMap<String, String>, ParserError<InfallibleIO>> {
    let mut result = MapHandler::default();
    parse_chars(source, &mut result)?;
    Ok(result.0)
}

/// Parse the str as a properties file.
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
///
/// # Example
/// ```rust
/// use jprop::{Element, ParserPosition, PropertyHandler};
///
/// struct PrintHandler;
///
/// impl PropertyHandler for PrintHandler {
///     fn handle(&mut self, _position: &ParserPosition, value: Element) -> bool {
///         match value {
///             Element::BlankLine => println!(),
///             Element::Comment(comment) => println!("COMMENT: {comment}"),
///             Element::Value(key,value) => println!("KV: {key}={value}"),
///         }
///         true
///     }
/// }
///
/// fn example() {
///     let my_dummy_prop_file = "k=v\n";
///
///     jprop::parse_str(my_dummy_prop_file, &mut PrintHandler).expect("Syntax error");
///     // Prints:
///     // KV: k=v
/// }
/// ```
pub fn parse_str(
    str: impl AsRef<str>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<InfallibleIO>> {
    let str = str.as_ref();
    parse_chars(str.chars(), handler)
}

/// Helper trait for Into<char> conversion that also supports &char.
pub trait IntoChar {
    fn into_char(self) -> char;
}

impl IntoChar for char {
    fn into_char(self) -> char {
        self
    }
}

impl IntoChar for &char {
    fn into_char(self) -> char {
        *self
    }
}
impl IntoChar for u8 {
    fn into_char(self) -> char {
        self.into()
    }
}

impl IntoChar for &u8 {
    fn into_char(self) -> char {
        (*self).into()
    }
}

/// Parse the chars as a properties file.
///
/// # Common parameters for 'source':
/// * `Vec<u8>` - Unicode 0x00-0xFF - or its ref/slice
/// * `Vec<char>` - or its ref/slice
///
/// # Errors
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
///
/// # Example
/// ```rust
///
/// use jprop::{Element, ParserPosition, PropertyHandler};
///
/// struct PrintHandler;
///
/// impl PropertyHandler for PrintHandler {
///     fn handle(&mut self, _position: &ParserPosition, value: Element) -> bool {
///         match value {
///             Element::BlankLine => println!(),
///             Element::Comment(comment) => println!("COMMENT: {comment}"),
///             Element::Value(key,value) => println!("KV: {key}={value}"),
///         }
///         true
///     }
/// }
///
/// fn example() {
///     let my_dummy_prop_file : Vec<char> = vec!['k', '=', 'v', '\n'];
///
///     jprop::parse_chars(&my_dummy_prop_file, &mut PrintHandler).expect("Syntax error");
///     // Prints:
///     // KV: k=v
/// }
///
/// fn example2() {
///     let my_dummy_prop_file = "k=v\n";
///
///     jprop::parse_chars(my_dummy_prop_file.chars(), &mut PrintHandler).expect("Syntax error");
///     // Prints:
///     // KV: k=v
/// }
/// ```
pub fn parse_chars<T: IntoChar>(
    source: impl IntoIterator<Item = T>,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<InfallibleIO>> {
    let mut input = source.into_iter().map(IntoChar::into_char);
    parse(&mut input, handler)
}

/// Low-level parsing function.
/// Parses a .properties file from a character-based input and invokes a callback handler
/// for each Element of the .properties file that is parsed.
///
/// There should be no need to use this function directly unless you need to, for example, parse
/// in a custom encoding that the JVM itself cannot even read.
///
/// # Errors
/// * if the character input errors.
/// * if an illegal sequence of characters is parsed.
///   * for example, non-Hex characters in a Unicode escape sequence
///
/// # Example
/// ```rust
///
/// use jprop::{Element, ParserPosition, PropertyHandler};
///
/// struct PrintHandler;
///
/// impl PropertyHandler for PrintHandler {
///     fn handle(&mut self, _position: &ParserPosition, value: Element) -> bool {
///         match value {
///             Element::BlankLine => println!(),
///             Element::Comment(comment) => println!("COMMENT: {comment}"),
///             Element::Value(key,value) => println!("KV: {key}={value}"),
///         }
///         true
///     }
/// }
///
/// fn example() {
///     let my_dummy_prop_file = "#beepbop\nkey=value\nanother_key=another_value";
///     jprop::parse(&mut my_dummy_prop_file.chars(), &mut PrintHandler).expect("Syntax error");
///     // Prints:
///     // COMMENT: #beepbop
///     // KV: key=value
///     // KV: another_key=another_value
/// }
/// ```
///
#[allow(clippy::too_many_lines)] //TODO later
pub fn parse<T: CharacterInput<E>, E>(
    input: &mut T,
    handler: &mut impl PropertyHandler,
) -> Result<ParserPosition, ParserError<E>> {
    let mut pos = ParserPosition::default();
    let mut state = State::LineStart;
    let mut key_buf = String::new();
    let mut value_buf = String::new();
    let mut unicode_buf = String::with_capacity(4);

    'parse_next: loop {
        let next_char = match input.next_character() {
            Ok(Some(c)) => c,
            Ok(None) => {
                return match state {
                    State::CarriageReturn | State::LineStart => {
                        handler.handle(&pos, Element::BlankLine);
                        Ok(pos)
                    }
                    State::Comment => {
                        handler.handle(&pos, Element::Comment(key_buf));
                        return Ok(pos);
                    }
                    State::Key | State::KeyWhitespace | State::BeginValue => {
                        handler.handle(&pos, Element::Value(key_buf, String::new()));
                        Ok(pos)
                    }
                    State::Value => {
                        handler.handle(&pos, Element::Value(key_buf, value_buf));
                        Ok(pos)
                    }
                    State::MultiLineTrim(is_value) | State::EscapeCarriageReturn(is_value) => {
                        if is_value {
                            handler.handle(&pos, Element::Value(key_buf, value_buf));
                        } else {
                            handler.handle(&pos, Element::Value(key_buf, String::new()));
                        }

                        Ok(pos)
                    }
                    State::Escape(_)
                    | State::Unicode(_)
                    | State::Unicode2ExpectBackslash(_, _)
                    | State::Unicode2ExpectU(_, _)
                    | State::Unicode2(_, _) => Err(ParserError::UnexpectedEof),
                }
            }
            Err(CharacterInputError::UnexpectedEof) => return Err(ParserError::UnexpectedEof),
            Err(CharacterInputError::InvalidInput) => return Err(ParserError::InvalidInput(pos)),
            Err(CharacterInputError::InputError(e)) => return Err(ParserError::InputError(pos, e)),
        };

        pos.next_char();

        //State machine automaton that will parse this shit.
        loop {
            match state {
                State::LineStart => {
                    match next_char {
                        ' ' => {
                            continue 'parse_next;
                        }
                        '#' | '!' => {
                            state = State::Comment;
                            continue;
                        }
                        '\r' => {
                            //Either CRLF (Windows) or CR (Mac)
                            if !handler.handle(&pos, Element::BlankLine) {
                                return Ok(pos);
                            }
                            state = State::CarriageReturn;
                            continue 'parse_next;
                        }
                        '\n' => {
                            //LF (Unix)
                            pos.next_line();
                            if !handler.handle(&pos, Element::BlankLine) {
                                return Ok(pos);
                            }
                            continue 'parse_next;
                        }
                        _ => {
                            state = State::Key;
                            continue;
                        }
                    }
                }
                State::CarriageReturn => {
                    pos.next_line();
                    state = State::LineStart;

                    if next_char == '\n' {
                        //Was CRLF (Windows)
                        continue 'parse_next;
                    }

                    //Was CR (Mac)
                    continue;
                }
                State::Comment => match next_char {
                    '\r' => {
                        if !handler.handle(&pos, Element::Comment(mem::take(&mut key_buf))) {
                            return Ok(pos);
                        }
                        state = State::CarriageReturn;
                        continue 'parse_next;
                    }
                    '\n' => {
                        if !handler.handle(&pos, Element::Comment(mem::take(&mut key_buf))) {
                            return Ok(pos);
                        }
                        pos.next_line();
                        state = State::LineStart;
                        continue 'parse_next;
                    }
                    _ => {
                        key_buf.push(next_char);
                        continue 'parse_next;
                    }
                },
                State::Key => match next_char {
                    '\r' => {
                        if !handler
                            .handle(&pos, Element::Value(mem::take(&mut key_buf), String::new()))
                        {
                            return Ok(pos);
                        }
                        state = State::CarriageReturn;
                        continue 'parse_next;
                    }
                    '\n' => {
                        if !handler
                            .handle(&pos, Element::Value(mem::take(&mut key_buf), String::new()))
                        {
                            return Ok(pos);
                        }
                        pos.next_line();
                        state = State::LineStart;
                        continue 'parse_next;
                    }
                    ' ' | '\t' => {
                        state = State::KeyWhitespace;
                        continue 'parse_next;
                    }
                    '=' | ':' => {
                        state = State::BeginValue;
                        continue 'parse_next;
                    }
                    '\\' => {
                        state = State::Escape(false);
                        continue 'parse_next;
                    }
                    _ => {
                        key_buf.push(next_char);
                        continue 'parse_next;
                    }
                },
                State::Escape(is_value) => match next_char {
                    'u' => {
                        state = State::Unicode(is_value);
                        continue 'parse_next;
                    }
                    'n' => {
                        if is_value {
                            value_buf.push('\n');
                            state = State::Value;
                        } else {
                            key_buf.push('\n');
                            state = State::Key;
                        }
                        continue 'parse_next;
                    }
                    't' => {
                        if is_value {
                            value_buf.push('\t');
                            state = State::Value;
                        } else {
                            key_buf.push('\t');
                            state = State::Key;
                        }
                        continue 'parse_next;
                    }
                    'r' => {
                        if is_value {
                            value_buf.push('\r');
                            state = State::Value;
                        } else {
                            key_buf.push('\r');
                            state = State::Key;
                        }
                        continue 'parse_next;
                    }
                    'f' => {
                        if is_value {
                            value_buf.push('\x0C');
                            state = State::Value;
                        } else {
                            key_buf.push('\x0C');
                            state = State::Key;
                        }
                        continue 'parse_next;
                    }
                    '=' | '\\' | '#' | '!' | ':' | ' ' => {
                        if is_value {
                            value_buf.push(next_char);
                            state = State::Value;
                        } else {
                            key_buf.push(next_char);
                            state = State::Key;
                        }
                        continue 'parse_next;
                    }
                    '\r' => {
                        state = State::EscapeCarriageReturn(is_value);
                        continue 'parse_next;
                    }
                    '\n' => {
                        pos.next_line();
                        state = State::MultiLineTrim(is_value);
                        continue 'parse_next;
                    }
                    _ => {
                        return Err(ParserError::InvalidEscapeCharacter(pos, next_char));
                    }
                },
                State::EscapeCarriageReturn(is_value) => {
                    state = State::MultiLineTrim(is_value);
                    if next_char == '\n' {
                        continue 'parse_next;
                    }
                    continue;
                }
                State::MultiLineTrim(is_value) => match next_char {
                    ' ' | '\t' | '\x0c' => {
                        continue 'parse_next;
                    }
                    _ => {
                        if is_value {
                            state = State::Value;
                        } else {
                            state = State::Key;
                        }
                        continue;
                    }
                },

                State::Unicode(is_value) => {
                    if !next_char.is_ascii_hexdigit() {
                        return Err(ParserError::InvalidUnicodeEscapeCharacter(pos, next_char));
                    }

                    unicode_buf.push(next_char);
                    if unicode_buf.len() < 4 {
                        continue 'parse_next;
                    }

                    debug_assert_eq!(unicode_buf.len(), 4);

                    let unicode = u16::from_str_radix(&unicode_buf, 16)
                        // This can't fall back to default really,
                        // 4 hex characters will always fit into u16 if is_ascii_hexdigit is true.
                        .unwrap_or_default();
                    unicode_buf.clear();

                    if (0xD800..=0xDFFF).contains(&unicode) {
                        state = State::Unicode2ExpectBackslash(is_value, unicode);
                        continue 'parse_next;
                    }

                    if let Some(Ok(char_code)) = char::decode_utf16(core::iter::once(&unicode).copied()).next() {
                        if is_value {
                            value_buf.push(char_code);
                            state = State::Value;
                            continue 'parse_next;
                        }

                        key_buf.push(char_code);
                        state = State::Key;
                        continue 'parse_next;
                    }

                    return Err(ParserError::InvalidUnicodeValue(pos, unicode));
                }
                State::Unicode2ExpectBackslash(is_value, unicode) => {
                    if next_char != '\\' {
                        return Err(ParserError::InvalidUnicodeValue(pos, unicode));
                    }

                    state = State::Unicode2ExpectU(is_value, unicode);
                    continue 'parse_next;
                }
                State::Unicode2ExpectU(is_value, unicode) => {
                    if next_char != 'u' {
                        return Err(ParserError::InvalidUnicodeValue(pos, unicode));
                    }

                    state = State::Unicode2(is_value, unicode);
                    continue 'parse_next;
                }
                State::Unicode2(is_value, unicode) => {
                    if !next_char.is_ascii_hexdigit() {
                        return Err(ParserError::InvalidUnicodeEscapeCharacter(pos, next_char));
                    }

                    unicode_buf.push(next_char);
                    if unicode_buf.len() < 4 {
                        continue 'parse_next;
                    }

                    debug_assert_eq!(unicode_buf.len(), 4);

                    let unicode2 = u16::from_str_radix(&unicode_buf, 16)
                        // This can't fall back to default really,
                        // 4 hex characters will always fit into u16 if is_ascii_hexdigit is true.
                        .unwrap_or_default();
                    unicode_buf.clear();

                    if let Some(Ok(char_code)) = char::decode_utf16([unicode, unicode2].iter().copied()).next() {
                        if is_value {
                            value_buf.push(char_code);
                            state = State::Value;
                            continue 'parse_next;
                        }

                        key_buf.push(char_code);
                        state = State::Key;
                        continue 'parse_next;
                    }

                    return Err(ParserError::InvalidUnicodeSurrogateValue(
                        pos, unicode, unicode2,
                    ));
                }
                State::KeyWhitespace => match next_char {
                    ' ' | '\t' | '\x0C' => {
                        continue 'parse_next;
                    }
                    ':' | '=' => {
                        state = State::BeginValue;
                        continue 'parse_next;
                    }
                    _ => {
                        state = State::BeginValue;
                        continue;
                    }
                },
                State::BeginValue => match next_char {
                    ' ' | '\t' | '\x0C' => {
                        continue 'parse_next;
                    }
                    _ => {
                        state = State::Value;
                        continue;
                    }
                },
                State::Value => match next_char {
                    '\\' => {
                        state = State::Escape(true);
                        continue 'parse_next;
                    }
                    '\r' => {
                        if !handler.handle(
                            &pos,
                            Element::Value(mem::take(&mut key_buf), mem::take(&mut value_buf)),
                        ) {
                            return Ok(pos);
                        }
                        state = State::CarriageReturn;
                        continue 'parse_next;
                    }
                    '\n' => {
                        if !handler.handle(
                            &pos,
                            Element::Value(mem::take(&mut key_buf), mem::take(&mut value_buf)),
                        ) {
                            return Ok(pos);
                        }
                        pos.next_line();
                        state = State::LineStart;
                        continue 'parse_next;
                    }
                    _ => {
                        value_buf.push(next_char);
                        continue 'parse_next;
                    }
                },
            }

            #[allow(unreachable_code)]
            //#[expect(unreachable_code)]
            {
                unreachable!();
            }
        }
    }
}

pub trait CharacterOutput<E> {
    /// Write a single character to the character output
    ///
    /// # Errors
    /// IO Errors
    fn write(&mut self, data: char) -> Result<(), E>;

    /// Determines if a character needs to be Unicode escaped.
    /// This function will NOT be called for every instance of the following characters:
    /// - #
    /// - =
    /// - !
    /// - \
    /// - u
    /// - t, n, r, f
    /// - 0-9 A-F
    /// - \n
    /// - \r
    /// - ' ' ordinary whitespace
    ///
    /// In addition to that, it also will not be called for every character in the `line_ending`.
    fn can_write(&mut self, data: char) -> bool;
}

impl CharacterOutput<InfallibleIO> for String {
    fn write(&mut self, data: char) -> Result<(), InfallibleIO> {
        self.push(data);
        Ok(())
    }

    fn can_write(&mut self, _: char) -> bool {
        true
    }
}

impl CharacterOutput<InfallibleIO> for &mut Vec<char> {
    fn write(&mut self, data: char) -> Result<(), InfallibleIO> {
        self.push(data);
        Ok(())
    }

    fn can_write(&mut self, _: char) -> bool {
        true
    }
}

/// Byte-based output trait (Poor man's `std::io::Write`)
pub trait ByteOutput<E> {
    /// Write a single byte to the output
    ///
    /// # Errors
    /// Some sort of IO Error
    fn write(&mut self, data: u8) -> Result<(), E>;
}

#[cfg(feature = "std")]
impl<T: std::io::Write> ByteOutput<std::io::Error> for T {
    fn write(&mut self, data: u8) -> Result<(), std::io::Error> {
        self.write_all(&[data])
    }
}

impl ByteOutput<InfallibleIO> for &mut Vec<u8> {
    fn write(&mut self, data: u8) -> Result<(), InfallibleIO> {
        self.push(data);
        Ok(())
    }
}

/// UTF-8 character output
struct UTF8Out<'a, T: ByteOutput<E>, E>(&'a mut T, PhantomData<E>);

impl<T: ByteOutput<E>, E> CharacterOutput<E> for UTF8Out<'_, T, E> {
    fn write(&mut self, data: char) -> Result<(), E> {
        let mut buf = [0u8; 4];
        let str = data.encode_utf8(&mut buf);
        for n in str.bytes() {
            self.0.write(n)?;
        }

        Ok(())
    }

    fn can_write(&mut self, _: char) -> bool {
        true
    }
}

/// US Ascii character output
struct ASCIIOut<'a, T: ByteOutput<E>, E>(&'a mut T, PhantomData<E>);

impl<T: ByteOutput<E>, E> CharacterOutput<E> for ASCIIOut<'_, T, E> {
    fn write(&mut self, data: char) -> Result<(), E> {
        if self.can_write(data) {
            return self.0.write(data as u8);
        }

        self.0.write(b'?')
    }

    fn can_write(&mut self, data: char) -> bool {
        data == '\r' || data == '\n' || (' '..'\x7F').contains(&data)
    }
}

/// Emits the 6-character sequence (\uXXXX, XXXX being HEX) needed to escape a single char.
/// For characters that need 2 utf-16 escape sequences (surrogates), it emits 12 characters.
fn escape_unicode<E>(target: &mut impl CharacterOutput<E>, c: char) -> Result<(), E> {
    static LUT: [char; 16] = [
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F',
    ];

    let mut buf = [0; 2];
    let bf = c.encode_utf16(&mut buf);
    for n in bf {
        let n = *n;
        target.write('\\')?;
        target.write('u')?;
        target.write(LUT[((n >> 12) & 0xF) as usize])?;
        target.write(LUT[((n >> 8) & 0xF) as usize])?;
        target.write(LUT[((n >> 4) & 0xF) as usize])?;
        target.write(LUT[((n) & 0xF) as usize])?;
    }

    Ok(())
}

/// Serialize a set of elements into a .properties file into some sort of byte-based output.
///
/// This method will write US-ASCII bytes into the output and escape all ascii control characters as well as all other non-ascii Unicode code points.
///
/// A file created by this function cannot be decoded by:
/// * Humans of average intelligence using a common text editor unless all keys and values only contain printable US-ASCII characters.
///     * Most Humans don't understand "\uXXXX" escaping, especially if you're using a language which uses a non-latin script where every single character is then Unicode escaped.
///
/// A file created by this function can be decoded by:
/// * this crate
/// * Resource bundles for any version of Java
/// * Any method in java.util.Properties
///
/// Provided implementations for `ByteOutput`:
/// * `&mut Vec<u8>`
/// * `&mut T where T: std::io::Write` (for example &mut File)
///
/// Types typically used as the `source`:
/// * `HashMap<String, String>` (or its reference)
/// * `Vec<(String, String)>` (or its reference/slice)
/// * `Vec<Element>` (or its reference/slice)
///
/// # Errors
/// Propagated from `ByteOutput`
///
/// # Example
/// ```rust
/// use std::collections::HashMap;
/// use std::fs::File;
///
/// use std::io;
///
/// fn example() -> io::Result<()> {
///     let mut my_props: HashMap<String, String> = HashMap::new();
///     my_props.insert("some_key".to_string(), "some_value".to_string());
///     my_props.insert("another_key".to_string(), "another_value".to_string());
///     //...
///
///     let mut output = File::open("/tmp/output.properties")?;
///     jprop::write_ascii(my_props, &mut output, "\n")?;
///     Ok(())
/// }
///
/// fn example2() -> io::Result<()> {
///     let mut my_props: Vec<(String, String)> = Vec::new();
///     my_props.push(("some_key".to_string(), "some_value".to_string()));
///     my_props.push(("another_key".to_string(), "another_value".to_string()));
///     my_props.push(("some_key".to_string(), "duplicated_value".to_string()));
///     //...
///
///     let mut output = File::open("/tmp/output.properties")?;
///     jprop::write_ascii(my_props, &mut output, "\n")?;
///     Ok(())
/// }
/// ```
///
pub fn write_ascii<E, I: Into<Element>>(
    source: impl IntoIterator<Item = I>,
    target: &mut impl ByteOutput<E>,
    line_ending: &str,
) -> Result<(), E> {
    let mut ascii = ASCIIOut(target, PhantomData);
    write(source, &mut ascii, line_ending)
}

/// Serialize a set of elements into a .properties file into some sort of byte-based output.
///
/// This method will write utf-8 bytes into the output and only escape ascii control characters, as well as
/// characters that must be escaped for encoding the .properties file.
/// All other Unicode characters are simply utf-8 encoded as is.
///
/// A file created by this function cannot be decoded by:
/// * java 4 or older.
/// * java 5 to 8 resource bundles unless a system property is set.
/// * java.util.Properties#load(java.io.File/java.io.InputStream) methods.
///     * Even with the latest Java version this won't work, as these methods always assume ISO-8859-1 encoding.
///
/// A file created by this function can be decoded by:
/// * this crate
/// * Humans of average intelligence using a common text editor
/// * `java.util.Properties#load(java.io.Reader)` method if the reader is, for example, an `InputStreamReader` with its charset set to utf-8.
/// * Java 9 or newer resource bundles.
///
/// Provided implementations for `ByteOutput`:
/// * `&mut Vec<u8>`
/// * `&mut T where T: std::io::Write` (for example &mut File)
///
/// Types typically used as the `source`:
/// * `HashMap<String, String>` (or its reference)
/// * `Vec<(String, String)>` (or its reference/slice)
/// * `Vec<Element>` (or its reference/slice)
///
/// # Errors
/// Propagated from `ByteOutput`
///
/// # Example
/// ```rust
/// use std::collections::HashMap;
/// use std::fs::File;
///
/// use std::io;
///
/// fn example() -> io::Result<()> {
///     let mut my_props: HashMap<String, String> = HashMap::new();
///     my_props.insert("some_key".to_string(), "some_value".to_string());
///     my_props.insert("another_key".to_string(), "another_value".to_string());
///     //...
///
///     let mut output = File::open("/tmp/output.properties")?;
///     jprop::write_utf8(my_props, &mut output, "\n")?;
///     Ok(())
/// }
///
/// fn example2() -> io::Result<()> {
///     let mut my_props: Vec<(String, String)> = Vec::new();
///     my_props.push(("some_key".to_string(), "some_value".to_string()));
///     my_props.push(("another_key".to_string(), "another_value".to_string()));
///     my_props.push(("some_key".to_string(), "duplicated_value".to_string()));
///     //...
///
///     let mut output = File::open("/tmp/output.properties")?;
///     jprop::write_ascii(my_props, &mut output, "\n")?;
///     Ok(())
/// }
/// ```
///
pub fn write_utf8<E, I: Into<Element>>(
    source: impl IntoIterator<Item = I>,
    target: &mut impl ByteOutput<E>,
    line_ending: &str,
) -> Result<(), E> {
    let mut utf = UTF8Out(target, PhantomData);
    write(source, &mut utf, line_ending)
}

/// Serialize a set of elements into a .properties file into some sort of character-based output.
/// The escaping/charset depends on the implementation of the `CharacterOutput`.
///
/// If you are looking to write to a file or any other byte-based output,
/// then use the `write_utf8` or `write_ascii` fn's, these fn's write to a byte-based output.
///
/// This function is primarily useful if you want to implement `CharacterOutput` for a custom type,
/// where you handle encoding characters to bytes yourself.
///
/// Provided implementations for `CharacterOutput`:
/// * &mut String - will use utf-8
/// * &mut Vec<char> - has no encoding. (Unicode)
///
/// Types typically used as the `source`:
/// * `HashMap<String, String>` (or its reference)
/// * `Vec<(String, String)>` (or its reference/slice)
/// * `Vec<Element>` (or its reference/slice)
///
/// Any `IntoIterator` over an item which implements Into<Element> can be used.
///
/// # Errors
/// Propagated from the `CharacterOutput`
///
/// # Example
/// ```rust
/// use std::collections::HashMap;
///
/// fn example() {
///     let mut my_props: HashMap<String, String> = HashMap::new();
///     my_props.insert("some_key".to_string(), "some_value".to_string());
///     my_props.insert("another_key".to_string(), "another_value".to_string());
///     //...
///
///     let mut output = String::new();
///     _= jprop::write(my_props, &mut output, "\n");
///
///     assert_eq!("some_key=some_value\nanother_key=another_value\n", &output);
/// }
/// ```
pub fn write<E, I: Into<Element>>(
    source: impl IntoIterator<Item = I>,
    target: &mut impl CharacterOutput<E>,
    line_ending: &str,
) -> Result<(), E> {
    for value in source {
        match value.into() {
            Element::BlankLine => {}
            Element::Comment(comment) => {
                if !comment.starts_with('#') && !comment.starts_with('!') {
                    target.write('#')?;
                }

                for c in comment.chars() {
                    if target.can_write(c) {
                        target.write(c)?;
                        continue;
                    }

                    //I am aware that this is for a comment and the deserializer will not unmangle it.
                    //However, I prefer this over emitting a fallback char such as '?'.
                    escape_unicode(target, c)?;
                }

                for c in line_ending.chars() {
                    target.write(c)?;
                }
            }
            Element::Value(key, value) => {
                for c in key.chars() {
                    match c {
                        '#' | '!' | '=' | ':' | '\\' | ' ' => {
                            target.write('\\')?;
                            target.write(c)?;
                        }
                        '\r' => {
                            target.write('\\')?;
                            target.write('r')?;
                        }
                        '\n' => {
                            target.write('\\')?;
                            target.write('n')?;
                        }
                        '\t' => {
                            target.write('\\')?;
                            target.write('t')?;
                        }
                        '\x0C' => {
                            target.write('\\')?;
                            target.write('f')?;
                        }
                        other => {
                            if !c.is_ascii_control() && target.can_write(other) {
                                target.write(other)?;
                                continue;
                            }

                            escape_unicode(target, other)?;
                        }
                    }
                }

                target.write('=')?;
                let mut had_non_whitespace = false;

                for c in value.chars() {
                    if c == ' ' {
                        if !had_non_whitespace {
                            target.write('\\')?;
                        }
                        target.write(' ')?;
                        continue;
                    }

                    had_non_whitespace = true;
                    match c {
                        '\\' => {
                            target.write('\\')?;
                            target.write(c)?;
                        }
                        '\r' => {
                            target.write('\\')?;
                            target.write('r')?;
                        }
                        '\n' => {
                            target.write('\\')?;
                            target.write('n')?;
                        }
                        '\t' => {
                            target.write('\\')?;
                            target.write('t')?;
                        }
                        '\x0C' => {
                            target.write('\\')?;
                            target.write('f')?;
                        }
                        other => {
                            //We don't trust dodgy parsers to parse ascii control chars even if they are supported by the encoding.
                            if !c.is_ascii_control() && target.can_write(other) {
                                target.write(other)?;
                                continue;
                            }

                            escape_unicode(target, other)?;
                        }
                    }
                }

                for c in line_ending.chars() {
                    target.write(c)?;
                }
            }
        }
    }

    Ok(())
}
