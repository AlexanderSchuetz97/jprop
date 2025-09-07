#![no_std]
extern crate alloc;

use alloc::string::String;
use alloc::vec::Vec;
use core::fmt::{Display, Formatter};
use core::marker::PhantomData;
use core::mem;
use core::str::Chars;
use std::collections::HashMap;

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
    ///
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>>;
}

pub trait ByteInput<E> {
    fn next_byte(&mut self) -> Result<Option<u8>, E>;
}

pub trait PropertyHandler {
    fn handle(&mut self, position: &ParserPosition, value: ParsedValue) -> bool;
}

//https://en.wikipedia.org/wiki/ISO/IEC_8859-1
//0 char is used as a substitute for undefined.
static ISO_8859_1: &[char] = &[
    //0x00-0x0F
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    //0x10-0x1F
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    //0x20-0x2F
    ' ', '!', '"', '#',
    '$', '%', '&', '\'',
    '(', ')', '*', '+',
    ',','-','.','/',
    //0x30-0x3F
    '0', '1', '2', '3',
    '4', '5', '6', '7',
    '8', '9', ':', ';',
    '<', '=', '>', '?',
    //0x40-0x4F
    '@', 'A', 'B', 'C',
    'D', 'E', 'F', 'G',
    'H', 'I', 'J', 'K',
    'L', 'M', 'N', 'O',
    //0x50-0x5F
    'P', 'Q', 'R', 'S',
    'T', 'U', 'V', 'W',
    'X', 'Y', 'Z', '[',
    '\\', ']', '^', '_',
    //0x60-0x6f
    '`', 'a', 'b', 'c',
    'd', 'e', 'f', 'g',
    'h', 'i', 'j', 'k',
    'l', 'm', 'n', 'o',
    //0x70-0x7F
    'p', 'q', 'r', 's',
    't', 'u', 'v', 'w',
    'x', 'y', 'z', '{',
    '|', '}', '~', '\0',
    //0x80-0x8F
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    //0x90-0x9F
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    '\0', '\0', '\0', '\0',
    //0xA0-0xAF
    '\u{00A0}', '¡', '¢', '£',
    '¤', '¥', '¦', '§',
    '¨', '©', 'ª', '«',
    '¬', '\u{00AD}', '®', '¯',
    //0xB0-0xBF
    '°', '±', '²', '³',
    '´', 'µ', '¶', '·',
    '¸', '¹', 'º', '»',
    '¼', '½', '¾', '¿',
    //0xC0-0xCF
    'À', 'Á', 'Â', 'Ã',
    'Ä', 'Å', 'Æ', 'Ç',
    'È', 'É', 'Ê', 'Ë',
    'Ì', 'Í', 'Î', 'Ï',
    //0xD0-0xDF
    'Ð', 'Ñ', 'Ò', 'Ó',
    'Ô', 'Õ', 'Ö', '×',
    'Ø', 'Ù', 'Ú', 'Û',
    'Ü', 'Ý', 'Þ', 'ß',
    //0xE0-0xEF
    'à', 'á', 'â', 'ã',
    'ä', 'å', 'æ', 'ç',
    'è', 'é', 'ê', 'ë',
    'ì', 'í', 'î', 'ï',
    //0xF0-0xFF
    'ð', 'ñ', 'ò', 'ó',
    'ô', 'õ', 'ö', '÷',
    'ø', 'ù', 'ú', 'û',
    'ü', 'ý', 'þ', 'ÿ',
];


struct UTF8<'a, T: ByteInput<E>, E>(&'a mut T, bool, bool, PhantomData<E>);
impl<T: ByteInput<E>, E> CharacterInput<E> for UTF8<'_, T, E> {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>> {
        let mut buf = [0u8; 5];

        buf[0] = match self.0.next_byte().map_err(CharacterInputError::InputError)? {
            None => return Ok(None), //EOF
            Some(d) => d
        };

        let first = buf[0];
        let iso = ISO_8859_1[first as usize];
        if self.2 {
            if iso == '\0' {
                return Err(CharacterInputError::InvalidInput);
            }

            return Ok(Some(iso));
        }

        if iso == '\0'{
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
        } else if first & 0b1111_1100 == 0b1111_1000 {
            5
        } else {
            return Err(CharacterInputError::InvalidInput);
        };

        for n in 1..cnt {
            buf[n] = self.0.next_byte()
                .map_err(CharacterInputError::InputError)?
                .ok_or(CharacterInputError::UnexpectedEof)?;
        }

        str::from_utf8(&buf[0..cnt])
            .ok()
            .and_then(|e| e.chars().next())
            .map(Some)
            .ok_or_else(|| CharacterInputError::InvalidInput)
    }
}
struct ISO88591<'a, T: ByteInput<E>, E>(&'a mut T, PhantomData<E>);

impl<T: ByteInput<E>, E> CharacterInput<E> for ISO88591<'_, T, E> {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<E>> {
        let byte = match self.0.next_byte().map_err(CharacterInputError::InputError)? {
            None => return Ok(None), //EOF
            Some(d) => d
        };

        let iso = ISO_8859_1[byte as usize];
        if iso == '\0' {
            return Err(CharacterInputError::InvalidInput);
        }

        Ok(Some(iso))
    }
}


#[derive(Debug, Eq, PartialEq)]
enum State {
    LineStart,
    CarriageReturn,
    Comment,
    Key,
    KeyWhitespace,
    BeginValue,
    Value,
    Escape(bool),
    MultiLineTrim(bool),
    EscapeCarriageReturn(bool),
    Unicode(bool),
}

#[derive(Debug, Eq, PartialEq, Clone, PartialOrd, Ord, Hash)]
pub enum ParsedValue {
    BlankLine,
    Comment(String),
    Value(String, String),
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, PartialOrd, Ord, Hash, Default)]
pub struct ParserPosition {
    pub character_total: u64,
    pub character_in_line: u64,
    pub line: u64,
}

impl Display for ParserPosition {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("line: {} pos: {}", self.line, self.character_in_line))
    }
}

//Doesnt need to be public.
impl ParserPosition {
    fn next_line(&mut self) {
        self.line += 1;
        self.character_in_line = 0;
    }

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

    /// The CharacterInput decoded an invalid character, which is not a valid Unicode literal, or some other decoding error occurred.
    /// This is not an io error but indicates invalid data/corruption.
    InvalidInput(ParserPosition),

    /// Input io error E occurred.
    InputError(ParserPosition, E),
    /// a character after a \ is invalid.
    InvalidEscapeCharacter(ParserPosition, char),
    /// a \uXXXX escape sequence did not have one of the 'X' characters be a hexadecimal digit.
    InvalidUnicodeEscapeCharacter(ParserPosition, char),
    /// a \uXXXX escape sequence contained a 32-bit hexadecimal number, which is NOT a valid Unicode literal.
    InvalidUnicodeValue(ParserPosition, u32),
}

impl<E: Display> Display for ParserError<E> {
    fn fmt(&self, f: &mut Formatter<'_>) -> core::fmt::Result {
        match self {
            ParserError::UnexpectedEof => f.write_str("UnexpectedEof"),
            ParserError::InvalidInput(pos) => {
                f.write_str("InvalidInput(")?;
                Display::fmt(pos, f)?;
                f.write_str(")")
            },
            ParserError::InputError(pos, e) => {
                f.write_str("InputError(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            ParserError::InvalidEscapeCharacter(pos, e) => {
                f.write_str("InvalidEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            ParserError::InvalidUnicodeEscapeCharacter(pos, e) => {
                f.write_str("InvalidUnicodeEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
            ParserError::InvalidUnicodeValue(pos, e) => {
                f.write_str("InvalidUnicodeEscapeCharacter(")?;
                Display::fmt(pos, f)?;
                f.write_str(", ")?;
                Display::fmt(e, f)?;
                f.write_str(")")
            }
        }
    }
}

impl CharacterInput<()> for Chars<'_> {
    fn next_character(&mut self) -> Result<Option<char>, CharacterInputError<()>> {
        Ok(self.next())
    }
}

struct SliceInput<'a>(&'a [u8], usize);

impl ByteInput<()> for SliceInput<'_> {
    fn next_byte(&mut self) -> Result<Option<u8>, ()> {
        if self.0.len() <= self.1 {
            return Ok(None);
        }

        let r =self.0[self.1];
        self.1 += 1;
        Ok(Some(r))
    }
}

impl<T: Fn(&ParserPosition, ParsedValue) -> bool> PropertyHandler for T {
    fn handle(&mut self, position: &ParserPosition, value: ParsedValue) -> bool {
        self(position, value)
    }
}

#[derive(Default, Debug)]
struct DocHandler(Vec<ParsedValue>);

impl PropertyHandler for DocHandler {
    fn handle(&mut self, _: &ParserPosition, value: ParsedValue) -> bool {
        self.0.push(value);
        true
    }
}

#[cfg(feature = "std")]
#[derive(Default, Debug)]
struct MapHandler(HashMap<String, String>);

#[cfg(feature = "std")]
impl PropertyHandler for MapHandler {
    fn handle(&mut self, _: &ParserPosition, value: ParsedValue) -> bool {
        if let ParsedValue::Value(key, value) = value {
            self.0.insert(key, value);
        }
        true
    }
}

#[derive(Default, Debug)]
struct VecHandler(Vec<(String, String)>);

impl PropertyHandler for VecHandler {
    fn handle(&mut self, _: &ParserPosition, value: ParsedValue) -> bool {
        if let ParsedValue::Value(key, value) = value {
            self.0.push((key, value));
        }
        true
    }
}

#[cfg(feature = "std")]
extern crate std;

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


pub fn parse_iso_8859_1_to_doc<E>(source: &mut impl ByteInput<E>) -> Result<Vec<ParsedValue>, ParserError<E>> {
    let mut result = DocHandler::default();
    parse_iso_8859_18(source, &mut result)?;
    Ok(result.0)
}

pub fn parse_iso_8859_1_to_vec<E>(source: &mut impl ByteInput<E>) -> Result<Vec<(String, String)>, ParserError<E>> {
    let mut result = VecHandler::default();
    parse_iso_8859_18(source, &mut result)?;
    Ok(result.0)
}


#[cfg(feature = "std")]
pub fn parse_iso_8859_1_to_map<E>(source: &mut impl ByteInput<E>) -> Result<HashMap<String, String>, ParserError<E>> {
    let mut result = MapHandler::default();
    parse_iso_8859_18(source, &mut result)?;
    Ok(result.0)
}

pub fn parse_iso_8859_18<E>(source: &mut impl ByteInput<E>, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<E>> {
    let mut n = ISO88591(source, Default::default());
    parse(&mut n, handler)
}


pub fn parse_utf8_to_doc<E>(source: &mut impl ByteInput<E>) -> Result<Vec<ParsedValue>, ParserError<E>> {
    let mut result = DocHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

pub fn parse_utf8_to_vec<E>(source: &mut impl ByteInput<E>) -> Result<Vec<(String, String)>, ParserError<E>> {
    let mut result = VecHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

#[cfg(feature = "std")]
pub fn parse_utf8_to_map<E>(source: &mut impl ByteInput<E>) -> Result<HashMap<String, String>, ParserError<E>> {
    let mut result = MapHandler::default();
    parse_utf8(source, &mut result)?;
    Ok(result.0)
}

pub fn parse_utf8<E>(source: &mut impl ByteInput<E>, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<E>> {
    let mut n = UTF8(source, false, false, Default::default());
    parse(&mut n, handler)
}

pub fn parse_bytes_iso_8859_1_to_doc(bytes: impl AsRef<[u8]>) -> Result<Vec<ParsedValue>, ParserError<()>> {
    let mut result = DocHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

pub fn parse_bytes_iso_8859_1_to_vec(bytes: impl AsRef<[u8]>) -> Result<Vec<(String, String)>, ParserError<()>> {
    let mut result = VecHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

#[cfg(feature = "std")]
pub fn parse_bytes_iso_8859_1_to_map(bytes: impl AsRef<[u8]>) -> Result<HashMap<String, String>, ParserError<()>> {
    let mut result = MapHandler::default();
    parse_bytes_iso_8859_1(bytes, &mut result)?;
    Ok(result.0)
}

pub fn parse_bytes_iso_8859_1(bytes: impl AsRef<[u8]>, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<()>> {
    let n = bytes.as_ref();
    let mut sl = SliceInput(n, 0);
    let mut iso = ISO88591(&mut sl, Default::default());
    parse(&mut iso, handler)
}

pub fn parse_bytes_utf8_to_doc(bytes: impl AsRef<[u8]>) -> Result<Vec<ParsedValue>, ParserError<()>> {
    let mut result = DocHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

pub fn parse_bytes_utf8_to_vec(bytes: impl AsRef<[u8]>) -> Result<Vec<(String, String)>, ParserError<()>> {
    let mut result = VecHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

#[cfg(feature = "std")]
pub fn parse_bytes_utf8_to_map(bytes: impl AsRef<[u8]>) -> Result<HashMap<String, String>, ParserError<()>> {
    let mut result = MapHandler::default();
    parse_bytes_utf8(bytes, &mut result)?;
    Ok(result.0)
}

pub fn parse_bytes_utf8(bytes: impl AsRef<[u8]>, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<()>> {
    let n = bytes.as_ref();
    let mut sli = SliceInput(n, 0);
    let mut utf = UTF8(&mut sli, false, false, Default::default());
    parse(&mut utf, handler)
}

pub fn parse_str_to_doc(str: impl AsRef<str>) -> Result<Vec<ParsedValue>, ParserError<()>> {
    let mut result = DocHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

pub fn parse_str_to_vec(str: impl AsRef<str>) -> Result<Vec<(String, String)>, ParserError<()>> {
    let mut result = VecHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

#[cfg(feature = "std")]
pub fn parse_str_to_map(str: impl AsRef<str>) -> Result<HashMap<String, String>, ParserError<()>> {
    let mut result = MapHandler::default();
    parse_str(str, &mut result)?;
    Ok(result.0)
}

pub fn parse_str(str: impl AsRef<str>, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<()>> {
    let str = str.as_ref();
    let mut input = str.chars();
    parse(&mut input, handler)
}

/// Low-level parsing function.
/// There should be no need to use this function directly unless you need to, for example, parse
/// in a custom encoding that the JVM itself cannot even read.
pub fn parse<T: CharacterInput<E>, E>(input: &mut T, handler: &mut impl PropertyHandler) -> Result<ParserPosition, ParserError<E>> {
    let mut pos = ParserPosition::default();
    let mut state = State::LineStart;
    let mut key_buf = String::new();
    let mut value_buf = String::new();
    let mut unicode_buf = String::with_capacity(4);

    'parse_next: loop {
        let next_char = match input.next_character() {
            Ok(Some(c)) => c,
            Ok(None) => return match state {
                State::CarriageReturn | State::LineStart => {
                    handler.handle(&pos, ParsedValue::BlankLine);
                    Ok(pos)
                },
                State::Comment => {
                    handler.handle(&pos, ParsedValue::Comment(key_buf));
                    return Ok(pos);
                }
                State::Key | State::KeyWhitespace | State::BeginValue => {
                    handler.handle(&pos, ParsedValue::Value(key_buf, String::new()));
                    Ok(pos)
                }
                State::Value => {
                    handler.handle(&pos, ParsedValue::Value(key_buf, value_buf));
                    Ok(pos)
                }
                State::MultiLineTrim(is_value) | State::EscapeCarriageReturn(is_value) => {
                    if is_value {
                        handler.handle(&pos, ParsedValue::Value(key_buf, value_buf));
                    } else {
                        handler.handle(&pos, ParsedValue::Value(key_buf, String::new()));
                    }

                    Ok(pos)
                }
                State::Escape(_) => Err(ParserError::UnexpectedEof),
                State::Unicode(_) => Err(ParserError::UnexpectedEof),
            },
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
                        },
                        '#' | '!' => {
                            state = State::Comment;
                            continue;
                        },
                        '\r' => {
                            //Either CRLF (Windows) or CR (Mac)
                            if !handler.handle(&pos, ParsedValue::BlankLine) {
                                return Ok(pos)
                            }
                            state = State::CarriageReturn;
                            continue 'parse_next;
                        },
                        '\n' => {
                            //LF (Unix)
                            pos.next_line();
                            if !handler.handle(&pos, ParsedValue::BlankLine) {
                                return Ok(pos)
                            }
                            continue 'parse_next;
                        },
                        _=> {
                            state = State::Key;
                            continue;
                        },
                    }
                }
                State::CarriageReturn => {
                    pos.next_line();

                    match next_char {
                        '\n' => {
                            //Was CRLF (Windows)
                            state = State::LineStart;
                            continue 'parse_next;
                        }
                        _=> {
                            //Was CR (Mac)
                            state = State::LineStart;
                            continue;
                        }
                    }
                }
                State::Comment => {
                    match next_char {
                        '\r' => {
                            if !handler.handle(&pos, ParsedValue::Comment(mem::replace(&mut key_buf, String::new()))) {
                                return Ok(pos)
                            }
                            state = State::CarriageReturn;
                            continue 'parse_next;
                        }
                        '\n' => {
                            if !handler.handle(&pos, ParsedValue::Comment(mem::replace(&mut key_buf, String::new()))) {
                                return Ok(pos)
                            }
                            pos.next_line();
                            state = State::LineStart;
                            continue 'parse_next;
                        }
                        _=> {
                            key_buf.push(next_char);
                            continue 'parse_next;
                        }
                    }
                }
                State::Key => {
                    match next_char {
                        '\r' => {
                            if !handler.handle(&pos, ParsedValue::Value(mem::replace(&mut key_buf, String::new()), String::new())) {
                                return Ok(pos)
                            }
                            state = State::CarriageReturn;
                            continue 'parse_next;
                        }
                        '\n' => {
                            if !handler.handle(&pos, ParsedValue::Value(mem::replace(&mut key_buf, String::new()), String::new())) {
                                return Ok(pos)
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
                        _=> {
                            key_buf.push(next_char);
                            continue 'parse_next;
                        }
                    }
                }
                State::Escape(is_value) => {
                    match next_char {
                        'u' => {
                            state = State::Unicode(is_value);
                            continue 'parse_next;
                        },
                        'n' => {
                            if is_value {
                                value_buf.push('\n');
                                state = State::Value
                            } else {
                                key_buf.push('\n');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        't' => {
                            if is_value {
                                value_buf.push('\t');
                                state = State::Value
                            } else {
                                key_buf.push('\t');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        'r' => {
                            if is_value {
                                value_buf.push('\r');
                                state = State::Value
                            } else {
                                key_buf.push('\r');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        'f' => {
                            if is_value {
                                value_buf.push('\x0C');
                                state = State::Value
                            } else {
                                key_buf.push('\x0C');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        ' ' => {
                            if is_value {
                                value_buf.push(' ');
                                state = State::Value
                            } else {
                                key_buf.push(' ');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        '\\' => {
                            if is_value {
                                value_buf.push('\\');
                                state = State::Value
                            } else {
                                key_buf.push('\\');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        '=' => {
                            if is_value {
                                value_buf.push('=');
                                state = State::Value
                            } else {
                                key_buf.push('=');
                                state = State::Key;
                            }
                            continue 'parse_next;
                        }
                        ':' => {
                            if is_value {
                                value_buf.push(':');
                                state = State::Value
                            } else {
                                key_buf.push(':');
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
                        _=> {
                            return Err(ParserError::InvalidEscapeCharacter(pos, next_char));
                        }
                    }
                }
                State::EscapeCarriageReturn(is_value) => {
                    state = State::MultiLineTrim(is_value);
                    if next_char == '\n' {
                        continue 'parse_next;
                    }
                    continue;
                }
                State::MultiLineTrim(is_value) => {
                    match next_char {
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
                    }
                }

                State::Unicode(is_value) => {
                    if next_char.is_ascii_hexdigit() {
                        unicode_buf.push(next_char);
                        if unicode_buf.len() == 4 {
                            let unicode = u32::from_str_radix(&unicode_buf, 16)
                                //This can't really fail, 4 hex characters will always fit into u32 if is_ascii_hexdigit is true.
                                .expect("State::KeyUnicode from_str_radix");
                            unicode_buf.clear();

                            if let Ok(char_code) = char::try_from(unicode) {
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
                        continue 'parse_next;
                    }
                    return Err(ParserError::InvalidUnicodeEscapeCharacter(pos, next_char));
                }
                State::KeyWhitespace => {
                    match next_char {
                        ' ' | '\t' | '\x0C' => {
                            continue 'parse_next;
                        }
                        ':' | '=' => {
                            state = State::BeginValue;
                            continue 'parse_next;
                        }
                        _=> {
                            state = State::BeginValue;
                            continue;
                        }
                    }
                }
                State::BeginValue => {
                    match next_char {
                        ' ' | '\t' | '\x0C' => {
                            continue 'parse_next;
                        }
                        _=> {
                            state = State::Value;
                            continue;
                        }
                    }
                }
                State::Value => {
                    match next_char {
                        '\\' => {
                            state = State::Escape(true);
                            continue 'parse_next;
                        }
                        '\r' => {
                            if !handler.handle(&pos, ParsedValue::Value(mem::replace(&mut key_buf, String::new()), mem::replace(&mut value_buf, String::new()))) {
                                return Ok(pos)
                            }
                            state = State::CarriageReturn;
                            continue 'parse_next;
                        }
                        '\n' => {
                            if !handler.handle(&pos, ParsedValue::Value(mem::replace(&mut key_buf, String::new()), mem::replace(&mut value_buf, String::new()))) {
                                return Ok(pos)
                            }
                            pos.next_line();
                            state = State::LineStart;
                            continue 'parse_next;
                        }
                        _=> {
                            value_buf.push(next_char);
                            continue 'parse_next;
                        }
                    }
                },
            }

            //Save me clippy!
            #[expect(unreachable_code)]
            {
                let _ = ();
                unreachable!();
            }
        }
    }
}