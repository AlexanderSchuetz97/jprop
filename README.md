# jprop
no-std parser for java `.properties` files that actually works 

## Motivation
As everyone should be aware, there are at least a dozen .properties parsers
on crates.io; unfortunately, none of them work properly.

A glance into https://en.wikipedia.org/wiki/.properties should reveal 
that there are many things that can go wrong when trying to parse .properties files.

I have evaluated five different .properties parsers, and none of them are capable of parsing the
files I need to parse, so I have come to the conclusion that I sadly have to make my own parser...

## Core features
* 0 dependencies
* no-std (feature gated, default features use std to allow for decoding of `std::io::Read`)
* full UTF-8 and ISO-8859-1 encoding support
  * This means you can parse your file with special symbols without \u escaping them.
    * This is widespread in java resource bundles, and half the parsers on crates.io already fail here.
  * UTF-8 implements a similar fallback to ISO-8859-1 as java does
  * fyi: java 4 and older only supports ISO-8859-1
  * fyi: java 5 to 8 uses ISO-8859-1 by default but can be configured to use UTF-8
  * fyi: java 9 and newer uses UTF-8 by default and uses a fallback to read ISO-8859-1.

* decoding of 'byte' or 'string' sources
  * byte as in from an io::Read or `&[u8]` and friends
  * string as in from a `&str`

* decoding of comments
  * Yes, in some use-cases this is sadly relevant.

* document decoding
  * Into `Vec<(String, String)>` or `HashMap<String, String>` (std only),
  * Into `Vec<Element>` Element being an enum, which is either Comment or Key+Value pair.

* stream decoding
  * Trait-based callback that accepts essentially an `fn(Element) -> bool`
    * element being an enum, which is either Comment or Key+Value pair.
    * Return false to stop parsing!

+ errors contain position information
  + character index,
  + character in line,
  + line number

## Example Usage

Parse key, value as a `HashMap<String, String>`
```rust
use std::collections::HashMap;
use std::fs::File;

pub fn read_test_properties() {
    let mut file = File::open("test.properties").expect("Failed to parse file");
    let data: HashMap<String, String> = jprop::parse_utf8_to_map(&mut file).expect("Failed to parse file");
    //use data here
    //profit?
}
```

Parse the entire document as a stream.
```rust
use jprop::{Element, ParserPosition};
use std::fs::File;

//Your handler would probably have some fields and be more complex.
struct Handler;

impl jprop::PropertyHandler for Handler {
  fn handle(&mut self, position: &ParserPosition, value: Element) -> bool {
    println!("Position {}:{}", position.line+1, position.character_in_line+1);
    match value {
      Element::BlankLine => println!(),
      Element::Comment(text) => println!("{}", text),
      Element::Value(key, value) => println!("{} = {}", key, value),
    }
    true
  }
}

pub fn read_test_properties_as_stream() {
  let mut file = File::open("test.properties").expect("Failed to parse file");
  let mut handler = Handler;
  jprop::parse_utf8(&mut file, &mut handler).expect("Failed to parse file");
  // use handler here
}
```

Parse from a &str
```rust
use std::collections::HashMap;

pub fn read_properties_from_string() {
    let test = "abc=abc\nbcd=bcd\n#..."; //or 'include_str!("some_file.properties");' but that only works with utf-8 files.
    let data: HashMap<String, String> = jprop::parse_str_to_map(test).expect("Failed to parse str");
    //use data here
    //profit?
}
```