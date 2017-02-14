extern crate ucd;

#[cfg(test)]
extern crate serde;

#[cfg(test)]
#[macro_use]
extern crate serde_derive;

#[cfg(test)]
extern crate serde_json;

pub mod word;
pub mod token;
pub mod lexer;
mod char;
mod reader;
mod test;
pub mod track;
pub mod error;
pub mod result;

pub use lexer::Lexer;
