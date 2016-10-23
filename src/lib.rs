//! A JavaScript parsing library.
//!
//! Esprit currently parses all of ES5 and bits of ES6. The goal
//! is to support all of ES6.
//!
//! Currently the parser is hard-coded to produce the Easter AST
//! data structures. Eventually it should be abstracted to support
//! pluggable builders.

extern crate serde;
extern crate serde_json;
extern crate estree;
extern crate unjson;
extern crate easter;
extern crate joker;

pub mod error;
pub mod result;
mod context;
mod tokens;
mod atom;
mod track;
mod parser;
mod state;
mod expr;
mod stack;

// type Parser<I: Iterator<Item=char>> = parser::Parser<I>;

use easter::prog::Script;
use parser::Parser;
use result::Result;

pub fn script(s: &str) -> Result<Script> {
    Parser::from(s).script()
}
