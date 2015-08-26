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
mod test;

// type Parser<I: Iterator<Item=char>> = parser::Parser<I>;

use std::cell::Cell;
use std::rc::Rc;
use joker::Lexer;
use joker::context::{SharedContext, Mode};
use easter::prog::Script;
use parser::Parser;
use result::Result;

pub fn script(source: &String) -> Result<Script> {
    let chars = source.chars();
    let cx = Rc::new(Cell::new(SharedContext::new(Mode::Sloppy)));
    let lexer = Lexer::new(chars, cx.clone());
    let mut parser = Parser::new(lexer, cx.clone());
    parser.script()
}
