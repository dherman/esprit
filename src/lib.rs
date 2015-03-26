//#![feature(io)]
//use std::io::prelude::*;
#![cfg(test)]

extern crate regex;

mod token;
mod lexer;
mod parser;
mod ast;
mod context;
mod eschar;
mod reader;
mod tokbuf;

use std::cell::Cell;
use std::rc::Rc;
use token::Token;
use lexer::Lexer;
use ast::Expr;
use ast::Binop;
use parser::Parser;
use context::Context;
use context::Mode;

#[test]
fn it_lexes() {
    let chars = "  1 + 1  ".chars();
    let cx = Rc::new(Cell::new(Context {
        asi: false,
        operator: false,
        comment_tokens: false,
        generator: false,
        mode: Mode::Sloppy
    }));
    let lexer = Lexer::new(chars, cx.clone());

    assert_eq!(lexer.collect(), vec![Token::DecimalInt("1".to_string()), Token::Plus, Token::DecimalInt("1".to_string())]);
}

#[test]
fn it_parses() {
    let chars = "  1 + 1  ".chars();
    let cx = Rc::new(Cell::new(Context {
        asi: false,
        operator: false,
        comment_tokens: false,
        generator: false,
        mode: Mode::Sloppy
    }));
    let lexer = Lexer::new(chars, cx.clone());
    let mut parser = Parser::new(lexer, cx.clone());

    assert_eq!(parser.expr().unwrap(), Expr::Binop(Binop::Plus, Box::new(Expr::Number(1.0)), Box::new(Expr::Number(1.0))));
}
