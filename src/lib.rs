//#![feature(io)]
//use std::io::prelude::*;
#![cfg(test)]

extern crate regex;

mod token;
mod lexer;
mod parser;
mod ast;
mod context;

use token::Token;
use lexer::Lexer;
use ast::Expr;
use ast::Binop;
use parser::Parser;
use context::SimpleContext;

#[test]
fn it_lexes() {
    let chars = "  1 + 1  ".chars();
    let cx = SimpleContext::new();
    let lexer = Lexer::new(chars, &cx);

    assert_eq!(lexer.collect(), vec![Token::DecimalInt("1".to_string()), Token::Plus, Token::DecimalInt("1".to_string())]);
}

#[test]
fn it_parses() {
    let chars = "  1 + 1  ".chars();
    let cx = SimpleContext::new();
    let lexer = Lexer::new(chars, &cx);
    let mut parser = Parser::new(lexer);

    assert_eq!(parser.expr().unwrap(), Expr::Binop(Binop::Plus, Box::new(Expr::Number(1.0)), Box::new(Expr::Number(1.0))));
}
