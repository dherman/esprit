//#![feature(io)]
//use std::io::prelude::*;
#![cfg(test)]

mod token;
mod lexer;
mod parser;
mod ast;

use token::Token;
use lexer::Lexer;
use ast::Expr;
use ast::Binop;
use parser::Parser;

#[test]
fn it_lexes() {
    let chars = "  1 + 1  ".chars();
    let lexer = Lexer::new(chars);

    assert_eq!(lexer.collect(), vec![Token::DecimalInt("1".to_string()), Token::Plus, Token::DecimalInt("1".to_string())]);
}

#[test]
fn it_parses() {
    let chars = "  1 + 1  ".chars();
    let lexer = Lexer::new(chars);
    let mut parser = Parser::new(lexer);

    assert_eq!(parser.expr().unwrap(), Expr::Binop(Binop::Plus, Box::new(Expr::Number(1.0)), Box::new(Expr::Number(1.0))));
}
