use token::Token;
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use ast::Binop;
use ast::Expr;
use context::Context;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedEOF,
    UnexpectedToken(Token)
}

pub struct Parser<I> {
    lexer: Lexer<I>,
    cx: Rc<Cell<Context>>
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    pub fn new(lexer: Lexer<I>, cx: Rc<Cell<Context>>) -> Parser<I> {
        Parser { lexer: lexer, cx: cx }
    }
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        let left = match self.lexer.next() {
            Some(Token::DecimalInt(_)) => Expr::Number(1.0),
            Some(t) => return Err(ParseError::UnexpectedToken(t)),
            None => return Err(ParseError::UnexpectedEOF)
        };

        let op = match self.lexer.next() {
            Some(Token::Plus) => Binop::Plus,
            Some(t) => return Err(ParseError::UnexpectedToken(t)),
            None => return Err(ParseError::UnexpectedEOF)
        };

        let right = match self.lexer.next() {
            Some(Token::DecimalInt(_)) => Expr::Number(1.0),
            Some(t) => return Err(ParseError::UnexpectedToken(t)),
            None => return Err(ParseError::UnexpectedEOF)
        };

        Ok(Expr::Binop(op, Box::new(left), Box::new(right)))
    }
}
