use token::{Token, TokenData};
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use ast::Binop;
use ast::Expr;
use lexer::LexError;
use context::Context;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    LexError(LexError)
}

pub struct Parser<I> {
    lexer: Lexer<I>,
    cx: Rc<Cell<Context>>
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    // FIXME: various from_<type> constructors (Iterator, Lexer, String, str)
    pub fn new(lexer: Lexer<I>, cx: Rc<Cell<Context>>) -> Parser<I> {
        Parser { lexer: lexer, cx: cx }
    }
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        let left = match self.lexer.read_token() {
            Ok(Token { data: TokenData::DecimalInt(_), .. }) => Expr::Number(1.0),
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let op = match self.lexer.read_token() {
            Ok(Token { data: TokenData::Plus, .. }) => Binop::Plus,
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let right = match self.lexer.read_token() {
            Ok(Token { data: TokenData::DecimalInt(_), .. }) => Expr::Number(1.0),
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        Ok(Expr::Binop(op, Box::new(left), Box::new(right)))
    }
}
