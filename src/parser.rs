use token::{Token, TokenData, Span, HasSpan, ReservedWord, span};
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use ast::{Expr, ExprData, Binop, BinopTag, Script, ScriptData, Stmt, StmtData};
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

trait Follows {
    fn follow_statement_list(&self) -> bool;
}

impl Follows for Token {
    // follow(StatementList) =
    //   follow(CaseClause)
    // U follow(DefaultClause)
    // U follow(FunctionBody)
    // U follow(ScriptBody)
    // U follow(ModuleBody)
    // U { '}' }
    // = { '}', 'case', 'default', EOF }
    // 
    // follow(CaseClause) =
    //   { '}' }
    // U first(CaseClause)
    // U first(DefaultClause)
    // = { '}', 'case', 'default' }
    // 
    // follow(DefaultClause) =
    //   { '}' }
    // U first(CaseClause)
    // = { '}', 'case' }
    // 
    // first(CaseClause) = { 'case' }
    // first(DefaultClause) = { 'default' }
    // 
    // follow(ScriptBody) = { EOF }
    // follow(ModuleBody) = { EOF }
    fn follow_statement_list(&self) -> bool {
        match self.data {
              TokenData::Reserved(ReservedWord::Case)
            | TokenData::Reserved(ReservedWord::Default)
            | TokenData::EOF
            | TokenData::RBrace => true,
            _ => false
        }
    }
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    pub fn script(&mut self) -> Result<Script, ParseError> {
        let stmts = try!(self.statement_list());
        Ok(Script { span: self.vec_span(&stmts), data: ScriptData { body: stmts } })
    }

    fn vec_span<T: HasSpan>(&self, v: &Vec<T>) -> Span {
        let len = v.len();
        if len == 0 {
            let here = self.lexer.posn();
            return Span { start: here, end: here };
        }
        span(&v[0], &v[len - 1])
    }

    fn peek(&mut self) -> Result<&Token, ParseError> {
        self.lexer.peek_token().map_err(ParseError::LexError)
    }

    pub fn statement_list(&mut self) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        while !try!(self.peek()).follow_statement_list() {
            stmts.push(try!(self.statement()))
        }
        Ok(stmts)
    }

    pub fn statement(&mut self) -> Result<Stmt, ParseError> {
        unimplemented!()
    }

/*
    pub fn module(&mut self) -> Result<Module, ParseError> {
        unimplemented!()
    }
*/

    pub fn expr(&mut self) -> Result<Expr, ParseError> {
        let left = match self.lexer.read_token() {
            Ok(Token { data: TokenData::DecimalInt(_), span }) => Expr { span: span, data: ExprData::Number(1.0) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let op = match self.lexer.read_token() {
            Ok(Token { data: TokenData::Plus, span }) => Binop { span: span, data: BinopTag::Plus },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let right = match self.lexer.read_token() {
            Ok(Token { data: TokenData::DecimalInt(_), span }) => Expr { span: span, data: ExprData::Number(1.0) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        Ok(Expr {
            span: span(&left, &right),
            data: ExprData::Binop(op, Box::new(left), Box::new(right))
        })
    }
}
