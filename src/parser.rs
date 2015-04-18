use loc::*;
use token::{Token, TokenData, ReservedWord};
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use ast::{Expr, ExprData, Binop, BinopTag, Script, ScriptData, Stmt, StmtData, Decl, StmtListItem, VarDtor, VarDtorData, Patt, PattData, AutoSemi, Id, IdData};
use lexer::LexError;
use context::Context;

pub type Parse<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    FailedASI(Token),
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

struct SpanTracker {
    start: Posn
}

impl SpanTracker {
    fn end<I, T>(&self, parser: &Parser<I>, data: T) -> Loc<T>
      where I: Iterator<Item=char>
    {
        Loc { data: data, span: Some(Span { start: self.start, end: parser.posn() }) }
    }

    fn end_with_auto_semi<I, T>(&self, parser: &mut Parser<I>, node: T, require_newline: bool)
        -> Parse<Loc<AutoSemi<T>>>
      where I: Iterator<Item=char>
    {
        let before = parser.posn();
        let found_newline = try!(parser.skip_newlines());
        match try!(parser.peek()) {
            &Token { data: TokenData::Semi, .. } => {
                parser.skip();
                Ok(Loc {
                    data: AutoSemi { inserted: false, node: node },
                    span: Some(Span { start: self.start, end: parser.posn() })
                })
            }
            &Token { data: TokenData::RBrace, .. }
          | &Token { data: TokenData::EOF, .. } => {
                Ok(Loc {
                    data: AutoSemi { inserted: true, node: node },
                    span: Some(Span { start: self.start, end: before })
                })
            }
            _ => {
                if require_newline && !found_newline {
                    let token = try!(parser.read());
                    return Err(ParseError::FailedASI(token));
                }
                Ok(Loc {
                    data: AutoSemi { inserted: true, node: node },
                    span: Some(Span { start: self.start, end: before })
                })
            }
        }
    }
}

impl<I> Parser<I>
  where I: Iterator<Item=char>
{
    pub fn script(&mut self) -> Parse<Script> {
        let items = try!(self.statement_list());
        Ok(Script { span: self.vec_span(&items), data: ScriptData { body: items } })
    }

    fn vec_span<T: HasSpan>(&self, v: &Vec<T>) -> Option<Span> {
        let len = v.len();
        if len == 0 {
            let here = self.posn();
            return Some(Span { start: here, end: here });
        }
        span(&v[0], &v[len - 1])
    }

    fn posn(&self) -> Posn {
        self.lexer.posn()
    }

    fn start(&self) -> SpanTracker {
        SpanTracker { start: self.posn() }
    }

    fn skip(&mut self) -> Parse<()> {
        self.lexer.skip_token().map_err(ParseError::LexError)
    }

    fn read(&mut self) -> Parse<Token> {
        self.lexer.read_token().map_err(ParseError::LexError)
    }

    fn peek(&mut self) -> Parse<&Token> {
        self.lexer.peek_token().map_err(ParseError::LexError)
    }

    fn skip_newlines(&mut self) -> Parse<bool> {
        let mut cx = self.cx.get();
        let newlines = cx.newlines;
        cx.newlines = true;
        self.cx.set(cx);
        let result = (try!(self.peek()).data == TokenData::Newline) && { try!(self.skip()); true };
        cx.newlines = newlines;
        self.cx.set(cx);
        Ok(result)
    }

    fn peek_same_line(&mut self) -> Parse<&Token> {
        let mut cx = self.cx.get();
        let newlines = cx.newlines;
        cx.newlines = true;
        self.cx.set(cx);
        let result = self.lexer.peek_token().map_err(ParseError::LexError);
        cx.newlines = newlines;
        self.cx.set(cx);
        result
    }

    fn expect(&mut self, expected: TokenData) -> Parse<()> {
        let token = try!(self.read());
        if token.data != expected {
            return Err(ParseError::UnexpectedToken(token));
        }
        Ok(())
    }

    fn matches(&mut self, expected: TokenData) -> Parse<bool> {
        let token = try!(self.read());
        if token.data != expected {
            self.lexer.unread_token(token);
            return Ok(false);
        }
        return Ok(true);
    }

    fn reread(&mut self, expected: TokenData) -> Token {
        debug_assert!(self.peek().map(|actual| actual.data == expected).unwrap_or(false));
        self.read().unwrap()
    }

    fn span<F, T>(&mut self, parse: &mut F) -> Parse<Loc<T>>
      where F: FnMut(&mut Self) -> Parse<T>
    {
        let start = self.posn();
        let data = try!(parse(self));
        let end = self.posn();
        Ok(Loc { data: data, span: Some(Span { start: start, end: end }) })
    }

    fn statement_list(&mut self) -> Parse<Vec<StmtListItem>> {
        let mut items = Vec::new();
        while !try!(self.peek()).follow_statement_list() {
            match try!(self.statement()) {
                Some(stmt) => { items.push(StmtListItem::Stmt(stmt)); }
                None => { items.push(StmtListItem::Decl(try!(self.declaration()))); }
            }
        }
        Ok(items)
    }

    pub fn declaration(&mut self) -> Parse<Decl> {
        unimplemented!()
    }

    pub fn statement(&mut self) -> Parse<Option<Stmt>> {
        match try!(self.peek()).data {
            TokenData::LBrace                           => self.block_statement().map(Some),
            TokenData::Reserved(ReservedWord::Var)      => self.var_statement().map(Some),
            TokenData::Semi                             => self.empty_statement().map(Some),
            TokenData::Reserved(ReservedWord::If)       => self.if_statement().map(Some),
            TokenData::Reserved(ReservedWord::Do)       => self.do_statement().map(Some),
            TokenData::Reserved(ReservedWord::While)    => self.while_statement().map(Some),
            TokenData::Reserved(ReservedWord::For)      => self.for_statement().map(Some),
            TokenData::Reserved(ReservedWord::Switch)   => self.switch_statement().map(Some),
            TokenData::Reserved(ReservedWord::Continue) => self.continue_statement().map(Some),
            TokenData::Reserved(ReservedWord::Break)    => self.break_statement().map(Some),
            TokenData::Reserved(ReservedWord::Return)   => self.return_statement().map(Some),
            TokenData::Reserved(ReservedWord::With)     => self.with_statement().map(Some),
            TokenData::Reserved(ReservedWord::Throw)    => self.throw_statement().map(Some),
            TokenData::Reserved(ReservedWord::Try)      => self.try_statement().map(Some),
            TokenData::Reserved(ReservedWord::Debugger) => self.debugger_statement().map(Some),
            _ => Ok(None)
        }
    }

    fn block_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::LBrace);
            let items = try!(this.statement_list());
            try!(this.expect(TokenData::RBrace));
            Ok(StmtData::Block(items))
        })
    }

    fn var_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(ReservedWord::Var));
        let dtors = try!(self.var_declaration_list());
        Ok(try!(span.end_with_auto_semi(self, dtors, true)).map(StmtData::Var))
    }

    fn auto_semi<T>(&mut self, node: T) -> Parse<AutoSemi<T>> {
        unimplemented!()
    }

    fn var_declaration_list(&mut self) -> Parse<Vec<VarDtor>> {
        let mut items = Vec::new();
        items.push(try!(self.var_declaration()));
        // FIXME: this is going to skip newlines in lookahead and break ASI at the end.
        //        should I always store Newline tokens in the lookahead buffer?
        while try!(self.matches(TokenData::Comma)) {
            items.push(try!(self.var_declaration()));
        }
        Ok(items)
    }

    fn id(&mut self) -> Parse<Id> {
        let token = try!(self.read());
        match token {
            Token { data: TokenData::Identifier(name), span: span } => Ok(Id::new(name, span)),
            _ => Err(ParseError::UnexpectedToken(token))
        }
    }

    fn var_declaration(&mut self) -> Parse<VarDtor> {
        self.span(&mut |this| {
            let lhs = try!(this.id()).into_patt();
            let rhs = if try!(this.matches(TokenData::Assign)) {
                Some(try!(this.assignment_expression()))
            } else {
                None
            };
            Ok(VarDtorData { id: lhs, init: rhs })
        })
    }

    fn empty_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn if_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn do_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn while_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn for_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn switch_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn break_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn continue_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn return_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn with_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn throw_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn try_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn debugger_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

/*
    pub fn module(&mut self) -> Parse<Module> {
        unimplemented!()
    }
*/

    fn primary_expression(&mut self) -> Parse<Expr> {
        let token = try!(self.read());
        match token.data {
            TokenData::Identifier(name) => {
                Ok(Id::new(name, token.span).into_expr())
            }
            TokenData::Reserved(ReservedWord::Null) => {
                Ok(ExprData::Null.into_loc(token.span))
            }
            TokenData::Reserved(ReservedWord::This) => {
                Ok(ExprData::This.into_loc(token.span))
            }
            TokenData::Number(literal) => {
                Ok(ExprData::Number(literal).into_loc(token.span))
            }
            _ => Err(ParseError::UnexpectedToken(token))
        }
    }

    fn assignment_expression(&mut self) -> Parse<Expr> {
        self.primary_expression()
    }

    pub fn expr(&mut self) -> Parse<Expr> {
        let left = match self.lexer.read_token() {
            Ok(Token { data: TokenData::Number(literal), span }) => Expr { span: span, data: ExprData::Number(literal) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let op = match self.lexer.read_token() {
            Ok(Token { data: TokenData::Plus, span }) => Binop { span: span, data: BinopTag::Plus },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let right = match self.lexer.read_token() {
            Ok(Token { data: TokenData::Number(literal), span }) => Expr { span: span, data: ExprData::Number(literal) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        Ok(Expr {
            span: span(&left, &right),
            data: ExprData::Binop(op, Box::new(left), Box::new(right))
        })
    }
}

#[cfg(test)]
mod tests {

    use test::{deserialize_parser_tests, ParserTest};
    use lexer::{Lexer, LexError};
    use context::{Context, Mode};
    use token::{Token, TokenData};
    use std::cell::Cell;
    use std::rc::Rc;
    use std::str::Chars;
    use parser::{Parser, Parse};
    use ast::*;
    use loc::*;

    fn parse(source: &String) -> Parse<Script> {
        let chars = source.chars();
        let context = Context {
            newlines: false,
            operator: false,
            comments: false,
            generator: false,
            mode: Mode::Sloppy
        };
        let cx = Rc::new(Cell::new(context));
        let lexer = Lexer::new(chars, cx.clone());
        let mut parser = Parser::new(lexer, cx.clone());
        parser.script()
    }

    #[test]
    pub fn go() {
        let tests = deserialize_parser_tests(include_str!("../tests/parser/tests.json"));
        for ParserTest { source, expected, .. } in tests {
            let result = parse(&source);
            match (result, expected) {
                (Ok(mut actual_ast), Ok(expected_ast)) => {
                    actual_ast = actual_ast.erase_loc();
                    // println!("{}", source);
                    // println!("expected AST: {:?}", expected_ast);
                    // println!("actual AST:   {:?}", actual_ast);
                    // println!("{:?}", actual_ast == expected_ast);
                    assert!(actual_ast == expected_ast);
                }
                (Err(_), Err(_)) => { }
                (Ok(_), Err(_)) => {
                    panic!("expected error");
                }
                (Err(_), Ok(_)) => {
                    panic!("unexpected error")
                }
            }
        }
    }

}
