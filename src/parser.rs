use track::*;
use token::{Token, TokenData, Word};
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use std::mem::replace;
use ast::*;
use lexer::LexError;
use context::{SharedContext, ParserContext, LabelType};

pub type Parse<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    FailedASI(Token),
    LexError(LexError),
    TopLevelReturn(Span),
    IllegalBreak(Token),
    InvalidLabel(Id)
}

pub struct Parser<I> {
    lexer: Lexer<I>,
    shared_cx: Rc<Cell<SharedContext>>,
    parser_cx: ParserContext
}

impl<I> Parser<I> where I: Iterator<Item=char> {
    // FIXME: various from_<type> constructors (Iterator, Lexer, String, str)
    pub fn new(lexer: Lexer<I>, cx: Rc<Cell<SharedContext>>) -> Parser<I> {
        Parser { lexer: lexer, shared_cx: cx, parser_cx: ParserContext::new() }
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
        match self.value {
              TokenData::Reserved(Word::Case)
            | TokenData::Reserved(Word::Default)
            | TokenData::EOF
            | TokenData::RBrace => true,
            _ => false
        }
    }
}

#[derive(Eq, PartialEq)]
enum Newline {
    Required,
    Optional
}

trait HasLabelType {
    fn label_type(&self) -> LabelType;
}

impl HasLabelType for Token {
    fn label_type(&self) -> LabelType {
        match self.value {
            TokenData::Reserved(Word::Do)
          | TokenData::Reserved(Word::While)
          | TokenData::Reserved(Word::For) => LabelType::Iteration,
            _                              => LabelType::Statement
        }
    }
}

struct SpanTracker {
    start: Posn
}

impl SpanTracker {
    fn end<I, T>(&self, parser: &Parser<I>, value: T) -> Tracked<T>
      where I: Iterator<Item=char>
    {
        Tracked { value: value, location: Some(Span { start: self.start, end: parser.posn() }) }
    }

    fn end_with_auto_semi<I, T, F>(&self, parser: &mut Parser<I>, newline: Newline, cons: F)
        -> Parse<Tracked<T>>
      where I: Iterator<Item=char>,
            F: FnOnce(Semi) -> T
    {
        let before = parser.posn();
        match try!(parser.peek()) {
            &Token { value: TokenData::Semi, location, .. } => {
                parser.skip();
                Ok(Tracked {
                    value: cons(Semi::Explicit(Some(location.start))),
                    location: Some(Span { start: self.start, end: parser.posn() })
                })
            }
            &Token { value: TokenData::RBrace, .. }
          | &Token { value: TokenData::EOF, .. } => {
                Ok(Tracked {
                    value: cons(Semi::Inserted),
                    location: Some(Span { start: self.start, end: before })
                })
            }
            &Token { newline: found_newline, .. } => {
                if newline == Newline::Required && !found_newline {
                    let token = try!(parser.read());
                    return Err(ParseError::FailedASI(token));
                }
                Ok(Tracked {
                    value: cons(Semi::Inserted),
                    location: Some(Span { start: self.start, end: before })
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
        Ok(Script { location: self.vec_span(&items), value: ScriptData { body: items } })
    }

    fn vec_span<T: Track>(&self, v: &Vec<T>) -> Option<Span> {
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

    fn expect(&mut self, expected: TokenData) -> Parse<()> {
        let token = try!(self.read());
        if token.value != expected {
            return Err(ParseError::UnexpectedToken(token));
        }
        Ok(())
    }

    fn matches(&mut self, expected: TokenData) -> Parse<bool> {
        let token = try!(self.read());
        if token.value != expected {
            self.lexer.unread_token(token);
            return Ok(false);
        }
        return Ok(true);
    }

    fn reread(&mut self, expected: TokenData) -> Token {
        debug_assert!(self.peek().map(|actual| actual.value == expected).unwrap_or(false));
        self.read().unwrap()
    }

    fn span<F, T>(&mut self, parse: &mut F) -> Parse<Tracked<T>>
      where F: FnMut(&mut Self) -> Parse<T>
    {
        let start = self.posn();
        let value = try!(parse(self));
        let end = self.posn();
        Ok(Tracked { value: value, location: Some(Span { start: start, end: end }) })
    }

    fn try_token_at<T, F>(&mut self, op: &mut F) -> Parse<Tracked<T>>
      where F: FnMut(TokenData, Span) -> Result<T, TokenData>
    {
        let Token { location, newline, value: token } = try!(self.read());
        match op(token, location) {
            Ok(node) => Ok(Tracked { location: Some(location), value: node }),
            Err(token) => Err(ParseError::UnexpectedToken(Token {
                location: location,
                newline: newline,
                value: token
            }))
        }
    }

    fn statement_list(&mut self) -> Parse<Vec<StmtListItem>> {
        let mut items = Vec::new();
        while !try!(self.peek()).follow_statement_list() {
            match try!(self.declaration_opt()) {
                Some(decl) => { items.push(StmtListItem::Decl(decl)); }
                None       => { items.push(StmtListItem::Stmt(try!(self.statement()))); }
            }
        }
        Ok(items)
    }

/*
    pub fn declaration(&mut self) -> Parse<Decl> {
        match try!(self.declaration_opt()) {
            Some(decl) => Ok(decl),
            None       => Err(ParseError::UnexpectedToken(try!(self.read())))
        }
    }
*/

    fn declaration_opt(&mut self) -> Parse<Option<Decl>> {
        match try!(self.peek()).value {
            TokenData::Reserved(Word::Function) => Ok(Some(try!(self.function_declaration()))),
            _                                   => Ok(None)
        }
    }

    fn function_declaration(&mut self) -> Parse<Decl> {
        self.span(&mut |this| {
            Ok(DeclData::Fun(try!(this.function())))
        })
    }

    fn formal_parameters(&mut self) -> Parse<Params> {
        self.span(&mut |this| {
            try!(this.expect(TokenData::LParen));
            let list = try!(this.pattern_list());
            try!(this.expect(TokenData::RParen));
            Ok(ParamsData { list: list })
        })
    }

    fn pattern_list(&mut self) -> Parse<Vec<Patt>> {
        let mut patts = Vec::new();
        if try!(self.peek()).value == TokenData::RParen {
            return Ok(patts);
        }
        patts.push(try!(self.pattern()));
        while try!(self.matches(TokenData::Comma)) {
            patts.push(try!(self.pattern()));
        }
        Ok(patts)
    }

    fn pattern(&mut self) -> Parse<Patt> {
        Ok(try!(self.id()).into_patt())
    }

    fn function(&mut self) -> Parse<Fun> {
        let outer_cx = replace(&mut self.parser_cx, ParserContext::new_function());
        let result = self.span(&mut |this| {
            this.reread(TokenData::Reserved(Word::Function));
            let id = try!(this.id_opt());
            let params = try!(this.formal_parameters());
            try!(this.expect(TokenData::LBrace));
            let body = try!(this.statement_list());
            try!(this.expect(TokenData::RBrace));
            Ok(FunData { id: id, params: params, body: body })
        });
        replace(&mut self.parser_cx, outer_cx);
        result
    }

    fn statement(&mut self) -> Parse<Stmt> {
        match try!(self.peek()).value {
            TokenData::LBrace                   => self.block_statement(),
            TokenData::Reserved(Word::Var)      => self.var_statement(),
            TokenData::Semi                     => self.empty_statement(),
            TokenData::Reserved(Word::If)       => self.if_statement(),
            TokenData::Reserved(Word::Continue) => self.continue_statement(),
            TokenData::Reserved(Word::Break)    => self.break_statement(),
            TokenData::Reserved(Word::Return)   => self.return_statement(),
            TokenData::Reserved(Word::With)     => self.with_statement(),
            TokenData::Reserved(Word::Switch)   => self.switch_statement(),
            TokenData::Reserved(Word::Throw)    => self.throw_statement(),
            TokenData::Reserved(Word::Try)      => self.try_statement(),
            TokenData::Reserved(Word::While)    => self.while_statement(),
            TokenData::Reserved(Word::Do)       => self.do_statement(),
            TokenData::Reserved(Word::For)      => self.for_statement(),
            TokenData::Reserved(Word::Debugger) => self.debugger_statement(),
            TokenData::Identifier(_)            => { let id = self.id().ok().unwrap(); self.id_statement(id) }
            _                                   => self.expression_statement()
        }
    }

    fn id_statement(&mut self, id: Id) -> Parse<Stmt> {
        //let id = self.id().ok().unwrap();
        match try!(self.peek()).value {
            TokenData::Colon => self.labelled_statement(id),
            _                => unimplemented!()
        }
    }

    fn labelled_statement(&mut self, id: Id) -> Parse<Stmt> {
        self.reread(TokenData::Colon);

        let mut labels = vec![id]; // vector of consecutive labels
        let mut expr_id = None;    // id that starts the statement following the labels, if any

        while let TokenData::Identifier(_) = try!(self.peek()).value {
            let id = self.id().ok().unwrap();
            if !try!(self.matches(TokenData::Colon)) {
                expr_id = Some(id);
                break;
            }
            labels.push(id);
        }

        match expr_id {
            Some(id) => {
                self.with_labels(labels, LabelType::Statement, |this| this.id_statement(id))
            }
            None     => {
                let label_type = try!(self.peek()).label_type();
                self.with_labels(labels, label_type, |this| this.statement())
            }
        }
    }

    fn with_labels<F>(&mut self, mut labels: Vec<Id>, label_type: LabelType, op: F) -> Parse<Stmt>
      where F: FnOnce(&mut Self) -> Parse<Stmt>
    {
        let mut label_strings = Vec::new();
        for id in labels.iter() {
            let label = Rc::new(id.value.name.clone());
            self.parser_cx.labels.insert(label.clone(), label_type);
            label_strings.push(label);
        }
        let result = op(self);
        for label in label_strings {
            self.parser_cx.labels.remove(&label);
        }
        let mut body = try!(result);
        labels.reverse();
        for id in labels {
            let location = span(&id, &body);
            body = StmtData::Label(id, Box::new(body)).tracked(location);
        }
        Ok(body)
    }

    fn expression_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        let expr = try!(self.expression());
        Ok(try!(span.end_with_auto_semi(self, Newline::Required, |semi| StmtData::Expr(expr, semi))))
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
        self.reread(TokenData::Reserved(Word::Var));
        let dtors = try!(self.var_declaration_list());
        Ok(try!(span.end_with_auto_semi(self, Newline::Required, |semi| StmtData::Var(dtors, semi))))
    }

    fn var_declaration_list(&mut self) -> Parse<Vec<VarDtor>> {
        let mut items = Vec::new();
        items.push(try!(self.var_declaration()));
        while try!(self.matches(TokenData::Comma)) {
            items.push(try!(self.var_declaration()));
        }
        Ok(items)
    }

    fn id(&mut self) -> Parse<Id> {
        self.try_token_at(&mut |data, location| {
            match data {
                TokenData::Identifier(name) => Ok(IdData { name: name }),
                _ => Err(data)
            }
        })
    }

    fn id_opt(&mut self) -> Parse<Option<Id>> {
        let next = try!(self.read());
        match next.value {
            TokenData::Identifier(name) => {
                Ok(Some((IdData { name: name }).tracked(Some(next.location))))
            }
            _                           => { self.lexer.unread_token(next); Ok(None) }
        }
    }

    fn var_declaration(&mut self) -> Parse<VarDtor> {
        self.span(&mut |this| {
            let lhs = try!(this.pattern());
            let rhs = if try!(this.matches(TokenData::Assign)) {
                Some(try!(this.assignment_expression()))
            } else {
                None
            };
            Ok(VarDtorData { id: lhs, init: rhs })
        })
    }

    fn empty_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            try!(this.expect(TokenData::Semi));
            Ok(StmtData::Empty)
        })
    }

    fn if_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            try!(this.expect(TokenData::Reserved(Word::If)));
            let test = try!(this.paren_expression());
            let cons = Box::new(try!(this.statement()));
            let alt = if try!(this.peek()).value == TokenData::Reserved(Word::Else) {
                this.reread(TokenData::Reserved(Word::Else));
                Some(Box::new(try!(this.statement())))
            } else {
                None
            };
            Ok(StmtData::If(test, cons, alt))
        })
    }

    fn do_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Word::Do));
        let body = Box::new(try!(self.statement()));
        try!(self.expect(TokenData::Reserved(Word::While)));
        let test = try!(self.paren_expression());
        Ok(try!(span.end_with_auto_semi(self, Newline::Optional, |semi| {
            StmtData::DoWhile(body, test, semi)
        })))
    }

    fn while_statement(&mut self) -> Parse<Stmt> {
        let outer_iteration = replace(&mut self.parser_cx.iteration, true);
        let result = self.span(&mut |this| {
            this.reread(TokenData::Reserved(Word::While));
            let test = try!(this.paren_expression());
            let body = Box::new(try!(this.statement()));
            Ok(StmtData::While(test, body))
        });
        replace(&mut self.parser_cx.iteration, outer_iteration);
        result
    }

    fn for_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn switch_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn break_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        let break_token = self.reread(TokenData::Reserved(Word::Break));
        let has_arg = {
            let next = try!(self.peek());
            !next.newline && next.value != TokenData::Semi && next.value != TokenData::RBrace
        };
        let arg = if has_arg {
            let id = try!(self.id());
            if !self.parser_cx.labels.contains_key(&Rc::new(id.value.name.clone())) {
                return Err(ParseError::InvalidLabel(id));
            }
            Some(id)
        } else {
            if !self.parser_cx.iteration && !self.parser_cx.switch {
                return Err(ParseError::IllegalBreak(break_token));
            }
            None
        };
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            StmtData::Break(arg, semi)
        })
    }

    fn continue_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn return_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Word::Return));
        let has_arg = {
            let next = try!(self.peek());
            !next.newline && next.value != TokenData::Semi && next.value != TokenData::RBrace
        };
        let arg = if has_arg { Some(try!(self.expression())) } else { None };
        let result = try!(span.end_with_auto_semi(self, Newline::Required, |semi| {
            StmtData::Return(arg, semi)
        }));
        if !self.parser_cx.function {
            Err(ParseError::TopLevelReturn(result.location.unwrap()))
        } else {
            Ok(result)
        }
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

    fn paren_expression(&mut self) -> Parse<Expr> {
        try!(self.expect(TokenData::LParen));
        let result = try!(self.expression());
        try!(self.expect(TokenData::RParen));
        Ok(result)
    }

    fn primary_expression(&mut self) -> Parse<Expr> {
        self.try_token_at(&mut |data, location| {
            match data {
                TokenData::Identifier(name)     => Ok(ExprData::Id(Id::new(name, Some(location)))),
                TokenData::Reserved(Word::Null) => Ok(ExprData::Null),
                TokenData::Reserved(Word::This) => Ok(ExprData::This),
                TokenData::Number(literal)      => Ok(ExprData::Number(literal)),
                _                               => Err(data)
            }
        })
    }

    fn assignment_expression(&mut self) -> Parse<Expr> {
        self.primary_expression()
    }

    fn expression(&mut self) -> Parse<Expr> {
        // FIXME: implement for real
        self.assignment_expression()
    }

    pub fn expr(&mut self) -> Parse<Expr> {
        let left = match self.lexer.read_token() {
            Ok(Token { value: TokenData::Number(literal), location, .. }) => Expr { location: Some(location), value: ExprData::Number(literal) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let op = match self.lexer.read_token() {
            Ok(Token { value: TokenData::Plus, location, .. }) => Binop { location: Some(location), value: BinopTag::Plus },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        let right = match self.lexer.read_token() {
            Ok(Token { value: TokenData::Number(literal), location, .. }) => Expr { location: Some(location), value: ExprData::Number(literal) },
            Ok(t) => return Err(ParseError::UnexpectedToken(t)),
            Err(e) => return Err(ParseError::LexError(e))
        };

        Ok(Expr {
            location: span(&left, &right),
            value: ExprData::Binop(op, Box::new(left), Box::new(right))
        })
    }
}

#[cfg(test)]
mod tests {

    use test::{deserialize_parser_tests, ParserTest};
    use lexer::{Lexer, LexError};
    use context::{SharedContext, Mode};
    use token::{Token, TokenData};
    use std::cell::Cell;
    use std::rc::Rc;
    use std::str::Chars;
    use parser::{Parser, Parse};
    use ast::*;
    use track::*;

    fn parse(source: &String) -> Parse<Script> {
        let chars = source.chars();
        let cx = Rc::new(Cell::new(SharedContext::new(Mode::Sloppy)));
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
                    actual_ast.untrack();
                    if (actual_ast != expected_ast) {
                        println!("");
                        println!("test:         {}", source);
                        println!("expected AST: {:?}", expected_ast);
                        println!("actual AST:   {:?}", actual_ast);
                    }
                    assert!(actual_ast == expected_ast);
                }
                (Err(_), Err(_)) => { }
                (Ok(mut actual_ast), Err(_)) => {
                    actual_ast.untrack();
                    println!("");
                    println!("expected error, got: {:?}", actual_ast);
                    panic!("expected error");
                }
                (Err(actual_err), Ok(expected_ast)) => {
                    println!("");
                    println!("expected AST: {:?}", expected_ast);
                    println!("actual error: {:?}", actual_err);
                    panic!("unexpected error")
                }
            }
        }
    }

}
