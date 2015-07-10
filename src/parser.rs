use track::*;
use token::{Token, TokenData, Reserved, Atom, Name};
use lexer::Lexer;

use std::cell::Cell;
use std::rc::Rc;
use std::mem::replace;
use ast::*;
use lexer::LexError;
use context::{SharedContext, ParserContext, LabelType, Mode};

pub type Parse<T> = Result<T, ParseError>;

#[derive(Debug, PartialEq)]
pub enum ParseError {
    UnexpectedToken(Token),
    FailedASI(Token),
    LexError(LexError),
    TopLevelReturn(Span),
    IllegalBreak(Token),
    IllegalContinue(Token),
    InvalidLabel(Id),
    InvalidLabelType(Id),
    ContextualKeyword(Id),
    IllegalStrictBinding(Id),
    ForOfLetExpr(Span),
    DuplicateDefault(Token),
    StrictWith(Token),
    ThrowArgument(Token)
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

trait First {
    fn first_binding(&self) -> bool;
}

trait Follows {
    fn follow_statement_list(&self) -> bool;
}

impl First for Token {
    // first(LexicalBinding) =
    //   first(BindingIdentifier)
    // U first(BindingPattern)
    // = IdentifierName
    // U first(BindingPattern)
    // = IdentifierName
    // U first(ObjectBindingPattern)
    // U first(ArrayBindingPattern)
    // = IdentifierName U { '{', '[' }
    fn first_binding(&self) -> bool {
        match self.value {
            TokenData::LBrace
          | TokenData::LBrack
          | TokenData::Identifier(_) => true,
            _ => false
        }
    }
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
              TokenData::Reserved(Reserved::Case)
            | TokenData::Reserved(Reserved::Default)
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
            TokenData::Reserved(Reserved::Do)
          | TokenData::Reserved(Reserved::While)
          | TokenData::Reserved(Reserved::For) => LabelType::Iteration,
            _                                  => LabelType::Statement
        }
    }
}

trait AtomExt {
    fn is_reserved(&self, Mode) -> bool;
    fn is_illegal_strict_binding(&self) -> bool;
}

impl AtomExt for Name {
    fn is_reserved(&self, mode: Mode) -> bool {
        match self {
            &Name::Atom(ref atom) => atom.is_reserved(mode),
            _ => false
        }
    }

    fn is_illegal_strict_binding(&self) -> bool {
        match *self {
            Name::Atom(ref atom) => atom.is_illegal_strict_binding(),
            _ => false
        }
    }
}

impl AtomExt for Atom {
    fn is_reserved(&self, mode: Mode) -> bool {
        // 12.1.1
        if mode.is_strict() {
            match *self {
                Atom::Implements
              | Atom::Interface
              | Atom::Let
              | Atom::Package
              | Atom::Private
              | Atom::Protected
              | Atom::Public
              | Atom::Static
              | Atom::Yield => true,
                _ => false
            }
        // 11.6.2.2
        } else {
            mode == Mode::Module && *self == Atom::Await
        }
    }

    // 12.1.1
    fn is_illegal_strict_binding(&self) -> bool {
        match *self {
            Atom::Arguments
          | Atom::Eval => true,
            _ => false
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
                parser.reread(TokenData::Semi);
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

    fn expect(&mut self, expected: TokenData) -> Parse<Token> {
        let token = try!(self.read());
        if token.value != expected {
            return Err(ParseError::UnexpectedToken(token));
        }
        Ok(token)
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

    fn has_arg_same_line(&mut self) -> Parse<bool> {
        let next = try!(self.peek());
        Ok(!next.newline && next.value != TokenData::Semi && next.value != TokenData::RBrace)
    }

    fn span<F, T>(&mut self, parse: &mut F) -> Parse<Tracked<T>>
      where F: FnMut(&mut Self) -> Parse<T>
    {
        let start = self.posn();
        let value = try!(parse(self));
        let end = self.posn();
        Ok(Tracked { value: value, location: Some(Span { start: start, end: end }) })
    }

    fn allow_in<F, T>(&mut self, allow_in: bool, parse: F) -> Parse<T>
      where F: FnOnce(&mut Self) -> Parse<T>
    {
        let allow_in = replace(&mut self.parser_cx.allow_in, allow_in);
        let result = parse(self);
        replace(&mut self.parser_cx.allow_in, allow_in);
        result
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
            TokenData::Reserved(Reserved::Function) => Ok(Some(try!(self.function_declaration()))),
            _                                       => Ok(None)
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
        match try!(self.peek()).value {
            TokenData::Identifier(_) => {
                let id = try!(self.binding_id());
                Ok(Patt::Simple(id))
            }
            _ => {
                let patt = try!(self.binding_pattern());
                Ok(Patt::Compound(patt))
            }
        }
    }

    fn binding_pattern(&mut self) -> Parse<CompoundPatt> {
        unimplemented!()
    }

    fn function(&mut self) -> Parse<Fun> {
        let outer_cx = replace(&mut self.parser_cx, ParserContext::new_function());
        let result = self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Function));
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
            TokenData::LBrace                       => self.block_statement(),
            TokenData::Reserved(Reserved::Var)      => self.var_statement(),
            TokenData::Semi                         => self.empty_statement(),
            TokenData::Reserved(Reserved::If)       => self.if_statement(),
            TokenData::Reserved(Reserved::Continue) => self.continue_statement(),
            TokenData::Reserved(Reserved::Break)    => self.break_statement(),
            TokenData::Reserved(Reserved::Return)   => self.return_statement(),
            TokenData::Reserved(Reserved::With)     => self.with_statement(),
            TokenData::Reserved(Reserved::Switch)   => self.switch_statement(),
            TokenData::Reserved(Reserved::Throw)    => self.throw_statement(),
            TokenData::Reserved(Reserved::Try)      => self.try_statement(),
            TokenData::Reserved(Reserved::While)    => self.while_statement(),
            TokenData::Reserved(Reserved::Do)       => self.do_statement(),
            TokenData::Reserved(Reserved::For)      => self.for_statement(),
            TokenData::Reserved(Reserved::Debugger) => self.debugger_statement(),
            TokenData::Identifier(_)                => {
                let id = self.id().ok().unwrap();
                self.id_statement(id)
            }
            _                                       => self.expression_statement()
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
        self.reread(TokenData::Reserved(Reserved::Var));
        let dtors = try!(self.declarator_list());
        Ok(try!(span.end_with_auto_semi(self, Newline::Required, |semi| StmtData::Var(dtors, semi))))
    }

    fn declarator_list(&mut self) -> Parse<Vec<Dtor>> {
        let mut items = Vec::new();
        items.push(try!(self.declarator()));
        while try!(self.matches(TokenData::Comma)) {
            items.push(try!(self.declarator()));
        }
        Ok(items)
    }

    fn binding_id(&mut self) -> Parse<Id> {
        let id = try!(self.id());
        if self.shared_cx.get().mode.is_strict() && id.value.name.is_illegal_strict_binding() {
            return Err(ParseError::IllegalStrictBinding(id));
        }
        Ok(id)
    }

    fn id(&mut self) -> Parse<Id> {
        let Token { location, newline, value: data } = try!(self.read());
        match data {
            TokenData::Identifier(name) => {
                if name.is_reserved(self.shared_cx.get().mode) {
                    return Err(ParseError::ContextualKeyword(Id {
                        value: IdData { name: name },
                        location: Some(location)
                    }));
                }
                Ok(Id { location: Some(location), value: IdData { name: name } })
            }
            _ => Err(ParseError::UnexpectedToken(Token {
                location: location,
                newline: newline,
                value: data
            }))
        }
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

    fn declarator(&mut self) -> Parse<Dtor> {
        self.span(&mut |this| {
            match try!(this.peek()).value {
                TokenData::Identifier(_) => {
                    let id = try!(this.binding_id());
                    let init = if try!(this.matches(TokenData::Assign)) {
                        Some(try!(this.assignment_expression()))
                    } else {
                        None
                    };
                    Ok(DtorData::Simple(id, init))
                }
                _ => {
                    let lhs = try!(this.binding_pattern());
                    try!(this.expect(TokenData::Assign));
                    let rhs = try!(this.assignment_expression());
                    Ok(DtorData::Compound(lhs, rhs))
                }
            }
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
            try!(this.expect(TokenData::Reserved(Reserved::If)));
            let test = try!(this.paren_expression());
            let cons = Box::new(try!(this.statement()));
            let alt = if try!(this.peek()).value == TokenData::Reserved(Reserved::Else) {
                this.reread(TokenData::Reserved(Reserved::Else));
                Some(Box::new(try!(this.statement())))
            } else {
                None
            };
            Ok(StmtData::If(test, cons, alt))
        })
    }

    fn iteration_body(&mut self) -> Parse<Stmt> {
        let iteration = replace(&mut self.parser_cx.iteration, true);
        let result = self.statement();
        replace(&mut self.parser_cx.iteration, iteration);
        result
    }

    fn do_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Do));
        let body = Box::new(try!(self.iteration_body()));
        try!(self.expect(TokenData::Reserved(Reserved::While)));
        let test = try!(self.paren_expression());
        Ok(try!(span.end_with_auto_semi(self, Newline::Optional, |semi| {
            StmtData::DoWhile(body, test, semi)
        })))
    }

    fn while_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::While));
            let test = try!(this.paren_expression());
            let body = Box::new(try!(this.iteration_body()));
            Ok(StmtData::While(test, body))
        })
    }

    fn for_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::For));
            try!(this.expect(TokenData::LParen));
            match try!(this.peek()).value {
                TokenData::Reserved(Reserved::Var)           => this.for_var(),
                TokenData::Identifier(Name::Atom(Atom::Let)) => this.for_let(),
                TokenData::Reserved(Reserved::Const)         => unimplemented!(),
                TokenData::Semi                              => {
                    this.reread(TokenData::Semi);
                    this.more_for(None)
                }
                _                                            => this.for_expr()
            }
        })
    }

    // 'for' '(' 'var' .
    fn for_var(&mut self) -> Parse<StmtData> {
        let var_token = self.reread(TokenData::Reserved(Reserved::Var));
        let var_location = var_token.location;
        let lhs = try!(self.pattern());
        match try!(self.peek()).value {
            // 'for' '(' 'var' id   '=' .
            // 'for' '(' 'var' patt '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                match lhs {
                    Patt::Simple(id) => {
                        let rhs = try!(self.allow_in(false, |this| this.assignment_expression()));
                        match try!(self.peek()).value {
                            // 'for' '(' 'var' id '=' expr ','  . ==> C-style
                            // 'for' '(' 'var' id '=' expr ';'  . ==> C-style
                            TokenData::Comma
                          | TokenData::Semi => {
                                let head = Some(try!(self.more_for_head(&var_location, Dtor::from_simple_init(id, rhs), ForHeadData::Var)));
                                self.more_for(head)
                            }
                            // 'for' '(' 'var' id '=' expr 'in' . ==> legacy enumeration
                            TokenData::Reserved(Reserved::In) => {
                                self.reread(TokenData::Reserved(Reserved::In));
                                let head = Box::new(ForInHead {
                                    location: span(&var_location, &rhs),
                                    value: ForInHeadData::VarInit(id, rhs)
                                });
                                self.more_for_in(head)
                            }
                            _ => Err(ParseError::UnexpectedToken(try!(self.read())))
                        }
                    }
                    // 'for' '(' 'var' patt '=' . ==> C-style
                    Patt::Compound(patt) => {
                        let rhs = try!(self.allow_in(false, |this| this.assignment_expression()));
                        let head = Some(try!(self.more_for_head(&var_location, Dtor::from_compound_init(patt, rhs), ForHeadData::Var)));
                        self.more_for(head)
                    }
                }
            }
            TokenData::Comma
          | TokenData::Semi => {
                // 'for' '(' 'var' id   ',' . ==> C-style
                // 'for' '(' 'var' id   ';' . ==> C-style
                // 'for' '(' 'var' patt ',' . ==> syntax error
                // 'for' '(' 'var' patt ';' . ==> syntax error
                let dtor = match Dtor::from_init_opt(lhs, None) {
                    Ok(dtor) => dtor,
                    Err(_) => { return Err(ParseError::UnexpectedToken(try!(self.read()))); }
                };
                let head = Some(try!(self.more_for_head(&var_location, dtor, ForHeadData::Var)));
                self.more_for(head)
            }
            // 'for' '(' 'var' id   'in' . ==> enumeration
            // 'for' '(' 'var' patt 'in' . ==> enumeration
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead {
                    location: span(&var_location, &lhs),
                    value: ForInHeadData::Var(lhs)
                });
                self.more_for_in(head)
            }
            // 'for' '(' 'var' id   'of' . ==> enumeration
            // 'for' '(' 'var' patt 'of' . ==> enumeration
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead {
                    location: span(&var_location, &lhs),
                    value: ForOfHeadData::Var(lhs)
                });
                self.more_for_of(head)
            }
            _ => Err(ParseError::UnexpectedToken(try!(self.read())))
        }
    }

    // 'for' '(' 'let' .
    fn for_let(&mut self) -> Parse<StmtData> {
        let let_token = self.reread(TokenData::Identifier(Name::Atom(Atom::Let)));
        let let_location = let_token.location;
        // 'for' '(' 'let' . !{id, patt} ==> C-style
        if !try!(self.peek()).first_binding() {
            let result = try!(self.for_id_expr(Id::new(Name::Atom(Atom::Let), Some(let_token.location))));
            if let StmtData::ForOf(_, _, _) = result {
                // FIXME: this really ought to report the *next* token but that's hard
                return Err(ParseError::ForOfLetExpr(let_location));
            }
            return Ok(result);
        }
        let lhs = try!(self.pattern());
        match try!(self.peek()).value {
            // 'for' '(' 'let' id   '=' . ==> C-style
            // 'for' '(' 'let' patt '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                let rhs = try!(self.allow_in(false, |this| this.assignment_expression()));
                let head = Some(try!(self.more_for_head(&let_location, Dtor::from_init(lhs, rhs), ForHeadData::Let)));
                self.more_for(head)
            }
            TokenData::Comma
          | TokenData::Semi => {
                // 'for' '(' 'let' id   ',' . ==> C-style
                // 'for' '(' 'let' id   ';' . ==> C-style
                // 'for' '(' 'let' patt ',' . ==> error
                // 'for' '(' 'let' patt ';' . ==> error
                let dtor = match Dtor::from_init_opt(lhs, None) {
                    Ok(dtor) => dtor,
                    Err(_) => { return Err(ParseError::UnexpectedToken(try!(self.read()))); }
                };
                let head = Some(try!(self.more_for_head(&let_location, dtor, ForHeadData::Let)));
                self.more_for(head)
            }
            // 'for' '(' 'let' id   'in' . ==> enumeration
            // 'for' '(' 'let' patt 'in' . ==> enumeration
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead {
                    location: span(&let_location, &lhs),
                    value: ForInHeadData::Let(lhs)
                });
                self.more_for_in(head)
            }
            // 'for' '(' 'let' id   'of' . ==> enumeration
            // 'for' '(' 'let' patt 'of' . ==> enumeration
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead {
                    location: span(&let_location, &lhs),
                    value: ForOfHeadData::Let(lhs)
                });
                self.more_for_of(head)
            }
            _ => Err(ParseError::UnexpectedToken(try!(self.read())))
        }
    }

    fn for_id_expr(&mut self, id: Id) -> Parse<StmtData> {
        let lhs = try!(self.allow_in(false, |this| this.id_expression(id)));
        self.more_for_expr(lhs)
    }

    fn for_expr(&mut self) -> Parse<StmtData> {
        let lhs = try!(self.allow_in(false, |this| this.expression()));
        self.more_for_expr(lhs)
    }

    fn more_for_expr(&mut self, lhs: Expr) -> Parse<StmtData> {
        match try!(self.peek()).value {
            TokenData::Semi => {
                let semi_location = self.reread(TokenData::Semi).location;
                let head = Some(Box::new(ForHead {
                    location: span(&lhs, &semi_location),
                    value: ForHeadData::Expr(lhs)
                }));
                self.more_for(head)
            }
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead {
                    location: lhs.location(),
                    value: ForInHeadData::Expr(lhs)
                });
                self.more_for_in(head)
            }
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead {
                    location: lhs.location(),
                    value: ForOfHeadData::Expr(lhs)
                });
                self.more_for_of(head)
            }
            _ => Err(ParseError::UnexpectedToken(try!(self.read())))
        }
    }

    // 'for' '(' dtor .
    fn more_for_head<F>(&mut self, start: &Span, dtor: Dtor, op: F) -> Parse<Box<ForHead>>
      where F: FnOnce(Vec<Dtor>) -> ForHeadData
    {
        let dtors = try!(self.allow_in(false, |this| {
            let mut dtors = vec![dtor];
            try!(this.more_dtors(&mut dtors));
            Ok(dtors)
        }));
        let semi_location = try!(self.expect(TokenData::Semi)).location;
        Ok(Box::new(ForHead {
            location: span(start, &semi_location),
            value: op(dtors)
        }))
    }

    // 'for' '(' head ';' .
    fn more_for(&mut self, head: Option<Box<ForHead>>) -> Parse<StmtData> {
        let test = try!(self.expression_opt_semi());
        let update = if try!(self.matches(TokenData::RParen)) {
            None
        } else {
            let node = Some(try!(self.allow_in(true, |this| this.expression())));
            try!(self.expect(TokenData::RParen));
            node
        };
        let body = Box::new(try!(self.iteration_body()));
        Ok(StmtData::For(head, test, update, body))
    }

    // 'for' '(' head 'in' .
    fn more_for_in(&mut self, head: Box<ForInHead>) -> Parse<StmtData> {
        let obj = try!(self.allow_in(true, |this| this.assignment_expression()));
        try!(self.expect(TokenData::RParen));
        let body = Box::new(try!(self.iteration_body()));
        Ok(StmtData::ForIn(head, obj, body))
    }

    // 'for' '(' head 'of' .
    fn more_for_of(&mut self, head: Box<ForOfHead>) -> Parse<StmtData> {
        let obj = try!(self.allow_in(true, |this| this.assignment_expression()));
        try!(self.expect(TokenData::RParen));
        let body = Box::new(try!(self.iteration_body()));
        Ok(StmtData::ForOf(head, obj, body))
    }

    fn expression_opt_semi(&mut self) -> Parse<Option<Expr>> {
        Ok(if try!(self.matches(TokenData::Semi)) {
            None
        } else {
            let expr = try!(self.allow_in(true, |this| this.expression()));
            try!(self.expect(TokenData::Semi));
            Some(expr)
        })
    }

    fn more_dtors(&mut self, dtors: &mut Vec<Dtor>) -> Parse<()> {
        while try!(self.matches(TokenData::Comma)) {
            dtors.push(try!(self.declarator()));
        }
        Ok(())
    }

    fn switch_statement(&mut self) -> Parse<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Switch));
            let disc = try!(this.paren_expression());
            let outer_switch = replace(&mut this.parser_cx.switch, true);
            let cases = this.switch_cases();
            replace(&mut this.parser_cx.switch, outer_switch);
            Ok(StmtData::Switch(disc, try!(cases)))
        })
    }

    fn switch_cases(&mut self) -> Parse<Vec<Case>> {
        try!(self.expect(TokenData::LBrace));
        let mut cases = Vec::new();
        let mut found_default = false;
        loop {
            match try!(self.peek()).value {
                TokenData::Reserved(Reserved::Case) => { cases.push(try!(self.case())); }
                TokenData::Reserved(Reserved::Default) => {
                    if found_default {
                        let token = self.reread(TokenData::Reserved(Reserved::Default));
                        return Err(ParseError::DuplicateDefault(token));
                    }
                    found_default = true;
                    cases.push(try!(self.default()));
                }
                _ => { break; }
            }
        }
        try!(self.expect(TokenData::RBrace));
        Ok(cases)
    }

    fn case(&mut self) -> Parse<Case> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Case));
            let test = try!(this.allow_in(true, |this| this.expression()));
            try!(this.expect(TokenData::Colon));
            let body = try!(this.case_body());
            Ok(CaseData { test: Some(test), body: body })
        })
    }

    fn case_body(&mut self) -> Parse<Vec<StmtListItem>> {
        let mut items = Vec::new();
        loop {
            match try!(self.peek()).value {
                TokenData::Reserved(Reserved::Case)
              | TokenData::Reserved(Reserved::Default)
              | TokenData::RBrace => { break; }
                _ => { }
            }
            match try!(self.declaration_opt()) {
                Some(decl) => { items.push(StmtListItem::Decl(decl)); }
                None       => { items.push(StmtListItem::Stmt(try!(self.statement()))); }
            }
        }
        Ok(items)
    }

    fn default(&mut self) -> Parse<Case> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Default));
            try!(this.expect(TokenData::Colon));
            let body = try!(this.case_body());
            Ok(CaseData { test: None, body: body })
        })
    }

    fn break_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        let break_token = self.reread(TokenData::Reserved(Reserved::Break));
        let arg = if try!(self.has_arg_same_line()) {
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
        let span = self.start();
        let continue_token = self.reread(TokenData::Reserved(Reserved::Continue));
        let arg = if try!(self.has_arg_same_line()) {
            let id = try!(self.id());
            match self.parser_cx.labels.get(&Rc::new(id.value.name.clone())) {
                None                        => { return Err(ParseError::InvalidLabel(id)); }
                Some(&LabelType::Statement) => { return Err(ParseError::InvalidLabelType(id)); }
                _                           => { }
            }
            Some(id)
        } else {
            if !self.parser_cx.iteration {
                return Err(ParseError::IllegalContinue(continue_token));
            }
            None
        };
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            StmtData::Cont(arg, semi)
        })
    }

    fn return_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Return));
        let arg = if try!(self.has_arg_same_line()) { Some(try!(self.expression())) } else { None };
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
        self.span(&mut |this| {
            let token = this.reread(TokenData::Reserved(Reserved::With));
            if this.shared_cx.get().mode.is_strict() {
                return Err(ParseError::StrictWith(token));
            }
            let obj = try!(this.paren_expression());
            let body = Box::new(try!(this.statement()));
            Ok(StmtData::With(obj, body))
        })
    }

    fn throw_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        let token = self.reread(TokenData::Reserved(Reserved::Throw));
        if !try!(self.has_arg_same_line()) {
            return Err(ParseError::ThrowArgument(token));
        }
        let arg = try!(self.expression());
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            StmtData::Throw(arg, semi)
        })
    }

    fn try_statement(&mut self) -> Parse<Stmt> {
        unimplemented!()
    }

    fn debugger_statement(&mut self) -> Parse<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Debugger));
        Ok(try!(span.end_with_auto_semi(self, Newline::Required, |semi| StmtData::Debugger(semi))))
    }

/*
    pub fn module(&mut self) -> Parse<Module> {
        unimplemented!()
    }
*/

    fn paren_expression(&mut self) -> Parse<Expr> {
        try!(self.expect(TokenData::LParen));
        let result = try!(self.allow_in(true, |this| this.expression()));
        try!(self.expect(TokenData::RParen));
        Ok(result)
    }

    fn primary_expression(&mut self) -> Parse<Expr> {
        self.try_token_at(&mut |data, location| {
            match data {
                TokenData::Identifier(name)         => {
                    Ok(ExprData::Id(Id::new(name, Some(location))))
                }
                TokenData::Reserved(Reserved::Null) => Ok(ExprData::Null),
                TokenData::Reserved(Reserved::This) => Ok(ExprData::This),
                TokenData::Number(literal)          => Ok(ExprData::Number(literal)),
                _                                   => Err(data)
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

    fn id_expression(&mut self, id: Id) -> Parse<Expr> {
        unimplemented!()
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
    use lexer::Lexer;
    use context::{SharedContext, Mode};
    use std::cell::Cell;
    use std::rc::Rc;
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
                (Ok(mut actual_ast), Some(expected_ast)) => {
                    actual_ast.untrack();
                    if actual_ast != expected_ast {
                        println!("");
                        println!("test:         {}", source);
                        println!("expected AST: {:?}", expected_ast);
                        println!("actual AST:   {:?}", actual_ast);
                    }
                    assert!(actual_ast == expected_ast);
                }
                (Err(_), None) => { }
                (Ok(mut actual_ast), None) => {
                    actual_ast.untrack();
                    println!("");
                    println!("test:                {}", source);
                    println!("expected error, got: {:?}", actual_ast);
                    panic!("expected error");
                }
                (Err(actual_err), Some(expected_ast)) => {
                    println!("");
                    println!("test:         {}", source);
                    println!("expected AST: {:?}", expected_ast);
                    println!("actual error: {:?}", actual_err);
                    panic!("unexpected error")
                }
            }
        }
    }

}
