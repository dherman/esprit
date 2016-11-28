use joker::track::*;
use joker::token::{Token, TokenData, StringLiteral};
use joker::word::{Atom, Name, Reserved};
use joker::lexer::Lexer;
use easter::stmt::{Stmt, StmtListItem, ForHead, ForInHead, ForOfHead, Case, Catch, Script, Dir, ModItem, Module};
use easter::expr::Expr;
use easter::decl::{Decl, Dtor, DtorExt};
use easter::patt::{Patt, CompoundPatt};
use easter::fun::{Fun, Params};
use easter::obj::{PropKey, PropVal, Prop, DotKey};
use easter::id::{Id, IdExt};
use easter::punc::{Unop, UnopTag, ToOp, Op};
use easter::cover::{IntoAssignTarget, IntoAssignPatt};

use std::rc::Rc;
use std::mem::replace;
use context::{Context, LabelType, WithContext, Goal, Mode};
use tokens::{First, Follows, HasLabelType};
use atom::AtomExt;
use track::Newline;
use result::Result;
use error::{Error, Check};
use track::{SpanTracker, Tracking};
use state::State;
use expr::{Deref, Suffix, Arguments, Prefix, Postfix};
use stack::{Stack, Infix};

pub use tristate::TriState as Strict;

pub struct Parser<I> {
    pub goal: Goal,
    pub validate: bool,       // should we do strict mode validation as eagerly as possible?
    pub deferred: Vec<Check>, // strict mode checks that haven't been performed yet
    pub lexer: Lexer<I>,
    pub context: Context
}

struct StringToken(Option<Span>, StringLiteral);

enum DirectiveMatch {
    Directive(Dir),
    StringStatement(StringToken),
    Other
}

enum ProgramItems {
    Script(Vec<StmtListItem>),
    Module(Vec<ModItem>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Program {
    Ambiguous(Vec<Check>, Script),
    Module(Module)
}

impl TrackingRef for Program {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Program::Ambiguous(_, ref script) => script.tracking_ref(),
            Program::Module(ref module) => module.tracking_ref()
        }
    }
}

impl TrackingMut for Program {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Program::Ambiguous(_, ref mut script) => script.tracking_mut(),
            Program::Module(ref mut module) => module.tracking_mut()
        }
    }
}

impl Untrack for Program {
    fn untrack(&mut self) {
        match *self {
            Program::Ambiguous(_, ref mut script) => script.untrack(),
            Program::Module(ref mut module) => module.untrack()
        }
    }
}

fn unexpected_module(module: Module) -> Error {
    let Module { location, dirs, items } = module;

    // If there's a "use module" pragma, blame that.
    if let Some(Dir { location, string, .. }) = dirs.into_iter().find(|dir| dir.pragma() == "use module") {
        return Error::UnexpectedDirective(location, string);
    }

    // If there's an import or export, blame that.
    for item in items {
        match item {
            ModItem::Import(import) => { return Error::ImportInScript(import); }
            ModItem::Export(export) => { return Error::ExportInScript(export); }
            _ => { }
        }
    }

    // We determined it's a module for some other reason. For
    // now this should never happen, but serves as catch-all
    // for any future reasons we might determine a program unit
    // is a module.
    return Error::UnexpectedModule(location);
}

impl Program {
    pub fn script(self) -> Result<Script> {
        match self {
            Program::Ambiguous(_, script) => Ok(script),
            Program::Module(module) => { return Err(unexpected_module(module)); }
        }
    }

    pub fn strict(self) -> Result<Script> {
        match self {
            Program::Ambiguous(checks, script) => {
                for check in checks {
                    check.perform(false)?;
                }

                Ok(script)
            }
            Program::Module(module) => { return Err(unexpected_module(module)); }
        }
    }

    pub fn module(self) -> Result<Module> {
        match self {
            Program::Ambiguous(checks, script) => {
                for check in checks {
                    check.perform(true)?;
                }

                let Script { location, dirs, items } = script;

                Ok(Module {
                    location: location,
                    dirs: dirs,
                    items: items.into_iter().map(|item| item.into_mod_item()).collect()
                })
            }
            Program::Module(module) => Ok(module)
        }
    }
}

impl<I: Iterator<Item=char>> Parser<I> {
    pub fn from_chars(goal: Goal, i: I) -> Parser<I> {
        let lexer = Lexer::new(i);
        Parser::new(goal, true, lexer)
    }

    pub fn new(goal: Goal, validate: bool, lexer: Lexer<I>) -> Parser<I> {
        Parser {
            goal: goal,
            validate: validate,
            deferred: Vec::new(),
            lexer: lexer,
            context: Context::new()
        }
    }

    fn take_deferred(&mut self) -> Vec<Check> {
        replace(&mut self.deferred, Vec::new())
    }

    fn match_directive(&mut self) -> Result<DirectiveMatch> {
        let span = self.start();
        let token1 = self.read()?;
        let (location, literal) = match token1.value {
            TokenData::String(literal) => (token1.location, literal),
            _ => {
                self.lexer.unread_token(token1);
                return Ok(DirectiveMatch::Other);
            }
        };

        if self.peek()?.expression_continuation() {
            return Ok(DirectiveMatch::StringStatement(StringToken(Some(location), literal)));
        }

        let dir = span.end_with_auto_semi(self, Newline::Required, |semi| Dir {
            location: None,
            string: literal,
            semi: semi
        })?;

        Ok(DirectiveMatch::Directive(dir))
    }

    pub fn module(&mut self) -> Result<Module> {
        debug_assert!(self.goal.definitely_module());
        self.span(&mut |this| {
            let (dirs, string) = this.body_directives()?;

            this.interpret_directives(&dirs);

            let items = this.module_items(string)?;

            Ok(Module {
                location: None,
                dirs: dirs,
                items: items
            })
        })
    }

    pub fn program(&mut self) -> Result<Program> {
        debug_assert!(!self.goal.definitely_script());
        debug_assert!(!self.goal.definitely_module());
        self.span(&mut |this| {
            let (dirs, string) = this.body_directives()?;

            this.interpret_directives(&dirs);

            match this.program_items(string)? {
                ProgramItems::Script(items) => {
                    let checks = this.take_deferred();
                    Ok(Program::Ambiguous(checks, Script {
                        location: None,
                        dirs: dirs,
                        items: items
                    }))
                }
                ProgramItems::Module(items) => Ok(Program::Module(Module {
                    location: None,
                    dirs: dirs,
                    items: items
                }))
            }
        })
    }

    pub fn script(&mut self) -> Result<Script> {
        self.span(&mut |this| {
            let (dirs, string) = this.body_directives()?;

            this.interpret_directives(&dirs);

            let items = this.body_items(string)?;

            Ok(Script {
                location: None,
                dirs: dirs,
                items: items
            })
        })
    }

    fn body_directives(&mut self) -> Result<(Vec<Dir>, Option<StringToken>)> {
        let mut dirs = Vec::new();

        loop {
            match self.match_directive()? {
                DirectiveMatch::Directive(dir) => { dirs.push(dir); }
                DirectiveMatch::StringStatement(token) => {
                    return Ok((dirs, Some(token)));
                }
                DirectiveMatch::Other => {
                    return Ok((dirs, None));
                }
            }
        }
    }

    fn interpret_directives(&mut self, dirs: &[Dir]) {
        let use_strict = dirs.iter().any(|dir| dir.pragma() == "use strict");
        let use_module = dirs.iter().any(|dir| dir.pragma() == "use module");

        if self.context.function.is_some() {
            if use_strict {
                self.context.function = Some(Strict::Yes);
            }
        } else {
            if use_module {
                self.goal = Goal::Module;
            } else if use_strict {
                self.goal = Goal::Script(Strict::Yes);
            }
        }
    }

    fn body_items(&mut self, string: Option<StringToken>) -> Result<Vec<StmtListItem>> {
        let mut items = Vec::new();

        if let Some(string) = string {
            items.push(StmtListItem::Stmt(self.string_statement(string)?));
        }

        while !self.peek()?.follow_statement_list() {
            match self.declaration_opt()? {
                Some(decl) => { items.push(StmtListItem::Decl(decl)); }
                None       => { items.push(StmtListItem::Stmt(self.statement()?)); }
            }
        }

        Ok(items)
    }

    fn force_deferred_module_validation(&mut self) -> Result<()> {
        if !self.validate {
            return Ok(());
        }

        let deferred = self.take_deferred();

        for check in deferred {
            check.perform(true)?;
        }

        Ok(())
    }

    fn program_items(&mut self, string: Option<StringToken>) -> Result<ProgramItems> {
        let mut stmts = Vec::new();

        if let Some(string) = string {
            stmts.push(StmtListItem::Stmt(self.string_statement(string)?));
        }

        while !self.peek()?.follow_statement_list() {
            match self.peek()?.value {
                TokenData::Reserved(Reserved::Import)
              | TokenData::Reserved(Reserved::Export) => {
                    self.force_deferred_module_validation()?;
                    let items = self.more_module_items(stmts.into_iter().map(|stmt| stmt.into_mod_item()).collect())?;
                    return Ok(ProgramItems::Module(items));
                }
                _ => { }
            }

            match self.declaration_opt()? {
                Some(decl) => { stmts.push(StmtListItem::Decl(decl)); }
                None       => { stmts.push(StmtListItem::Stmt(self.statement()?)); }
            }
        }

        Ok(ProgramItems::Script(stmts))
    }

    fn module_items(&mut self, string: Option<StringToken>) -> Result<Vec<ModItem>> {
        let mut items = Vec::new();

        if let Some(string) = string {
            items.push(ModItem::Stmt(self.string_statement(string)?));
        }

        self.more_module_items(items)
    }

    fn more_module_items(&mut self, mut items: Vec<ModItem>) -> Result<Vec<ModItem>> {
        while !self.peek()?.follow_statement_list() {
            match self.peek()?.value {
                // ES6: import declaration
                TokenData::Reserved(Reserved::Import) => unimplemented!(),
                // ES6: export declaration
                TokenData::Reserved(Reserved::Export) => unimplemented!(),
                _ => { }
            }

            match self.declaration_opt()? {
                Some(decl) => { items.push(ModItem::Decl(decl)); }
                None       => { items.push(ModItem::Stmt(self.statement()?)); }
            }
        }

        Ok(items)
    }

    fn statement_list(&mut self) -> Result<Vec<StmtListItem>> {
        let mut items = Vec::new();
        while !self.peek()?.follow_statement_list() {
            //println!("statement at: {:?}", self.peek()?.location().unwrap().start);
            match self.declaration_opt()? {
                Some(decl) => { items.push(StmtListItem::Decl(decl)); }
                None       => { items.push(StmtListItem::Stmt(self.statement()?)); }
            }
        }
        Ok(items)
    }

/*
    pub fn declaration(&mut self) -> Result<Decl> {
        match self.declaration_opt()? {
            Some(decl) => Ok(decl),
            None       => Err(Error::UnexpectedToken(self.read()?))
        }
    }
*/

    fn declaration_opt(&mut self) -> Result<Option<Decl>> {
        match self.peek()?.value {
            TokenData::Reserved(Reserved::Function) => Ok(Some(self.function_declaration()?)),
            _                                       => Ok(None)
        }
    }

    fn function_declaration(&mut self) -> Result<Decl> {
        self.span(&mut |this| {
            Ok(Decl::Fun(this.function()?))
        })
    }

    fn formal_parameters(&mut self) -> Result<Params> {
        self.span(&mut |this| {
            this.expect(TokenData::LParen)?;
            let list = this.pattern_list()?;
            this.expect(TokenData::RParen)?;
            Ok(Params { location: None, list: list })
        })
    }

    fn pattern_list(&mut self) -> Result<Vec<Patt<Id>>> {
        let mut patts = Vec::new();
        if self.peek()?.value == TokenData::RParen {
            return Ok(patts);
        }
        patts.push(self.pattern()?);
        while self.matches(TokenData::Comma)? {
            patts.push(self.pattern()?);
        }
        Ok(patts)
    }

    fn pattern(&mut self) -> Result<Patt<Id>> {
        match self.peek()?.value {
            TokenData::Identifier(_) => {
                let id = self.binding_id()?;
                Ok(Patt::Simple(id))
            }
            _ => {
                let patt = self.binding_pattern()?;
                Ok(Patt::Compound(patt))
            }
        }
    }

    fn binding_pattern(&mut self) -> Result<CompoundPatt<Id>> {
        if !self.peek()?.first_binding() {
            return Err(Error::UnexpectedToken(self.read()?));
        }
        Err(Error::UnsupportedFeature("destructuring"))
    }

    fn strict(&self) -> Strict {
        match self.context.function {
            Some(strict) => strict,
            None => self.goal.strict()
        }
    }

    fn in_function<T, F>(&mut self, f: &mut F) -> T
        where F: FnMut(&mut Self) -> T
    {
        let strict = self.strict();
        let outer = replace(&mut self.context, Context::new_function(strict));
        let result = f(self);
        replace(&mut self.context, outer);
        result
    }

    fn function(&mut self) -> Result<Fun> {
        self.in_function(&mut |this| {
            this.span(&mut |this| {
                this.reread(TokenData::Reserved(Reserved::Function));
                let id = this.id_opt()?;
                let params = this.formal_parameters()?;
                this.expect(TokenData::LBrace)?;
                // ES6: if the body has "use strict" check for simple parameters
                let body = this.script()?;
                if body.dirs.iter().any(|dir| dir.pragma() == "use strict") {
                    for param in &params.list {
                        if let Patt::Compound(ref compound) = *param {
                            return Err(Error::CompoundParamWithUseStrict(compound.clone()));
                        }
                    }
                }
                this.expect(TokenData::RBrace)?;
                Ok(Fun { location: None, id: id, params: params, body: body })
            })
        })
    }

    fn statement(&mut self) -> Result<Stmt> {
        match self.peek()?.value {
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
                let id = self.id().unwrap();
                self.id_statement(id)
            }
            _                                       => self.expression_statement()
        }
    }

    fn id_statement(&mut self, id: Id) -> Result<Stmt> {
        match self.peek_op()?.value {
            TokenData::Colon => self.labelled_statement(id),
            TokenData::Identifier(_) | TokenData::LBrace | TokenData::LBrack
            if id.name == Name::Atom(Atom::Let) => {
                self.let_statement(id.tracking_ref().unwrap().start)
            },
            _ => {
                let span = self.start();
                let expr = self.id_expression(id)?;
                Ok(span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Expr(None, expr, semi))?)
            }
        }
    }

    fn string_statement(&mut self, string: StringToken) -> Result<Stmt> {
        let span = self.start();
        let expr = self.string_expression(string)?;
        Ok(span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Expr(None, expr, semi))?)
    }

    fn labelled_statement(&mut self, id: Id) -> Result<Stmt> {
        self.reread(TokenData::Colon);

        let mut labels = vec![id]; // vector of consecutive labels
        let mut expr_id = None;    // id that starts the statement following the labels, if any

        while let TokenData::Identifier(_) = self.peek()?.value {
            let id = self.id().unwrap();
            if !self.matches_op(TokenData::Colon)? {
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
                let label_type = self.peek()?.label_type();
                self.with_labels(labels, label_type, |this| this.statement())
            }
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        let expr = self.allow_in(true, |this| this.expression())?;
        Ok(span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Expr(None, expr, semi))?)
    }

    fn block_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::LBrace);
            let items = this.statement_list()?;
            this.expect(TokenData::RBrace)?;
            Ok(Stmt::Block(None, items))
        })
    }

    fn var_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Var));
        let dtors = self.declarator_list()?;
        span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Var(None, dtors, semi))
    }

    fn let_statement(&mut self, start: Posn) -> Result<Stmt> {
        let span = SpanTracker::new(start);
        let dtors = self.declarator_list()?;
        span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Let(None, dtors, semi))
    }

    fn declarator_list(&mut self) -> Result<Vec<Dtor>> {
        let mut items = Vec::new();
        items.push(self.declarator()?);
        while self.matches(TokenData::Comma)? {
            items.push(self.declarator()?);
        }
        Ok(items)
    }

    fn binding_id(&mut self) -> Result<Id> {
        let id = self.id()?;
        if id.name.is_illegal_strict_binding() {
            let error = Error::IllegalStrictBinding(id.tracking_ref().unwrap(), id.name.atom().unwrap());
            if self.goal.definitely_strict() {
                if !self.validate {
                    self.deferred.push(Check::Failed(error));
                } else {
                    return Err(error);
                }
            } else if !self.goal.definitely_sloppy() {
                self.deferred.push(Check::Strict(error));
            }
        }
        Ok(id)
    }

    fn id(&mut self) -> Result<Id> {
        let Token { location, newline, value: data } = self.read()?;
        match data {
            TokenData::Identifier(name) => {
                let reserved = name.is_reserved(self.goal);
                if reserved.unknown() {
                    self.deferred.push(Check::Reserved(location, name.atom().unwrap()));
                } else if reserved.yes() {
                    let error = Error::ContextualKeyword(location, name.atom().unwrap());
                    if !self.validate {
                        self.deferred.push(Check::Failed(error));
                    } else {
                        return Err(error);
                    }
                }
                Ok(Id { location: Some(location), name: name })
            }
            _ => Err(Error::UnexpectedToken(Token {
                location: location,
                newline: newline,
                value: data
            }))
        }
    }

    fn id_opt(&mut self) -> Result<Option<Id>> {
        let next = self.read()?;
        match next.value {
            TokenData::Identifier(name) => {
                Ok(Some(Id { location: Some(next.location), name: name }))
            }
            _                           => { self.lexer.unread_token(next); Ok(None) }
        }
    }

    fn declarator(&mut self) -> Result<Dtor> {
        self.span(&mut |this| {
            match this.peek()?.value {
                TokenData::Identifier(_) => {
                    let id = this.binding_id()?;
                    let init = if this.matches(TokenData::Assign)? {
                        Some(this.assignment_expression()?)
                    } else {
                        None
                    };
                    Ok(Dtor::Simple(None, id, init))
                }
                _ => {
                    let lhs = this.binding_pattern()?;
                    this.expect(TokenData::Assign)?;
                    let rhs = this.assignment_expression()?;
                    Ok(Dtor::Compound(None, lhs, rhs))
                }
            }
        })
    }

    fn empty_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.expect(TokenData::Semi)?;
            Ok(Stmt::Empty(None))
        })
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.expect(TokenData::Reserved(Reserved::If))?;
            let test = this.paren_expression()?;
            let cons = Box::new(this.statement()?);
            let alt = if this.peek()?.value == TokenData::Reserved(Reserved::Else) {
                this.reread(TokenData::Reserved(Reserved::Else));
                Some(Box::new(this.statement()?))
            } else {
                None
            };
            Ok(Stmt::If(None, test, cons, alt))
        })
    }

    fn iteration_body(&mut self) -> Result<Stmt> {
        let iteration = replace(&mut self.context.iteration, true);
        let result = self.statement();
        replace(&mut self.context.iteration, iteration);
        result
    }

    fn do_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Do));
        let body = Box::new(self.iteration_body()?);
        self.expect(TokenData::Reserved(Reserved::While))?;
        let test = self.paren_expression()?;
        Ok(span.end_with_auto_semi(self, Newline::Optional, |semi| {
            Stmt::DoWhile(None, body, test, semi)
        })?)
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::While));
            let test = this.paren_expression()?;
            let body = Box::new(this.iteration_body()?);
            Ok(Stmt::While(None, test, body))
        })
    }

    fn for_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::For));
            this.expect(TokenData::LParen)?;
            match this.peek()?.value {
                TokenData::Reserved(Reserved::Var)           => this.for_var(),
                TokenData::Identifier(Name::Atom(Atom::Let)) => this.for_let(),
                TokenData::Reserved(Reserved::Const)         => { return Err(Error::UnsupportedFeature("const")); }
                TokenData::Semi                              => {
                    this.reread(TokenData::Semi);
                    this.more_for(None)
                }
                _                                            => this.for_expr()
            }
        })
    }

    // 'for' '(' 'var' .
    fn for_var(&mut self) -> Result<Stmt> {
        let var_token = self.reread(TokenData::Reserved(Reserved::Var));
        let var_location = Some(var_token.location);
        let lhs = self.pattern()?;
        match self.peek()?.value {
            // 'for' '(' 'var' id   '=' .
            // 'for' '(' 'var' patt '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                match lhs {
                    Patt::Simple(id) => {
                        let rhs = self.allow_in(false, |this| this.assignment_expression())?;
                        match self.peek()?.value {
                            // 'for' '(' 'var' id '=' expr ','  . ==> C-style
                            // 'for' '(' 'var' id '=' expr ';'  . ==> C-style
                            TokenData::Comma
                          | TokenData::Semi => {
                                let head = Some(self.more_for_head(&var_location, Dtor::from_simple_init(id, rhs), ForHead::Var)?);
                                self.more_for(head)
                            }
                            // 'for' '(' 'var' id '=' expr 'in' . ==> legacy enumeration
                            TokenData::Reserved(Reserved::In) => {
                                self.reread(TokenData::Reserved(Reserved::In));
                                let head = Box::new(ForInHead::VarInit(span(&var_location, &rhs), id, rhs));
                                self.more_for_in(head)
                            }
                            _ => Err(Error::UnexpectedToken(self.read()?))
                        }
                    }
                    // 'for' '(' 'var' patt '=' . ==> C-style
                    Patt::Compound(patt) => {
                        let rhs = self.allow_in(false, |this| this.assignment_expression())?;
                        let head = Some(self.more_for_head(&var_location, Dtor::from_compound_init(patt, rhs), ForHead::Var)?);
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
                    Err(_) => { return Err(Error::UnexpectedToken(self.read()?)); }
                };
                let head = Some(self.more_for_head(&var_location, dtor, ForHead::Var)?);
                self.more_for(head)
            }
            // 'for' '(' 'var' id   'in' . ==> enumeration
            // 'for' '(' 'var' patt 'in' . ==> enumeration
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead::Var(span(&var_location, &lhs), lhs));
                self.more_for_in(head)
            }
            // 'for' '(' 'var' id   'of' . ==> enumeration
            // 'for' '(' 'var' patt 'of' . ==> enumeration
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead::Var(span(&var_location, &lhs), lhs));
                self.more_for_of(head)
            }
            _ => Err(Error::UnexpectedToken(self.read()?))
        }
    }

    // 'for' '(' 'let' .
    fn for_let(&mut self) -> Result<Stmt> {
        let let_token = self.reread(TokenData::Identifier(Name::Atom(Atom::Let)));
        let let_location = Some(let_token.location);
        // 'for' '(' 'let' . !{id, patt} ==> error
        let lhs = self.pattern()?;
        match self.peek()?.value {
            // 'for' '(' 'let' id   '=' . ==> C-style
            // 'for' '(' 'let' patt '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                let rhs = self.allow_in(false, |this| this.assignment_expression())?;
                let head = Some(self.more_for_head(&let_location, Dtor::from_init(lhs, rhs), ForHead::Let)?);
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
                    Err(_) => { return Err(Error::UnexpectedToken(self.read()?)); }
                };
                let head = Some(self.more_for_head(&let_location, dtor, ForHead::Let)?);
                self.more_for(head)
            }
            // 'for' '(' 'let' id   'in' . ==> enumeration
            // 'for' '(' 'let' patt 'in' . ==> enumeration
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead::Let(span(&let_location, &lhs), lhs));
                self.more_for_in(head)
            }
            // 'for' '(' 'let' id   'of' . ==> enumeration
            // 'for' '(' 'let' patt 'of' . ==> enumeration
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead::Let(span(&let_location, &lhs), lhs));
                self.more_for_of(head)
            }
            _ => Err(Error::UnexpectedToken(self.read()?))
        }
    }

    fn for_expr(&mut self) -> Result<Stmt> {
        let lhs = self.allow_in(false, |this| this.expression())?;
        match self.peek()?.value {
            TokenData::Semi => {
                let semi_location = Some(self.reread(TokenData::Semi).location);
                let head = Some(Box::new(ForHead::Expr(span(&lhs, &semi_location), lhs)));
                self.more_for(head)
            }
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let lhs_location = *lhs.tracking_ref();
                let lhs = match lhs.into_assign_patt() {
                    Ok(lhs) => lhs,
                    Err(cover_err) => { return Err(Error::InvalidLHS(lhs_location, cover_err)); }
                };
                let head = Box::new(ForInHead::Patt(lhs));
                self.more_for_in(head)
            }
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let lhs_location = *lhs.tracking_ref();
                let lhs = match lhs.into_assign_patt() {
                    Ok(lhs) => lhs,
                    Err(cover_err) => { return Err(Error::InvalidLHS(lhs_location, cover_err)); }
                };
                let head = Box::new(ForOfHead::Patt(lhs));
                self.more_for_of(head)
            }
            _ => Err(Error::UnexpectedToken(self.read()?))
        }
    }

    // 'for' '(' dtor .
    fn more_for_head<F>(&mut self, start: &Option<Span>, dtor: Dtor, op: F) -> Result<Box<ForHead>>
      where F: FnOnce(Option<Span>, Vec<Dtor>) -> ForHead
    {
        let dtors = self.allow_in(false, |this| {
            let mut dtors = vec![dtor];
            this.more_dtors(&mut dtors)?;
            Ok(dtors)
        })?;
        let semi_location = Some(self.expect(TokenData::Semi)?.location);
        Ok(Box::new(op(span(start, &semi_location), dtors)))
    }

    // 'for' '(' head ';' .
    fn more_for(&mut self, head: Option<Box<ForHead>>) -> Result<Stmt> {
        let test = self.expression_opt_semi()?;
        let update = if self.matches(TokenData::RParen)? {
            None
        } else {
            let node = Some(self.allow_in(true, |this| this.expression())?);
            self.expect(TokenData::RParen)?;
            node
        };
        let body = Box::new(self.iteration_body()?);
        Ok(Stmt::For(None, head, test, update, body))
    }

    // 'for' '(' head 'in' .
    fn more_for_in(&mut self, head: Box<ForInHead>) -> Result<Stmt> {
        let obj = self.allow_in(true, |this| this.assignment_expression())?;
        self.expect(TokenData::RParen)?;
        let body = Box::new(self.iteration_body()?);
        Ok(Stmt::ForIn(None, head, obj, body))
    }

    // 'for' '(' head 'of' .
    fn more_for_of(&mut self, head: Box<ForOfHead>) -> Result<Stmt> {
        let obj = self.allow_in(true, |this| this.assignment_expression())?;
        self.expect(TokenData::RParen)?;
        let body = Box::new(self.iteration_body()?);
        Ok(Stmt::ForOf(None, head, obj, body))
    }

    fn expression_opt_semi(&mut self) -> Result<Option<Expr>> {
        Ok(if self.matches(TokenData::Semi)? {
            None
        } else {
            let expr = self.allow_in(true, |this| this.expression())?;
            self.expect(TokenData::Semi)?;
            Some(expr)
        })
    }

    fn more_dtors(&mut self, dtors: &mut Vec<Dtor>) -> Result<()> {
        while self.matches(TokenData::Comma)? {
            dtors.push(self.declarator()?);
        }
        Ok(())
    }

    fn switch_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Switch));
            let disc = this.paren_expression()?;
            let outer_switch = replace(&mut this.context.switch, true);
            let cases = this.switch_cases();
            replace(&mut this.context.switch, outer_switch);
            Ok(Stmt::Switch(None, disc, cases?))
        })
    }

    fn switch_cases(&mut self) -> Result<Vec<Case>> {
        self.expect(TokenData::LBrace)?;
        let mut cases = Vec::new();
        let mut found_default = false;
        loop {
            match self.peek()?.value {
                TokenData::Reserved(Reserved::Case) => { cases.push(self.case()?); }
                TokenData::Reserved(Reserved::Default) => {
                    if found_default {
                        let token = self.reread(TokenData::Reserved(Reserved::Default));
                        return Err(Error::DuplicateDefault(token));
                    }
                    found_default = true;
                    cases.push(self.default()?);
                }
                _ => { break; }
            }
        }
        self.expect(TokenData::RBrace)?;
        Ok(cases)
    }

    fn case(&mut self) -> Result<Case> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Case));
            let test = this.allow_in(true, |this| this.expression())?;
            this.expect(TokenData::Colon)?;
            let body = this.statement_list()?;
            Ok(Case { location: None, test: Some(test), body: body })
        })
    }

    fn default(&mut self) -> Result<Case> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Default));
            this.expect(TokenData::Colon)?;
            let body = this.statement_list()?;
            Ok(Case { location: None, test: None, body: body })
        })
    }

    fn break_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        let break_token = self.reread(TokenData::Reserved(Reserved::Break));
        let arg = if self.has_arg_same_line()? {
            let id = self.id()?;
            if !self.context.labels.contains_key(&Rc::new(id.name.clone())) {
                return Err(Error::InvalidLabel(id));
            }
            Some(id)
        } else {
            if !self.context.iteration && !self.context.switch {
                return Err(Error::IllegalBreak(break_token));
            }
            None
        };
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            Stmt::Break(None, arg, semi)
        })
    }

    fn continue_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        let continue_token = self.reread(TokenData::Reserved(Reserved::Continue));
        let arg = if self.has_arg_same_line()? {
            let id = self.id()?;
            match self.context.labels.get(&Rc::new(id.name.clone())) {
                None                        => { return Err(Error::InvalidLabel(id)); }
                Some(&LabelType::Statement) => { return Err(Error::InvalidLabelType(id)); }
                _                           => { }
            }
            Some(id)
        } else {
            if !self.context.iteration {
                return Err(Error::IllegalContinue(continue_token));
            }
            None
        };
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            Stmt::Cont(None, arg, semi)
        })
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Return));
        let arg = if self.has_arg_same_line()? {
            Some(self.allow_in(true, |this| this.expression())?)
        } else {
            None
        };
        let result = span.end_with_auto_semi(self, Newline::Required, |semi| {
            Stmt::Return(None, arg, semi)
        })?;
        if self.context.function.is_none() {
            Err(Error::TopLevelReturn(result.tracking_ref().unwrap()))
        } else {
            Ok(result)
        }
    }

    fn with_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            let token = this.reread(TokenData::Reserved(Reserved::With));
            if this.goal.definitely_strict() {
                let error = Error::StrictWith(token);
                if !this.validate {
                    this.deferred.push(Check::Failed(error));
                } else {
                    return Err(error);
                }
            } else if !this.goal.definitely_sloppy() {
                this.deferred.push(Check::Strict(Error::StrictWith(token)));
            }
            let obj = this.paren_expression()?;
            let body = Box::new(this.statement()?);
            Ok(Stmt::With(None, obj, body))
        })
    }

    fn throw_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        let token = self.reread(TokenData::Reserved(Reserved::Throw));
        if !self.has_arg_same_line()? {
            return Err(Error::ThrowArgument(token));
        }
        let arg = self.allow_in(true, |this| this.expression())?;
        span.end_with_auto_semi(self, Newline::Required, |semi| {
            Stmt::Throw(None, arg, semi)
        })
    }

    fn block(&mut self) -> Result<Vec<StmtListItem>> {
        self.expect(TokenData::LBrace)?;
        let result = self.statement_list()?;
        self.expect(TokenData::RBrace)?;
        Ok(result)
    }

    fn try_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Try));
            let body = this.block()?;
            match this.peek()?.value {
                TokenData::Reserved(Reserved::Catch)
              | TokenData::Reserved(Reserved::Finally) => { }
                _ => {
                    return Err(Error::OrphanTry(this.read()?));
                }
            }
            let catch = this.catch_opt()?.map(Box::new);
            let finally = this.finally_opt()?;
            Ok(Stmt::Try(None, body, catch, finally))
        })
    }

    fn catch_opt(&mut self) -> Result<Option<Catch>> {
        match self.peek()?.value {
            TokenData::Reserved(Reserved::Catch) => {
                self.span(&mut |this| {
                    this.reread(TokenData::Reserved(Reserved::Catch));
                    this.expect(TokenData::LParen)?;
                    let param = this.pattern()?;
                    this.expect(TokenData::RParen)?;

                    let body = this.block()?;
                    Ok(Catch { location: None, param: param, body: body })
                }).map(Some)
            }
            _ => Ok(None)
        }
    }

    fn finally_opt(&mut self) -> Result<Option<Vec<StmtListItem>>> {
        Ok(match self.peek()?.value {
            TokenData::Reserved(Reserved::Finally) => {
                self.reread(TokenData::Reserved(Reserved::Finally));
                Some(self.block()?)
            }
            _ => None
        })
    }

    fn debugger_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Debugger));
        Ok(span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Debugger(None, semi))?)
    }

    fn paren_expression(&mut self) -> Result<Expr> {
        self.expect(TokenData::LParen)?;
        let result = self.allow_in(true, |this| this.expression())?;
        self.expect(TokenData::RParen)?;
        Ok(result)
    }

    // PrimaryExpression ::=
    //   "this"
    //   IdentifierReference
    //   Literal
    //   ArrayLiteral
    //   ObjectLiteral
    //   FunctionExpression
    //   ClassExpression
    //   GeneratorExpression
    //   RegularExpressionLiteral
    //   "(" Expression ")"
    fn primary_expression(&mut self) -> Result<Expr> {
        let token = self.read()?;
        let location = Some(token.location);
        Ok(match token.value {
            TokenData::Identifier(name)          => Expr::Id(Id::new(name, location)),
            TokenData::Reserved(Reserved::Null)  => Expr::Null(location),
            TokenData::Reserved(Reserved::This)  => Expr::This(location),
            TokenData::Reserved(Reserved::True)  => Expr::True(location),
            TokenData::Reserved(Reserved::False) => Expr::False(location),
            TokenData::Number(literal)           => Expr::Number(location, literal),
            TokenData::String(literal)           => Expr::String(location, literal),
            TokenData::RegExp(literal)           => Expr::RegExp(location, literal),
            TokenData::LBrack                    => { return self.array_literal(token); }
            TokenData::LBrace                    => { return self.object_literal(token); }
            TokenData::Reserved(Reserved::Function) => {
                self.lexer.unread_token(token);
                return Ok(Expr::Fun(self.function()?));
            }
            TokenData::LParen => {
                self.lexer.unread_token(token);
                return self.paren_expression();
            }
            // ES6: more cases
            _ => { return Err(Error::UnexpectedToken(token)); }
        })
    }

    fn array_literal(&mut self, start: Token) -> Result<Expr> {
        let start_location = Some(start.location);
        let mut elts = Vec::new();
        loop {
            // Optional final comma does not count as an element.
            if self.peek()?.value == TokenData::RBrack {
                break;
            }
            elts.push(self.array_element()?);
            if !self.matches(TokenData::Comma)? {
                break;
            }
        }
        let end_location = Some(self.expect(TokenData::RBrack)?.location);
        Ok(Expr::Arr(span(&start_location, &end_location), elts))
    }

    fn array_element(&mut self) -> Result<Option<Expr>> {
        if { let t = self.peek()?; t.value == TokenData::Comma || t.value == TokenData::RBrack } {
            return Ok(None);
        }
        // ES6: ellipsis
        self.allow_in(true, |this| this.assignment_expression().map(Some))
    }

    fn object_literal(&mut self, start: Token) -> Result<Expr> {
        let start_location = Some(start.location);
        let mut props = Vec::new();
        loop {
            if self.peek()?.value == TokenData::RBrace {
                break;
            }
            props.push(self.object_property()?);
            if !self.matches(TokenData::Comma)? {
                break;
            }
        }
        let end_location = Some(self.expect(TokenData::RBrace)?.location);
        Ok(Expr::Obj(span(&start_location, &end_location), props))
    }

    fn more_prop_init(&mut self, key: PropKey) -> Result<Prop> {
        self.reread(TokenData::Colon);
        let val = self.allow_in(true, |this| this.assignment_expression())?;
        let key_location = *key.tracking_ref();
        let val_location = *val.tracking_ref();
        Ok(Prop {
            location: span(&key_location, &val_location),
            key: key,
            val: PropVal::Init(val)
        })
    }

    fn property_key_opt(&mut self) -> Result<Option<PropKey>> {
        let token = self.read()?;
        let location = Some(token.location);
        Ok(Some(match token.value {
            TokenData::Identifier(name) => PropKey::Id(location, name.into_string()),
            TokenData::Reserved(word) => PropKey::Id(location, word.into_string()),
            TokenData::String(s) => PropKey::String(location, s),
            TokenData::Number(n) => PropKey::Number(location, n),
            _ => {
                self.lexer.unread_token(token);
                return Ok(None);
            }
        }))
    }

    fn property_key(&mut self) -> Result<PropKey> {
        match self.property_key_opt()? {
            Some(key) => Ok(key),
            None => Err(Error::UnexpectedToken(self.read()?))
        }
    }

    fn object_property(&mut self) -> Result<Prop> {
        let first = self.read()?;
        match first.value {
            TokenData::Identifier(Name::Atom(Atom::Get)) => {
                if let Some(key) = self.property_key_opt()? {
                    let paren_location = Some(self.expect(TokenData::LParen)?.location);
                    self.expect(TokenData::RParen)?;
                    self.expect(TokenData::LBrace)?;
                    let body = self.in_function(&mut |this| this.script())?;
                    let end_location = Some(self.expect(TokenData::RBrace)?.location);
                    let val_location = span(&paren_location, &end_location);
                    let prop_location = span(&key, &end_location);
                    return Ok(Prop {
                        location: prop_location,
                        key: key,
                        val: PropVal::Get(val_location, body)
                    });
                }
                match self.peek()?.value {
                    // ES6: TokenData::LParen => unimplemented!(),
                    TokenData::Colon => {
                        let key_location = Some(first.location);
                        self.more_prop_init(PropKey::Id(key_location, "get".to_string()))
                    }
                    // ES6: treat as elided optional initializer
                    _ => { return Err(Error::UnexpectedToken(self.read()?)); }
                }
            }
            TokenData::Identifier(Name::Atom(Atom::Set)) => {
                if let Some(key) = self.property_key_opt()? {
                    let paren_location = Some(self.expect(TokenData::LParen)?.location);
                    let param = self.pattern()?;
                    self.expect(TokenData::RParen)?;
                    self.expect(TokenData::LBrace)?;
                    // ES6: if the body has "use strict" check for simple parameters
                    let body = self.in_function(&mut |this| this.script())?;
                    let end_location = Some(self.expect(TokenData::RBrace)?.location);
                    let val_location = span(&paren_location, &end_location);
                    let prop_location = span(&key, &end_location);
                    return Ok(Prop {
                        location: prop_location,
                        key: key,
                        val: PropVal::Set(val_location, param, body)
                    });
                }
                match self.peek()?.value {
                    // ES6: TokenData::LParen => unimplemented!(),
                    TokenData::Colon => {
                        let key_location = Some(first.location);
                        self.more_prop_init(PropKey::Id(key_location, "set".to_string()))
                    }
                    // ES6: treat as elided optional initializer
                    _ => { return Err(Error::UnexpectedToken(self.read()?)); }
                }
            }
            // ES6: TokenData::Star
            _ => {
                self.lexer.unread_token(first);
                let key = self.property_key()?;
                match self.peek()?.value {
                    TokenData::Colon => self.more_prop_init(key),
                    // ES6: TokenData::LParen =>
                    // ES6: treat as elided optional initializer
                    _ => { return Err(Error::UnexpectedToken(self.read()?)); }
                }
            }
        }
    }

    // MemberBaseExpression ::=
    //   PrimaryExpression
    //   "new" "." "target"
    fn member_base_expression(&mut self) -> Result<Expr> {
        if let Some(new) = self.matches_token(TokenData::Reserved(Reserved::New))? {
            self.expect(TokenData::Dot)?;
            let target_location = Some(self.expect(TokenData::Identifier(Name::Atom(Atom::Target)))?.location);
            return Ok(Expr::NewTarget(span(&Some(new.location), &target_location)));
        }
        self.primary_expression()
    }

    // "new"+n . (MemberBaseExpression | "super" Deref) Deref* Arguments<n Suffix*
    fn new_expression(&mut self, news: Vec<Token>) -> Result<Expr> {
        // ES6: if let Some(super) = self.match_token(TokenData::Reserved(Reserved::Super))? {
        let base = self.member_base_expression()?;
        self.more_new_expression(news, base)
    }

    // "new"+n MemberBaseExpression . Deref* Arguments<n Suffix*
    fn more_new_expression(&mut self, news: Vec<Token>, mut base: Expr) -> Result<Expr> {
        while let Some(deref) = self.deref_opt()? {
            base = deref.append_to(base);
        }
        let mut has_args = true;
        for new in news.into_iter().rev() {
            has_args = has_args && self.peek_op()?.value == TokenData::LParen;
            base = if has_args {
                self.arguments()?.append_to_new(new, base)
            } else {
                let location = span(&Some(new.location), &base);
                Expr::New(location, Box::new(base), None)
            };
        }
        self.more_suffixes(base)
    }

    // CallExpression ::=
    //   (MemberBaseExpression | "super" Suffix) Suffix*
    fn call_expression(&mut self) -> Result<Expr> {
        // ES6: super
        let base = self.primary_expression()?;
        self.more_suffixes(base)
    }

    // Suffix ::=
    //   Deref
    //   Arguments
    fn suffix_opt(&mut self) -> Result<Option<Suffix>> {
        match self.peek_op()?.value {
            TokenData::Dot    => self.deref_dot().map(|deref| Some(Suffix::Deref(deref))),
            TokenData::LBrack => self.deref_brack().map(|deref| Some(Suffix::Deref(deref))),
            TokenData::LParen => self.arguments().map(|args| Some(Suffix::Arguments(args))),
            _ => Ok(None)
        }
    }


    // Argument ::= "..."? AssignmentExpression
    fn argument(&mut self) -> Result<Expr> {
        // ES6: if let ellipsis = self.matches(TokenData::Ellipsis)? { ... }
        self.allow_in(true, |this| this.assignment_expression())
    }

    // Arguments ::= "(" Argument*[","] ")"
    fn arguments(&mut self) -> Result<Arguments> {
        self.expect(TokenData::LParen)?;
        if let Some(end) = self.matches_token(TokenData::RParen)? {
            return Ok(Arguments { args: Vec::new(), end: end });
        }
        let mut args = Vec::new();
        loop {
            args.push(self.argument()?);
            if !self.matches(TokenData::Comma)? {
                break;
            }
        }
        let end = self.expect(TokenData::RParen)?;
        Ok(Arguments { args: args, end: end })
    }

/*
    fn deref(&mut self) -> Result<Deref> {
        match self.peek_op()?.value {
            TokenData::LBrack => self.deref_brack(),
            TokenData::Dot    => self.deref_dot(),
            _ => Err(Error::UnexpectedToken(self.read_op()?))
        }
    }
*/

    // Deref ::=
    //   "[" Expression "]"
    //   "." IdentifierName
    fn deref_opt(&mut self) -> Result<Option<Deref>> {
        match self.peek_op()?.value {
            TokenData::LBrack => self.deref_brack().map(Some),
            TokenData::Dot    => self.deref_dot().map(Some),
            _ => Ok(None)
        }
    }

    fn deref_brack(&mut self) -> Result<Deref> {
        self.reread(TokenData::LBrack);
        let expr = self.allow_in(true, |this| this.expression())?;
        let end = self.expect(TokenData::RBrack)?;
        Ok(Deref::Brack(expr, end))
    }

    fn id_name(&mut self) -> Result<DotKey> {
        let token = self.read()?;
        Ok(DotKey {
            location: Some(token.location),
            value: match token.value {
                TokenData::Identifier(name) => name.into_string(),
                TokenData::Reserved(word) => word.into_string(),
                _ => { return Err(Error::UnexpectedToken(token)); }
            }
        })
    }

    fn deref_dot(&mut self) -> Result<Deref> {
        self.reread(TokenData::Dot);
        Ok(Deref::Dot(self.id_name()?))
    }

    // MemberBaseExpression . Suffix*
    fn more_suffixes(&mut self, mut result: Expr) -> Result<Expr> {
        while let Some(suffix) = self.suffix_opt()? {
            result = suffix.append_to(result);
        }
        Ok(result)
    }

    // LHSExpression ::=
    //   NewExpression
    //   CallExpression
    fn lhs_expression(&mut self) -> Result<Expr> {
        let mut news = Vec::new();
        while self.peek()?.value == TokenData::Reserved(Reserved::New) {
            news.push(self.reread(TokenData::Reserved(Reserved::New)));
        }
        if news.len() > 0 {
            if self.matches_op(TokenData::Dot)? {
                let target_location = Some(self.expect(TokenData::Identifier(Name::Atom(Atom::Target)))?.location);
                let new = news.pop();
                let new_location = new.map(|new| new.location);
                let new_target = Expr::NewTarget(span(&new_location, &target_location));
                if news.len() > 0 {
                    self.more_new_expression(news, new_target)
                } else {
                    self.more_suffixes(new_target)
                }
            } else {
                self.new_expression(news)
            }
        } else {
            self.call_expression()
        }
    }

    // IDUnaryExpression ::=
    //   IdentifierReference Suffix* PostfixOperator?
    fn id_unary_expression(&mut self, id: Id) -> Result<Expr> {
        self.unary_suffixes(Expr::Id(id))
    }

    // StringUnaryExpression ::=
    //   StringLiteral Suffix* PostfixOperator?
    fn string_unary_expression(&mut self, string: StringToken) -> Result<Expr> {
        self.unary_suffixes(Expr::String(string.0, string.1))
    }

    fn unary_suffixes(&mut self, mut result: Expr) -> Result<Expr> {
        result = self.more_suffixes(result)?;
        if let Some(postfix) = self.match_postfix_operator_opt()? {
            let result_location = *result.tracking_ref();
            result = match result.into_assign_target().map(Box::new) {
                Ok(target) => {
                    match postfix {
                        Postfix::Inc(location) => Expr::PostInc(Some(location), target),
                        Postfix::Dec(location) => Expr::PostDec(Some(location), target)
                    }
                }
                Err(cover_err) => { return Err(Error::InvalidLHS(result_location, cover_err)); }
            };
        }
        Ok(result)
    }

    // UnaryExpression ::=
    //   Prefix* LHSExpression PostfixOperator?
    fn unary_expression(&mut self) -> Result<Expr> {
        let mut prefixes = Vec::new();
        while let Some(prefix) = self.match_prefix()? {
            prefixes.push(prefix);
        }
        let mut arg = self.lhs_expression()?;
        if let Some(postfix) = self.match_postfix_operator_opt()? {
            let arg_location = *arg.tracking_ref();
            arg = match arg.into_assign_target().map(Box::new) {
                Ok(target) => {
                    match postfix {
                        Postfix::Inc(location) => Expr::PostInc(Some(location), target),
                        Postfix::Dec(location) => Expr::PostDec(Some(location), target)
                    }
                }
                Err(cover_err) => { return Err(Error::InvalidLHS(arg_location, cover_err)); }
            };
        }
        for prefix in prefixes.into_iter().rev() {
            match prefix {
                Prefix::Unop(op)      => {
                    let location = span(&op, &arg);
                    arg = Expr::Unop(location, op, Box::new(arg));
                }
                _ => {
                    let arg_location = *arg.tracking_ref();
                    arg = match arg.into_assign_target().map(Box::new) {
                        Ok(target) => {
                            match prefix {
                                Prefix::Inc(location) => Expr::PreInc(Some(location), target),
                                Prefix::Dec(location) => Expr::PreDec(Some(location), target),
                                Prefix::Unop(_) => unreachable!()
                            }
                        }
                        Err(cover_err) => { return Err(Error::InvalidLHS(arg_location, cover_err)); }
                    };
                }
            }
        }
        Ok(arg)
    }

    // Prefix ::=
    //   Unop
    //   "++"
    //   "--"
    fn match_prefix(&mut self) -> Result<Option<Prefix>> {
        let token = self.read()?;
        Ok(match token.value {
            TokenData::Inc => Some(Prefix::Inc(token.location)),
            TokenData::Dec => Some(Prefix::Dec(token.location)),
            _ => {
                self.lexer.unread_token(token);
                self.match_unop()?.map(Prefix::Unop)
            }
        })
    }

    // Unop ::=
    //   "delete"
    //   "void"
    //   "typeof"
    //   "+"
    //   "-"
    //   "~"
    //   "!"
    fn match_unop(&mut self) -> Result<Option<Unop>> {
        let token = self.read()?;
        let tag = match token.value {
            TokenData::Reserved(Reserved::Delete) => UnopTag::Delete,
            TokenData::Reserved(Reserved::Void)   => UnopTag::Void,
            TokenData::Reserved(Reserved::Typeof) => UnopTag::Typeof,
            TokenData::Plus                       => UnopTag::Plus,
            TokenData::Minus                      => UnopTag::Minus,
            TokenData::Tilde                      => UnopTag::BitNot,
            TokenData::Bang                       => UnopTag::Not,
            _ => { self.lexer.unread_token(token); return Ok(None); }
        };
        Ok(Some(Op { location: Some(token.location), tag: tag }))
    }

    // PostfixOperator ::=
    //   [no line terminator] "++"
    //   [no line terminator] "--"
    fn match_postfix_operator_opt(&mut self) -> Result<Option<Postfix>> {
        let next = self.read_op()?;
        if !next.newline {
            match next.value {
                TokenData::Inc => { return Ok(Some(Postfix::Inc(next.location))); }
                TokenData::Dec => { return Ok(Some(Postfix::Dec(next.location))); }
                _ => { }
            }
        }
        self.lexer.unread_token(next);
        Ok(None)
    }

    // ConditionalExpression ::=
    //   UnaryExpression (Infix UnaryExpression)* ("?" AssignmentExpression ":" AssignmentExpression)?
    fn conditional_expression(&mut self) -> Result<Expr> {
        let left = self.unary_expression()?;
        let test = self.more_infix_expressions(left)?;
        self.more_conditional(test)
    }

    // IDConditionalExpression ::=
    //   IDUnaryExpression (Infix UnaryExpression)* ("?" AssignmentExpression ":" AssignmentExpression)?
    fn id_conditional_expression(&mut self, id: Id) -> Result<Expr> {
        let left = self.id_unary_expression(id)?;
        let test = self.more_infix_expressions(left)?;
        self.more_conditional(test)
    }

    // StringConditionalExpression ::=
    //   StringUnaryExpression (Infix UnaryExpression)* ("?" AssignmentExpression ":" AssignmentExpression)?
    fn string_conditional_expression(&mut self, string: StringToken) -> Result<Expr> {
        let left = self.string_unary_expression(string)?;
        let test = self.more_infix_expressions(left)?;
        self.more_conditional(test)
    }

    fn more_conditional(&mut self, left: Expr) -> Result<Expr> {
        if self.matches_op(TokenData::Question)? {
            let cons = self.allow_in(true, |this| this.assignment_expression())?;
            self.expect(TokenData::Colon)?;
            let alt = self.assignment_expression()?;
            let location = span(&cons, &alt);
            return Ok(Expr::Cond(location, Box::new(left), Box::new(cons), Box::new(alt)));
        }
        Ok(left)
    }

    // AssignmentExpression ::=
    //   YieldPrefix* "yield"
    //   YieldPrefix* ConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?
    fn assignment_expression(&mut self) -> Result<Expr> {
        let left = self.conditional_expression()?;
        self.more_assignment(left)
    }

    // IDAssignmentExpression ::=
    //   IDConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?
    fn id_assignment_expression(&mut self, id: Id) -> Result<Expr> {
        let left = self.id_conditional_expression(id)?;
        self.more_assignment(left)
    }

    // StringAssignmentExpression ::=
    //   StringConditionalExpression (("=" | AssignmentOperator) AssignmentExpression)?
    fn string_assignment_expression(&mut self, string: StringToken) -> Result<Expr> {
        let left = self.string_conditional_expression(string)?;
        self.more_assignment(left)
    }

    fn more_assignment(&mut self, left: Expr) -> Result<Expr> {
        let token = self.read_op()?;
        let left_location = *left.tracking_ref();
        if token.value == TokenData::Assign {
            let left = match left.into_assign_patt() {
                Ok(left) => left,
                Err(cover_err) => { return Err(Error::InvalidLHS(left_location, cover_err)); }
            };
            let right = self.assignment_expression()?;
            let location = span(&left, &right);
            return Ok(Expr::Assign(location, left, Box::new(right)));
        } else if let Some(op) = token.to_assop() {
            let left = match left.into_assign_target() {
                Ok(left) => left,
                Err(cover_err) => { return Err(Error::InvalidLHS(left_location, cover_err)); }
            };
            let right = self.assignment_expression()?;
            let location = span(&left, &right);
            return Ok(Expr::BinAssign(location, op, left, Box::new(right)));
        }
        self.lexer.unread_token(token);
        Ok(left)
    }

    fn more_infix_expressions(&mut self, left: Expr) -> Result<Expr> {
        let mut stack = Stack::new();
        let mut operand = left;
        while let Some(op) = self.match_infix()? {
            stack.extend(operand, op);
            //println!("{}\n", stack);
            operand = self.unary_expression()?;
        }
        Ok(stack.finish(operand))
    }

    fn match_infix(&mut self) -> Result<Option<Infix>> {
        let token = self.read_op()?;
        let result = token.to_binop(self.context.allow_in).map_or_else(|| {
            token.to_logop().map(Infix::Logop)
        }, |op| Some(Infix::Binop(op)));
        if result.is_none() {
            self.lexer.unread_token(token);
        }
        Ok(result)
    }

    // Expression ::=
    //   AssignmentExpression ("," AssignmentExpression)*
    fn expression(&mut self) -> Result<Expr> {
        let first = self.assignment_expression()?;
        self.more_expressions(first)
    }

    // IDExpression ::=
    //   IDAssignmentExpression ("," AssignmentExpression)*
    fn id_expression(&mut self, id: Id) -> Result<Expr> {
        let first = self.id_assignment_expression(id)?;
        self.more_expressions(first)
    }

    // StringExpression ::=
    //   StringAssignmentExpression ("," AssignmentExpression)*
    fn string_expression(&mut self, string: StringToken) -> Result<Expr> {
        let first = self.string_assignment_expression(string)?;
        self.more_expressions(first)
    }

    fn more_expressions(&mut self, first: Expr) -> Result<Expr> {
        if self.peek()?.value != TokenData::Comma {
            return Ok(first);
        }
        let mut elts = vec![first];
        while self.matches(TokenData::Comma)? {
            elts.push(self.assignment_expression()?);
        }
        let location = self.vec_span(&elts);
        Ok(Expr::Seq(location, elts))
    }
}
