use joker::track::*;
use joker::token::{Token, TokenData};
use joker::word::{Atom, Name, Reserved};
use joker::lexer::Lexer;
use easter::stmt::{Stmt, Block, StmtListItem, ForHead, ForInHead, ForOfHead, Case, Catch, Script, Dir, ModItem, Module};
use easter::expr::{Expr, ExprListItem};
use easter::decl::{Decl, Dtor, ConstDtor, DtorExt};
use easter::patt::{Patt, RestPatt, CompoundPatt};
use easter::fun::{Fun, Params};
use easter::obj::{PropKey, PropVal, Prop, DotKey};
use easter::id::{Id, IdExt};
use easter::punc::{Unop, UnopTag, ToOp, Op};
use easter::cover::{IntoAssignTarget, IntoAssignPatt};

use std::rc::Rc;
use std::mem::replace;
use context::{Context, LabelType, WithContext, Goal};
use tokens::{First, Follows, HasLabelType};
use atom::AtomExt;
use track::Newline;
use result::Result;
use error::{Error, Check};
use track::{SpanTracker, Tracking};
use state::State;
use expr::{Deref, Suffix, Arguments, Prefix, Postfix};
use stack::{Stack, Infix};

use tristate::TriState;
pub use tristate::TriState as Strict;

pub struct Parser<I> {
    pub goal: Goal,
    pub validate: bool,       // should we do strict mode validation as eagerly as possible?
    pub deferred: Vec<Check>, // strict mode checks that haven't been performed yet
    pub lexer: Lexer<I>,
    pub context: Context
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
    pub fn from_chars(i: I) -> Parser<I> {
        let lexer = Lexer::new(i);
        Parser::new(true, lexer)
    }

    pub fn new(validate: bool, lexer: Lexer<I>) -> Parser<I> {
        Parser {
            goal: Goal::Unknown,
            validate: validate,
            deferred: Vec::new(),
            lexer: lexer,
            context: Context::new()
        }
    }

    fn take_deferred(&mut self) -> Vec<Check> {
        replace(&mut self.deferred, Vec::new())
    }

    fn unexpected<T>(&mut self) -> Result<T> {
        Err(Error::UnexpectedToken(self.lexer.reread_token()))
    }

    fn match_directive_opt(&mut self) -> Result<Option<Dir>> {
        let span = self.start();
        let token1 = self.read()?;

        if let TokenData::String(ref literal) = token1.value {
            if !self.peek()?.expression_continuation() {
                return Ok(Some(span.end_with_auto_semi(self, Newline::Required, |semi| Dir {
                    location: None,
                    string: literal.clone(),
                    semi: semi
                })?));
            }
        }

        self.lexer.unread_token(token1);
        Ok(None)
    }

    fn set_module(&mut self) {
        self.goal = Goal::Module;
        self.context.strict = Strict::Yes;
    }

    pub fn module(&mut self) -> Result<Module> {
        debug_assert_eq!(self.goal, Goal::Unknown);
        self.set_module();
        self.span(&mut |this| {
            Ok(Module {
                location: None,
                dirs: this.body_directives()?,
                items: this.module_items()?
            })
        })
    }

    pub fn program(&mut self) -> Result<Program> {
        debug_assert_eq!(self.goal, Goal::Unknown);
        self.span(&mut |this| {
            let dirs = this.body_directives()?;

            match this.program_items()? {
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

    pub fn script(&mut self, strict: bool) -> Result<Script> {
        debug_assert_eq!(self.goal, Goal::Unknown);
        self.goal = Goal::Script;
        self.context.strict = TriState::from(strict);
        self.script_body()
    }

    fn script_body(&mut self) -> Result<Script> {
        self.span(&mut |this| {
            Ok(Script {
                location: None,
                dirs: this.body_directives()?,
                items: this.statement_list()?
            })
        })
    }

    fn body_directives(&mut self) -> Result<Vec<Dir>> {
        let mut dirs = Vec::new();

        while let Some(dir) = self.match_directive_opt()? {
            match dir.pragma() {
                "use strict" => {
                    self.context.strict = Strict::Yes;
                }
                "use module" if !self.context.function => {
                    self.set_module();
                }
                _ => {}
            }
            dirs.push(dir);
        }

        Ok(dirs)
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

    fn program_items(&mut self) -> Result<ProgramItems> {
        let mut stmts: Vec<StmtListItem> = Vec::new();

        loop {
            match self.peek()?.value {
                TokenData::EOF => break,
                TokenData::Reserved(Reserved::Import)
              | TokenData::Reserved(Reserved::Export) => {
                    self.force_deferred_module_validation()?;
                    let items = self.more_module_items(stmts.into_iter().map(|stmt| stmt.into_mod_item()).collect())?;
                    return Ok(ProgramItems::Module(items));
                }
                _ => { }
            }

            stmts.push(self.stmt_list_item(true)?);
        }

        Ok(ProgramItems::Script(stmts))
    }

    fn module_items(&mut self) -> Result<Vec<ModItem>> {
        self.more_module_items(Vec::new())
    }

    fn more_module_items(&mut self, mut items: Vec<ModItem>) -> Result<Vec<ModItem>> {
        loop {
            match self.peek()?.value {
                TokenData::EOF => break,
                // ES6: import declaration
                TokenData::Reserved(Reserved::Import) => unimplemented!(),
                // ES6: export declaration
                TokenData::Reserved(Reserved::Export) => unimplemented!(),
                _ => { }
            }

            items.push(ModItem::StmtListItem(self.stmt_list_item(true)?));
        }

        Ok(items)
    }

    fn statement_list(&mut self) -> Result<Vec<StmtListItem>> {
        let mut items = Vec::new();
        while !self.peek()?.follow_statement_list() {
            //println!("statement at: {:?}", self.peek()?.location().unwrap().start);
            items.push(self.stmt_list_item(true)?);
        }
        Ok(items)
    }

    fn function_declaration(&mut self) -> Result<Decl> {
        self.span(&mut |this| {
            Ok(Decl::Fun(this.function(|this| this.id(true))?))
        })
    }

    fn formal_parameters(&mut self) -> Result<Params> {
        self.span(&mut |this| {
            this.expect(TokenData::LParen)?;
            let mut list = Vec::new();
            let mut rest = None;
            loop {
                match this.peek()?.value {
                    TokenData::RParen => {
                        break;
                    }
                    TokenData::Ellipsis => {
                        rest = Some(this.span(&mut |this| {
                            this.reread(TokenData::Ellipsis);
                            Ok(RestPatt {
                                location: None,
                                patt: this.pattern()?
                            })
                        })?);
                        break;
                    }
                    _ => {
                        list.push(this.pattern()?);
                        if !this.matches(TokenData::Comma)? {
                            break;
                        }
                    }
                }
            }
            this.expect(TokenData::RParen)?;
            Ok(Params {
                location: None,
                list: list,
                rest: rest
            })
        })
    }

    fn pattern(&mut self) -> Result<Patt<Id>> {
        match self.peek()?.value {
            TokenData::Identifier(_) => {
                let id = self.id(true)?;
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
            return self.unexpected();
        }
        Err(Error::UnsupportedFeature("destructuring"))
    }

    fn strict_check<F>(&mut self, f: F) -> Result<()>
        where F: FnOnce(&mut Self) -> Option<Check>
    {
        let strict = self.context.strict;
        if strict != Strict::No {
            if let Some(check) = f(self) {
                match check {
                    Check::Strict(error) => {
                        if strict == Strict::Yes && self.validate {
                            return Err(error);
                        } else {
                            self.deferred.push(Check::Strict(error));
                        }
                    }
                    Check::Module(error) => {
                        if self.goal == Goal::Module && self.validate {
                            return Err(error);
                        } else if self.goal != Goal::Script {
                            self.deferred.push(Check::Module(error));
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn function<Id, F>(&mut self, get_id: F) -> Result<Fun<Id>>
        where F: Fn(&mut Self) -> Result<Id>
    {
        self.span(&mut |this| {
            this.reread(TokenData::Reserved(Reserved::Function));
            let id = get_id(this)?;
            let params = this.formal_parameters()?;
            let body = this.function_body(&params.list)?;
            Ok(Fun { location: None, id: id, params: params, body: body })
        })
    }

    fn function_body(&mut self, params: &[Patt<Id>]) -> Result<Script> {
        let inner = self.context.new_function();
        let outer = replace(&mut self.context, inner);
        self.expect(TokenData::LBrace)?;
        // ES6: if the body has "use strict" check for simple parameters
        let body = self.script_body()?;
        self.strict_check(|_| {
            if body.dirs.iter().any(|dir| dir.pragma() == "use strict") {
                for param in params {
                    if let Patt::Compound(ref compound) = *param {
                        return Some(Check::Strict(Error::CompoundParamWithUseStrict(compound.clone())));
                    }
                }
            }
            None
        })?;
        self.expect(TokenData::RBrace)?;
        self.context = outer;
        Ok(body)
    }

    fn stmt_list_item(&mut self, allow_decl: bool) -> Result<StmtListItem> {
        (match self.peek()?.value {
            TokenData::Reserved(Reserved::Function) => {
                if !allow_decl {
                    return self.unexpected();
                }
                return self.function_declaration().map(StmtListItem::Decl);
            }
            TokenData::LBrace                       => self.block().map(Stmt::Block),
            TokenData::Reserved(Reserved::Var)      => self.var_statement(),
            TokenData::Reserved(Reserved::Const)    => {
                if !allow_decl {
                    return self.unexpected();
                }
                return self.const_declaration().map(StmtListItem::Decl);
            }
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
                let token = self.lexer.reread_token();
                match self.peek_op()?.value {
                    TokenData::Colon => {
                        let id = self.new_id_from_token(false, token)?;
                        self.labelled_statement(id)
                    },
                    TokenData::Identifier(_) | TokenData::LBrace | TokenData::LBrack if token.value == TokenData::Identifier(Name::Atom(Atom::Let)) => {
                        if !allow_decl {
                            return self.unexpected();
                        }
                        return self.let_declaration(token.location.start).map(StmtListItem::Decl);
                    },
                    _ => {
                        self.lexer.unread_token(token);
                        self.expression_statement()
                    }
                }
            }
            _  => self.expression_statement()
        }).map(StmtListItem::Stmt)
    }

    fn statement(&mut self) -> Result<Stmt> {
        self.stmt_list_item(false).map(|item| match item {
            StmtListItem::Stmt(stmt) => stmt,
            _ => unreachable!()
        })
    }

    fn labelled_statement(&mut self, id: Id) -> Result<Stmt> {
        self.reread(TokenData::Colon);

        let mut labels = vec![id]; // vector of consecutive labels

        while let TokenData::Identifier(_) = self.peek()?.value {
            let token = self.lexer.reread_token();
            if !self.matches_op(TokenData::Colon)? {
                self.lexer.unread_token(token);
                break;
            }
            labels.push(self.new_id_from_token(false, token)?);
        }

        let label_type = self.peek()?.label_type();
        self.with_labels(labels, label_type, |this| this.statement())
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        let expr = self.allow_in(true, |this| this.expression())?;
        Ok(span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Expr(None, expr, semi))?)
    }

    fn block(&mut self) -> Result<Block> {
        self.span(&mut |this| {
            this.expect(TokenData::LBrace)?;
            let items = this.statement_list()?;
            this.expect(TokenData::RBrace)?;
            Ok(Block {
                location: None,
                items: items
            })
        })
    }

    fn var_statement(&mut self) -> Result<Stmt> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Var));
        let dtors = self.comma_separated(Self::declarator)?;
        span.end_with_auto_semi(self, Newline::Required, |semi| Stmt::Var(None, dtors, semi))
    }

    fn let_declaration(&mut self, start: Posn) -> Result<Decl> {
        let span = SpanTracker::new(start);
        let dtors = self.comma_separated(Self::declarator)?;
        span.end_with_auto_semi(self, Newline::Required, |semi| Decl::Let(None, dtors, semi))
    }

    fn const_declaration(&mut self) -> Result<Decl> {
        let span = self.start();
        self.reread(TokenData::Reserved(Reserved::Const));
        let dtors = self.comma_separated(Self::const_declarator)?;
        span.end_with_auto_semi(self, Newline::Required, |semi| Decl::Const(None, dtors, semi))
    }

    fn new_id_from_token(&mut self, binding: bool, token: Token) -> Result<Id> {
        match token.value {
            TokenData::Identifier(name) => self.new_id(binding, name, token.location),
            _ => unreachable!()
        }
    }

    fn new_id(&mut self, binding: bool, name: Name, location: Span) -> Result<Id> {
        self.strict_check(|_| {
            if binding && name.is_illegal_strict_binding() {
                return Some(Check::Strict(Error::IllegalStrictBinding(location, name.atom().unwrap())));
            }
            let is_reserved = name.is_strict_reserved();
            if is_reserved != TriState::No {
                let error = Error::ContextualKeyword(location, name.atom().unwrap());
                return Some(if is_reserved == TriState::Yes {
                    Check::Strict(error)
                } else {
                    // TriState::Unknown means word is reserved only
                    // for module goal
                    Check::Module(error)
                });
            }
            None
        })?;
        Ok(Id::new(name, Some(location)))
    }

    fn id(&mut self, binding: bool) -> Result<Id> {
        let Token { location, newline, value: data } = self.read()?;
        match data {
            TokenData::Identifier(name) => self.new_id(binding, name, location),
            _ => Err(Error::UnexpectedToken(Token {
                location: location,
                newline: newline,
                value: data
            }))
        }
    }

    fn id_opt(&mut self, binding: bool) -> Result<Option<Id>> {
        let next = self.read()?;
        match next.value {
            TokenData::Identifier(name) => self.new_id(binding, name, next.location).map(Some),
            _ => {
                self.lexer.unread_token(next);
                Ok(None)
            }
        }
    }

    fn declarator(&mut self) -> Result<Dtor> {
        self.span(&mut |this| {
            match this.peek()?.value {
                TokenData::Identifier(_) => {
                    let id = this.id(true)?;
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

    fn const_declarator(&mut self) -> Result<ConstDtor> {
        let lhs = self.pattern()?;
        self.expect(TokenData::Assign)?;
        let rhs = self.assignment_expression()?;
        Ok(ConstDtor::from_init(lhs, rhs))
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
                TokenData::Reserved(Reserved::Const)         => this.for_const(),
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
                                self.more_for_head(&var_location, Dtor::from_simple_init(id, rhs), ForHead::Var)
                            }
                            // 'for' '(' 'var' id '=' expr 'in' . ==> legacy enumeration
                            TokenData::Reserved(Reserved::In) => {
                                self.reread(TokenData::Reserved(Reserved::In));
                                let head = Box::new(ForInHead::VarInit(span(&var_location, &rhs), id, rhs));
                                self.more_for_in(head)
                            }
                            _ => self.unexpected()
                        }
                    }
                    // 'for' '(' 'var' patt '=' . ==> C-style
                    Patt::Compound(patt) => {
                        let rhs = self.allow_in(false, |this| this.assignment_expression())?;
                        self.more_for_head(&var_location, Dtor::from_compound_init(patt, rhs), ForHead::Var)
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
                    Err(_) => { return self.unexpected(); }
                };
                self.more_for_head(&var_location, dtor, ForHead::Var)
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
            _ => self.unexpected()
        }
    }

    // 'for' '(' 'let' .
    fn for_let(&mut self) -> Result<Stmt> {
        let let_token = self.reread(TokenData::Identifier(Name::Atom(Atom::Let)));
        if !self.peek()?.first_binding() {
            self.lexer.unread_token(let_token);
            return self.for_expr();
        }
        let let_location = Some(let_token.location);
        // 'for' '(' 'let' . !{id, patt} ==> error
        let lhs = self.pattern()?;
        match self.peek()?.value {
            // 'for' '(' 'let' id   '=' . ==> C-style
            // 'for' '(' 'let' patt '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                let rhs = self.allow_in(false, |this| this.assignment_expression())?;
                self.more_for_head(&let_location, Dtor::from_init(lhs, rhs), ForHead::Let)
            }
            TokenData::Comma
          | TokenData::Semi => {
                // 'for' '(' 'let' id   ',' . ==> C-style
                // 'for' '(' 'let' id   ';' . ==> C-style
                // 'for' '(' 'let' patt ',' . ==> error
                // 'for' '(' 'let' patt ';' . ==> error
                let dtor = match Dtor::from_init_opt(lhs, None) {
                    Ok(dtor) => dtor,
                    Err(_) => { return self.unexpected(); }
                };
                self.more_for_head(&let_location, dtor, ForHead::Let)
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
            _ => self.unexpected()
        }
    }

    // 'for' '(' 'const' .
    fn for_const(&mut self) -> Result<Stmt> {
        let const_token = self.reread(TokenData::Reserved(Reserved::Const));
        if !self.peek()?.first_binding() {
            self.lexer.unread_token(const_token);
            return self.for_expr();
        }
        let const_location = Some(const_token.location);
        // 'for' '(' 'const' . !{id, patt} ==> error
        let lhs = self.pattern()?;
        match self.peek()?.value {
            // 'for' '(' 'const' {id, patt}   '=' . ==> C-style
            TokenData::Assign => {
                self.reread(TokenData::Assign);
                let dtors = self.allow_in(false, |this| {
                    let rhs = this.assignment_expression()?;
                    let dtor = ConstDtor::from_init(lhs, rhs);
                    this.more_comma(dtor, Self::const_declarator)
                })?;
                let semi_location = Some(self.expect(TokenData::Semi)?.location);
                let head = Box::new(ForHead::Const(span(&const_location, &semi_location), dtors));
                self.more_for(Some(head))
            }
            // 'for' '(' 'const' {id, patt}   'in' . ==> enumeration
            TokenData::Reserved(Reserved::In) => {
                self.reread(TokenData::Reserved(Reserved::In));
                let head = Box::new(ForInHead::Const(span(&const_location, &lhs), lhs));
                self.more_for_in(head)
            }
            // 'for' '(' 'const' {id, patt}   'of' . ==> enumeration
            TokenData::Identifier(Name::Atom(Atom::Of)) => {
                self.reread(TokenData::Identifier(Name::Atom(Atom::Of)));
                let head = Box::new(ForOfHead::Const(span(&const_location, &lhs), lhs));
                self.more_for_of(head)
            }
            _ => self.unexpected()
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
            _ => self.unexpected()
        }
    }

    // 'for' '(' dtor .
    fn more_for_head<F>(&mut self, start: &Option<Span>, dtor: Dtor, op: F) -> Result<Stmt>
      where F: FnOnce(Option<Span>, Vec<Dtor>) -> ForHead
    {
        let dtors = self.allow_in(false, |this| {
            this.more_comma(dtor, Self::declarator)
        })?;
        let semi_location = Some(self.expect(TokenData::Semi)?.location);
        let head = Box::new(op(span(start, &semi_location), dtors));
        self.more_for(Some(head))
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

    fn comma_separated<T, F>(&mut self, f: F) -> Result<Vec<T>>
        where F: Fn(&mut Self) -> Result<T>
    {
        let first = f(self)?;
        self.more_comma(first, f)
    }

    fn more_comma<T, F>(&mut self, first: T, f: F) -> Result<Vec<T>>
        where F: Fn(&mut Self) -> Result<T>
    {
        let mut items = vec![first];
        while self.matches(TokenData::Comma)? {
            items.push(f(self)?);
        }
        Ok(items)
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
            let id = self.id(false)?;
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
            let id = self.id(false)?;
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
        if !self.context.function {
            Err(Error::TopLevelReturn(result.tracking_ref().unwrap()))
        } else {
            Ok(result)
        }
    }

    fn with_statement(&mut self) -> Result<Stmt> {
        self.span(&mut |this| {
            let token = this.reread(TokenData::Reserved(Reserved::With));
            this.strict_check(|_| {
                Some(Check::Strict(Error::StrictWith(token)))
            })?;
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

    fn finally_opt(&mut self) -> Result<Option<Block>> {
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
        let location = token.location;
        Ok(match token.value {
            TokenData::Identifier(name)          => Expr::Id(self.new_id(false, name, location)?),
            TokenData::Reserved(Reserved::Null)  => Expr::Null(Some(location)),
            TokenData::Reserved(Reserved::This)  => Expr::This(Some(location)),
            TokenData::Reserved(Reserved::True)  => Expr::True(Some(location)),
            TokenData::Reserved(Reserved::False) => Expr::False(Some(location)),
            TokenData::Number(literal)           => Expr::Number(Some(location), literal),
            TokenData::String(literal)           => Expr::String(Some(location), literal),
            TokenData::RegExp(literal)           => Expr::RegExp(Some(location), literal),
            TokenData::LBrack                    => { return self.array_literal(token); }
            TokenData::LBrace                    => { return self.object_literal(token); }
            TokenData::Reserved(Reserved::Function) => {
                self.lexer.unread_token(token);
                return Ok(Expr::Fun(self.function(|this| this.id_opt(true))?));
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
        self.allow_in(true, |this| {
            let start_location = Some(start.location);
            let mut elts = Vec::new();
            loop {
                // Optional final comma does not count as an element.
                if this.peek()?.value == TokenData::RBrack {
                    break;
                }
                elts.push(this.array_element()?);
                if !this.matches(TokenData::Comma)? {
                    break;
                }
            }
            let end_location = Some(this.expect(TokenData::RBrack)?.location);
            Ok(Expr::Arr(span(&start_location, &end_location), elts))
        })
    }

    fn expr_list_item(&mut self) -> Result<ExprListItem> {
        match self.peek()?.value {
            TokenData::Ellipsis => {
                self.span(&mut |this| {
                    this.reread(TokenData::Ellipsis);
                    let expr = this.assignment_expression()?;
                    Ok(ExprListItem::Spread(None, expr))
                })
            }
            _ => {
                Ok(ExprListItem::Expr(self.assignment_expression()?))
            }
        }
    }

    fn array_element(&mut self) -> Result<Option<ExprListItem>> {
        if self.peek()?.value == TokenData::Comma {
            return Ok(None);
        }
        self.expr_list_item().map(Some)
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
        Ok(match self.peek()?.value {
            TokenData::Colon => {
                self.skip()?;
                let val = self.allow_in(true, |this| this.assignment_expression())?;
                Prop::Regular(span(key.tracking_ref(), val.tracking_ref()), key, PropVal::Init(val))
            }
            TokenData::LParen => {
                let params = self.formal_parameters()?;
                let body = self.function_body(&params.list)?;
                Prop::Method(Fun {
                    location: span(key.tracking_ref(), body.tracking_ref()),
                    id: key,
                    params: params,
                    body: body
                })
            }
            TokenData::Comma | TokenData::RBrace => {
                if let PropKey::Id(location, name) = key {
                    Prop::Shorthand(self.new_id(false, Name::from(name), location.unwrap())?)
                } else {
                    return self.unexpected();
                }
            }
            _ => { return self.unexpected(); }
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
            None => self.unexpected()
        }
    }

    fn object_property(&mut self) -> Result<Prop> {
        let first = self.read()?;
        match first.value {
            TokenData::Identifier(Name::Atom(Atom::Get)) => {
                if let Some(key) = self.property_key_opt()? {
                    let paren_location = Some(self.expect(TokenData::LParen)?.location);
                    self.expect(TokenData::RParen)?;
                    let body = self.function_body(&vec![])?;
                    let val_location = span(&paren_location, &body);
                    let prop_location = span(&key, &body);
                    return Ok(Prop::Regular(prop_location, key, PropVal::Get(val_location, body)));
                }
                let key_location = Some(first.location);
                self.more_prop_init(PropKey::Id(key_location, "get".to_string()))
            }
            TokenData::Identifier(Name::Atom(Atom::Set)) => {
                if let Some(key) = self.property_key_opt()? {
                    let paren_location = Some(self.expect(TokenData::LParen)?.location);
                    let param = self.pattern()?;
                    self.expect(TokenData::RParen)?;
                    let body = self.function_body(&[param.clone()])?;
                    let val_location = span(&paren_location, &body);
                    let prop_location = span(&key, &body);
                    return Ok(Prop::Regular(prop_location, key, PropVal::Set(val_location, param, body)));
                }
                let key_location = Some(first.location);
                self.more_prop_init(PropKey::Id(key_location, "set".to_string()))
            }
            TokenData::Reserved(_) => {
                match self.peek()?.value {
                    TokenData::Comma | TokenData::RBrace => {
                        return Err(Error::UnexpectedToken(first));
                    }
                    _ => {
                        self.lexer.unread_token(first);
                        let key = self.property_key()?;
                        self.more_prop_init(key)
                    }
                }
            }
            // ES6: TokenData::Star
            _ => {
                self.lexer.unread_token(first);
                let key = self.property_key()?;
                self.more_prop_init(key)
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

    // Arguments ::= "(" Argument*[","] ")"
    fn arguments(&mut self) -> Result<Arguments> {
        self.allow_in(true, |this| {
            this.expect(TokenData::LParen)?;
            let mut args = Vec::new();
            if this.peek()?.value != TokenData::RParen {
                loop {
                    args.push(this.expr_list_item()?);
                    if !this.matches(TokenData::Comma)? {
                        break;
                    }
                }
            }
            let end = this.expect(TokenData::RParen)?;
            Ok(Arguments { args: args, end: end })
        })
    }

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

    fn more_expressions(&mut self, first: Expr) -> Result<Expr> {
        if self.peek()?.value != TokenData::Comma {
            return Ok(first);
        }
        let elts = self.more_comma(first, Self::assignment_expression)?;
        let location = self.vec_span(&elts);
        Ok(Expr::Seq(location, elts))
    }
}
