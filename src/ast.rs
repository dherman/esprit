#![allow(dead_code)]

use track::*;
use token::{NumberLiteral};

#[derive(Debug, PartialEq, Eq)]
pub struct AutoSemi<T> {
    pub inserted: bool,
    pub node: T
}

impl<T> Untrack for AutoSemi<T>
  where T: Untrack
{
    fn untrack(&mut self) {
        self.inserted = false;
        self.node.untrack();
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IdData {
    pub name: String
}

impl Untrack for IdData {
    fn untrack(&mut self) { }
}

pub type Id = Tracked<IdData>;

impl Id {
    pub fn new(name: String, location: Option<Span>) -> Id {
        Id {
            value: IdData { name: name },
            location: location
        }
    }

    pub fn into_patt(self) -> Patt {
        self.map_self(PattData::Id)
    }

    pub fn into_expr(self) -> Expr {
        self.map_self(ExprData::Id)
    }
}

#[derive(Debug, PartialEq)]
pub struct FunData {
    pub id: Option<Id>,
    pub params: Vec<Patt>,
    pub body: Vec<Stmt>
}

impl Untrack for FunData {
    fn untrack(&mut self) {
        self.id.untrack();
        self.params.untrack();
        self.body.untrack();
    }
}

pub type Fun = Tracked<FunData>;

#[derive(Debug, PartialEq)]
pub enum StmtData {
    Empty,
    Block(Vec<StmtListItem>),
    Var(AutoSemi<Vec<VarDtor>>),
    Expr(Expr),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Label(Id, Box<Stmt>),
    Break(Option<Id>),
    Cont(Option<Id>),
    With(Expr, Box<Stmt>),
    Switch(Expr, Vec<Case>),
    Return(Option<Expr>),
    Throw(Expr),
    Try(Vec<Stmt>, Option<Box<Catch>>, Option<Vec<Stmt>>),
    While(Expr, Box<Stmt>),
    DoWhile(Box<Stmt>, Expr),
    For(Option<Box<ForHead>>, Option<Expr>, Option<Expr>),
    ForIn(Box<ForInHead>, Expr, Box<Stmt>),
    Debugger
}

impl Untrack for StmtData {
    fn untrack(&mut self) {
        match *self {
            StmtData::Empty                                             => { }
            StmtData::Block(ref mut items)                              => { items.untrack(); }
            StmtData::Var(ref mut dtors)                                => { dtors.untrack(); }
            StmtData::Expr(ref mut expr)                                => { expr.untrack(); }
            StmtData::If(ref mut test, ref mut cons, ref mut alt)       => { test.untrack(); cons.untrack(); alt.untrack(); }
            StmtData::Label(ref mut lab, ref mut stmt)                  => { lab.untrack(); stmt.untrack(); }
            StmtData::Break(ref mut lab)                                => { lab.untrack(); }
            StmtData::Cont(ref mut lab)                                 => { lab.untrack(); }
            StmtData::With(ref mut expr, ref mut stmt)                  => { expr.untrack(); stmt.untrack(); }
            StmtData::Switch(ref mut expr, ref mut cases)               => { expr.untrack(); cases.untrack(); }
            StmtData::Return(ref mut expr)                              => { expr.untrack(); }
            StmtData::Throw(ref mut expr)                               => { expr.untrack(); }
            StmtData::Try(ref mut body, ref mut catch, ref mut finally) => { body.untrack(); catch.untrack(); finally.untrack(); }
            StmtData::While(ref mut expr, ref mut stmt)                 => { expr.untrack(); stmt.untrack(); }
            StmtData::DoWhile(ref mut stmt, ref mut expr)               => { stmt.untrack(); expr.untrack(); }
            StmtData::For(ref mut init, ref mut test, ref mut incr)     => { init.untrack(); test.untrack(); incr.untrack(); }
            StmtData::ForIn(ref mut lhs, ref mut rhs, ref mut body)     => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            StmtData::Debugger                                          => { }
        }
    }
}

pub type Stmt = Tracked<StmtData>;

#[derive(Debug, PartialEq)]
pub enum ForHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

impl Untrack for ForHeadData {
    fn untrack(&mut self) {
        match *self {
            ForHeadData::Var(ref mut vec)   => { vec.untrack(); }
            ForHeadData::Expr(ref mut expr) => { expr.untrack(); }
        }
    }
}

pub type ForHead = Tracked<ForHeadData>;

#[derive(Debug, PartialEq)]
pub enum ForInHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

impl Untrack for ForInHeadData {
    fn untrack(&mut self) {
        match *self {
            ForInHeadData::Var(ref mut dtor)  => { dtor.untrack(); }
            ForInHeadData::Expr(ref mut expr) => { expr.untrack(); }
        }
    }
}

pub type ForInHead = Tracked<ForInHeadData>;

#[derive(Debug, PartialEq)]
pub enum DeclData {
    Fun(Fun)
}

impl Untrack for DeclData {
    fn untrack(&mut self) {
        match *self {
            DeclData::Fun(ref mut fun) => { fun.untrack(); }
        }
    }
}

pub type Decl = Tracked<DeclData>;

#[derive(Debug, PartialEq)]
pub struct VarDtorData {
    pub id: Patt,
    pub init: Option<Expr>
}

impl Untrack for VarDtorData {
    fn untrack(&mut self) {
        self.id.untrack();
        self.init.untrack();
    }
}

pub type VarDtor = Tracked<VarDtorData>;

#[derive(Debug, PartialEq)]
pub struct CatchData {
    pub param: Patt,
    pub body: Vec<Stmt>
}

impl Untrack for CatchData {
    fn untrack(&mut self) {
        self.param.untrack();
        self.body.untrack();
    }
}

pub type Catch = Tracked<CatchData>;

#[derive(Debug, PartialEq)]
pub struct CaseData {
    pub test: Option<Expr>,
    pub body: Vec<Stmt>
}

impl Untrack for CaseData {
    fn untrack(&mut self) {
        self.test.untrack();
        self.body.untrack();
    }
}

pub type Case = Tracked<CaseData>;

#[derive(Debug, Eq, PartialEq)]
pub enum UnopTag {
    Minus,
    Plus,
    Not,
    BitNot,
    Typeof,
    Void,
    Delete
}

pub type Unop = Tracked<UnopTag>;

impl Untrack for Unop {
    fn untrack(&mut self) {
        self.location = None;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum BinopTag {
    Eq,
    NEq,
    StrictEq,
    StrictNEq,
    Lt,
    LEq,
    Gt,
    GEq,
    LShift,
    RShift,
    URShift,
    Plus,
    Minus,
    Times,
    Div,
    Mod,
    BitOr,
    BitXor,
    BitAnd,
    In,
    Instanceof,
}

pub type Binop = Tracked<BinopTag>;

impl Untrack for Binop {
    fn untrack(&mut self) {
        self.location = None;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum LogopTag {
    Or,
    And
}

pub type Logop = Tracked<LogopTag>;

impl Untrack for Logop {
    fn untrack(&mut self) {
        self.location = None;
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum AssopTag {
    Eq,
    PlusEq,
    MinusEq,
    TimesEq,
    DivEq,
    ModEq,
    LShiftEq,
    RShiftEq,
    URShiftEq,
    BitOrEq,
    BitXorEq,
    BitAndEq
}

pub type Assop = Tracked<AssopTag>;

impl Untrack for Assop {
    fn untrack(&mut self) {
        self.location = None;
    }
}

#[derive(Debug, PartialEq)]
pub enum ExprData {
    This,
    Id(Id),
    Arr(Vec<Option<Expr>>),
    Obj(Vec<Prop>),
    Fun(Fun),
    Seq(Vec<Expr>),
    Unop(Unop, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Logop(Logop, Box<Expr>, Box<Expr>),
    PreInc(Box<Expr>),
    PostInc(Box<Expr>),
    PreDec(Box<Expr>),
    PostDec(Box<Expr>),
    Assign(Assop, Patt, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    New(Box<Expr>, Vec<Expr>),
    Dot(Box<Expr>, String),
    Brack(Box<Expr>, Box<Expr>),
    True,
    False,
    Null,
    Number(NumberLiteral),
    RegExp(String),
    String(String)
}

impl Untrack for ExprData {
    fn untrack(&mut self) {
        match *self {
            ExprData::This                                           => { }
            ExprData::Id(ref mut id)                                 => { id.untrack(); }
            ExprData::Arr(ref mut exprs)                             => { exprs.untrack(); }
            ExprData::Obj(ref mut props)                             => { props.untrack(); }
            ExprData::Fun(ref mut fun)                               => { fun.untrack(); }
            ExprData::Seq(ref mut exprs)                             => { exprs.untrack(); }
            ExprData::Unop(ref mut op, ref mut expr)                 => { op.untrack(); expr.untrack(); }
            ExprData::Binop(ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            ExprData::Logop(ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            ExprData::PreInc(ref mut expr)                           => { expr.untrack(); }
            ExprData::PostInc(ref mut expr)                          => { expr.untrack(); }
            ExprData::PreDec(ref mut expr)                           => { expr.untrack(); }
            ExprData::PostDec(ref mut expr)                          => { expr.untrack(); }
            ExprData::Assign(ref mut op, ref mut patt, ref mut expr) => { op.untrack(); patt.untrack(); expr.untrack(); }
            ExprData::Cond(ref mut test, ref mut cons, ref mut alt)  => { test.untrack(); cons.untrack(); alt.untrack(); }
            ExprData::Call(ref mut callee, ref mut args)             => { callee.untrack(); args.untrack(); }
            ExprData::New(ref mut ctor, ref mut args)                => { ctor.untrack(); args.untrack(); }
            ExprData::Dot(ref mut obj, ref mut prop)                 => { obj.untrack(); }
            ExprData::Brack(ref mut obj, ref mut prop)               => { obj.untrack(); prop.untrack(); }
            ExprData::True                                           => { }
            ExprData::False                                          => { }
            ExprData::Null                                           => { }
            ExprData::Number(_)                                      => { } // FIXME: lit.untrack()
            ExprData::RegExp(_)                                      => { }
            ExprData::String(_)                                      => { }
        }
    }
}

pub type Expr = Tracked<ExprData>;

#[derive(Debug, PartialEq)]
pub struct PropData {
    pub key: PropKey,
    pub val: PropVal
}

impl Untrack for PropData {
    fn untrack(&mut self) {
        self.key.untrack();
        self.val.untrack();
    }
}

pub type Prop = Tracked<PropData>;

#[derive(Debug, PartialEq)]
pub enum PropKeyData {
    Id(Id),
    String(String),
    Number(f64),
    Null,
    True,
    False
}

impl Untrack for PropKeyData {
    fn untrack(&mut self) {
        match *self {
            PropKeyData::Id(ref mut id)    => { id.untrack(); }
            PropKeyData::String(_)         => { }
            PropKeyData::Number(_)         => { }
            PropKeyData::Null              => { }
            PropKeyData::True              => { }
            PropKeyData::False             => { }
        }
    }
}

pub type PropKey = Tracked<PropKeyData>;

#[derive(Debug, PartialEq)]
pub enum PropValData {
    Init(Expr),
    Get(Vec<Stmt>),
    Set(Patt, Vec<Stmt>)
}

impl Untrack for PropValData {
    fn untrack(&mut self) {
        match *self {
            PropValData::Init(ref mut expr)               => { expr.untrack(); }
            PropValData::Get(ref mut stmts)               => { stmts.untrack(); }
            PropValData::Set(ref mut patt, ref mut stmts) => { patt.untrack(); stmts.untrack(); }
        }
    }
}

pub type PropVal = Tracked<PropValData>;

#[derive(Debug, Eq, PartialEq)]
pub enum PattData {
    Id(Id)
}

impl Untrack for PattData {
    fn untrack(&mut self) {
        match *self {
            PattData::Id(ref mut id) => { id.untrack(); }
        }
    }
}

pub type Patt = Tracked<PattData>;

#[derive(Debug, PartialEq)]
pub struct ScriptData {
    pub body: Vec<StmtListItem>
}

impl Untrack for ScriptData {
    fn untrack(&mut self) {
        self.body.untrack();
    }
}

pub type Script = Tracked<ScriptData>;

#[derive(Debug, PartialEq)]
pub enum StmtListItem {
    Decl(Decl),
    Stmt(Stmt)
}

impl Untrack for StmtListItem {
    fn untrack(&mut self) {
        match *self {
            StmtListItem::Decl(ref mut decl) => { decl.untrack(); }
            StmtListItem::Stmt(ref mut stmt) => { stmt.untrack(); }
        }
    }
}

impl Track for StmtListItem {
    fn location(&self) -> Option<Span> {
        match *self {
            StmtListItem::Decl(ref decl) => decl.location(),
            StmtListItem::Stmt(ref stmt) => stmt.location()
        }
    }
}


/*
#[derive(Debug, PartialEq)]
pub struct ModuleData {
    pub body: Vec<ModItem>
}

pub type Module = Tracked<ModuleData>;
*/
