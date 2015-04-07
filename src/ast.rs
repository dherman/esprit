#![allow(dead_code)]

use token::{Posn, Span, Loc};

#[derive(Debug, Eq, PartialEq)]
pub struct IdData {
    pub name: String
}

pub type Id = Loc<IdData>;

#[derive(Debug, PartialEq)]
pub struct FunData {
    pub id: Option<Id>,
    pub params: Vec<Patt>,
    pub body: Vec<Stmt>
}

pub type Fun = Loc<FunData>;

#[derive(Debug, PartialEq)]
pub enum StmtData {
    Empty,
    Block(Vec<Stmt>),
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

pub type Stmt = Loc<StmtData>;

#[derive(Debug, PartialEq)]
pub enum ForHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

pub type ForHead = Loc<ForHeadData>;

#[derive(Debug, PartialEq)]
pub enum ForInHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

pub type ForInHead = Loc<ForInHeadData>;

#[derive(Debug, PartialEq)]
pub enum DeclData {
    Fun(Fun),
    Var(Vec<VarDtor>)
}

pub type Decl = Loc<DeclData>;

#[derive(Debug, PartialEq)]
pub struct VarDtorData {
    id: Patt,
    init: Option<Expr>
}

pub type VarDtor = Loc<VarDtorData>;

#[derive(Debug, PartialEq)]
pub struct CatchData {
    pub param: Patt,
    pub body: Vec<Stmt>
}

pub type Catch = Loc<CatchData>;

#[derive(Debug, PartialEq)]
pub struct CaseData {
    pub test: Option<Expr>,
    pub body: Vec<Stmt>
}

pub type Case = Loc<CaseData>;

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

pub type Unop = Loc<UnopTag>;

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

pub type Binop = Loc<BinopTag>;

#[derive(Debug, Eq, PartialEq)]
pub enum LogopTag {
    Or,
    And
}

pub type Logop = Loc<LogopTag>;

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

pub type Assop = Loc<AssopTag>;

#[derive(Debug, PartialEq)]
pub enum ExprData {
    This,
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
    Number(f64),
    RegExp(String),
    String(String)
}

pub type Expr = Loc<ExprData>;

#[derive(Debug, PartialEq)]
pub struct PropData {
    pub key: PropKey,
    pub val: PropVal
}

pub type Prop = Loc<PropData>;

#[derive(Debug, PartialEq)]
pub enum PropKeyData {
    Id(Id),
    String(String),
    Number(f64),
    Null,
    True,
    False
}

pub type PropKey = Loc<PropKeyData>;

#[derive(Debug, PartialEq)]
pub enum PropValData {
    Init(Expr),
    Get(Vec<Stmt>),
    Set(Patt, Vec<Stmt>)
}

pub type PropVal = Loc<PropValData>;

#[derive(Debug, Eq, PartialEq)]
pub enum PattData {
    Id(Id)
}

pub type Patt = Loc<PattData>;
