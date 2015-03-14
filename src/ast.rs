#![allow(dead_code)]

#[derive(Debug, Eq, PartialEq)]
pub struct Id {
    name: String
}

#[derive(Debug, PartialEq)]
pub struct Fun {
    id: Option<Id>,
    params: Vec<Patt>,
    body: Vec<Stmt>
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
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

#[derive(Debug, PartialEq)]
pub enum ForHead {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub enum ForInHead {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

#[derive(Debug, PartialEq)]
pub enum Decl {
    Fun(Fun),
    Var(Vec<VarDtor>)
}

#[derive(Debug, PartialEq)]
pub struct VarDtor {
    id: Patt,
    init: Option<Expr>
}

#[derive(Debug, PartialEq)]
pub struct Catch {
    param: Patt,
    body: Vec<Stmt>
}

#[derive(Debug, PartialEq)]
pub struct Case {
    test: Option<Expr>,
    body: Vec<Stmt>
}

#[derive(Debug, Eq, PartialEq)]
pub enum Unop {
    Minus,
    Plus,
    Not,
    BitNot,
    Typeof,
    Void,
    Delete
}

#[derive(Debug, Eq, PartialEq)]
pub enum Binop {
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

#[derive(Debug, Eq, PartialEq)]
pub enum Logop {
    Or,
    And
}

#[derive(Debug, Eq, PartialEq)]
pub enum Assop {
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

#[derive(Debug, PartialEq)]
pub enum Expr {
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

#[derive(Debug, PartialEq)]
pub struct Prop {
    key: PropKey,
    val: PropVal
}

#[derive(Debug, PartialEq)]
pub enum PropKey {
    Id(Id),
    String(String),
    Number(f64),
    Null,
    True,
    False
}

#[derive(Debug, PartialEq)]
pub enum PropVal {
    Init(Expr),
    Get(Vec<Stmt>),
    Set(Patt, Vec<Stmt>)
}

#[derive(Debug, Eq, PartialEq)]
pub enum Patt {
    Id(Id)
}
