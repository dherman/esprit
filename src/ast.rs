#![allow(dead_code)]

use loc::*;
use token::{NumberLiteral};

#[derive(Debug, PartialEq, Eq)]
pub struct AutoSemi<T> {
    pub inserted: bool,
    pub node: T
}

impl<T> EraseLoc for AutoSemi<T>
  where T: EraseLoc
{
    fn erase_loc(self) -> Self {
        AutoSemi {
            inserted: self.inserted,
            node: self.node.erase_loc()
        }
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IdData {
    pub name: String
}

impl EraseLoc for IdData {
    fn erase_loc(self) -> Self { self }
}

pub type Id = Loc<IdData>;

impl Id {
    pub fn new(name: String, span: Option<Span>) -> Id {
        Id {
            data: IdData { name: name },
            span: span
        }
    }

    pub fn into_patt(self) -> Patt {
        Patt {
            span: self.span,
            data: PattData::Id(self)
        }
    }

    pub fn into_expr(self) -> Expr {
        Expr {
            span: self.span,
            data: ExprData::Id(self)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunData {
    pub id: Option<Id>,
    pub params: Vec<Patt>,
    pub body: Vec<Stmt>
}

impl EraseLoc for FunData {
    fn erase_loc(self) -> Self {
        FunData {
            id: self.id.erase_loc(),
            params: self.params.erase_loc(),
            body: self.body.erase_loc()
        }
    }
}

pub type Fun = Loc<FunData>;

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

impl EraseLoc for StmtData {
    fn erase_loc(self) -> Self {
        match self {
            StmtData::Empty                     => self,
            StmtData::Block(items)              => StmtData::Block(items.erase_loc()),
            StmtData::Var(dtors)                => StmtData::Var(dtors.erase_loc()),
            StmtData::Expr(expr)                => StmtData::Expr(expr.erase_loc()),
            StmtData::If(test, cons, alt)       => StmtData::If(test.erase_loc(), cons.erase_loc(), alt.erase_loc()),
            StmtData::Label(lab, stmt)          => StmtData::Label(lab.erase_loc(), stmt.erase_loc()),
            StmtData::Break(lab)                => StmtData::Break(lab.erase_loc()),
            StmtData::Cont(lab)                 => StmtData::Cont(lab.erase_loc()),
            StmtData::With(expr, stmt)          => StmtData::With(expr.erase_loc(), stmt.erase_loc()),
            StmtData::Switch(expr, cases)       => StmtData::Switch(expr.erase_loc(), cases.erase_loc()),
            StmtData::Return(expr)              => StmtData::Return(expr.erase_loc()),
            StmtData::Throw(expr)               => StmtData::Throw(expr.erase_loc()),
            StmtData::Try(body, catch, finally) => StmtData::Try(body.erase_loc(), catch.erase_loc(), finally.erase_loc()),
            StmtData::While(expr, stmt)         => StmtData::While(expr.erase_loc(), stmt.erase_loc()),
            StmtData::DoWhile(stmt, expr)       => StmtData::DoWhile(stmt.erase_loc(), expr.erase_loc()),
            StmtData::For(init, test, incr)     => StmtData::For(init.erase_loc(), test.erase_loc(), incr.erase_loc()),
            StmtData::ForIn(lhs, rhs, body)     => StmtData::ForIn(lhs.erase_loc(), rhs.erase_loc(), body.erase_loc()),
            StmtData::Debugger                  => self
        }
    }
}

pub type Stmt = Loc<StmtData>;

#[derive(Debug, PartialEq)]
pub enum ForHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

impl EraseLoc for ForHeadData {
    fn erase_loc(self) -> Self {
        match self {
            ForHeadData::Var(vec) => ForHeadData::Var(vec.erase_loc()),
            ForHeadData::Expr(expr) => ForHeadData::Expr(expr.erase_loc())
        }
    }
}

pub type ForHead = Loc<ForHeadData>;

#[derive(Debug, PartialEq)]
pub enum ForInHeadData {
    Var(Vec<VarDtor>),
    Expr(Expr)
}

impl EraseLoc for ForInHeadData {
    fn erase_loc(self) -> Self {
        match self {
            ForInHeadData::Var(dtor) => ForInHeadData::Var(dtor.erase_loc()),
            ForInHeadData::Expr(expr) => ForInHeadData::Expr(expr.erase_loc())
        }
    }
}

pub type ForInHead = Loc<ForInHeadData>;

#[derive(Debug, PartialEq)]
pub enum DeclData {
    Fun(Fun)
}

impl EraseLoc for DeclData {
    fn erase_loc(self) -> Self {
        let DeclData::Fun(fun) = self;
        DeclData::Fun(fun.erase_loc())
    }
}

pub type Decl = Loc<DeclData>;

#[derive(Debug, PartialEq)]
pub struct VarDtorData {
    pub id: Patt,
    pub init: Option<Expr>
}

impl EraseLoc for VarDtorData {
    fn erase_loc(self) -> Self {
        let VarDtorData { id, init } = self;
        VarDtorData { id: id.erase_loc(), init: init.erase_loc() }
    }
}

pub type VarDtor = Loc<VarDtorData>;

#[derive(Debug, PartialEq)]
pub struct CatchData {
    pub param: Patt,
    pub body: Vec<Stmt>
}

impl EraseLoc for CatchData {
    fn erase_loc(self) -> Self {
        let CatchData { param, body } = self;
        CatchData { param: param.erase_loc(), body: body.erase_loc() }
    }
}

pub type Catch = Loc<CatchData>;

#[derive(Debug, PartialEq)]
pub struct CaseData {
    pub test: Option<Expr>,
    pub body: Vec<Stmt>
}

impl EraseLoc for CaseData {
    fn erase_loc(self) -> Self {
        let CaseData { test, body } = self;
        CaseData { test: test.erase_loc(), body: body.erase_loc() }
    }
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

impl EraseLoc for Unop {
    fn erase_loc(mut self) -> Self {
        self.span = None;
        self
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

pub type Binop = Loc<BinopTag>;

impl EraseLoc for Binop {
    fn erase_loc(mut self) -> Self {
        self.span = None;
        self
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum LogopTag {
    Or,
    And
}

pub type Logop = Loc<LogopTag>;

impl EraseLoc for Logop {
    fn erase_loc(mut self) -> Self {
        self.span = None;
        self
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

pub type Assop = Loc<AssopTag>;

impl EraseLoc for Assop {
    fn erase_loc(mut self) -> Self {
        self.span = None;
        self
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

impl EraseLoc for ExprData {
    fn erase_loc(self) -> Self {
        match self {
            ExprData::This                   => self,
            ExprData::Id(id)                 => ExprData::Id(id.erase_loc()),
            ExprData::Arr(exprs)             => ExprData::Arr(exprs.erase_loc()),
            ExprData::Obj(props)             => ExprData::Obj(props.erase_loc()),
            ExprData::Fun(fun)               => ExprData::Fun(fun.erase_loc()),
            ExprData::Seq(exprs)             => ExprData::Seq(exprs.erase_loc()),
            ExprData::Unop(op, expr)         => ExprData::Unop(op.erase_loc(), expr.erase_loc()),
            ExprData::Binop(op, left, right) => ExprData::Binop(op.erase_loc(), left.erase_loc(), right.erase_loc()),
            ExprData::Logop(op, left, right) => ExprData::Logop(op.erase_loc(), left.erase_loc(), right.erase_loc()),
            ExprData::PreInc(expr)           => ExprData::PreInc(expr.erase_loc()),
            ExprData::PostInc(expr)          => ExprData::PostInc(expr.erase_loc()),
            ExprData::PreDec(expr)           => ExprData::PreDec(expr.erase_loc()),
            ExprData::PostDec(expr)          => ExprData::PostDec(expr.erase_loc()),
            ExprData::Assign(op, patt, expr) => ExprData::Assign(op.erase_loc(), patt.erase_loc(), expr.erase_loc()),
            ExprData::Cond(test, cons, alt)  => ExprData::Cond(test.erase_loc(), cons.erase_loc(), alt.erase_loc()),
            ExprData::Call(callee, args)     => ExprData::Call(callee.erase_loc(), args.erase_loc()),
            ExprData::New(ctor, args)        => ExprData::New(ctor.erase_loc(), args.erase_loc()),
            ExprData::Dot(obj, prop)         => ExprData::Dot(obj.erase_loc(), prop),
            ExprData::Brack(obj, prop)       => ExprData::Brack(obj.erase_loc(), prop.erase_loc()),
            ExprData::True                   => self,
            ExprData::False                  => self,
            ExprData::Null                   => self,
            ExprData::Number(_)              => self, // FIXME: lit.erase()
            ExprData::RegExp(_)              => self,
            ExprData::String(_)              => self
        }
    }
}

pub type Expr = Loc<ExprData>;

#[derive(Debug, PartialEq)]
pub struct PropData {
    pub key: PropKey,
    pub val: PropVal
}

impl EraseLoc for PropData {
    fn erase_loc(self) -> Self {
        let PropData { key, val } = self;
        PropData { key: key.erase_loc(), val: val.erase_loc() }
    }
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

impl EraseLoc for PropKeyData {
    fn erase_loc(self) -> Self {
        match self {
            PropKeyData::Id(id)    => PropKeyData::Id(id.erase_loc()),
            PropKeyData::String(_) => self,
            PropKeyData::Number(_) => self,
            PropKeyData::Null      => self,
            PropKeyData::True      => self,
            PropKeyData::False     => self
        }
    }
}

pub type PropKey = Loc<PropKeyData>;

#[derive(Debug, PartialEq)]
pub enum PropValData {
    Init(Expr),
    Get(Vec<Stmt>),
    Set(Patt, Vec<Stmt>)
}

impl EraseLoc for PropValData {
    fn erase_loc(self) -> Self {
        match self {
            PropValData::Init(expr)       => PropValData::Init(expr.erase_loc()),
            PropValData::Get(stmts)       => PropValData::Get(stmts.erase_loc()),
            PropValData::Set(patt, stmts) => PropValData::Set(patt.erase_loc(), stmts.erase_loc())
        }
    }
}

pub type PropVal = Loc<PropValData>;

#[derive(Debug, Eq, PartialEq)]
pub enum PattData {
    Id(Id)
}

impl EraseLoc for PattData {
    fn erase_loc(self) -> Self {
        match self {
            PattData::Id(id) => PattData::Id(id.erase_loc())
        }
    }
}

pub type Patt = Loc<PattData>;

#[derive(Debug, PartialEq)]
pub struct ScriptData {
    pub body: Vec<StmtListItem>
}

impl EraseLoc for ScriptData {
    fn erase_loc(self) -> Self {
        let ScriptData { body } = self;
        ScriptData { body: body.erase_loc() }
    }
}

pub type Script = Loc<ScriptData>;

#[derive(Debug, PartialEq)]
pub enum StmtListItem {
    Decl(Decl),
    Stmt(Stmt)
}

impl EraseLoc for StmtListItem {
    fn erase_loc(self) -> Self {
        match self {
            StmtListItem::Decl(decl) => StmtListItem::Decl(decl.erase_loc()),
            StmtListItem::Stmt(stmt) => StmtListItem::Stmt(stmt.erase_loc())
        }
    }
}

impl HasSpan for StmtListItem {
    fn span(&self) -> Option<Span> {
        match self {
            &StmtListItem::Decl(ref decl) => decl.span(),
            &StmtListItem::Stmt(ref stmt) => stmt.span()
        }
    }
}


/*
#[derive(Debug, PartialEq)]
pub struct ModuleData {
    pub body: Vec<ModItem>
}

pub type Module = Loc<ModuleData>;
*/
