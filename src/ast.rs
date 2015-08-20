#![allow(dead_code)]

use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use track::*;
use token::{NumberLiteral, Name, StringLiteral, TokenData, Token, Reserved};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Semi {
    Inserted,
    Explicit(Option<Posn>)
}

impl Untrack for Semi {
    fn untrack(&mut self) {
        *self = Semi::Explicit(None);
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct IdData {
    pub name: Name
}

impl Untrack for IdData {
    fn untrack(&mut self) { }
}

pub type Id = Tracked<IdData>;

impl Id {
    pub fn new(name: Name, location: Option<Span>) -> Id {
        Id {
            value: IdData { name: name },
            location: location
        }
    }

    pub fn into_patt(self) -> Patt {
        Patt::Simple(self)
    }

    pub fn into_expr(self) -> Expr {
        self.map_self(ExprData::Id)
    }

    pub fn into_dtor(self) -> Dtor {
        Dtor {
            location: self.location,
            value: DtorData::Simple(self, None)
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ParamsData {
    pub list: Vec<Patt>
}

pub type Params = Tracked<ParamsData>;

impl Untrack for ParamsData {
    fn untrack(&mut self) {
        self.list.untrack();
    }
}

#[derive(Debug, PartialEq)]
pub struct FunData {
    pub id: Option<Id>,
    pub params: Params,
    pub body: Vec<StmtListItem>
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
    Var(Vec<Dtor>, Semi),
    Expr(Expr, Semi),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Label(Id, Box<Stmt>),
    Break(Option<Id>, Semi),
    Cont(Option<Id>, Semi),
    With(Expr, Box<Stmt>),
    Switch(Expr, Vec<Case>),
    Return(Option<Expr>, Semi),
    Throw(Expr, Semi),
    Try(Vec<StmtListItem>, Option<Box<Catch>>, Option<Vec<StmtListItem>>),
    While(Expr, Box<Stmt>),
    DoWhile(Box<Stmt>, Expr, Semi),
    For(Option<Box<ForHead>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForIn(Box<ForInHead>, Expr, Box<Stmt>),
    ForOf(Box<ForOfHead>, Expr, Box<Stmt>),
    Debugger(Semi)
}

impl Untrack for StmtData {
    fn untrack(&mut self) {
        match *self {
            StmtData::Empty                                                       => { }
            StmtData::Block(ref mut items)                                        => { items.untrack(); }
            StmtData::Var(ref mut dtors, ref mut semi)                            => { dtors.untrack(); semi.untrack(); }
            StmtData::Expr(ref mut expr, ref mut semi)                            => { expr.untrack(); semi.untrack(); }
            StmtData::If(ref mut test, ref mut cons, ref mut alt)                 => { test.untrack(); cons.untrack(); alt.untrack(); }
            StmtData::Label(ref mut lab, ref mut stmt)                            => { lab.untrack(); stmt.untrack(); }
            StmtData::Break(ref mut lab, ref mut semi)                            => { lab.untrack(); semi.untrack(); }
            StmtData::Cont(ref mut lab, ref mut semi)                             => { lab.untrack(); semi.untrack(); }
            StmtData::With(ref mut expr, ref mut stmt)                            => { expr.untrack(); stmt.untrack(); }
            StmtData::Switch(ref mut expr, ref mut cases)                         => { expr.untrack(); cases.untrack(); }
            StmtData::Return(ref mut expr, ref mut semi)                          => { expr.untrack(); semi.untrack(); }
            StmtData::Throw(ref mut expr, ref mut semi)                           => { expr.untrack(); semi.untrack(); }
            StmtData::Try(ref mut body, ref mut catch, ref mut finally)           => { body.untrack(); catch.untrack(); finally.untrack(); }
            StmtData::While(ref mut expr, ref mut stmt)                           => { expr.untrack(); stmt.untrack(); }
            StmtData::DoWhile(ref mut stmt, ref mut expr, ref mut semi)           => { stmt.untrack(); expr.untrack(); semi.untrack(); }
            StmtData::For(ref mut init, ref mut test, ref mut incr, ref mut body) => { init.untrack(); test.untrack(); incr.untrack(); body.untrack(); }
            StmtData::ForIn(ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            StmtData::ForOf(ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            StmtData::Debugger(ref mut semi)                                      => { semi.untrack(); }
        }
    }
}

pub type Stmt = Tracked<StmtData>;

#[derive(Debug, PartialEq)]
pub enum ForHeadData {
    Var(Vec<Dtor>),
    Let(Vec<Dtor>),
    Expr(Expr)
}

impl Untrack for ForHeadData {
    fn untrack(&mut self) {
        match *self {
            ForHeadData::Var(ref mut vec)   => { vec.untrack(); }
            ForHeadData::Let(ref mut vec)   => { vec.untrack(); }
            ForHeadData::Expr(ref mut expr) => { expr.untrack(); }
        }
    }
}

pub type ForHead = Tracked<ForHeadData>;

#[derive(Debug, PartialEq)]
pub enum ForInHeadData {
    VarInit(Id, Expr),
    Var(Patt),
    Let(Patt),
    Expr(Expr)
}

impl Untrack for ForInHeadData {
    fn untrack(&mut self) {
        match *self {
            ForInHeadData::VarInit(ref mut id, ref mut expr) => { id.untrack(); expr.untrack(); }
            ForInHeadData::Var(ref mut patt)                 => { patt.untrack(); }
            ForInHeadData::Let(ref mut patt)                 => { patt.untrack(); }
            ForInHeadData::Expr(ref mut expr)                => { expr.untrack(); }
        }
    }
}

pub type ForInHead = Tracked<ForInHeadData>;

#[derive(Debug, PartialEq)]
pub enum ForOfHeadData {
    Var(Patt),
    Let(Patt),
    Expr(Expr)
}

impl Untrack for ForOfHeadData {
    fn untrack(&mut self) {
        match *self {
            ForOfHeadData::Var(ref mut patt)  => { patt.untrack(); }
            ForOfHeadData::Let(ref mut patt)  => { patt.untrack(); }
            ForOfHeadData::Expr(ref mut expr) => { expr.untrack(); }
        }
    }
}

pub type ForOfHead = Tracked<ForOfHeadData>;

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
pub enum DtorData {
    Simple(Id, Option<Expr>),
    Compound(CompoundPatt, Expr)
}

impl Untrack for DtorData {
    fn untrack(&mut self) {
        match *self {
            DtorData::Simple(ref mut id, ref mut init)     => { id.untrack(); init.untrack(); }
            DtorData::Compound(ref mut patt, ref mut init) => { patt.untrack(); init.untrack(); }
        }
    }
}

pub type Dtor = Tracked<DtorData>;

pub trait DtorExt {
    fn from_simple_init(Id, Expr) -> Dtor;
    fn from_compound_init(CompoundPatt, Expr) -> Dtor;
    fn from_init(Patt, Expr) -> Dtor;
    fn from_init_opt(Patt, Option<Expr>) -> Result<Dtor, CompoundPatt>;
}

impl DtorExt for Dtor {
    fn from_compound_init(lhs: CompoundPatt, rhs: Expr) -> Dtor {
        Dtor {
            location: span(&lhs, &rhs),
            value: DtorData::Compound(lhs, rhs)
        }
    }

    fn from_simple_init(lhs: Id, rhs: Expr) -> Dtor {
        Dtor {
            location: span(&lhs, &rhs),
            value: DtorData::Simple(lhs, Some(rhs))
        }
    }

    fn from_init(lhs: Patt, rhs: Expr) -> Dtor {
        Dtor {
            location: span(&lhs, &rhs),
            value: match lhs {
                Patt::Simple(id) => DtorData::Simple(id, Some(rhs)),
                Patt::Compound(patt) => DtorData::Compound(patt, rhs)
            }
        }
    }

    fn from_init_opt(lhs: Patt, rhs: Option<Expr>) -> Result<Dtor, CompoundPatt> {
        match (lhs, rhs) {
            (Patt::Simple(id), rhs) => {
                let location = id.location();
                Ok(Dtor {
                    value: DtorData::Simple(id, rhs),
                    location: location
                })
            }
            (Patt::Compound(patt), None) => Err(patt),
            (Patt::Compound(patt), Some(rhs)) => {
                let location = span(&patt, &rhs);
                Ok(Dtor {
                    value: DtorData::Compound(patt, rhs),
                    location: location
                })
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct CatchData {
    pub param: Patt,
    pub body: Vec<StmtListItem>
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
    pub body: Vec<StmtListItem>
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

pub trait Precedence {
    fn precedence(&self) -> u32;
}

impl<T: Precedence> Precedence for Tracked<T> {
    fn precedence(&self) -> u32 {
        self.value.precedence()
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

impl Precedence for BinopTag {
    fn precedence(&self) -> u32 {
        match *self {
            BinopTag::Eq         => 7,
            BinopTag::NEq        => 7,
            BinopTag::StrictEq   => 7,
            BinopTag::StrictNEq  => 7,
            BinopTag::Lt         => 8,
            BinopTag::LEq        => 8,
            BinopTag::Gt         => 8,
            BinopTag::GEq        => 8,
            BinopTag::LShift     => 9,
            BinopTag::RShift     => 9,
            BinopTag::URShift    => 9,
            BinopTag::Plus       => 10,
            BinopTag::Minus      => 10,
            BinopTag::Times      => 11,
            BinopTag::Div        => 11,
            BinopTag::Mod        => 11,
            BinopTag::BitOr      => 4,
            BinopTag::BitXor     => 5,
            BinopTag::BitAnd     => 6,
            BinopTag::In         => 8,
            BinopTag::Instanceof => 8,
        }
    }
}

pub type Binop = Tracked<BinopTag>;

impl Display for Binop {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match self.value {
            BinopTag::Eq         => "==",
            BinopTag::NEq        => "!=",
            BinopTag::StrictEq   => "===",
            BinopTag::StrictNEq  => "!==",
            BinopTag::Lt         => "<",
            BinopTag::LEq        => "<=",
            BinopTag::Gt         => ">",
            BinopTag::GEq        => ">=",
            BinopTag::LShift     => "<<",
            BinopTag::RShift     => ">>",
            BinopTag::URShift    => ">>>",
            BinopTag::Plus       => "+",
            BinopTag::Minus      => "-",
            BinopTag::Times      => "*",
            BinopTag::Div        => "/",
            BinopTag::Mod        => "%",
            BinopTag::BitOr      => "|",
            BinopTag::BitXor     => "^",
            BinopTag::BitAnd     => "&",
            BinopTag::In         => "in",
            BinopTag::Instanceof => "instanceof"
        })
    }
}

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

impl Precedence for LogopTag {
    fn precedence(&self) -> u32 {
        match *self {
            LogopTag::Or  => 2,
            LogopTag::And => 3
        }
    }
}

pub type Logop = Tracked<LogopTag>;

impl Display for Logop {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match self.value {
            LogopTag::Or  => "||",
            LogopTag::And => "&&"
        })
    }
}

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

impl Precedence for AssopTag {
    fn precedence(&self) -> u32 { 0 }
}

pub type Assop = Tracked<AssopTag>;

impl Display for Assop {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_str(match self.value {
            AssopTag::Eq        => "=",
            AssopTag::PlusEq    => "+=",
            AssopTag::MinusEq   => "-=",
            AssopTag::TimesEq   => "*=",
            AssopTag::DivEq     => "/=",
            AssopTag::ModEq     => "%=",
            AssopTag::LShiftEq  => "<<=",
            AssopTag::RShiftEq  => ">>=",
            AssopTag::URShiftEq => ">>>=",
            AssopTag::BitOrEq   => "|=",
            AssopTag::BitXorEq  => "^=",
            AssopTag::BitAndEq  => "&="
        })
    }
}

impl Untrack for Assop {
    fn untrack(&mut self) {
        self.location = None;
    }
}

pub trait ToOp {
    fn to_binop(&self, bool) -> Option<Binop>;
    fn to_logop(&self) -> Option<Logop>;
    fn to_assop(&self) -> Option<Assop>;
}

impl ToOp for Token {
    fn to_binop(&self, allow_in: bool) -> Option<Binop> {
        Some(match self.value {
            TokenData::Star                               => BinopTag::Times,
            TokenData::Slash                              => BinopTag::Div,
            TokenData::Mod                                => BinopTag::Mod,
            TokenData::Plus                               => BinopTag::Plus,
            TokenData::Minus                              => BinopTag::Minus,
            TokenData::LShift                             => BinopTag::LShift,
            TokenData::RShift                             => BinopTag::RShift,
            TokenData::URShift                            => BinopTag::URShift,
            TokenData::LAngle                             => BinopTag::Lt,
            TokenData::RAngle                             => BinopTag::Gt,
            TokenData::LEq                                => BinopTag::LEq,
            TokenData::GEq                                => BinopTag::GEq,
            TokenData::Reserved(Reserved::Instanceof)     => BinopTag::Instanceof,
            TokenData::Reserved(Reserved::In) if allow_in => BinopTag::In,
            TokenData::Eq                                 => BinopTag::Eq,
            TokenData::NEq                                => BinopTag::NEq,
            TokenData::StrictEq                           => BinopTag::StrictEq,
            TokenData::StrictNEq                          => BinopTag::StrictNEq,
            TokenData::BitAnd                             => BinopTag::BitAnd,
            TokenData::BitXor                             => BinopTag::BitXor,
            TokenData::BitOr                              => BinopTag::BitOr,
            _ => { return None; }
        }.tracked(self.location()))
    }

    fn to_logop(&self) -> Option<Logop> {
        Some(match self.value {
            TokenData::LogicalAnd => LogopTag::And,
            TokenData::LogicalOr  => LogopTag::Or,
            _ => { return None; }
        }.tracked(self.location()))
    }

    fn to_assop(&self) -> Option<Assop> {
        Some(match self.value {
            TokenData::Assign        => AssopTag::Eq,
            TokenData::PlusAssign    => AssopTag::PlusEq,
            TokenData::MinusAssign   => AssopTag::MinusEq,
            TokenData::StarAssign    => AssopTag::TimesEq,
            TokenData::SlashAssign   => AssopTag::DivEq,
            TokenData::ModAssign     => AssopTag::ModEq,
            TokenData::LShiftAssign  => AssopTag::LShiftEq,
            TokenData::RShiftAssign  => AssopTag::RShiftEq,
            TokenData::URShiftAssign => AssopTag::URShiftEq,
            TokenData::BitAndAssign  => AssopTag::BitAndEq,
            TokenData::BitOrAssign   => AssopTag::BitOrEq,
            TokenData::BitXorAssign  => AssopTag::BitXorEq,
            _ => { return None; }
        }.tracked(self.location()))
    }
}

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
    Assign(Assop, APatt, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    New(Box<Expr>, Option<Vec<Expr>>),
    Dot(Box<Expr>, DotKey),
    Brack(Box<Expr>, Box<Expr>),
    NewTarget,
    True,
    False,
    Null,
    Number(NumberLiteral),
    RegExp(String, Vec<char>),
    String(StringLiteral)
}

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&ExprData::This,                      &ExprData::This)                      => true,
            (&ExprData::Id(ref id_l),              &ExprData::Id(ref id_r))              => id_l == id_r,
            (&ExprData::Arr(ref elts_l),           &ExprData::Arr(ref elts_r))           => elts_l == elts_r,
            (&ExprData::Obj(ref props_l),          &ExprData::Obj(ref props_r))          => props_l == props_r,
            (&ExprData::Fun(ref fun_l),            &ExprData::Fun(ref fun_r))            => fun_l == fun_r,
            (&ExprData::Seq(ref exprs_l),          &ExprData::Seq(ref exprs_r))          => exprs_l == exprs_r,
            (&ExprData::Unop(ref op_l, ref arg_l), &ExprData::Unop(ref op_r, ref arg_r)) => (op_l, arg_l) == (op_r, arg_r),
            (&ExprData::Binop(ref op_l, ref arg1_l, ref arg2_l),
             &ExprData::Binop(ref op_r, ref arg1_r, ref arg2_r))                         => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&ExprData::Logop(ref op_l, ref arg1_l, ref arg2_l),
             &ExprData::Logop(ref op_r, ref arg1_r, ref arg2_r))                         => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&ExprData::PreInc(ref arg_l),         &ExprData::PreInc(ref arg_r))
          | (&ExprData::PostInc(ref arg_l),        &ExprData::PostInc(ref arg_r))
          | (&ExprData::PreDec(ref arg_l),         &ExprData::PreDec(ref arg_r))
          | (&ExprData::PostDec(ref arg_l),        &ExprData::PostDec(ref arg_r))        => arg_l == arg_r,
            (&ExprData::Assign(ref op_l, ref patt_l, ref arg_l),
             &ExprData::Assign(ref op_r, ref patt_r, ref arg_r))                         => (op_l, patt_l, arg_l) == (op_r, patt_r, arg_r),
            (&ExprData::Cond(ref test_l, ref cons_l, ref alt_l),
             &ExprData::Cond(ref test_r, ref cons_r, ref alt_r))                         => (test_l, cons_l, alt_l) == (test_r, cons_r, alt_r),
            (&ExprData::Call(ref callee_l, ref args_l),
             &ExprData::Call(ref callee_r, ref args_r))                                  => (callee_l, args_l) == (callee_r, args_r),
            (&ExprData::New(ref callee_l, None),   &ExprData::New(ref callee_r, None))   => callee_l == callee_r,
            (&ExprData::New(ref callee_l, None),   &ExprData::New(ref callee_r, Some(ref args)))
          | (&ExprData::New(ref callee_l, Some(ref args)),
             &ExprData::New(ref callee_r, None))                                         => (callee_l == callee_r) && args.is_empty(),
            (&ExprData::New(ref callee_l, Some(ref args_l)),
             &ExprData::New(ref callee_r, Some(ref args_r)))                             => (callee_l, args_l) == (callee_r, args_r),
            (&ExprData::Dot(ref obj_l, ref key_l), &ExprData::Dot(ref obj_r, ref key_r)) => (obj_l, key_l) == (obj_r, key_r),
            (&ExprData::Brack(ref obj_l, ref prop_l),
             &ExprData::Brack(ref obj_r, ref prop_r))                                    => (obj_l, prop_l) == (obj_r, prop_r),
            (&ExprData::NewTarget,          &ExprData::NewTarget)                        => true,
            (&ExprData::True,               &ExprData::True)                             => true,
            (&ExprData::False,              &ExprData::False)                            => true,
            (&ExprData::Null,               &ExprData::Null)                             => true,
            (&ExprData::Number(ref lit_l),  &ExprData::Number(ref lit_r))                => lit_l.value() == lit_r.value(),
            (&ExprData::RegExp(ref src_l, ref flags_l),
             &ExprData::RegExp(ref src_r, ref flags_r))                                  => (src_l, flags_l) == (src_r, flags_r),
            (&ExprData::String(ref lit_l),  &ExprData::String(ref lit_r))                => lit_l == lit_r,
            _ => false
        }
    }
}

impl Debug for ExprData {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &ExprData::This                                => fmt.write_str("This"),
            &ExprData::Id(ref id)                          => fmt.debug_tuple("Id").field(id).finish(),
            &ExprData::Arr(ref elts)                       => fmt.debug_tuple("Arr").field(elts).finish(),
            &ExprData::Obj(ref props)                      => fmt.debug_tuple("Obj").field(props).finish(),
            &ExprData::Fun(ref fun)                        => fmt.debug_tuple("Fun").field(fun).finish(),
            &ExprData::Seq(ref exprs)                      => fmt.debug_tuple("Seq").field(exprs).finish(),
            &ExprData::Unop(ref op, ref arg)               => fmt.debug_tuple("Unop").field(op).field(arg).finish(),
            &ExprData::Binop(ref op, ref left, ref right)  => fmt.debug_tuple("Binop").field(op).field(left).field(right).finish(),
            &ExprData::Logop(ref op, ref left, ref right)  => fmt.debug_tuple("Logop").field(op).field(left).field(right).finish(),
            &ExprData::PreInc(ref arg)                     => fmt.debug_tuple("PreInc").field(arg).finish(),
            &ExprData::PostInc(ref arg)                    => fmt.debug_tuple("PostInc").field(arg).finish(),
            &ExprData::PreDec(ref arg)                     => fmt.debug_tuple("PreDec").field(arg).finish(),
            &ExprData::PostDec(ref arg)                    => fmt.debug_tuple("PostDec").field(arg).finish(),
            &ExprData::Assign(ref op, ref left, ref right) => fmt.debug_tuple("Assign").field(op).field(left).field(right).finish(),
            &ExprData::Cond(ref test, ref cons, ref alt)   => fmt.debug_tuple("Cond").field(test).field(cons).field(alt).finish(),
            &ExprData::Call(ref callee, ref args)          => fmt.debug_tuple("Call").field(callee).field(args).finish(),
            &ExprData::New(ref ctor, None) => {
                let args: Vec<Expr> = vec![];
                fmt.debug_tuple("New").field(ctor).field(&args).finish()
            }
            &ExprData::New(ref ctor, Some(ref args))       => fmt.debug_tuple("New").field(ctor).field(args).finish(),
            &ExprData::Dot(ref expr, ref key)              => fmt.debug_tuple("Dot").field(expr).field(key).finish(),
            &ExprData::Brack(ref expr, ref prop)           => fmt.debug_tuple("Brack").field(expr).field(prop).finish(),
            &ExprData::NewTarget                           => fmt.write_str("NewTarget"),
            &ExprData::True                                => fmt.write_str("True"),
            &ExprData::False                               => fmt.write_str("False"),
            &ExprData::Null                                => fmt.write_str("Null"),
            &ExprData::Number(ref lit)                     => fmt.debug_tuple("Number").field(lit).finish(),
            &ExprData::RegExp(ref source, ref flags)       => fmt.debug_tuple("RegExp").field(source).field(flags).finish(),
            &ExprData::String(ref lit)                     => fmt.debug_tuple("String").field(lit).finish()
        }
    }
}

// FIXME: should produce more detailed error information

impl Expr {
    pub fn into_assignment_pattern(self) -> Result<APatt, Option<Span>> {
        Ok(match self.value {
            ExprData::Id(id)           => APatt::Simple(AssignTargetData::Id(id).tracked(self.location)),
            ExprData::Dot(obj, key)    => APatt::Simple(AssignTargetData::Dot(obj, key).tracked(self.location)),
            ExprData::Brack(obj, prop) => APatt::Simple(AssignTargetData::Brack(obj, prop).tracked(self.location)),
            ExprData::Obj(props) => {
                let mut prop_patts = Vec::with_capacity(props.len());
                for prop in props {
                    prop_patts.push(try!(prop.into_assignment_property()));
                }
                APatt::Compound(CompoundAPattData::Obj(prop_patts).tracked(self.location))
            }
            ExprData::Arr(exprs) => {
                let mut patts = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    patts.push(match expr {
                        Some(expr) => Some(try!(expr.into_assignment_pattern())),
                        None => None
                    });
                }
                APatt::Compound(CompoundAPattData::Arr(patts).tracked(self.location))
            }
            _ => { return Err(self.location); }
        })
    }
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
            ExprData::Dot(ref mut obj, ref mut key)                  => { obj.untrack(); key.untrack(); }
            ExprData::Brack(ref mut obj, ref mut prop)               => { obj.untrack(); prop.untrack(); }
            ExprData::NewTarget                                      => { }
            ExprData::True                                           => { }
            ExprData::False                                          => { }
            ExprData::Null                                           => { }
            ExprData::Number(_)                                      => { }
            ExprData::RegExp(_, _)                                   => { }
            ExprData::String(_)                                      => { }
        }
    }
}

pub type Expr = Tracked<ExprData>;

#[derive(Debug, PartialEq)]
pub struct DotKeyData(pub String);

pub type DotKey = Tracked<DotKeyData>;

impl Untrack for DotKeyData {
    fn untrack(&mut self) { }
}

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

impl Prop {
    pub fn into_assignment_property(self) -> Result<PropAPatt, Option<Span>> {
        let key = self.value.key;
        let patt = match self.value.val.value {
            PropValData::Init(expr) => try!(expr.into_assignment_pattern()),
            _ => { return Err(self.value.val.location); }
        };
        Ok((PropAPattData { key: key, patt: patt }).tracked(self.location))
    }
}

#[derive(Debug, PartialEq)]
pub enum PropKeyData {
    Id(String),
    String(StringLiteral),
    Number(NumberLiteral)
}

impl Untrack for PropKeyData {
    fn untrack(&mut self) { }
}

pub type PropKey = Tracked<PropKeyData>;

#[derive(Debug, PartialEq)]
pub enum PropValData {
    Init(Expr),
    Get(Vec<StmtListItem>),
    Set(Patt, Vec<StmtListItem>)
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

// FIXME: abstract Patt and APatt by parameterizing over the leaf node type

#[derive(Debug, PartialEq)]
pub enum CompoundPattData {
    Arr(Vec<Option<Patt>>),
    Obj(Vec<PropPatt>)
}

impl Untrack for CompoundPattData {
    fn untrack(&mut self) {
        match *self {
            CompoundPattData::Arr(ref mut patts) => { patts.untrack(); }
            CompoundPattData::Obj(ref mut props) => { props.untrack(); }
        }
    }
}

pub type CompoundPatt = Tracked<CompoundPattData>;

#[derive(Debug, PartialEq)]
pub struct PropPattData {
    pub key: PropKey,
    pub patt: Patt
}

impl Untrack for PropPattData {
    fn untrack(&mut self) {
        self.key.untrack();
        self.patt.untrack();
    }
}

pub type PropPatt = Tracked<PropPattData>;

#[derive(Debug, PartialEq)]
pub enum Patt {
    Simple(Id),
    Compound(CompoundPatt)
}

impl Patt {
    pub fn is_simple(&self) -> bool {
        match *self {
            Patt::Simple(_)   => true,
            Patt::Compound(_) => false
        }
    }
}

impl Track for Patt {
    fn location(&self) -> Option<Span> {
        match *self {
            Patt::Simple(ref id)     => id.location(),
            Patt::Compound(ref patt) => patt.location()
        }
    }
}

impl Untrack for Patt {
    fn untrack(&mut self) {
        match *self {
            Patt::Simple(ref mut id)     => { id.untrack(); }
            Patt::Compound(ref mut patt) => { patt.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum APatt {
    Simple(AssignTarget),
    Compound(CompoundAPatt)
}

impl Track for APatt {
    fn location(&self) -> Option<Span> {
        match *self {
            APatt::Simple(ref target) => target.location(),
            APatt::Compound(ref patt) => patt.location()
        }
    }
}

impl Untrack for APatt {
    fn untrack(&mut self) {
        match *self {
            APatt::Simple(ref mut target) => { target.untrack(); }
            APatt::Compound(ref mut patt) => { patt.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum AssignTargetData {
    Id(Id),
    Dot(Box<Expr>, DotKey),
    Brack(Box<Expr>, Box<Expr>)
}

impl Untrack for AssignTargetData {
    fn untrack(&mut self) {
        match *self {
            AssignTargetData::Id(ref mut id)                   => { id.untrack(); }
            AssignTargetData::Dot(ref mut obj, ref mut prop)   => { obj.untrack(); prop.untrack(); }
            AssignTargetData::Brack(ref mut obj, ref mut prop) => { obj.untrack(); prop.untrack(); }
        }
    }
}

pub type AssignTarget = Tracked<AssignTargetData>;

#[derive(Debug, PartialEq)]
pub enum CompoundAPattData {
    Arr(Vec<Option<APatt>>),
    Obj(Vec<PropAPatt>)
}

impl Untrack for CompoundAPattData {
    fn untrack(&mut self) {
        match *self {
            CompoundAPattData::Arr(ref mut patts) => { patts.untrack(); }
            CompoundAPattData::Obj(ref mut props) => { props.untrack(); }
        }
    }
}

pub type CompoundAPatt = Tracked<CompoundAPattData>;

#[derive(Debug, PartialEq)]
pub struct PropAPattData {
    pub key: PropKey,
    pub patt: APatt
}

impl Untrack for PropAPattData {
    fn untrack(&mut self) {
        self.key.untrack();
        self.patt.untrack();
    }
}

pub type PropAPatt = Tracked<PropAPattData>;

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
