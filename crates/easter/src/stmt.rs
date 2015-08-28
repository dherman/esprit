use joker::track::*;

use id::Id;
use expr::Expr;
use decl::{Decl, Dtor};
use patt::Patt;
use punc::Semi;

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
    Var(Patt<Id>),
    Let(Patt<Id>),
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
    Var(Patt<Id>),
    Let(Patt<Id>),
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
pub struct CatchData {
    pub param: Patt<Id>,
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
