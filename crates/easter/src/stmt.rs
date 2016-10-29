use joker::track::*;

use id::Id;
use expr::Expr;
use decl::Dtor;
use fun::Fun;
use patt::{Patt, AssignTarget};
use punc::Semi;

#[derive(Debug, PartialEq)]
pub struct Block {
    pub location: Option<Span>,
    pub body: Vec<Stmt>
}

impl TrackingRef for Block {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Block {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Block {
    fn untrack(&mut self) {
        self.location = None;
        self.body.untrack();
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Empty(Option<Span>),
    Block(Block),
    Var(Option<Span>, Vec<Dtor>, Semi),
    Fun(Fun),
    Expr(Option<Span>, Expr, Semi),
    If(Option<Span>, Expr, Box<Stmt>, Option<Box<Stmt>>),
    Label(Option<Span>, Id, Box<Stmt>),
    Break(Option<Span>, Option<Id>, Semi),
    Cont(Option<Span>, Option<Id>, Semi),
    With(Option<Span>, Expr, Box<Stmt>),
    Switch(Option<Span>, Expr, Vec<Case>),
    Return(Option<Span>, Option<Expr>, Semi),
    Throw(Option<Span>, Expr, Semi),
    Try(Option<Span>, Block, Option<Box<Catch>>, Option<Block>),
    While(Option<Span>, Expr, Box<Stmt>),
    DoWhile(Option<Span>, Box<Stmt>, Expr, Semi),
    For(Option<Span>, Option<Box<ForHead>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForIn(Option<Span>, Box<ForInHead>, Expr, Box<Stmt>),
    ForOf(Option<Span>, Box<ForOfHead>, Expr, Box<Stmt>),
    Debugger(Option<Span>, Semi)
}

impl TrackingRef for Stmt {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Stmt::Empty(ref location)
          | Stmt::Block(Block { ref location, .. })
          | Stmt::Var(ref location, _, _)
          | Stmt::Fun(Fun { ref location, .. })
          | Stmt::Expr(ref location, _, _)
          | Stmt::If(ref location, _, _, _)
          | Stmt::Label(ref location, _, _)
          | Stmt::Break(ref location, _, _)
          | Stmt::Cont(ref location, _, _)
          | Stmt::With(ref location, _, _)
          | Stmt::Switch(ref location, _, _)
          | Stmt::Return(ref location, _, _)
          | Stmt::Throw(ref location, _, _)
          | Stmt::Try(ref location, _, _, _)
          | Stmt::While(ref location, _, _)
          | Stmt::DoWhile(ref location, _, _, _)
          | Stmt::For(ref location, _, _, _, _)
          | Stmt::ForIn(ref location, _, _, _)
          | Stmt::ForOf(ref location, _, _, _)
          | Stmt::Debugger(ref location, _) => location
        }
    }
}

impl TrackingMut for Stmt {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Stmt::Empty(ref mut location)
          | Stmt::Block(Block { ref mut location, .. })
          | Stmt::Var(ref mut location, _, _)
          | Stmt::Fun(Fun { ref mut location, .. })
          | Stmt::Expr(ref mut location, _, _)
          | Stmt::If(ref mut location, _, _, _)
          | Stmt::Label(ref mut location, _, _)
          | Stmt::Break(ref mut location, _, _)
          | Stmt::Cont(ref mut location, _, _)
          | Stmt::With(ref mut location, _, _)
          | Stmt::Switch(ref mut location, _, _)
          | Stmt::Return(ref mut location, _, _)
          | Stmt::Throw(ref mut location, _, _)
          | Stmt::Try(ref mut location, _, _, _)
          | Stmt::While(ref mut location, _, _)
          | Stmt::DoWhile(ref mut location, _, _, _)
          | Stmt::For(ref mut location, _, _, _, _)
          | Stmt::ForIn(ref mut location, _, _, _)
          | Stmt::ForOf(ref mut location, _, _, _)
          | Stmt::Debugger(ref mut location, _) => location
        }
    }
}

impl Untrack for Stmt {
    fn untrack(&mut self) {
        *self.tracking_mut() = None;
        match *self {
            Stmt::Empty(_)                                                       => { }
            Stmt::Block(ref mut block)                                           => { block.untrack(); }
            Stmt::Var(_, ref mut dtors, ref mut semi)                            => { dtors.untrack(); semi.untrack(); }
            Stmt::Fun(ref mut fun)                                               => { fun.untrack(); }
            Stmt::Expr(_, ref mut expr, ref mut semi)                            => { expr.untrack(); semi.untrack(); }
            Stmt::If(_, ref mut test, ref mut cons, ref mut alt)                 => { test.untrack(); cons.untrack(); alt.untrack(); }
            Stmt::Label(_, ref mut lab, ref mut stmt)                            => { lab.untrack(); stmt.untrack(); }
            Stmt::Break(_, ref mut lab, ref mut semi)                            => { lab.untrack(); semi.untrack(); }
            Stmt::Cont(_, ref mut lab, ref mut semi)                             => { lab.untrack(); semi.untrack(); }
            Stmt::With(_, ref mut expr, ref mut stmt)                            => { expr.untrack(); stmt.untrack(); }
            Stmt::Switch(_, ref mut expr, ref mut cases)                         => { expr.untrack(); cases.untrack(); }
            Stmt::Return(_, ref mut expr, ref mut semi)                          => { expr.untrack(); semi.untrack(); }
            Stmt::Throw(_, ref mut expr, ref mut semi)                           => { expr.untrack(); semi.untrack(); }
            Stmt::Try(_, ref mut body, ref mut catch, ref mut finally)           => { body.untrack(); catch.untrack(); finally.untrack(); }
            Stmt::While(_, ref mut expr, ref mut stmt)                           => { expr.untrack(); stmt.untrack(); }
            Stmt::DoWhile(_, ref mut stmt, ref mut expr, ref mut semi)           => { stmt.untrack(); expr.untrack(); semi.untrack(); }
            Stmt::For(_, ref mut init, ref mut test, ref mut incr, ref mut body) => { init.untrack(); test.untrack(); incr.untrack(); body.untrack(); }
            Stmt::ForIn(_, ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            Stmt::ForOf(_, ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            Stmt::Debugger(_, ref mut semi)                                      => { semi.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ForHead {
    Var(Option<Span>, Vec<Dtor>),
    Let(Option<Span>, Vec<Dtor>),
    Expr(Option<Span>, Expr)
}

impl TrackingRef for ForHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForHead::Var(ref location, _)
          | ForHead::Let(ref location, _)
          | ForHead::Expr(ref location, _) => location
        }
    }
}

impl TrackingMut for ForHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForHead::Var(ref mut location, _)
          | ForHead::Let(ref mut location, _)
          | ForHead::Expr(ref mut location, _) => location
        }
    }
}

impl Untrack for ForHead {
    fn untrack(&mut self) {
        match *self {
            ForHead::Var(ref mut location, ref mut vec)
          | ForHead::Let(ref mut location, ref mut vec) => {
                *location = None;
                vec.untrack();
            }
            ForHead::Expr(ref mut location, ref mut expr) => {
                *location = None;
                expr.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ForInHead {
    VarInit(Option<Span>, Id, Expr),
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Patt(Patt<AssignTarget>)
}

impl TrackingRef for ForInHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForInHead::VarInit(ref location, _, _)
          | ForInHead::Var(ref location, _)
          | ForInHead::Let(ref location, _) => location,
            ForInHead::Patt(ref patt) => patt.tracking_ref()
        }
    }
}

impl TrackingMut for ForInHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForInHead::VarInit(ref mut location, _, _)
          | ForInHead::Var(ref mut location, _)
          | ForInHead::Let(ref mut location, _) => location,
            ForInHead::Patt(ref mut patt) => patt.tracking_mut()
        }
    }
}

impl Untrack for ForInHead {
    fn untrack(&mut self) {
        match *self {
            ForInHead::VarInit(ref mut location, ref mut id, ref mut expr) => {
                *location = None;
                id.untrack();
                expr.untrack();
            }
            ForInHead::Var(ref mut location, ref mut patt)
          | ForInHead::Let(ref mut location, ref mut patt) => {
                *location = None;
                patt.untrack();
            }
            ForInHead::Patt(ref mut patt) => {
                patt.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum ForOfHead {
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Patt(Patt<AssignTarget>)
}

impl TrackingRef for ForOfHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForOfHead::Var(ref location, _)
          | ForOfHead::Let(ref location, _) => location,
            ForOfHead::Patt(ref patt) => patt.tracking_ref()
        }
    }
}

impl TrackingMut for ForOfHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForOfHead::Var(ref mut location, _)
          | ForOfHead::Let(ref mut location, _) => location,
            ForOfHead::Patt(ref mut patt) => patt.tracking_mut()
        }
    }
}

impl Untrack for ForOfHead {
    fn untrack(&mut self) {
        match *self {
            ForOfHead::Var(ref mut location, ref mut patt)
          | ForOfHead::Let(ref mut location, ref mut patt) => {
                *location = None;
                patt.untrack();
            }
            ForOfHead::Patt(ref mut patt) => { patt.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Catch {
    pub location: Option<Span>,
    pub param: Patt<Id>,
    pub body: Block
}

impl TrackingRef for Catch {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Catch {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Catch {
    fn untrack(&mut self) {
        self.location = None;
        self.param.untrack();
        self.body.untrack();
    }
}

#[derive(Debug, PartialEq)]
pub struct Case {
    pub location: Option<Span>,
    pub test: Option<Expr>,
    pub body: Vec<Stmt>
}

impl TrackingRef for Case {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Case {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Case {
    fn untrack(&mut self) {
        self.location = None;
        self.test.untrack();
        self.body.untrack();
    }
}
