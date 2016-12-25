use joker::track::*;
use joker::token::{StringLiteral, NumberLiteral};

use id::Id;
use expr::Expr;
use stmt::Script;
use patt::Patt;

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct DotKey {
    pub location: Option<Span>,
    pub value: String
}

impl Untrack for DotKey {
    fn untrack(&mut self) { self.location = None; }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct Prop {
    pub location: Option<Span>,
    pub key: PropKey,
    pub val: PropVal
}

impl Untrack for Prop {
    fn untrack(&mut self) {
        self.location = None;
        self.key.untrack();
        self.val.untrack();
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub enum PropKey {
    Id(Option<Span>, String),
    String(Option<Span>, StringLiteral),
    Number(Option<Span>, NumberLiteral)
}

impl Untrack for PropKey {
    fn untrack(&mut self) {
        *self.tracking_mut() = None;
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub enum PropVal {
    Init(Expr),
    Get(Option<Span>, Script),
    Set(Option<Span>, Patt<Id>, Script)
}

impl Untrack for PropVal {
    fn untrack(&mut self) {
        match *self {
            PropVal::Init(ref mut expr) => { expr.untrack(); }
            PropVal::Get(ref mut location, ref mut stmts) => {
                *location = None;
                stmts.untrack();
            }
            PropVal::Set(ref mut location, ref mut patt, ref mut stmts) => {
                *location = None;
                patt.untrack();
                stmts.untrack();
            }
        }
    }
}
