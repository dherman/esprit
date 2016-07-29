use joker::track::*;
use joker::token::{StringLiteral, NumberLiteral};

use id::Id;
use expr::Expr;
use stmt::StmtListItem;
use patt::Patt;

#[derive(Debug, PartialEq)]
pub struct DotKey {
    pub location: Option<Span>,
    pub value: String
}

impl TrackingRef for DotKey {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for DotKey {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for DotKey {
    fn untrack(&mut self) { self.location = None; }
}

#[derive(Debug, PartialEq)]
pub struct Prop {
    pub location: Option<Span>,
    pub key: PropKey,
    pub val: PropVal
}

impl TrackingRef for Prop {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Prop {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Prop {
    fn untrack(&mut self) {
        self.location = None;
        self.key.untrack();
        self.val.untrack();
    }
}

#[derive(Debug, PartialEq)]
pub enum PropKey {
    Id(Option<Span>, String),
    String(Option<Span>, StringLiteral),
    Number(Option<Span>, NumberLiteral)
}

impl TrackingRef for PropKey {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            PropKey::Id(ref location, _)
          | PropKey::String(ref location, _)
          | PropKey::Number(ref location, _) => location
        }
    }
}

impl TrackingMut for PropKey {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            PropKey::Id(ref mut location, _)
          | PropKey::String(ref mut location, _)
          | PropKey::Number(ref mut location, _) => location
        }
    }
}

impl Untrack for PropKey {
    fn untrack(&mut self) {
        *self.tracking_mut() = None;
    }
}

#[derive(Debug, PartialEq)]
pub enum PropVal {
    Init(Expr),
    Get(Option<Span>, Vec<StmtListItem>),
    Set(Option<Span>, Patt<Id>, Vec<StmtListItem>)
}

impl TrackingRef for PropVal {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            PropVal::Init(ref expr) => expr.tracking_ref(),
            PropVal::Get(ref location, _)
          | PropVal::Set(ref location, _, _) => location
        }
    }
}

impl TrackingMut for PropVal {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            PropVal::Init(ref mut expr) => expr.tracking_mut(),
            PropVal::Get(ref mut location, _)
          | PropVal::Set(ref mut location, _, _) => location
        }
    }
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
