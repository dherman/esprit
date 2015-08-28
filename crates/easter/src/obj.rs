use joker::track::*;
use joker::token::{StringLiteral, NumberLiteral};

use id::Id;
use expr::{Expr, IntoAssignPatt};
use stmt::StmtListItem;
use patt::{Patt, PropPatt, PropPattData, AssignTarget};

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

pub trait IntoAssignProp {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Option<Span>>;
}

impl IntoAssignProp for Prop {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Option<Span>> {
        let key = self.value.key;
        let patt = match self.value.val.value {
            PropValData::Init(expr) => try!(expr.into_assign_patt()),
            _ => { return Err(self.value.val.location); }
        };
        Ok(PropPattData { key: key, patt: patt }.tracked(self.location))
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
    Set(Patt<Id>, Vec<StmtListItem>)
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
