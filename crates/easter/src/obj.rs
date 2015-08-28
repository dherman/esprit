use joker::track::*;
use joker::token::{StringLiteral, NumberLiteral};

use id::Id;
use expr::Expr;
use stmt::StmtListItem;
use patt::Patt;

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
