use joker::track::*;

use id::Id;
use expr::Expr;
use obj::{PropKey, DotKey};

#[derive(Debug, PartialEq)]
pub enum CompoundPattData<T> {
    Arr(Vec<Option<Patt<T>>>),
    Obj(Vec<PropPatt<T>>)
}

impl<T: Untrack> Untrack for CompoundPattData<T> {
    fn untrack(&mut self) {
        match *self {
            CompoundPattData::Arr(ref mut patts) => { patts.untrack(); }
            CompoundPattData::Obj(ref mut props) => { props.untrack(); }
        }
    }
}

pub type CompoundPatt<T> = Tracked<CompoundPattData<T>>;

#[derive(Debug, PartialEq)]
pub struct PropPattData<T> {
    pub key: PropKey,
    pub patt: Patt<T>
}

impl<T: Untrack> Untrack for PropPattData<T> {
    fn untrack(&mut self) {
        self.key.untrack();
        self.patt.untrack();
    }
}

pub type PropPatt<T> = Tracked<PropPattData<T>>;

#[derive(Debug, PartialEq)]
pub enum Patt<T> {
    Simple(T),
    Compound(CompoundPatt<T>)
}

impl<T> Patt<T> {
    pub fn is_simple(&self) -> bool {
        match *self {
            Patt::Simple(_)   => true,
            Patt::Compound(_) => false
        }
    }
}

impl<T: Track> Track for Patt<T> {
    fn location(&self) -> Option<Span> {
        match *self {
            Patt::Simple(ref simple) => simple.location(),
            Patt::Compound(ref patt) => patt.location()
        }
    }
}

impl<T: Untrack> Untrack for Patt<T> {
    fn untrack(&mut self) {
        match *self {
            Patt::Simple(ref mut simple) => { simple.untrack(); }
            Patt::Compound(ref mut patt) => { patt.untrack(); }
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
