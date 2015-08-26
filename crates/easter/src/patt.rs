use joker::track::*;

use id::Id;
use expr::Expr;
use obj::{PropKey, DotKey};

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
