use joker::track::*;

use id::Id;
use fun::Fun;
use patt::{Patt, CompoundPatt};
use expr::Expr;

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
