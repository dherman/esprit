use joker::track::*;
use joker::token::StringLiteral;

use id::Id;
use fun::Fun;
use patt::{Patt, CompoundPatt};
use expr::Expr;
use punc::Semi;

#[derive(Debug, PartialEq, Clone)]
pub enum Import {
    // ES6: more import forms
    ForEffect(Option<Span>, StringLiteral)
}

impl TrackingRef for Import {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Import::ForEffect(ref location, _) => location
        }
    }
}

impl TrackingMut for Import {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Import::ForEffect(ref mut location, _) => location
        }
    }
}

impl Untrack for Import {
    fn untrack(&mut self) {
        match *self {
            Import::ForEffect(ref mut location, _) => { *location = None; }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Export {
    // ES6: more export forms
    Var(Option<Span>, Vec<Dtor>, Semi),
    Decl(Decl)
}

impl TrackingRef for Export {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Export::Var(ref location, _, _) => location,
            Export::Decl(ref decl) => decl.tracking_ref()
        }
    }
}

impl TrackingMut for Export {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Export::Var(ref mut location, _, _) => location,
            Export::Decl(ref mut decl) => decl.tracking_mut()
        }
    }
}

impl Untrack for Export {
    fn untrack(&mut self) {
        match *self {
            Export::Var(ref mut location, ref mut dtors, ref mut semi) => {
                *location = None;
                dtors.untrack();
                semi.untrack();
            }
            Export::Decl(ref mut decl) => {
                decl.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Decl {
    Fun(Fun)
}

impl TrackingRef for Decl {
    fn tracking_ref(&self) -> &Option<Span> {
        let Decl::Fun(ref fun) = *self;
        fun.tracking_ref()
    }
}

impl TrackingMut for Decl {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        let Decl::Fun(ref mut fun) = *self;
        fun.tracking_mut()
    }
}

impl Untrack for Decl {
    fn untrack(&mut self) {
        let Decl::Fun(ref mut fun) = *self;
        fun.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Dtor {
    Simple(Option<Span>, Id, Option<Expr>),
    Compound(Option<Span>, CompoundPatt<Id>, Expr)
}

impl TrackingRef for Dtor {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Dtor::Simple(ref location, _, _)
          | Dtor::Compound(ref location, _, _) => location
        }
    }
}

impl TrackingMut for Dtor {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Dtor::Simple(ref mut location, _, _)
          | Dtor::Compound(ref mut location, _, _) => location
        }
    }
}

impl Untrack for Dtor {
    fn untrack(&mut self) {
        match *self {
            Dtor::Simple(ref mut location, ref mut id, ref mut init) => {
                *location = None;
                id.untrack();
                init.untrack();
            }
            Dtor::Compound(ref mut location, ref mut patt, ref mut init) => {
                *location = None;
                patt.untrack();
                init.untrack();
            }
        }
    }
}

pub trait DtorExt {
    fn from_simple_init(Id, Expr) -> Dtor;
    fn from_compound_init(CompoundPatt<Id>, Expr) -> Dtor;
    fn from_init(Patt<Id>, Expr) -> Dtor;
    fn from_init_opt(Patt<Id>, Option<Expr>) -> Result<Dtor, CompoundPatt<Id>>;
}

impl DtorExt for Dtor {
    fn from_compound_init(lhs: CompoundPatt<Id>, rhs: Expr) -> Dtor {
        Dtor::Compound(span(&lhs, &rhs), lhs, rhs)
    }

    fn from_simple_init(lhs: Id, rhs: Expr) -> Dtor {
        Dtor::Simple(span(&lhs, &rhs), lhs, Some(rhs))
    }

    fn from_init(lhs: Patt<Id>, rhs: Expr) -> Dtor {
        let loc = span(&lhs, &rhs);
        match lhs {
            Patt::Simple(id) => Dtor::Simple(loc, id, Some(rhs)),
            Patt::Compound(patt) => Dtor::Compound(loc, patt, rhs)
        }
    }

    fn from_init_opt(lhs: Patt<Id>, rhs: Option<Expr>) -> Result<Dtor, CompoundPatt<Id>> {
        match (lhs, rhs) {
            (Patt::Simple(id), rhs) => {
                Ok(Dtor::Simple(*id.tracking_ref(), id, rhs))
            }
            (Patt::Compound(patt), None) => Err(patt),
            (Patt::Compound(patt), Some(rhs)) => {
                Ok(Dtor::Compound(span(&patt, &rhs), patt, rhs))
            }
        }
    }
}
