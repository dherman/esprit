use joker::track::*;
use joker::token::StringLiteral;

use id::Id;
use fun::Fun;
use patt::{Patt, CompoundPatt};
use expr::Expr;
use punc::Semi;

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum Import {
    // ES6: more import forms
    ForEffect(Option<Span>, StringLiteral)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum Export {
    // ES6: more export forms
    Var(Option<Span>, Vec<Dtor>, Semi),
    Decl(Decl)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum Decl {
    Fun(Fun<Id>),
    Let(Option<Span>, Vec<Dtor>, Semi),
    Const(Option<Span>, Vec<ConstDtor>, Semi)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum Dtor {
    Simple(Option<Span>, Id, Option<Expr>),
    Compound(Option<Span>, CompoundPatt<Id>, Expr)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct ConstDtor {
    pub location: Option<Span>,
    pub patt: Patt<Id>,
    pub value: Expr
}

pub trait DtorExt: Sized {
    fn from_simple_init(Id, Expr) -> Self;
    fn from_compound_init(CompoundPatt<Id>, Expr) -> Self;
    fn from_init(Patt<Id>, Expr) -> Self;
    fn from_init_opt(Patt<Id>, Option<Expr>) -> Result<Self, Patt<Id>>;
}

impl DtorExt for Dtor {
    fn from_compound_init(lhs: CompoundPatt<Id>, rhs: Expr) -> Dtor {
        Dtor::Compound(span(&lhs, &rhs), lhs, rhs)
    }

    fn from_simple_init(lhs: Id, rhs: Expr) -> Dtor {
        Dtor::Simple(span(&lhs, &rhs), lhs, Some(rhs))
    }

    fn from_init(lhs: Patt<Id>, rhs: Expr) -> Dtor {
        match lhs {
            Patt::Simple(id) => Dtor::from_simple_init(id, rhs),
            Patt::Compound(patt) => Dtor::from_compound_init(patt, rhs)
        }
    }

    fn from_init_opt(lhs: Patt<Id>, rhs: Option<Expr>) -> Result<Dtor, Patt<Id>> {
        match (lhs, rhs) {
            (Patt::Simple(id), rhs) => {
                Ok(Dtor::Simple(*id.tracking_ref(), id, rhs))
            }
            (lhs @ Patt::Compound(_), None) => Err(lhs),
            (Patt::Compound(patt), Some(rhs)) => {
                Ok(Dtor::from_compound_init(patt, rhs))
            }
        }
    }
}

impl DtorExt for ConstDtor {
    fn from_compound_init(lhs: CompoundPatt<Id>, rhs: Expr) -> ConstDtor {
        ConstDtor::from_init(Patt::Compound(lhs), rhs)
    }

    fn from_simple_init(lhs: Id, rhs: Expr) -> ConstDtor {
        ConstDtor::from_init(Patt::Simple(lhs), rhs)
    }

    fn from_init(lhs: Patt<Id>, rhs: Expr) -> ConstDtor {
        ConstDtor {
            location: span(&lhs, &rhs),
            patt: lhs,
            value: rhs
        }
    }

    fn from_init_opt(lhs: Patt<Id>, rhs: Option<Expr>) -> Result<ConstDtor, Patt<Id>> {
        match rhs {
            Some(rhs) => Ok(ConstDtor::from_init(lhs, rhs)),
            None => Err(lhs)
        }
    }
}