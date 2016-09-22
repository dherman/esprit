use std::ops::{Deref, DerefMut};
use joker::word::Name;
use joker::track::{TrackingRef, TrackingMut, Untrack, Span};

use expr::Expr;
use decl::Dtor;
use patt::Patt;

#[derive(Debug, Eq, PartialEq)]
pub struct Id {
    pub location: Option<Span>,
    pub name: Name
}

impl Deref for Id {
    type Target = Option<Span>;

    fn deref(&self) -> &Option<Span> {
        &self.location
    }
}

impl DerefMut for Id {
    fn deref_mut(&mut self) -> &mut Option<Span> {
        &mut self.location
    }
}

impl TrackingRef for Id {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Id {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Id {
    fn untrack(&mut self) { self.location = None; }
}

pub trait IdExt {
    fn new(Name, Option<Span>) -> Id;
    fn into_patt(self) -> Patt<Id>;
    fn into_expr(self) -> Expr;
    fn into_dtor(self) -> Dtor;
}

impl IdExt for Id {
    fn new(name: Name, location: Option<Span>) -> Id {
        Id {
            location: location,
            name: name
        }
    }

    fn into_patt(self) -> Patt<Id> {
        Patt::Simple(self)
    }

    fn into_expr(self) -> Expr {
        Expr::Id(self)
    }

    fn into_dtor(self) -> Dtor {
        Dtor::Simple(*self.tracking_ref(), self, None)
    }
}
