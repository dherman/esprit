use joker::word::Name;
use joker::track::{TrackingRef, TrackingMut, Untrack, Span};

use expr::Expr;
use decl::Dtor;
use patt::Patt;

#[derive(Debug, Eq, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct Id {
    pub location: Option<Span>,
    pub name: Name
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
