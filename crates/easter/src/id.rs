use joker::token::Name;
use joker::track::*;

use expr::{Expr, ExprData};
use decl::{Dtor, DtorData};
use patt::Patt;

#[derive(Debug, Eq, PartialEq)]
pub struct IdData {
    pub name: Name
}

impl Untrack for IdData {
    fn untrack(&mut self) { }
}

pub type Id = Tracked<IdData>;

pub trait IdExt {
    fn new(Name, Option<Span>) -> Id;
    fn into_patt(self) -> Patt;
    fn into_expr(self) -> Expr;
    fn into_dtor(self) -> Dtor;
}

impl IdExt for Id {
    fn new(name: Name, location: Option<Span>) -> Id {
        Id {
            value: IdData { name: name },
            location: location
        }
    }

    fn into_patt(self) -> Patt {
        Patt::Simple(self)
    }

    fn into_expr(self) -> Expr {
        self.map_self(ExprData::Id)
    }

    fn into_dtor(self) -> Dtor {
        Dtor {
            location: self.location,
            value: DtorData::Simple(self, None)
        }
    }
}
