use joker::track::*;

use id::Id;
use expr::Expr;
use obj::{PropKey, DotKey};

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub enum CompoundPatt<T> {
    Arr(Option<Span>, Vec<Option<Patt<T>>>),
    Obj(Option<Span>, Vec<PropPatt<T>>)
}

impl<T: Untrack> Untrack for CompoundPatt<T> {
    fn untrack(&mut self) {
        match *self {
            CompoundPatt::Arr(ref mut location, ref mut patts) => {
                *location = None;
                patts.untrack();
            }
            CompoundPatt::Obj(ref mut location, ref mut props) => {
                *location = None;
                props.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct PropPatt<T> {
    pub location: Option<Span>,
    pub key: PropKey,
    pub patt: Patt<T>
}

impl<T: Untrack> Untrack for PropPatt<T> {
    fn untrack(&mut self) {
        self.location = None;
        self.key.untrack();
        self.patt.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
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

impl<T: TrackingRef> TrackingRef for Patt<T> {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Patt::Simple(ref simple) => simple.tracking_ref(),
            Patt::Compound(ref patt) => patt.tracking_ref()
        }
    }
}

impl<T: TrackingMut> TrackingMut for Patt<T> {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Patt::Simple(ref mut simple) => simple.tracking_mut(),
            Patt::Compound(ref mut patt) => patt.tracking_mut()
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

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum AssignTarget {
    Id(Id),
    Dot(Option<Span>, Box<Expr>, DotKey),
    Brack(Option<Span>, Box<Expr>, Box<Expr>)
}

