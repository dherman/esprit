use joker::track::*;

use id::Id;
use patt::Patt;
use stmt::Script;

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct Params {
    pub location: Option<Span>,
    pub list: Vec<Patt<Id>>
}

impl Untrack for Params {
    fn untrack(&mut self) {
        self.location = None;
        self.list.untrack();
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut)]
pub struct Fun {
    pub location: Option<Span>,
    pub id: Option<Id>,
    pub params: Params,
    pub body: Script
}

impl Untrack for Fun {
    fn untrack(&mut self) {
        self.location = None;
        self.id.untrack();
        self.params.untrack();
        self.body.untrack();
    }
}
