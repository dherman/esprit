use joker::track::*;

use id::Id;
use patt::Patt;
use stmt::Script;

#[derive(Debug, PartialEq, Clone)]
pub struct Params {
    pub location: Option<Span>,
    pub list: Vec<Patt<Id>>
}

impl TrackingRef for Params {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Params {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Params {
    fn untrack(&mut self) {
        self.location = None;
        self.list.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    pub location: Option<Span>,
    pub id: Option<Id>,
    pub params: Params,
    pub body: Script
}

impl TrackingRef for Fun {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Fun {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Fun {
    fn untrack(&mut self) {
        self.location = None;
        self.id.untrack();
        self.params.untrack();
        self.body.untrack();
    }
}
