use joker::track::*;

use id::Id;
use patt::Patt;
use stmt::Script;

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Params {
    pub location: Option<Span>,
    pub list: Vec<Patt<Id>>
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Fun<Id> {
    pub location: Option<Span>,
    pub id: Id,
    pub params: Params,
    pub body: Script
}
