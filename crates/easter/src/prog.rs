use joker::track::*;

use stmt::StmtListItem;

#[derive(Debug, PartialEq)]
pub struct Script {
    pub location: Option<Span>,
    pub body: Vec<StmtListItem>
}

impl TrackingRef for Script {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Script {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Script {
    fn untrack(&mut self) {
        self.location = None;
        self.body.untrack();
    }
}

/*
#[derive(Debug, PartialEq)]
pub struct Module {
    pub location: Option<Span>,
    pub body: Vec<ModItem>
}
*/
