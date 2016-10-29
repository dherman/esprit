use joker::track::*;

use stmt::Stmt;

#[derive(Debug, PartialEq)]
pub struct Script {
    pub body: Vec<Stmt>
}

impl Untrack for Script {
    fn untrack(&mut self) {
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
