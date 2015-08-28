use joker::track::*;

use id::Id;
use patt::Patt;
use stmt::StmtListItem;

#[derive(Debug, PartialEq)]
pub struct ParamsData {
    pub list: Vec<Patt<Id>>
}

pub type Params = Tracked<ParamsData>;

impl Untrack for ParamsData {
    fn untrack(&mut self) {
        self.list.untrack();
    }
}

#[derive(Debug, PartialEq)]
pub struct FunData {
    pub id: Option<Id>,
    pub params: Params,
    pub body: Vec<StmtListItem>
}

impl Untrack for FunData {
    fn untrack(&mut self) {
        self.id.untrack();
        self.params.untrack();
        self.body.untrack();
    }
}

pub type Fun = Tracked<FunData>;
