use joker::track::*;

use stmt::StmtListItem;

#[derive(Debug, PartialEq)]
pub struct ScriptData {
    pub body: Vec<StmtListItem>
}

impl Untrack for ScriptData {
    fn untrack(&mut self) {
        self.body.untrack();
    }
}

pub type Script = Tracked<ScriptData>;

/*
#[derive(Debug, PartialEq)]
pub struct ModuleData {
    pub body: Vec<ModItem>
}

pub type Module = Tracked<ModuleData>;
*/
