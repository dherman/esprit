use unjson::ty::Object;
use easter::stmt::Script;
use result::Result;
use node::ExtractNode;

pub trait IntoScript {
    fn into_script(self) -> Result<Script>;
}

impl IntoScript for Object {
    fn into_script(mut self) -> Result<Script> {
        self.extract_script("body")
    }
}
