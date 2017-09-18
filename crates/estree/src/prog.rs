use unjson::ty::Object;
use easter::stmt::{Module, Script};
use result::Result;
use node::ExtractNode;

pub trait IntoModule {
    fn into_module(self) -> Result<Module>;
}

impl IntoModule for Object {
    fn into_module(mut self) -> Result<Module> {
        self.extract_module("body")
    }
}

pub trait IntoScript {
    fn into_script(self) -> Result<Script>;
}

impl IntoScript for Object {
    fn into_script(mut self) -> Result<Script> {
        self.extract_script("body")
    }
}
