use easter::fun::Fun;
use unjson::ty::Object;
use unjson::ExtractField;

use result::Result;
use error::Error;
use node::ExtractNode;

pub trait IntoFun<Id> {
    fn into_fun(self, Id) -> Result<Fun<Id>>;
}

impl<Id> IntoFun<Id> for Object {
    fn into_fun(mut self, id: Id) -> Result<Fun<Id>> {
        let generator = self.extract_bool_opt("generator").map_err(Error::Json)?.unwrap_or(false);
        let params = self.extract_params("params")?;
        let mut obj = self.extract_object("body").map_err(Error::Json)?;
        let body = obj.extract_script("body")?;
        Ok(Fun { location: None, id: id, params: params, body: body, generator: generator })
    }
}
