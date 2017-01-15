use easter::fun::{Fun, Params};
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
        let params = Params {
            location: None,
            list: self.extract_patt_list("params")?
        };
        let mut obj = self.extract_object("body").map_err(Error::Json)?;
        let body = obj.extract_script("body")?;
        Ok(Fun { location: None, id: id, params: params, body: body })
    }
}
