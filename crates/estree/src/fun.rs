use easter::fun::{Fun, Params};
use unjson::ty::Object;
use unjson::ExtractField;

use result::Result;
use error::Error;
use node::ExtractNode;

pub trait IntoFun {
    fn into_fun(self) -> Result<Fun>;
}

impl IntoFun for Object {
    fn into_fun(mut self) -> Result<Fun> {
        let id = self.extract_id_opt("id")?;
        let params = Params {
            location: None,
            list: self.extract_patt_list("params")?
        };
        let mut obj = self.extract_object("body").map_err(Error::Json)?;
        let body = obj.extract_script("body")?;
        Ok(Fun { location: None, id: id, params: params, body: body })
    }
}
