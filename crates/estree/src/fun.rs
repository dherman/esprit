use easter::fun::{Fun, Params};
use easter::stmt::Stmt;
use unjson::ty::Object;
use unjson::ExtractField;

use tag::TagOf;
use result::Result;
use error::{Error, node_type_error};
use node::ExtractNode;
use stmt::IntoStmt;

pub trait IntoFun {
    fn into_fun(self) -> Result<Fun>;
}

impl IntoFun for Object {
    fn into_fun(mut self) -> Result<Fun> {
        let id = try!(self.extract_id_opt("id"));
        let params = Params {
            location: None,
            list: try!(self.extract_patt_list("params"))
        };
        let obj = try!(self.extract_object("body").map_err(Error::Json));
        let tag = try!(obj.tag());
        let body = try!(obj.into_block());
        Ok(Fun { location: None, id: id, params: params, body: body })
    }
}
