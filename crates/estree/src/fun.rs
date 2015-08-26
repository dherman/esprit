use easter::fun::{Fun, FunData, ParamsData};
use easter::stmt::StmtData;
use unjson::ty::Object;
use unjson::ExtractField;
use joker::track::*;

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
        let params = (ParamsData {
            list: try!(self.extract_patt_list("params"))
        }).tracked(None);
        let obj = try!(self.extract_object("body").map_err(Error::Json));
        let tag = try!(obj.tag());
        let body = match try!(obj.into_stmt()).value {
            StmtData::Block(items) => items,
            _ => { return node_type_error("BlockStatement", tag); }
        };
        Ok(FunData { id: id, params: params, body: body }.tracked(None))
    }
}
