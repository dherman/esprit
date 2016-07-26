use easter::obj::{Prop, PropData, PropKey, PropKeyData, PropValData};
use easter::expr::ExprData;
use unjson::ty::{Object, Ty};
use unjson::{Unjson, ExtractField};
use joker::track::*;

use tag::{Tag, TagOf};
use id::IntoId;
use result::Result;
use error::{Error, type_error, array_error};
use node::ExtractNode;
use expr::IntoExpr;
use fun::IntoFun;

pub trait IntoObj {
    fn into_prop(self) -> Result<Prop>;
    fn into_prop_key(self) -> Result<PropKey>;
}

impl IntoObj for Object {
    fn into_prop(mut self) -> Result<Prop> {
        let key = try!(try!(self.extract_object("key").map_err(Error::Json)).into_prop_key());
        let mut val = try!(self.extract_object("value").map_err(Error::Json));
        let kind = try!(self.extract_string("kind").map_err(Error::Json));
        let val = (match &kind[..] {
            "init" => PropValData::Init(try!(val.into_expr())),
            "get" => PropValData::Get(try!(try!(val.extract_object("body").map_err(Error::Json)).extract_stmt_list("body"))),
            "set" => {
                let fun = try!(val.into_fun()).value;
                let params = fun.params.value.list;
                if params.len() != 1 {
                    return array_error(1, params.len());
                }
                let param = params.into_iter().next().unwrap();
                PropValData::Set(param, fun.body)
            }
            _ => { return type_error("'init', 'get', or 'set'", Ty::String); }
        }).tracked(None);
        Ok(PropData { key: key, val: val }.tracked(None))
    }

    fn into_prop_key(self) -> Result<PropKey> {
        if try!(self.tag()) == Tag::Identifier {
            let id = try!(self.into_id());
            return Ok(PropKeyData::Id(id.value.name.into_string()).tracked(None));
        }
        match try!(self.into_lit()).value {
            ExprData::Number(lit) => Ok(PropKeyData::Number(lit).tracked(None)),
            ExprData::String(lit) => Ok(PropKeyData::String(lit).tracked(None)),
            _ => { return type_error("identifier, number literal, or string literal", Ty::Object); }
        }
    }
}
