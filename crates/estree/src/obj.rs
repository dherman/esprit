use easter::obj::{Prop, PropKey, PropVal};
use easter::expr::Expr;
use unjson::ty::{Object, Ty};
use unjson::ExtractField;

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
        let key = self.extract_object("key").map_err(Error::Json)?;
        let mut val = self.extract_object("value").map_err(Error::Json)?;
        let kind = self.extract_string("kind").map_err(Error::Json)?;
        let val = match &kind[..] {
            "init" => {
                if self.extract_bool("method").map_err(Error::Json)? {
                    let fun = val.into_fun(key.into_prop_key()?)?;
                    return Ok(Prop::Method(fun))
                } else if self.extract_bool("shorthand").map_err(Error::Json)? {
                    return Ok(Prop::Shorthand(key.into_id()?));
                } else {
                    PropVal::Init(val.into_expr()?)
                }
            },
            "get" => PropVal::Get(None, val.extract_object("body").map_err(Error::Json)?.extract_script("body")?),
            "set" => {
                let fun = val.into_fun(())?;
                let params = fun.params.list;
                if params.len() != 1 {
                    return array_error(1, params.len());
                }
                let param = params.into_iter().next().unwrap();
                PropVal::Set(None, param, fun.body)
            }
            _ => { return type_error("'init', 'get', or 'set'", Ty::String); }
        };
        Ok(Prop::Regular(None, key.into_prop_key()?, val))
    }

    fn into_prop_key(self) -> Result<PropKey> {
        if self.tag()? == Tag::Identifier {
            let id = self.into_id()?;
            return Ok(PropKey::Id(None, id.name.into_string()));
        }
        Ok(match self.into_lit()? {
            Expr::Number(_, lit) => PropKey::Number(None, lit),
            Expr::String(_, lit) => PropKey::String(None, lit),
            _ => { return type_error("identifier, number literal, or string literal", Ty::Object); }
        })
    }
}
