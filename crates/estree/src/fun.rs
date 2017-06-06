use easter::fun::{ Fun, Params };
use unjson::ty::Object;
use unjson::ExtractField;
use serde::ser::*;

use result::Result;
use error::Error;
use node::ExtractNode;
use util::*;

pub trait IntoFun<Id> {
    fn into_fun(self, Id) -> Result<Fun<Id>>;
}

impl<Id> IntoFun<Id> for Object {
    fn into_fun(mut self, id: Id) -> Result<Fun<Id>> {
        let params = self.extract_params("params")?;
        let mut obj = self.extract_object("body").map_err(Error::Json)?;
        let body = obj.extract_script("body")?;
        Ok(Fun { location: None, id: id, params: params, body: body })
    }
}

impl<'a, Id> Serialize for Serialization<'a, Fun<Id>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        let params = Serialization::new(&self.data().params);
        let body = Serialization::new(&self.data().body.items);
        match *self.container() {
            Container::FunctionDeclaration =>
                tag(json!({
                    "type": "FunctionDeclaration",
                    "params": params,
                    "body": body,
                })).serialize(serializer),
            Container::FunctionExpression =>
                tag(json!({
                    "type": "FunctionExpression",
                    "params": params,
                    "body": body,
                })).serialize(serializer),
            _ =>
                tag(json!({
                    "params": params,
                    "body": body,
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, Params> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(None)?;
        for item in &self.data().list {
            seq.serialize_element(&Serialization::new(item))?;
        }
        if let Some(ref rest) = self.data().rest {
            seq.serialize_element(&Serialization::new(rest))?;
        }
        seq.end()
    }
}
