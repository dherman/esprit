extern crate unjson;
extern crate serde;
extern crate serde_json;
extern crate joker;
extern crate easter;

mod tag;
pub mod error;
pub mod result;
mod stmt;
mod expr;
mod id;
mod node;
mod fun;
mod patt;
mod obj;
mod decl;
mod prog;
mod lit;

use serde::de::Error;
use serde::de::{Deserialize, Deserializer};
use easter::prog::Script;
use unjson::ty::Object;
pub use prog::IntoScript;

pub struct ESTreeScript(Script);

impl Deserialize for ESTreeScript {
    fn deserialize<D: Deserializer>(de: &mut D) -> ::std::result::Result<Self, D::Error> {
        let json: Object = try!(Deserialize::deserialize(de));
        match json.into_script() {
            Ok(script) => Ok(ESTreeScript(script)),
            Err(err)   => Err(D::Error::custom(&format!("{}", err)[..]))
        }
    }
}

/*
pub struct ESTreeStmt(Stmt);

impl Deserialize for ESTreeStmt {
    fn deserialize<D: Deserializer>(de: &mut D) -> ::std::result::Result<Self, D::Error> {
        let json: Object = try!(Deserialize::deserialize(de));
        match json.into_stmt() {
            Ok(stmt) => Ok(ESTreeStmt(stmt)),
            Err(err) => Err(D::Error::syntax(&format!("{}", err)[..]))
        }
    }
}

pub struct ESTreeExpr(Expr);

impl Deserialize for ESTreeExpr {
    ...
}
*/

