use easter::decl::{Dtor, DtorExt};
use unjson::ty::Object;

use result::Result;
use error::Error;
use node::ExtractNode;

pub trait IntoDecl {
    fn into_dtor(self) -> Result<Dtor>;
}

impl IntoDecl for Object {
    fn into_dtor(mut self) -> Result<Dtor> {
        let lhs = try!(self.extract_patt("id"));
        let init = try!(self.extract_expr_opt("init"));
        Dtor::from_init_opt(lhs, init).map_err(Error::UninitializedPattern)
    }
}
