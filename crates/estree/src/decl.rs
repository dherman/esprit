use easter::decl::{Dtor, ConstDtor, DtorExt};
use easter::patt::Patt;
use unjson::ty::Object;

use result::Result;
use error::Error;
use node::ExtractNode;

pub trait IntoDecl {
    fn into_dtor(self) -> Result<Dtor>;
}

impl IntoDecl for Object {
    fn into_dtor(mut self) -> Result<Dtor> {
        let lhs = self.extract_patt("id")?;
        let init = self.extract_expr_opt("init")?;
        Dtor::from_init_opt(lhs, init).map_err(Error::UninitializedPattern)
    }
}

pub trait IntoConst {
    fn into_const(self) -> Result<Vec<ConstDtor>>;
}

impl IntoConst for Vec<Dtor> {
    fn into_const(self) -> Result<Vec<ConstDtor>> {
        self.into_iter().map(|dtor| {
            Ok(match dtor {
                Dtor::Simple(_, id, Some(expr)) => {
                    ConstDtor::from_simple_init(id, expr)
                }
                Dtor::Simple(_, id, None) => {
                    return Err(Error::UninitializedPattern(Patt::Simple(id)));
                }
                Dtor::Compound(_, compound, expr) => {
                    ConstDtor::from_compound_init(compound, expr)
                }
            })
        }).collect()
    }
}