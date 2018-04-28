use easter::decl::{Decl, Dtor, ConstDtor, DtorExt};
use easter::patt::Patt;
use unjson::ty::Object;
use serde::ser::*;

use result::Result;
use error::Error;
use node::ExtractNode;
use util::*;

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

impl<'a> Serialize for Serialization<'a, Decl> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::decl::Decl::*;
        match *self.data() {
            Fun(ref f) =>
                Serialization::in_context(f, Container::FunctionDeclaration)
                    .serialize(serializer),
            Let(_, ref dtors, _) =>
                tag(json!({
                    "type": "VariableDeclaration",
                    "declarations": Serialization::new(dtors),
                    "kind": "let"
                })).serialize(serializer),
            Const(_, ref cdtors, _) =>
                tag(json!({
                    "type": "VariableDeclaration",
                    "declarations": Serialization::new(cdtors),
                    "kind": "const"
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, Dtor> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::decl::Dtor::*;
        match *self.data() {
            Simple(_, ref id, ref expr) =>
                tag(json!({
                    "type": "VariableDeclarator",
                    "id": {
                        "type": "Identifier",
                        "name": Serialization::new(id),
                    },
                    "init": Serialization::new(expr)
                })).serialize(serializer),
            Compound(_, ref pattern, ref expr) =>
                tag(json!({
                    "type": "VariableDeclarator",
                    "id": Serialization::new(pattern),
                    "init": Serialization::new(expr)
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, ConstDtor> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        tag(json!({
            "type": "VariableDeclarator",
            "id": Serialization::new(&self.data().patt),
            "init": Serialization::new(&self.data().value),
        })).serialize(serializer)
    }
}
