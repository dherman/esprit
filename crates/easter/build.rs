extern crate syn;

#[macro_use]
extern crate quote;

use std::env;
use std::path::{PathBuf, Path as FilePath};

use syn::*;
use quote::Tokens;

fn get_location_ident() -> Ident {
    Ident::from("location")
}

fn get_location_type() -> Ty {
    Ty::Path(None, Path {
        global: false,
        segments: vec![PathSegment {
            ident: Ident::from("Option"),
            parameters: PathParameters::AngleBracketed(AngleBracketedParameterData {
                lifetimes: vec![],
                types: vec![Ty::Path(None, Path::from("Span"))],
                bindings: vec![]
            })
        }]
    })
}

fn get_self_expr() -> Box<Expr> {
    Box::new(Expr::from(ExprKind::Path(None, Path::from("self"))))
}

fn expand_tracking_ref_data(path: Path, data: &VariantData, mutability: Mutability) -> Arm {
    let location_pat = Pat::Ident(BindingMode::ByRef(mutability), get_location_ident(), None);

    let (pat, field) = match *data {
        VariantData::Struct(_) => (
            Pat::Struct(path, vec![FieldPat {
                ident: get_location_ident(),
                pat: Box::new(location_pat),
                is_shorthand: true
            }], true),
            data.fields().iter().find(|field| field.ident == Some(get_location_ident())).unwrap()
        ),
        VariantData::Tuple(ref fields) => (
            Pat::TupleStruct(path, {
                let mut v = vec![location_pat];
                v.extend(std::iter::repeat(Pat::Wild).take(fields.len() - 1));
                v
            }, None),
            &data.fields()[0]
        ),
        VariantData::Unit => panic!("Empty unit is not trackable")
    };

    let expr = Expr::from(ExprKind::Path(None, Path::from(get_location_ident())));

    Arm {
        attrs: vec![],
        pats: vec![pat],
        guard: None,
        body: Box::new(if field.ty == get_location_type() {
            expr
        } else {
            Expr::from(ExprKind::MethodCall(
                Ident::from(if mutability == Mutability::Immutable { "tracking_ref" } else { "tracking_mut" }),
                vec![],
                vec![expr]
            ))
        })
    }
}

fn expand_tracking_ref(ast: &MacroInput, mutability: Mutability) -> Tokens {
    // Used in the quasi-quotation below as `#name`
    let name = &ast.ident;

    let body = match ast.body {
        Body::Struct(ref data) => {
            vec![expand_tracking_ref_data(Path::from(name.clone()), data, mutability)]
        },
        Body::Enum(ref variants) => {
            variants.iter().map(|var| {
                let path = Path {
                    global: false,
                    segments: vec![
                        PathSegment::from(name.clone()),
                        PathSegment::from(var.ident.clone())
                    ]
                };
                expand_tracking_ref_data(path, &var.data, mutability)
            }).collect()
        }
    };

    let body = Expr::from(ExprKind::Match(
        Box::new(Expr::from(ExprKind::Unary(UnOp::Deref, get_self_expr()))),
        body
    ));

    // Helper is provided for handling complex generic types correctly and effortlessly
    let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

    let (impl_name, method) = if mutability == Mutability::Immutable {
        (Token::Ident(Ident::from("TrackingRef")), quote! {
            tracking_ref(&self) -> &Option<Span>
        })
    } else {
        (Token::Ident(Ident::from("TrackingMut")), quote! {
            tracking_mut(&mut self) -> &mut Option<Span>
        })
    };

    quote! {
        // The generated impl
        impl #impl_generics #impl_name for #name #ty_generics #where_clause {
            fn #method {
                #body
            }
        }
    }
}

pub fn expand_file<S, D>(src: S, dst: D) -> Result<(), String>
    where S: AsRef<FilePath>,
          D: AsRef<FilePath>
{
    let mut registry = Registry::new();
    registry.add_derive("TrackingRef", |input: MacroInput| {
        let tokens = expand_tracking_ref(&input, Mutability::Immutable);
        let items = parse_items(&tokens.to_string())?;
        Ok(Expanded {
            new_items: items,
            original: Some(input),
        })
    });
    registry.add_derive("TrackingMut", |input: MacroInput| {
        let tokens = expand_tracking_ref(&input, Mutability::Mutable);
        let items = parse_items(&tokens.to_string())?;
        Ok(Expanded {
            new_items: items,
            original: Some(input),
        })
    });
    registry.expand_file(src, dst)
}

fn main() {
    let out_dir = env::var_os("OUT_DIR").unwrap();
    let out_path = FilePath::new(&out_dir);

    let src_path = {
        let mut buf = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        buf.push("src");
        buf
    };

    for entry in src_path.read_dir().unwrap() {
        let path = entry.unwrap().path();
        let file_name = path.file_name().unwrap();
        if file_name != "lib.rs" {
            let dest = out_path.join(file_name);
            expand_file(&path, &dest).unwrap();
        }
    }
}