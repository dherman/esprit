extern crate syn;

#[macro_use]
extern crate quote;

extern crate proc_macro;
use proc_macro::TokenStream;

use std::iter;
use syn::*;
use quote::Tokens;

struct Context {
    location_ident: Ident,
    location_expr: Expr,
    location_type: Ty,
    deref_self_expr: Expr,
}

impl Context {
    pub fn new() -> Self {
        Context {
            location_ident: Ident::from("location"),

            location_expr: Expr::from(ExprKind::Path(None, Path::from("location"))),

            location_type: Ty::Path(None, Path {
                global: false,
                segments: vec![PathSegment {
                    ident: Ident::from("Option"),
                    parameters: PathParameters::AngleBracketed(AngleBracketedParameterData {
                        lifetimes: vec![],
                        types: vec![Ty::Path(None, Path::from("Span"))],
                        bindings: vec![]
                    })
                }]
            }),

            deref_self_expr: Expr::from(ExprKind::Unary(
                UnOp::Deref,
                Box::new(Expr::from(ExprKind::Path(None, Path::from("self"))))
            ))
        }
    }

    fn expand_tracking_data(&self, path: Path, data: &VariantData, mutability: Mutability) -> Arm {
        let location_pat = Pat::Ident(BindingMode::ByRef(mutability), self.location_ident.clone(), None);

        let expr = self.location_expr.clone();

        let (pat, expr) = match *data {
            VariantData::Struct(_) => (
                Pat::Struct(path, vec![FieldPat {
                    ident: self.location_ident.clone(),
                    pat: Box::new(location_pat),
                    is_shorthand: true
                }], true),
                if data.fields().iter().any(|field| field.ident.as_ref() == Some(&self.location_ident) && field.ty == self.location_type) {
                    expr
                } else {
                    panic!("Struct does not containt `location: Option<Span>`")
                }
            ),
            VariantData::Tuple(ref fields) => (
                Pat::TupleStruct(path, {
                    let mut v = Vec::with_capacity(fields.len());
                    v.push(location_pat);
                    v.extend(iter::repeat(Pat::Wild).take(fields.len() - 1));
                    v
                }, None),
                if data.fields()[0].ty == self.location_type {
                    expr
                } else {
                    Expr::from(ExprKind::MethodCall(
                        Ident::from(if mutability == Mutability::Immutable { "tracking_ref" } else { "tracking_mut" }),
                        vec![],
                        vec![expr]
                    ))
                }
            ),
            VariantData::Unit => panic!("Empty unit is not trackable")
        };

        Arm {
            attrs: vec![],
            pats: vec![pat],
            guard: None,
            body: Box::new(expr)
        }
    }

    fn expand_tracking(&self, ast: &MacroInput, mutability: Mutability) -> Tokens {
        // Helper is provided for handling complex generic types correctly and effortlessly
        let (impl_generics, ty_generics, where_clause) = ast.generics.split_for_impl();

        let (impl_name, method) = if mutability == Mutability::Immutable {
            (Ident::from("TrackingRef"), quote! {
                tracking_ref(&self) -> &Option<Span>
            })
        } else {
            (Ident::from("TrackingMut"), quote! {
                tracking_mut(&mut self) -> &mut Option<Span>
            })
        };

        // Used in the quasi-quotation below as `#name`
        let name = &ast.ident;

        let body = Expr::from(ExprKind::Match(
            Box::new(self.deref_self_expr.clone()),
            match ast.body {
                Body::Struct(ref data) => {
                    vec![self.expand_tracking_data(Path::from(name.clone()), data, mutability)]
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
                        self.expand_tracking_data(path, &var.data, mutability)
                    }).collect()
                }
            }
        ));

        quote! {
            // The generated impl
            impl #impl_generics #impl_name for #name #ty_generics #where_clause {
                fn #method {
                    #body
                }
            }
        }
    }

    fn expand_untrack_data(&self, path: Path, data: &VariantData) -> Arm {
        let (pat, idents) = match *data {
            VariantData::Struct(ref fields) => {
                let mut field_pats = Vec::with_capacity(fields.len());
                let mut idents = Vec::with_capacity(fields.len());

                for field in fields {
                    let ident = field.ident.as_ref().unwrap();

                    field_pats.push(FieldPat {
                        ident: ident.clone(),
                        pat: Box::new(Pat::Ident(BindingMode::ByRef(Mutability::Mutable), ident.clone(), None)),
                        is_shorthand: true
                    });

                    idents.push(ident.clone());
                }

                (Pat::Struct(path, field_pats, false), idents)
            },
            VariantData::Tuple(ref fields) => {
                let mut pats = Vec::with_capacity(fields.len());
                let mut idents = Vec::with_capacity(fields.len());

                for i in 0..fields.len() {
                    let ident = Ident::from(format!("_f{}", i));

                    pats.push(Pat::Ident(BindingMode::ByRef(Mutability::Mutable), ident.clone(), None));

                    idents.push(ident);
                }

                (Pat::TupleStruct(path, pats, None), idents)
            },
            VariantData::Unit => {
                (Pat::Path(None, path), vec![])
            }
        };

        let expr = Expr::from(ExprKind::Block(BlockCheckMode::Default, Block {
            stmts: idents.into_iter().map(|ident| {
                Stmt::Semi(Box::new(Expr::from(ExprKind::MethodCall(
                    Ident::from("untrack"),
                    vec![],
                    vec![Expr::from(ExprKind::Path(None, Path::from(ident)))]
                ))))
            }).collect()
        }));

        Arm {
            attrs: vec![],
            pats: vec![pat],
            guard: None,
            body: Box::new(expr)
        }
    }

    pub fn expand_tracking_ref(&self, ast: &MacroInput) -> Tokens {
        self.expand_tracking(ast, Mutability::Immutable)
    }

    pub fn expand_tracking_mut(&self, ast: &MacroInput) -> Tokens {
        self.expand_tracking(ast, Mutability::Mutable)
    }

    pub fn expand_untrack(&self, ast: &MacroInput) -> Tokens {
        let mut generics = ast.generics.clone();

        let bound = TyParamBound::Trait(PolyTraitRef {
            bound_lifetimes: vec![],
            trait_ref: Path::from("Untrack")
        }, TraitBoundModifier::None);

        for ty in &mut generics.ty_params {
            ty.bounds.push(bound.clone());
        }

        let (impl_generics, ty_generics, where_clause) = generics.split_for_impl();

        let name = &ast.ident;

        let body = Expr::from(ExprKind::Match(
            Box::new(self.deref_self_expr.clone()),
            match ast.body {
                Body::Struct(ref data) => {
                    vec![self.expand_untrack_data(Path::from(name.clone()), data)]
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
                        self.expand_untrack_data(path, &var.data)
                    }).collect()
                }
            }
        ));

        quote! {
            // The generated impl
            impl #impl_generics Untrack for #name #ty_generics #where_clause {
                fn untrack(&mut self) {
                    #body
                }
            }
        }
    }
}

macro_rules! register_tracking_derive {
    ($for_trait:ident, $method:ident) => {
        #[proc_macro_derive($for_trait)]
        pub fn $method(input: TokenStream) -> TokenStream {
            let source = input.to_string();
            let context = Context::new();

            // Parse the string representation into a syntax tree
            let ast = parse_macro_input(&source).unwrap();

            // Build the output, possibly using quasi-quotation
            let expanded = context.$method(&ast);

            // Parse back to a token stream and return it
            expanded.parse().unwrap()
        }
    }
}

register_tracking_derive!(TrackingRef, expand_tracking_ref);
register_tracking_derive!(TrackingMut, expand_tracking_mut);
register_tracking_derive!(Untrack, expand_untrack);
