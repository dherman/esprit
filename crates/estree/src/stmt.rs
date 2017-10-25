use easter::stmt::{Stmt, Block, ForHead, ForInHead, ForOfHead, ModItem, StmtListItem, Case, Catch};
use easter::decl::{Decl, Export, ExportSpecifier};
use easter::expr::Expr;
use easter::punc::Semi;
use easter::patt::Patt;
use easter::cover::IntoAssignPatt;
use unjson::ty::Object;
use unjson::{Unjson, ExtractField};

use tag::{Tag, TagOf};
use decl::IntoConst;
use expr::IntoExpr;
use fun::IntoFun;
use error::{Error, string_error, array_error, node_type_error};
use result::Result;
use node::ExtractNode;
use lit::IntoStringLiteral;

trait IntoForHead {
    fn into_for_head(self) -> Result<ForHead>;
}

impl IntoForHead for Object {
    fn into_for_head(mut self) -> Result<ForHead> {
        Ok(match self.tag()? {
            Tag::VariableDeclaration => {
                let dtors = self.extract_dtor_list("declarations")?;
                let kind = self.extract_string("kind").map_err(Error::Json)?;
                match &kind[..] {
                    "var" => ForHead::Var(None, dtors),
                    "let" => ForHead::Let(None, dtors),
                    "const" => ForHead::Const(None, dtors.into_const()?),
                    _ => { return string_error("var or let", kind); }
                }
            }
            _ => ForHead::Expr(None, self.into_expr()?)
        })
    }
}

trait IntoForInHead {
    fn into_for_in_head(self) -> Result<ForInHead>;
}

impl IntoForInHead for Object {
    fn into_for_in_head(mut self) -> Result<ForInHead> {
        Ok(match self.tag()? {
            Tag::VariableDeclaration => {
                let mut dtors = self.extract_array("declarations").map_err(Error::Json)?;
                let kind = self.extract_string("kind").map_err(Error::Json)?;
                if dtors.len() != 1 {
                    return array_error(1, dtors.len());
                }
                let mut obj = dtors.remove(0).into_object().map_err(Error::Json)?;
                let lhs = obj.extract_patt("id")?;
                let init = obj.extract_expr_opt("init")?;
                match (&kind[..], lhs, init) {
                    ("var", Patt::Simple(id), Some(init)) => {
                        ForInHead::VarInit(None, id, init)
                    }
                    (_, _, Some(init)) => {
                        return Err(Error::UnexpectedInitializer(init));
                    }
                    ("var", lhs, _) => ForInHead::Var(None, lhs),
                    ("let", lhs, _) => ForInHead::Let(None, lhs),
                    ("const", lhs, _) => ForInHead::Const(None, lhs),
                    (_, _, _) => { return string_error("var or let", kind); }
                }
            }
            _ => {
                let expr = self.into_expr()?;
                match expr.into_assign_patt() {
                    Ok(patt) => ForInHead::Patt(patt),
                    _ => { return Err(Error::InvalidLHS("left")); }
                }
            }
        })
    }
}

trait IntoForOfHead {
    fn into_for_of_head(self) -> Result<ForOfHead>;
}

impl IntoForOfHead for Object {
    fn into_for_of_head(mut self) -> Result<ForOfHead> {
        Ok(match self.tag()? {
            Tag::VariableDeclaration => {
                let mut dtors = self.extract_array("declarations").map_err(Error::Json)?;
                let kind = self.extract_string("kind").map_err(Error::Json)?;
                if dtors.len() != 1 {
                    return array_error(1, dtors.len());
                }
                let mut obj = dtors.remove(0).into_object().map_err(Error::Json)?;
                let lhs = obj.extract_patt("id")?;
                match &kind[..] {
                    "var" => ForOfHead::Var(None, lhs),
                    "let" => ForOfHead::Let(None, lhs),
                    "const" => ForOfHead::Const(None, lhs),
                    _ => { return string_error("var or let", kind); }
                }
            },
            _ => {
                let expr = self.into_expr()?;
                match expr.into_assign_patt() {
                    Ok(patt) => ForOfHead::Patt(patt),
                    _ => { return Err(Error::InvalidLHS("left")); }
                }
            }
        })
    }
}

pub trait IntoStmt {
    fn into_stmt(self) -> Result<Stmt>;
    fn into_stmt_list_item(self) -> Result<StmtListItem>;
    fn into_case(self) -> Result<Case>;
    fn into_catch(self) -> Result<Catch>;
    fn into_block(self) -> Result<Block>;
}

fn into_stmt_list_item(mut this: Object, allow_decl: bool) -> Result<StmtListItem> {
    let tag = this.tag()?;
    Ok(StmtListItem::Stmt(match tag {
        Tag::FunctionDeclaration => {
            if !allow_decl {
                return node_type_error("statement", tag);
            }
            let id = this.extract_id("id")?;
            return Ok(StmtListItem::Decl(Decl::Fun(this.into_fun(id)?)));
        }
        Tag::VariableDeclaration => {
            let dtors = this.extract_dtor_list("declarations")?;
            let kind = this.extract_string("kind").map_err(Error::Json)?;
            match &kind[..] {
                "var" => Stmt::Var(None, dtors, Semi::Explicit(None)),
                "let" => {
                    if !allow_decl {
                        return string_error("var", kind);
                    }
                    return Ok(StmtListItem::Decl(Decl::Let(None, dtors, Semi::Explicit(None))));
                },
                "const" => {
                    if !allow_decl {
                        return string_error("var", kind);
                    }
                    return Ok(StmtListItem::Decl(Decl::Const(None, dtors.into_const()?, Semi::Explicit(None))));
                }
                _ => { return string_error("var or let", kind); }
            }
        }
        Tag::EmptyStatement => Stmt::Empty(None),
        Tag::ExpressionStatement => {
            let expr = this.extract_expr("expression")?;
            Stmt::Expr(None, expr, Semi::Explicit(None))
        }
        Tag::IfStatement => {
            let test = this.extract_expr("test")?;
            let cons = Box::new(this.extract_stmt("consequent")?);
            let alt = this.extract_stmt_opt("alternate")?.map(Box::new);
            Stmt::If(None, test, cons, alt)
        }
        Tag::DoWhileStatement => {
            let body = Box::new(this.extract_stmt("body")?);
            let test = this.extract_expr("test")?;
            Stmt::DoWhile(None, body, test, Semi::Explicit(None))
        }
        Tag::WhileStatement => {
            let test = this.extract_expr("test")?;
            let body = Box::new(this.extract_stmt("body")?);
            Stmt::While(None, test, body)
        }
        Tag::ForStatement => {
            let init = match this.extract_object_opt("init").map_err(Error::Json)? {
                None      => None,
                Some(obj) => Some(Box::new(obj.into_for_head()?))
            };
            let test = this.extract_expr_opt("test")?;
            let update = this.extract_expr_opt("update")?;
            let body = Box::new(this.extract_stmt("body")?);
            Stmt::For(None, init, test, update, body)
        }
        Tag::ForInStatement => {
            let left = this.extract_object("left").map_err(Error::Json)?.into_for_in_head()?;
            let right = this.extract_expr("right")?;
            let body = this.extract_stmt("body")?;
            Stmt::ForIn(None, Box::new(left), right, Box::new(body))
        }
        Tag::ForOfStatement => {
            let left = this.extract_object("left").map_err(Error::Json)?.into_for_of_head()?;
            let right = this.extract_expr("right")?;
            let body = this.extract_stmt("body")?;
            Stmt::ForOf(None, Box::new(left), right, Box::new(body))
        }
        Tag::BlockStatement => {
            Stmt::Block(this.into_block()?)
        }
        Tag::ReturnStatement => {
            let arg = this.extract_expr_opt("argument")?;
            Stmt::Return(None, arg, Semi::Explicit(None))
        }
        Tag::LabeledStatement => {
            let label = this.extract_id("label")?;
            let body = Box::new(this.extract_stmt("body")?);
            Stmt::Label(None, label, body)
        }
        Tag::BreakStatement => {
            let label = this.extract_id_opt("label")?;
            Stmt::Break(None, label, Semi::Explicit(None))
        }
        Tag::ContinueStatement => {
            let label = this.extract_id_opt("label")?;
            Stmt::Cont(None, label, Semi::Explicit(None))
        }
        Tag::SwitchStatement => {
            let disc = this.extract_expr("discriminant")?;
            let cases = this.extract_case_list("cases")?;
            Stmt::Switch(None, disc, cases)
        }
        Tag::WithStatement => {
            let obj = this.extract_expr("object")?;
            let body = Box::new(this.extract_stmt("body")?);
            Stmt::With(None, obj, body)
        }
        Tag::ThrowStatement => {
            let arg = this.extract_expr("argument")?;
            Stmt::Throw(None, arg, Semi::Explicit(None))
        }
        Tag::DebuggerStatement => {
            Stmt::Debugger(None, Semi::Explicit(None))
        }
        Tag::TryStatement => {
            let body = this.extract_block("block")?;
            let catch = this.extract_catch_opt("handler")?.map(Box::new);
            let finally = match this.extract_object_opt("finalizer").map_err(Error::Json)? {
                Some(finalizer)     => Some(finalizer.into_block()?),
                None                => None
            };
            Stmt::Try(None, body, catch, finally)
        }
        _ => { return node_type_error(if allow_decl { "statement or declaration" } else { "statement" }, tag); }
    }))
}

impl IntoStmt for Object {
    fn into_stmt(self) -> Result<Stmt> {
        into_stmt_list_item(self, false).map(|item| match item {
            StmtListItem::Stmt(stmt) => stmt,
            _ => unreachable!()
        })
    }

    fn into_stmt_list_item(self) -> Result<StmtListItem> {
        into_stmt_list_item(self, true)
    }
    fn into_case(mut self) -> Result<Case> {
        let test = self.extract_expr_opt("test")?;
        let body = self.extract_stmt_list("consequent")?;
        Ok(Case { location: None, test: test, body: body })
    }

    fn into_catch(mut self) -> Result<Catch> {
        let param = self.extract_patt("param")?;
        let body = self.extract_block("body")?;
        Ok(Catch { location: None, param: param, body: body })
    }

    fn into_block(mut self) -> Result<Block> {
        match self.tag()? {
            Tag::BlockStatement => Ok(Block {
                location: None,
                items: self.extract_stmt_list("body")?
            }),
            tag => node_type_error("block statement", tag)
        }
    }
}

pub trait IntoModItem {
    fn into_mod_item(self) -> Result<ModItem>;
    fn into_export_all(self) -> Result<Export>;
    fn into_export_named(self) -> Result<Export>;
    fn into_export_default(self) -> Result<Export>;
    fn into_export_specifier(self) -> Result<ExportSpecifier>;
}

impl IntoModItem for Object {
    fn into_mod_item(self) -> Result<ModItem> {
        Ok(match self.tag()? {
            Tag::ExportDefaultDeclaration => ModItem::Export(self.into_export_default()?),
            Tag::ExportAllDeclaration => ModItem::Export(self.into_export_all()?),
            Tag::ExportNamedDeclaration => ModItem::Export(self.into_export_named()?),
            _ => ModItem::StmtListItem(self.into_stmt_list_item()?)
        })
    }

    fn into_export_default(mut self) -> Result<Export> {
        let mut declaration = self.extract_object("declaration").map_err(Error::Json)?;
        let expr = match declaration.tag()? {
            Tag::FunctionDeclaration => {
                let id = declaration.extract_id_opt("id")?;
                Expr::Fun(declaration.into_fun(id)?)
            }
            _ => declaration.into_expr()?
        };
        Ok(Export::Default(None, expr, Semi::Explicit(None)))
    }

    fn into_export_all(mut self) -> Result<Export> {
        let mut source = self.extract_object("source").map_err(Error::Json)?;
        let value = source.extract_string("value").map_err(Error::Json)?;
        Ok(Export::All(None, value.into_string_literal(), Semi::Explicit(None)))
    }

    fn into_export_named(mut self) -> Result<Export> {
        let declaration = self.extract_object_opt("declaration").map_err(Error::Json)?;

        match declaration {
            None => {
                let specifiers = self.extract_exports_list("specifiers")?;
                let from = match self.extract_object_opt("source").map_err(Error::Json)? {
                    Some(object) => {
                        match object.into_lit()? {
                            Expr::String(_, literal) => Some(literal),
                            _ => unimplemented!()
                        }
                    },
                    None => None
                };
                return Ok(Export::List(None, specifiers, from, Semi::Explicit(None)));
            },
            Some(mut declaration) => {
                match declaration.tag()? {
                    Tag::FunctionDeclaration => {
                        let id = declaration.extract_id("id")?;
                        Ok(Export::Decl(Decl::Fun(declaration.into_fun(id)?)))
                    },
                    Tag::VariableDeclaration => {
                        let dtors = declaration.extract_dtor_list("declarations")?;
                        let kind = declaration.extract_string("kind").map_err(Error::Json)?;
                        match &kind[..] {
                            "var" => Ok(Export::Var(None, dtors, Semi::Explicit(None))),
                            "let" => Ok(Export::Decl(Decl::Let(None, dtors, Semi::Explicit(None)))),
                            "const" => Ok(Export::Decl(Decl::Const(None, dtors.into_const()?, Semi::Explicit(None)))),
                            _ => { return string_error("var or let or const", kind); }
                        }
                    },
                    _ => unimplemented!()
                }
            }
        }
    }

    fn into_export_specifier(mut self) -> Result<ExportSpecifier> {
        Ok(ExportSpecifier {
            location: None,
            exported: self.extract_id("exported")?,
            local: self.extract_id("local")?
        })
    }
}
