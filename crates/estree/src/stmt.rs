use easter::stmt::{Stmt, ForHead, ForInHead, ForOfHead, StmtListItem, Case, Catch};
use easter::decl::Decl;
use easter::punc::Semi;
use easter::patt::Patt;
use easter::cover::IntoAssignPatt;
use unjson::ty::Object;
use unjson::{Unjson, ExtractField};

use tag::{Tag, TagOf};
use expr::IntoExpr;
use fun::IntoFun;
use error::{Error, string_error, array_error, node_type_error};
use result::Result;
use node::ExtractNode;

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
                match &kind[..] {
                    "var" => match lhs {
                        Patt::Simple(id) => {
                            match init {
                                None       => ForInHead::Var(None, Patt::Simple(id)),
                                Some(expr) => ForInHead::VarInit(None, id, expr)
                            }
                        }
                        Patt::Compound(patt) => {
                            match init {
                                None       => ForInHead::Var(None, Patt::Compound(patt)),
                                Some(expr) => { return Err(Error::UnexpectedInitializer(expr)); }
                            }
                        }
                    },
                    "let" => {
                        match init {
                            None       => ForInHead::Let(None, lhs),
                            Some(expr) => { return Err(Error::UnexpectedInitializer(expr)); }
                        }
                    }
                    _ => { return string_error("var or let", kind); }
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
}

impl IntoStmt for Object {
    fn into_stmt(mut self) -> Result<Stmt> {
        let tag = self.tag()?;
        Ok(match tag {
            Tag::VariableDeclaration => {
                let dtors = self.extract_dtor_list("declarations")?;
                Stmt::Var(None, dtors, Semi::Explicit(None))
            }
            Tag::EmptyStatement => Stmt::Empty(None),
            Tag::ExpressionStatement => {
                let expr = self.extract_expr("expression")?;
                Stmt::Expr(None, expr, Semi::Explicit(None))
            }
            Tag::IfStatement => {
                let test = self.extract_expr("test")?;
                let cons = Box::new(self.extract_stmt("consequent")?);
                let alt = self.extract_stmt_opt("alternate")?.map(Box::new);
                Stmt::If(None, test, cons, alt)
            }
            Tag::DoWhileStatement => {
                let body = Box::new(self.extract_stmt("body")?);
                let test = self.extract_expr("test")?;
                Stmt::DoWhile(None, body, test, Semi::Explicit(None))
            }
            Tag::WhileStatement => {
                let test = self.extract_expr("test")?;
                let body = Box::new(self.extract_stmt("body")?);
                Stmt::While(None, test, body)
            }
            Tag::ForStatement => {
                let init = match self.extract_object_opt("init").map_err(Error::Json)? {
                    None      => None,
                    Some(obj) => Some(Box::new(obj.into_for_head()?))
                };
                let test = self.extract_expr_opt("test")?;
                let update = self.extract_expr_opt("update")?;
                let body = Box::new(self.extract_stmt("body")?);
                Stmt::For(None, init, test, update, body)
            }
            Tag::ForInStatement => {
                let left = self.extract_object("left").map_err(Error::Json)?.into_for_in_head()?;
                let right = self.extract_expr("right")?;
                let body = self.extract_stmt("body")?;
                Stmt::ForIn(None, Box::new(left), right, Box::new(body))
            }
            Tag::ForOfStatement => {
                let left = self.extract_object("left").map_err(Error::Json)?.into_for_of_head()?;
                let right = self.extract_expr("right")?;
                let body = self.extract_stmt("body")?;
                Stmt::ForOf(None, Box::new(left), right, Box::new(body))
            }
            Tag::BlockStatement => {
                let body = self.extract_stmt_list("body")?;
                Stmt::Block(None, body)
            }
            Tag::ReturnStatement => {
                let arg = self.extract_expr_opt("argument")?;
                Stmt::Return(None, arg, Semi::Explicit(None))
            }
            Tag::LabeledStatement => {
                let label = self.extract_id("label")?;
                let body = Box::new(self.extract_stmt("body")?);
                Stmt::Label(None, label, body)
            }
            Tag::BreakStatement => {
                let label = self.extract_id_opt("label")?;
                Stmt::Break(None, label, Semi::Explicit(None))
            }
            Tag::ContinueStatement => {
                let label = self.extract_id_opt("label")?;
                Stmt::Cont(None, label, Semi::Explicit(None))
            }
            Tag::SwitchStatement => {
                let disc = self.extract_expr("discriminant")?;
                let cases = self.extract_case_list("cases")?;
                Stmt::Switch(None, disc, cases)
            }
            Tag::WithStatement => {
                let obj = self.extract_expr("object")?;
                let body = Box::new(self.extract_stmt("body")?);
                Stmt::With(None, obj, body)
            }
            Tag::ThrowStatement => {
                let arg = self.extract_expr("argument")?;
                Stmt::Throw(None, arg, Semi::Explicit(None))
            }
            Tag::DebuggerStatement => {
                Stmt::Debugger(None, Semi::Explicit(None))
            }
            Tag::TryStatement => {
                let mut block = self.extract_object("block").map_err(Error::Json)?;
                let body = block.extract_stmt_list("body")?;
                let catch = self.extract_catch_opt("handler")?.map(Box::new);
                let finally = match self.extract_object_opt("finalizer").map_err(Error::Json)? {
                    Some(mut finalizer) => Some(finalizer.extract_stmt_list("body")?),
                    None                => None
                };
                Stmt::Try(None, body, catch, finally)
            }
            _ => { return node_type_error("statement", tag); }
        })
    }

    fn into_stmt_list_item(self) -> Result<StmtListItem> {
        Ok(if self.tag()? == Tag::FunctionDeclaration {
            StmtListItem::Decl(Decl::Fun(self.into_fun()?))
        } else {
            StmtListItem::Stmt(self.into_stmt()?)
        })
    }

    fn into_case(mut self) -> Result<Case> {
        let test = self.extract_expr_opt("test")?;
        let body = self.extract_stmt_list("consequent")?;
        Ok(Case { location: None, test: test, body: body })
    }

    fn into_catch(mut self) -> Result<Catch> {
        let param = self.extract_patt("param")?;
        let mut body = self.extract_object("body").map_err(Error::Json)?;
        let body = body.extract_stmt_list("body")?;
        Ok(Catch { location: None, param: param, body: body })
    }
}
