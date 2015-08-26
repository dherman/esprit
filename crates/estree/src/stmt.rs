use easter::stmt::{StmtData, Stmt, ForHeadData, ForHead, ForInHeadData, ForInHead, ForOfHeadData, ForOfHead, StmtListItem, Case, CaseData, Catch, CatchData};
use easter::decl::DeclData;
use easter::punc::Semi;
use easter::patt::Patt;
use unjson::ty::Object;
use unjson::{Unjson, ExtractField};
use joker::track::*;

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
        Ok(match try!(self.tag()) {
            Tag::VariableDeclaration => {
                let dtors = try!(self.extract_dtor_list("declarations"));
                let kind = try!(self.extract_string("kind").map_err(Error::Json));
                match &kind[..] {
                    "var" => ForHeadData::Var(dtors),
                    "let" => ForHeadData::Let(dtors),
                    _ => { return string_error("var or let", kind); }
                }
            }
            _ => ForHeadData::Expr(try!(self.into_expr()))
        }.tracked(None))
    }
}

trait IntoForInHead {
    fn into_for_in_head(self) -> Result<ForInHead>;
}

impl IntoForInHead for Object {
    fn into_for_in_head(mut self) -> Result<ForInHead> {
        Ok(match try!(self.tag()) {
            Tag::VariableDeclaration => {
                let mut dtors = try!(self.extract_array("declarations").map_err(Error::Json));
                let kind = try!(self.extract_string("kind").map_err(Error::Json));
                if dtors.len() != 1 {
                    return array_error(1, dtors.len());
                }
                let mut obj = try!(dtors.remove(0).into_object().map_err(Error::Json));
                let lhs = try!(obj.extract_patt("id"));
                let init = try!(obj.extract_expr_opt("init"));
                match &kind[..] {
                    "var" => match lhs {
                        Patt::Simple(id) => {
                            match init {
                                None       => ForInHeadData::Var(Patt::Simple(id)),
                                Some(expr) => ForInHeadData::VarInit(id, expr)
                            }
                        }
                        Patt::Compound(patt) => {
                            match init {
                                None       => ForInHeadData::Var(Patt::Compound(patt)),
                                Some(expr) => { return Err(Error::UnexpectedInitializer(expr)); }
                            }
                        }
                    },
                    "let" => {
                        match init {
                            None       => ForInHeadData::Let(lhs),
                            Some(expr) => { return Err(Error::UnexpectedInitializer(expr)); }
                        }
                    }
                    _ => { return string_error("var or let", kind); }
                }
            }
            _ => ForInHeadData::Expr(try!(self.into_expr()))
        }.tracked(None))
    }
}

trait IntoForOfHead {
    fn into_for_of_head(self) -> Result<ForOfHead>;
}

impl IntoForOfHead for Object {
    fn into_for_of_head(mut self) -> Result<ForOfHead> {
        Ok(match try!(self.tag()) {
            Tag::VariableDeclaration => {
                let mut dtors = try!(self.extract_array("declarations").map_err(Error::Json));
                let kind = try!(self.extract_string("kind").map_err(Error::Json));
                if dtors.len() != 1 {
                    return array_error(1, dtors.len());
                }
                let mut obj = try!(dtors.remove(0).into_object().map_err(Error::Json));
                let lhs = try!(obj.extract_patt("id"));
                match &kind[..] {
                    "var" => ForOfHeadData::Var(lhs),
                    "let" => ForOfHeadData::Let(lhs),
                    _ => { return string_error("var or let", kind); }
                }
            },
            _ => ForOfHeadData::Expr(try!(self.into_expr()))
        }.tracked(None))
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
        let tag = try!(self.tag());
        Ok(match tag {
            Tag::VariableDeclaration => {
                let dtors = try!(self.extract_dtor_list("declarations"));
                StmtData::Var(dtors, Semi::Explicit(None))
            }
            Tag::EmptyStatement => StmtData::Empty,
            Tag::ExpressionStatement => {
                let expr = try!(self.extract_expr("expression"));
                StmtData::Expr(expr, Semi::Explicit(None))
            }
            Tag::IfStatement => {
                let test = try!(self.extract_expr("test"));
                let cons = Box::new(try!(self.extract_stmt("consequent")));
                let alt = try!(self.extract_stmt_opt("alternate")).map(Box::new);
                StmtData::If(test, cons, alt)
            }
            Tag::DoWhileStatement => {
                let body = Box::new(try!(self.extract_stmt("body")));
                let test = try!(self.extract_expr("test"));
                StmtData::DoWhile(body, test, Semi::Explicit(None))
            }
            Tag::WhileStatement => {
                let test = try!(self.extract_expr("test"));
                let body = Box::new(try!(self.extract_stmt("body")));
                StmtData::While(test, body)
            }
            Tag::ForStatement => {
                let init = match try!(self.extract_object_opt("init").map_err(Error::Json)) {
                    None      => None,
                    Some(obj) => Some(Box::new(try!(obj.into_for_head())))
                };
                let test = try!(self.extract_expr_opt("test"));
                let update = try!(self.extract_expr_opt("update"));
                let body = Box::new(try!(self.extract_stmt("body")));
                StmtData::For(init, test, update, body)
            }
            Tag::ForInStatement => {
                let left = try!(try!(self.extract_object("left").map_err(Error::Json)).into_for_in_head());
                let right = try!(self.extract_expr("right"));
                let body = try!(self.extract_stmt("body"));
                StmtData::ForIn(Box::new(left), right, Box::new(body))
            }
            Tag::ForOfStatement => {
                let left = try!(try!(self.extract_object("left").map_err(Error::Json)).into_for_of_head());
                let right = try!(self.extract_expr("right"));
                let body = try!(self.extract_stmt("body"));
                StmtData::ForOf(Box::new(left), right, Box::new(body))
            }
            Tag::BlockStatement => {
                let body = try!(self.extract_stmt_list("body"));
                StmtData::Block(body)
            }
            Tag::ReturnStatement => {
                let arg = try!(self.extract_expr_opt("argument"));
                StmtData::Return(arg, Semi::Explicit(None))
            }
            Tag::LabeledStatement => {
                let label = try!(self.extract_id("label"));
                let body = Box::new(try!(self.extract_stmt("body")));
                StmtData::Label(label, body)
            }
            Tag::BreakStatement => {
                let label = try!(self.extract_id_opt("label"));
                StmtData::Break(label, Semi::Explicit(None))
            }
            Tag::ContinueStatement => {
                let label = try!(self.extract_id_opt("label"));
                StmtData::Cont(label, Semi::Explicit(None))
            }
            Tag::SwitchStatement => {
                let disc = try!(self.extract_expr("discriminant"));
                let cases = try!(self.extract_case_list("cases"));
                StmtData::Switch(disc, cases)
            }
            Tag::WithStatement => {
                let obj = try!(self.extract_expr("object"));
                let body = Box::new(try!(self.extract_stmt("body")));
                StmtData::With(obj, body)
            }
            Tag::ThrowStatement => {
                let arg = try!(self.extract_expr("argument"));
                StmtData::Throw(arg, Semi::Explicit(None))
            }
            Tag::DebuggerStatement => {
                StmtData::Debugger(Semi::Explicit(None))
            }
            Tag::TryStatement => {
                let mut block = try!(self.extract_object("block").map_err(Error::Json));
                let body = try!(block.extract_stmt_list("body"));
                let catch = try!(self.extract_catch_opt("handler")).map(Box::new);
                let finally = match try!(self.extract_object_opt("finalizer").map_err(Error::Json)) {
                    Some(mut finalizer) => Some(try!(finalizer.extract_stmt_list("body"))),
                    None                => None
                };
                StmtData::Try(body, catch, finally)
            }
            _ => { return node_type_error("statement", tag); }
        }.tracked(None))
    }

    fn into_stmt_list_item(self) -> Result<StmtListItem> {
        Ok(if try!(self.tag()) == Tag::FunctionDeclaration {
            StmtListItem::Decl(DeclData::Fun(try!(self.into_fun())).tracked(None))
        } else {
            StmtListItem::Stmt(try!(self.into_stmt()))
        })
    }

    fn into_case(mut self) -> Result<Case> {
        let test = try!(self.extract_expr_opt("test"));
        let body = try!(self.extract_stmt_list("consequent"));
        Ok(CaseData { test: test, body: body }.tracked(None))
    }

    fn into_catch(mut self) -> Result<Catch> {
        let param = try!(self.extract_patt("param"));
        let mut body = try!(self.extract_object("body").map_err(Error::Json));
        let body = try!(body.extract_stmt_list("body"));
        Ok(CatchData { param: param, body: body }.tracked(None))
    }
}
