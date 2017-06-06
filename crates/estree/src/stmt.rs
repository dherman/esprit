use easter::stmt::{Stmt, Block, ForHead, ForInHead, ForOfHead, StmtListItem, Case, Catch};
use easter::decl::Decl;
use easter::punc::Semi;
use easter::patt::Patt;
use easter::cover::IntoAssignPatt;
use serde::ser::*;
use unjson::ty::Object;
use unjson::{Unjson, ExtractField};

use tag::{Tag, TagOf};
use decl::IntoConst;
use expr::IntoExpr;
use fun::IntoFun;
use error::{Error, string_error, array_error, node_type_error};
use result::Result;
use node::ExtractNode;
use util::*;

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


impl<'a> Serialize for Serialization<'a, Stmt> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::stmt::Stmt::*;
        match *self.data() {
            Expr(_, ref expr, _) => {
                tag(json!({
                    "type": "ExpressionStatement",
                    "expression": Serialization::new(expr)
                })).serialize(serializer)
            },
            Block(ref block) =>
                Serialization::new(&block.items)
                    .serialize(serializer),
            Empty(_) =>
                tag(json!({
                    "type": "EmptyStatement"
                })).serialize(serializer),
            Debugger(_, _) =>
                tag(json!({
                    "type": "DebuggerStatement"
                })).serialize(serializer),
            With(_, ref expr, ref stmt) =>
                tag(json!({
                    "type": "WithStatement",
                    "object": Serialization::new(expr),
                    "body": Serialization::new(stmt),
                })).serialize(serializer),
            Return(_, ref expr, _) =>
                tag(json!({
                    "type": "ReturnStatement",
                    "argument": Serialization::new(expr),
                })).serialize(serializer),
            Label(_, ref id, ref stmt) =>
                tag(json!({
                    "type": "LabeledStatement",
                    "label": Serialization::new(id),
                    "body": Serialization::new(stmt)
                })).serialize(serializer),
            Break(_, ref id, _) =>
                tag(json!({
                    "type": "BreakStatement",
                    "label": Serialization::new(id),
                })).serialize(serializer),
            Cont(_, ref id, _) =>
                tag(json!({
                    "type": "ContinueStatement",
                    "label": Serialization::new(id),
                })).serialize(serializer),
            If(_, ref expr, ref stmt, ref else_branch) =>
                tag(json!({
                    "type": "IfStatement",
                    "test": Serialization::new(expr),
                    "consequent": Serialization::new(stmt),
                    "alternate": Serialization::new(else_branch)
                })).serialize(serializer),
            Switch(_, ref expr, ref cases) =>
                tag(json!({
                    "type": "SwitchStatement",
                    "discriminant": Serialization::new(expr),
                    "cases": Serialization::new(cases)
                })).serialize(serializer),
            Throw(_, ref expr, _) =>
                tag(json!({
                    "type": "ThrowStatement",
                    "argument": Serialization::new(expr),
                })).serialize(serializer),
            Try(_, ref block, ref catch, ref finalizer) =>
                tag(json!({
                    "type": "TryStatement",
                    "block": Serialization::new(block),
                    "handler": Serialization::new(catch),
                    "finalizer": Serialization::new(finalizer)
                })).serialize(serializer),
            While(_, ref test, ref body) =>
                tag(json!({
                    "type": "WhileStatement",
                    "test": Serialization::new(test),
                    "body": Serialization::new(body)
                })).serialize(serializer),
            DoWhile(_, ref body, ref test, _) =>
                tag(json!({
                    "type": "DoWhileStatement",
                    "test": Serialization::new(test),
                    "body": Serialization::new(body)
                })).serialize(serializer),
            For(_, ref init, ref test, ref update, ref body) =>
                tag(json!({
                    "type": "ForStatement",
                    "init": Serialization::new(init),
                    "test": Serialization::new(test),
                    "update": Serialization::new(update),
                    "body": Serialization::new(body)
                })).serialize(serializer),
            ForIn(_, ref head, ref expr, ref body) =>
                tag(json!({
                    "type": "ForInStatement",
                    "left": Serialization::new(head),
                    "right": Serialization::new(expr),
                    "body": Serialization::new(body)
                })).serialize(serializer),
            ForOf(_, ref head, ref expr, ref body) => // Non-standard, still accepted by Esprima
                tag(json!({
                    "type": "ForOfStatement",
                    "left": Serialization::new(head),
                    "right": Serialization::new(expr),
                    "body": Serialization::new(body)
                })).serialize(serializer),
            Var(_, ref dtors, _) =>
                tag(json!({
                    "type": "VariableDeclaration",
                    "declarations": Serialization::new(dtors),
                    "kind": "var" // Other declarations are defined as part of Decl
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, Case> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        tag(json!({
            "type": "SwitchCase",
            "test": Serialization::new(&self.data().test),
            "consequent": Serialization::new(&self.data().body)
        })).serialize(serializer)
    }
}



impl<'a> Serialize for Serialization<'a, StmtListItem> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::stmt::StmtListItem::*;
        match *self.data() {
            Decl(ref d) => Serialization::new(d).serialize(serializer),
            Stmt(ref s) => Serialization::new(s).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, Block> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        tag(json!({
            "type": "BlockStatement",
            "body": Serialization::new(&self.data().items)
        })).serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, ForHead> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::stmt::ForHead::*;
        match *self.data() {
            Var(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "var"
                })).serialize(serializer),
            Let(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "let"
                })).serialize(serializer),
            Const(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "const"
                })).serialize(serializer),
            Expr(_, ref expr) =>
                Serialization::new(expr).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, ForOfHead> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::stmt::ForOfHead::*;
        match *self.data() {
            Var(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "var"
                })).serialize(serializer),
            Let(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "let"
                })).serialize(serializer),
            Const(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "const"
                })).serialize(serializer),
            Patt(ref patt) =>
                Serialization::new(patt).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, ForInHead> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::stmt::ForInHead::*;
        match *self.data() {
            Var(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "var"
                })).serialize(serializer),
            VarInit(_, ref id, ref expr) =>
                tag(json!({
                    "type": "VariableDeclaration",
                    "declarations": [{
                        "type": "VariableDeclarator",
                        "id": Serialization::new(id),
                        "init": Serialization::new(expr),
                        "loc": null,
                    }]
                })).serialize(serializer),
            Let(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "let"
                })).serialize(serializer),
            Const(_, ref dtors) =>
                tag(json!({
                    "type": "VariableDeclaration", // FIXME: We should be able to factor this out
                    "declarations": Serialization::new(dtors),
                    "kind": "const"
                })).serialize(serializer),
            Patt(ref patt) =>
                Serialization::new(patt).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, Catch> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        json!({
            "type": "CatchClause",
            "param": Serialization::new(&self.data().param),
            "body": Serialization::new(&self.data().body)
        }).serialize(serializer)
    }
}
