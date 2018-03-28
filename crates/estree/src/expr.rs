use serde_json::value::Value;
use easter::expr::{Expr, ExprListItem};
use easter::obj::DotKey;
use easter::id::IdExt;
use easter::punc::{Unop, Binop, Assop, Logop};
use unjson::ty::{Object, TyOf};
use unjson::ExtractField;
use joker::token::RegExpLiteral;

use tag::{Tag, TagOf};
use id::IntoId;
use result::Result;
use error::{string_error, node_type_error, type_error};
use node::ExtractNode;
use fun::IntoFun;
use lit::{IntoStringLiteral, IntoNumberLiteral};

pub trait IntoExpr {
    fn into_expr(self) -> Result<Expr>;
    fn into_expr_list_item(self) -> Result<ExprListItem>;
    fn into_lit(self) -> Result<Expr>;
}

impl IntoExpr for Object {
    fn into_expr(mut self) -> Result<Expr> {
        let tag = self.tag()?;
        Ok(match tag {
            Tag::Identifier => { return Ok(self.into_id()?.into_expr()); }
            Tag::Literal => IntoExpr::into_lit(self)?,
            Tag::BinaryExpression => {
                let str = self.extract_string("operator")?;
                let op: Binop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("binary operator", str); }
                };
                let left = self.extract_expr("left")?;
                let right = self.extract_expr("right")?;
                Expr::Binop(None, op, Box::new(left), Box::new(right))
            }
            Tag::AssignmentExpression => {
                let str = self.extract_string("operator")?;
                let right = Box::new(self.extract_expr("right")?);
                match &str[..] {
                    "=" => Expr::Assign(None, self.extract_assign_patt("left")?, right),
                    _ => {
                        let op: Assop = match str.parse() {
                            Ok(op) => op,
                            Err(_) => { return string_error("assignment operator", str); }
                        };
                        Expr::BinAssign(None, op, self.extract_assign_target("left")?, right)
                    }
                }
            }
            Tag::LogicalExpression => {
                let str = self.extract_string("operator")?;
                let op: Logop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("logical operator", str); }
                };
                let left = self.extract_expr("left")?;
                let right = self.extract_expr("right")?;
                Expr::Logop(None, op, Box::new(left), Box::new(right))
            }
            Tag::UnaryExpression => {
                let str = self.extract_string("operator")?;
                let op: Unop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("unary operator", str); }
                };
                let arg = self.extract_expr("argument")?;
                Expr::Unop(None, op, Box::new(arg))
            }
            Tag::UpdateExpression => {
                let op = self.extract_string("operator")?;
                let arg = Box::new(self.extract_assign_target("argument")?);
                let prefix = self.extract_bool("prefix")?;
                match (&op[..], prefix) {
                    ("++", true)  => Expr::PreInc(None, arg),
                    ("++", false) => Expr::PostInc(None, arg),
                    ("--", true)  => Expr::PreDec(None, arg),
                    ("--", false) => Expr::PostDec(None, arg),
                    _ => { return string_error("'++' or '--'", op); }
                }
            }
            Tag::MemberExpression => {
                let obj = Box::new(self.extract_expr("object")?);
                if self.extract_bool("computed")? {
                    let prop = Box::new(self.extract_expr("property")?);
                    Expr::Brack(None, obj, prop)
                } else {
                    let id = self.extract_object("property")?.into_id()?;
                    let key = DotKey { location: None, value: id.name.into_string() };
                    Expr::Dot(None, obj, key)
                }
            }
            Tag::CallExpression => {
                let callee = Box::new(self.extract_expr("callee")?);
                let args = self.extract_expr_list("arguments")?;
                Expr::Call(None, callee, args)
            }
            Tag::NewExpression => {
                let callee = Box::new(self.extract_expr("callee")?);
                let args = self.extract_expr_list("arguments")?;
                Expr::New(None, callee, Some(args))
            }
            Tag::ArrayExpression => {
                let elts = self.extract_expr_opt_list("elements")?;
                Expr::Arr(None, elts)
            }
            Tag::FunctionExpression => {
                let id = self.extract_id_opt("id")?;
                let fun = self.into_fun(id)?;
                Expr::Fun(fun)
            }
            Tag::SequenceExpression => {
                let exprs = self.extract_exprs("expressions")?;
                Expr::Seq(None, exprs)
            }
            Tag::ObjectExpression => {
                let props = self.extract_prop_list("properties")?;
                Expr::Obj(None, props)
            }
            Tag::ConditionalExpression => {
                let test = Box::new(self.extract_expr("test")?);
                let cons = Box::new(self.extract_expr("consequent")?);
                let alt = Box::new(self.extract_expr("alternate")?);
                Expr::Cond(None, test, cons, alt)
            }
            Tag::ThisExpression => Expr::This(None),
            Tag::MetaProperty => {
                let meta = self.extract_id("meta")?.name;
                let prop = self.extract_id("property")?.name;
                match (meta.as_ref(), prop.as_ref()) {
                    ("new", "target") => Expr::NewTarget(None),
                    (meta, prop) => { return string_error("new.target", format!("{}.{}", meta, prop)); }
                }
            }
            _ => { return node_type_error("expression", tag); }
        })
    }

    fn into_expr_list_item(mut self) -> Result<ExprListItem> {
        match self.tag()? {
            Tag::SpreadElement => {
                Ok(ExprListItem::Spread(None, self.extract_expr("argument")?))
            }
            _ => self.into_expr().map(ExprListItem::Expr)
        }
    }

    fn into_lit(mut self) -> Result<Expr> {
        let json = self.extract_field("value")?;
        Ok(match json {
            Value::Null if !self.contains_key("regex") => Expr::Null(None),
            Value::Bool(true) => Expr::True(None),
            Value::Bool(false) => Expr::False(None),
            Value::String(value) => Expr::String(None, value.into_string_literal()),
            Value::I64(val) => Expr::Number(None, val.into_number_literal()),
            Value::U64(val) => Expr::Number(None, val.into_number_literal()),
            Value::F64(val) => Expr::Number(None, val.into_number_literal()),
            Value::Null | Value::Object(_) => {
                let mut regex = self.extract_object("regex")?;
                let pattern = regex.extract_string("pattern")?;
                let flags = regex.extract_string("flags")?;
                Expr::RegExp(None, RegExpLiteral {
                    pattern: pattern,
                    flags: flags.chars().collect()
                })
            }
            _ => { return type_error("null, number, boolean, string, or object", json.ty()); }
        })
    }
}
