use serde_json::value::Value;
use serde::ser::*;
use easter::expr::*;
use easter::obj::{ DotKey, Prop };
use easter::id::IdExt;
use easter::punc::*;
use unjson::ty::{Object, TyOf};
use unjson::ExtractField;
use joker::token::RegExpLiteral;

use tag::{Tag, TagOf};
use id::IntoId;
use result::Result;
use error::{Error, string_error, node_type_error, type_error};
use node::ExtractNode;
use fun::IntoFun;
use lit::{IntoStringLiteral, IntoNumberLiteral};
use util::*;

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
                let str = self.extract_string("operator").map_err(Error::Json)?;
                let op: Binop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("binary operator", str); }
                };
                let left = self.extract_expr("left")?;
                let right = self.extract_expr("right")?;
                Expr::Binop(None, op, Box::new(left), Box::new(right))
            }
            Tag::AssignmentExpression => {
                let str = self.extract_string("operator").map_err(Error::Json)?;
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
                let str = self.extract_string("operator").map_err(Error::Json)?;
                let op: Logop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("logical operator", str); }
                };
                let left = self.extract_expr("left")?;
                let right = self.extract_expr("right")?;
                Expr::Logop(None, op, Box::new(left), Box::new(right))
            }
            Tag::UnaryExpression => {
                let str = self.extract_string("operator").map_err(Error::Json)?;
                let op: Unop = match str.parse() {
                    Ok(op) => op,
                    Err(_) => { return string_error("unary operator", str); }
                };
                let arg = self.extract_expr("argument")?;
                Expr::Unop(None, op, Box::new(arg))
            }
            Tag::UpdateExpression => {
                let op = self.extract_string("operator").map_err(Error::Json)?;
                let arg = Box::new(self.extract_assign_target("argument")?);
                let prefix = self.extract_bool("prefix").map_err(Error::Json)?;
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
                if self.extract_bool("computed").map_err(Error::Json)? {
                    let prop = Box::new(self.extract_expr("property")?);
                    Expr::Brack(None, obj, prop)
                } else {
                    let id = self.extract_object("property").map_err(Error::Json)?.into_id()?;
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
        let json = self.extract_field("value").map_err(Error::Json)?;
        Ok(match json {
            Value::Null if !self.contains_key("regex") => Expr::Null(None),
            Value::Bool(true) => Expr::True(None),
            Value::Bool(false) => Expr::False(None),
            Value::String(value) => Expr::String(None, value.into_string_literal()),
            Value::Number(val) => Expr::Number(None, val.into_number_literal()),
            Value::Null | Value::Object(_) => {
                let mut regex = self.extract_object("regex").map_err(Error::Json)?;
                let pattern = regex.extract_string("pattern").map_err(Error::Json)?;
                let flags = regex.extract_string("flags").map_err(Error::Json)?;
                Expr::RegExp(None, RegExpLiteral {
                    pattern: pattern,
                    flags: flags.chars().collect()
                })
            }
            _ => { return type_error("null, number, boolean, string, or object", json.ty()); }
        })
    }
}

impl<'a> Serialize for Serialization<'a, ExprListItem> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::expr::ExprListItem::*;
        match *self.data() {
            Expr(ref expr) => Serialization::new(expr).serialize(serializer),
            Spread(_, ref expr) =>
                tag(json!({
                    "type": "SpreadElement",
                    "argument": Serialization::new(expr)
                })).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, Op<UnopTag>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::punc::UnopTag::*;
        match self.data().tag {
            Minus   => "-",
            Plus    => "+",
            Not     => "!",
            BitNot  => "~",
            Typeof  => "typeof",
            Void    => "void",
            Delete  => "delete"
        }.serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, Op<BinopTag>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::punc::BinopTag::*;
        match self.data().tag {
            Eq        => "==",
            NEq       => "!=",
            StrictEq  => "===",
            StrictNEq => "!==",
            Lt        => "<",
            LEq       => "<=",
            Gt        => ">",
            GEq       => ">=",
            LShift    => "<<",
            RShift    => ">>",
            URShift   => ">>>",
            Plus      => "+",
            Minus     => "-",
            Times     => "*",
            Div       => "%",
            Mod       => "%",
            BitOr     => "|",
            BitXor    => "^",
            BitAnd    => "&",
            In        => "in",
            Instanceof => "instanceof",
        }.serialize(serializer)
    }
}


impl<'a> Serialize for Serialization<'a, Op<AssopTag>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::punc::AssopTag::*;
        match self.data().tag {
            PlusEq     => "+=",
            MinusEq    => "-=",
            TimesEq    => "*=",
            DivEq      => "/=",
            ModEq      => "%=",
            LShiftEq   => "<<=",
            RShiftEq   => ">>=",
            URShiftEq  => ">>>=",
            BitOrEq    => "|=",
            BitXorEq   => "^=",
            BitAndEq   => "&="
        }.serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, Op<LogopTag>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::punc::LogopTag::*;
        match self.data().tag {
            Or   => "||",
            And  => "&&",
        }.serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, Prop> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::obj::Prop::*;
        use easter::obj::PropVal::*;

        match *self.data() {
            Regular(_, ref key, Init(ref init)) =>
                tag(json!({
                    "type": "Property",
                    "key": Serialization::new(key),
                    "value": Serialization::new(init),
                    "kind": "init"
                })).serialize(serializer),
            Regular(_, ref key, Get(_, ref getter)) =>
                tag(json!({
                    "type": "Property",
                    "key": Serialization::new(key),
                    "value": Serialization::new(&getter.items),
                    "kind": "get"
                })).serialize(serializer),
            Regular(_, ref key, Set(_, /*ignored id?*/_, ref setter)) => // FIXME: If I read EStree correctly, `id` is ignored here
                tag(json!({
                    "type": "Property",
                    "key": Serialization::new(key),
                    "value": Serialization::new(&setter.items),
                    "kind": "set"
                })).serialize(serializer),
            Method(ref method) =>
                tag(json!({
                    "type": "Property",
                    "key": Serialization::new(&method.id),
                    "value": Serialization::in_context(method, Container::FunctionExpression),
                    "kind": "init"
                })).serialize(serializer),
            Shorthand(ref id) =>
                tag(json!({
                    "type": "Property",
                    "key": Serialization::new(id),
                    "value": Serialization::new(id),
                    "kind": "init"
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, Expr> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::expr::Expr::*;
        match *self.data() {
            This(_) =>
                tag(json!({
                    "type": "ThisExpression",
                })).serialize(serializer),
            Arr(_, ref items) =>
                tag(json!({
                    "type": "ArrayExpression",
                    "elements": Serialization::new(items)
                })).serialize(serializer),
            Obj(_, ref props) =>
                tag(json!({
                    "type": "ObjectExpression",
                    "properties": Serialization::new(props)
                })).serialize(serializer),
            Fun(ref fun) =>
                Serialization::in_context(fun, Container::FunctionExpression)
                    .serialize(serializer),
            Unop(_, ref op, ref expr) =>
                tag(json!({
                    "type": "UnaryExpression",
                    "operator": Serialization::new(op),
                    "prefix": true,
                    "argument": Serialization::new(expr)
                })).serialize(serializer),
            PreInc(_, ref expr) =>
                tag(json!({
                    "type": "UpdateExpression",
                    "operator": "++",
                    "argument": Serialization::new(expr),
                    "prefix": true,
                })).serialize(serializer),
            PostInc(_, ref expr) =>
                tag(json!({
                    "type": "UpdateExpression",
                    "operator": "++",
                    "argument": Serialization::new(expr),
                    "prefix": false,
                })).serialize(serializer),
            PreDec(_, ref expr) =>
                tag(json!({
                    "type": "UpdateExpression",
                    "operator": "--",
                    "argument": Serialization::new(expr),
                    "prefix": true,
                })).serialize(serializer),
            PostDec(_, ref expr) =>
                tag(json!({
                    "type": "UpdateExpression",
                    "operator": "--",
                    "argument": Serialization::new(expr),
                    "prefix": false,
                })).serialize(serializer),
            Binop(_, ref op, ref left, ref right) =>
                tag(json!({
                    "type": "BinaryExpression",
                    "operator": Serialization::new(op),
                    "left": Serialization::new(left),
                    "right": Serialization::new(right),
                })).serialize(serializer),
            Assign(_, ref patt, ref expr) =>
                tag(json!({
                    "type": "AssignmentExpression",
                    "operator": "=",
                    "left": Serialization::new(patt),
                    "right": Serialization::new(expr),
                })).serialize(serializer),
            BinAssign(_, ref op, ref patt, ref expr) =>
                tag(json!({
                    "type": "AssignmentExpression",
                    "operator": Serialization::new(op),
                    "left": Serialization::new(patt),
                    "right": Serialization::new(expr),
                })).serialize(serializer),
            Logop(_, ref op, ref patt, ref expr) =>
                tag(json!({
                    "type": "LogicalExpression",
                    "operator": Serialization::new(op),
                    "left": Serialization::new(patt),
                    "right": Serialization::new(expr),
                })).serialize(serializer),
            Dot(_, ref expr, ref key) =>
                tag(json!({
                    "type": "MemberExpression",
                    "object": Serialization::new(expr),
                    "property": Serialization::new(key),
                    "computed": false,
                })).serialize(serializer),
            Brack(_, ref obj, ref key) =>
                tag(json!({
                    "type": "MemberExpression",
                    "object": Serialization::new(obj),
                    "property": Serialization::new(key),
                    "computed": true,
                })).serialize(serializer),
            Cond(_, ref test, ref consequent, ref alternate) =>
                tag(json!({
                    "type": "ConditionalExpression",
                    "test": Serialization::new(test),
                    "consequent": Serialization::new(consequent),
                    "alternate": Serialization::new(alternate),
                })).serialize(serializer),
            Call(_, ref callee, ref args) =>
                tag(json!({
                    "type": "CallExpression",
                    "callee": Serialization::new(callee),
                    "arguments": Serialization::new(args),
                })).serialize(serializer),
            New(_, ref callee, ref args) =>
                tag(json!({
                    "type": "NewExpression",
                    "callee": Serialization::new(callee),
                    "arguments": Serialization::new(args),
                })).serialize(serializer),
            Seq(_, ref exprs) =>
                tag(json!({
                    "type": "SequenceExpression",
                    "expressions": Serialization::new(exprs)
                })).serialize(serializer),
            True(_) =>
                tag(json!({
                    "type": "Literal",
                    "value": true
                })).serialize(serializer),
            False(_) =>
                tag(json!({
                    "type": "Literal",
                    "value": false
                })).serialize(serializer),
            Null(_) =>
                tag(json!({
                    "type": "Literal",
                    "value": null
                })).serialize(serializer),
            Number(_, ref lit) =>
                tag(json!({
                    "type": "Literal",
                    "value": Serialization::new(lit)
                })).serialize(serializer),
            RegExp(_, ref lit) =>
                tag(json!({
                    "type": "Literal",
                    "value": Serialization::new(lit)
                })).serialize(serializer),
            String(_, ref lit) =>
                tag(json!({
                    "type": "Literal",
                    "value": Serialization::new(lit)
                })).serialize(serializer),
            Id(ref id) =>
                tag(json!({
                    "type": "Identifier",
                    "name": Serialization::new(id)
                })).serialize(serializer),
            NewTarget(_) => {
                // FIXME: I couldn't figure out what a `NewTarget` is. At least, this
                // code should be harmless.
                serializer.serialize_str("Hi, I'm a NewTarget, I have no idea what I am.")
            }
        }
    }
}
