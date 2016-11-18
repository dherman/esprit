use std::fmt;
use std::fmt::{Display, Formatter};
use std::str::FromStr;
use unjson::ty::Object;
use unjson::*;

use result::Result;
use error::Error;

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Tag {
    Identifier,
    Literal,

    BinaryExpression,
    AssignmentExpression,
    LogicalExpression,
    UnaryExpression,
    UpdateExpression,
    MemberExpression,
    CallExpression,
    NewExpression,
    ArrayExpression,
    FunctionExpression,
    SequenceExpression,
    ObjectExpression,
    ConditionalExpression,
    ThisExpression,

    FunctionDeclaration,
    VariableDeclaration,

    EmptyStatement,
    ExpressionStatement,
    IfStatement,
    DoWhileStatement,
    WhileStatement,
    ForStatement,
    ForInStatement,
    ForOfStatement,
    BlockStatement,
    ReturnStatement,
    LabeledStatement,
    BreakStatement,
    ContinueStatement,
    SwitchStatement,
    WithStatement,
    ThrowStatement,
    DebuggerStatement,
    TryStatement
}

impl Display for Tag {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_fmt(format_args!("{:?}", self))
    }
}

impl FromStr for Tag {
    type Err = Error;

    fn from_str(s: &str) -> Result<Tag> {
        Ok(match s {
            "Identifier"            => Tag::Identifier,
            "Literal"               => Tag::Literal,
            "BinaryExpression"      => Tag::BinaryExpression,
            "AssignmentExpression"  => Tag::AssignmentExpression,
            "LogicalExpression"     => Tag::LogicalExpression,
            "UnaryExpression"       => Tag::UnaryExpression,
            "UpdateExpression"      => Tag::UpdateExpression,
            "MemberExpression"      => Tag::MemberExpression,
            "CallExpression"        => Tag::CallExpression,
            "NewExpression"         => Tag::NewExpression,
            "ArrayExpression"       => Tag::ArrayExpression,
            "FunctionExpression"    => Tag::FunctionExpression,
            "SequenceExpression"    => Tag::SequenceExpression,
            "ObjectExpression"      => Tag::ObjectExpression,
            "ConditionalExpression" => Tag::ConditionalExpression,
            "ThisExpression"        => Tag::ThisExpression,
            "FunctionDeclaration"   => Tag::FunctionDeclaration,
            "VariableDeclaration"   => Tag::VariableDeclaration,
            "EmptyStatement"        => Tag::EmptyStatement,
            "ExpressionStatement"   => Tag::ExpressionStatement,
            "IfStatement"           => Tag::IfStatement,
            "DoWhileStatement"      => Tag::DoWhileStatement,
            "WhileStatement"        => Tag::WhileStatement,
            "ForStatement"          => Tag::ForStatement,
            "ForInStatement"        => Tag::ForInStatement,
            "ForOfStatement"        => Tag::ForOfStatement,
            "BlockStatement"        => Tag::BlockStatement,
            "ReturnStatement"       => Tag::ReturnStatement,
            "LabeledStatement"      => Tag::LabeledStatement,
            "BreakStatement"        => Tag::BreakStatement,
            "ContinueStatement"     => Tag::ContinueStatement,
            "SwitchStatement"       => Tag::SwitchStatement,
            "WithStatement"         => Tag::WithStatement,
            "ThrowStatement"        => Tag::ThrowStatement,
            "DebuggerStatement"     => Tag::DebuggerStatement,
            "TryStatement"          => Tag::TryStatement,
            _ => { return Err(Error::InvalidTypeTag(String::from(s))); }
        })
    }
}

pub trait TagOf {
    fn tag(&self) -> Result<Tag>;
}

impl TagOf for Object {
    fn tag(&self) -> Result<Tag> {
        let str = self.get_string("type").map_err(Error::Json)?;
        str.parse()
           .map_err(|_| Error::InvalidTypeTag(String::from(str)))
    }
}
