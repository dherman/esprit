use std::fmt;
use std::fmt::{Display, Formatter};

use easter::id::Id;
use easter::expr::Expr;
use easter::patt::{CompoundPatt, CompoundPattData};
use unjson;
use unjson::ty::Ty;
use result::Result;
use tag::Tag;

pub enum Error {
    Json(unjson::error::Error),
    InvalidTypeTag(String),
    NodeTypeMismatch(&'static str, Tag),
    UnexpectedInitializer(Expr),
    InvalidLHS(&'static str),
    UninitializedPattern(CompoundPatt<Id>)
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Error::Json(ref err) => {
                fmt.write_fmt(format_args!("{}", err))
            }
            &Error::InvalidTypeTag(ref actual) => {
                fmt.write_fmt(format_args!("expected node type tag, got {}", actual))
            }
            &Error::NodeTypeMismatch(ref expected, ref actual) => {
                fmt.write_fmt(format_args!("expected {} node, got {}", expected, actual))
            }
            &Error::UnexpectedInitializer(_) => {
                fmt.write_fmt(format_args!("unexpected initializer in for-in loop"))
            }
            &Error::InvalidLHS(_) => {
                fmt.write_fmt(format_args!("invalid left-hand side of assignment"))
            }
            &Error::UninitializedPattern(ref patt) => {
                let ty = match patt.value {
                    CompoundPattData::Arr(_) => "array",
                    CompoundPattData::Obj(_) => "object"
                };
                fmt.write_fmt(format_args!("uninitialized {} pattern in declarator", ty))
            }
        }
    }
}

pub fn type_error<T>(expected: &'static str, actual: Ty) -> Result<T> {
    unjson::error::type_error(expected, actual).map_err(Error::Json)
}

pub fn field_error<T>(name: &'static str) -> Result<T> {
    unjson::error::field_error(name).map_err(Error::Json)
}

pub fn array_error<T>(expected: usize, actual: usize) -> Result<T> {
    unjson::error::array_error(expected, actual).map_err(Error::Json)
}

pub fn index_error<T>(len: usize, index: usize) -> Result<T> {
    unjson::error::index_error(len, index).map_err(Error::Json)
}

pub fn string_error<T>(expected: &'static str, actual: String) -> Result<T> {
    unjson::error::string_error(expected, actual).map_err(Error::Json)
}

pub fn tag_error<T>(actual: String) -> Result<T> {
    Err(Error::InvalidTypeTag(actual))
}

pub fn node_type_error<T>(expected: &'static str, actual: Tag) -> Result<T> {
    Err(Error::NodeTypeMismatch(expected, actual))
}
