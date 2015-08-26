use std::fmt;
use std::fmt::{Display, Formatter};
use ty::*;
use result::Result;

#[derive(Debug, PartialEq)]
pub enum Error {
    TypeMismatch(&'static str, Ty),
    MissingField(&'static str),
    // FIXME: need two different errors for out of bounds vs wrong length
    InvalidArrayLength(usize, usize),
    IllegalString(&'static str, String)
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Error::TypeMismatch(ref expected, ref ty) => {
                fmt.write_fmt(format_args!("expected {}, got {}", expected, ty))
            }
            &Error::MissingField(ref name) => {
                fmt.write_fmt(format_args!("missing object field '{}'", name))
            }
            &Error::InvalidArrayLength(ref minimum, ref actual) => {
                fmt.write_fmt(format_args!("expected array of length >= {}, got array of length {}", minimum, actual))
            }
            &Error::IllegalString(ref expected, ref actual) => {
                fmt.write_fmt(format_args!("expected {}, got {:?}", expected, actual))
            }
        }
    }
}

pub fn type_error<T>(expected: &'static str, actual: Ty) -> Result<T> {
    Err(Error::TypeMismatch(expected, actual))
}

pub fn field_error<T>(name: &'static str) -> Result<T> {
    Err(Error::MissingField(name))
}

pub fn array_error<T>(minimum: usize, actual: usize) -> Result<T> {
    Err(Error::InvalidArrayLength(minimum, actual))
}

pub fn string_error<T>(expected: &'static str, actual: String) -> Result<T> {
    Err(Error::IllegalString(expected, actual))
}
