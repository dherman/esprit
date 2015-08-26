use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedEOF,
    // FIXME: split this up into specific situational errors
    UnexpectedChar(char),
    InvalidDigit(char),
    IllegalUnicode(u32)
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Error::UnexpectedEOF => {
                fmt.write_str("unexpected end of input")
            }
            &Error::UnexpectedChar(ref ch) => {
                fmt.write_fmt(format_args!("unexpected character: {:?}", *ch))
            }
            &Error::InvalidDigit(ref ch) => {
                fmt.write_fmt(format_args!("invalid digit: {:?}", *ch))
            }
            &Error::IllegalUnicode(ref u) => {
                fmt.write_fmt(format_args!("illegal code unit: \\u{{{:04x}}}", u))
            }
        }
    }
}
