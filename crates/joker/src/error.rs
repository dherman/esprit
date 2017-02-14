use std::error::Error as StdError;
use std::fmt;
use std::fmt::{Display, Formatter};
use word::Reserved;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    IncompleteWordEscape(Option<char>),
    UnterminatedComment,
    UnterminatedRegExp(Option<char>),
    MissingExponent(Option<char>),
    UnterminatedString(Option<char>),
    MissingBinaryDigits,
    MissingOctalDigits,
    MissingHexDigits,
    IllegalChar(char),
    InvalidDigit(char),
    IllegalUnicode(u32),
    IdAfterNumber(char),
    DigitAfterNumber(char),
    ReservedWordWithEscapes(Reserved)
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match *self {
            Error::IllegalChar(ref ch)  |
            Error::InvalidDigit(ref ch) => fmt.write_fmt(format_args!("{}: {:?}", self.description(), *ch)),
            Error::ReservedWordWithEscapes(ref word) => fmt.write_fmt(format_args!("{}: {:?}", self.description(), word)),
            Error::IllegalUnicode(ref u) => fmt.write_fmt(format_args!("{}: \\u{{{:04x}}}", self.description(), u)),
            _ => fmt.write_str(self.description()),
        }
    }
}

impl StdError for Error {
    fn description(&self) -> &str {
        match *self {
            Error::IncompleteWordEscape(_) => "incomplete word escape",
            Error::UnterminatedComment => "unterminated block comment",
            Error::UnterminatedRegExp(_) => "unterminated regexp literal",
            Error::MissingExponent(_) => "missing exponent",
            Error::UnterminatedString(_) => "unterminated string",
            Error::MissingBinaryDigits => "missing binary digits",
            Error::MissingOctalDigits => "missing octal digits",
            Error::MissingHexDigits => "missing hex digits",
            Error::IllegalChar(_) => "illegal character",
            Error::InvalidDigit(_) => "invalid digit",
            Error::IllegalUnicode(_) => "illegal code unit",
            Error::IdAfterNumber(_) => "identifier starts immediately after numeric literal",
            Error::DigitAfterNumber(_) => "numeric literal starts immediately after previous numeric literal",
            Error::ReservedWordWithEscapes(_) => "reserved word with escapes",
        }
    }

    fn cause(&self) -> Option<&StdError> {
        None
    }
}
