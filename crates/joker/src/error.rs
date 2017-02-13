use std::error;
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

impl Error {
    fn as_str(&self) -> &'static str {
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
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match *self {
            Error::IllegalChar(ref ch)  |
            Error::InvalidDigit(ref ch) => fmt.write_str(&format!("{}: {:?}", self.as_str(), *ch)),
            Error::ReservedWordWithEscapes(ref word) => fmt.write_str(&format!("{}: {:?}", self.as_str(), word)),
            Error::IllegalUnicode(ref u) => fmt.write_str(&format!("{}: \\u{{{:04x}}}", self.as_str(), u)),
            _ => fmt.write_str(self.as_str()),
        }
    }
}

impl error::Error for Error {
    fn description(&self) -> &str {
        self.as_str()
    }

    fn cause(&self) -> Option<&error::Error> {
        None
    }
}
