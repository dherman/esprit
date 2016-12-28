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
        match self {
            &Error::IncompleteWordEscape(_) => {
                fmt.write_str("incomplete word escape")
            }
            &Error::UnterminatedComment => {
                fmt.write_str("unterminated block comment")
            }
            &Error::UnterminatedRegExp(_) => {
                fmt.write_str("unterminated regexp literal")
            }
            &Error::MissingExponent(_) => {
                fmt.write_str("missing exponent")
            }
            &Error::UnterminatedString(_) => {
                fmt.write_str("unterminated string")
            }
            &Error::MissingBinaryDigits => {
                fmt.write_str("missing binary digits")
            }
            &Error::MissingOctalDigits => {
                fmt.write_str("missing octal digits")
            }
            &Error::MissingHexDigits => {
                fmt.write_str("missing hex digits")
            }
            &Error::IllegalChar(ref ch) => {
                fmt.write_fmt(format_args!("illegal character: {:?}", *ch))
            }
            &Error::InvalidDigit(ref ch) => {
                fmt.write_fmt(format_args!("invalid digit: {:?}", *ch))
            }
            &Error::IllegalUnicode(ref u) => {
                fmt.write_fmt(format_args!("illegal code unit: \\u{{{:04x}}}", u))
            }
            &Error::IdAfterNumber(_) => {
                fmt.write_str("identifier starts immediately after numeric literal")
            }
            &Error::DigitAfterNumber(_) => {
                fmt.write_str("numeric literal starts immediately after previous numeric literal")
            }
            &Error::ReservedWordWithEscapes(ref word) => {
                fmt.write_fmt(format_args!("reserved word with escapes: {:?}", word))
            }
        }
    }
}
