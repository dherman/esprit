use std::fmt;
use std::fmt::{Debug, Formatter};
use track::*;
use word::{Reserved, Name};

#[derive(Debug, PartialEq)]
pub struct Token {
    pub location: Span,
    pub newline: bool,    // was there a newline between the preceding token and this one?
    pub value: TokenData
}

impl Track for Token {
    fn location(&self) -> Option<Span> {
        Some(self.location)
    }
}

impl Token {
    pub fn new(start: Posn, end: Posn, value: TokenData) -> Token {
        Token {
            location: Span { start: start, end: end },
            newline: false,
            value: value
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum TokenData {
    Reserved(Reserved),

    // 11.7 Punctuators
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,
    Dot,
    //Ellipsis,
    Semi,
    Comma,
    LAngle,
    RAngle,
    LEq,
    GEq,
    Eq,
    NEq,
    StrictEq,
    StrictNEq,
    Plus,
    Minus,
    Star,
    Mod,
    Slash,
    Inc,
    Dec,
    LShift,
    RShift,
    URShift,
    BitAnd,
    BitOr,
    BitXor,
    Bang,
    Tilde,
    LogicalAnd,
    LogicalOr,
    Question,
    Colon,
    Assign,
    PlusAssign,
    MinusAssign,
    StarAssign,
    SlashAssign,
    ModAssign,
    LShiftAssign,
    RShiftAssign,
    URShiftAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    Arrow,

    Number(NumberLiteral),

    String(StringLiteral),
    RegExp(String, Vec<char>),

    Identifier(Name),

    EOF
}

pub struct StringLiteral {
    pub source: String,
    pub value: String
}

impl Debug for StringLiteral {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_struct("StringLiteral")
            .field("value", &self.value)
            .finish()
    }
}

impl PartialEq for StringLiteral {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

#[derive(PartialEq)]
pub enum NumberLiteral {
    DecimalInt(String, Option<Exp>),
    RadixInt(Radix, String),
    Float(Option<String>, Option<String>, Option<Exp>)
}

impl Debug for NumberLiteral {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_tuple("NumberLiteral")
            .field(&self.value())
            .finish()
    }
}

fn format_sign(sign: &Option<Sign>) -> String {
    match *sign {
        Some(Sign::Minus) => "-",
        _ => ""
    }.to_string()
}

fn format_int(src: &Option<String>) -> String {
    match *src {
        None        => "".to_string(),
        Some(ref s) => s.to_string()
    }
}

impl NumberLiteral {
    pub fn value(&self) -> Option<f64> {
        match *self {
            NumberLiteral::DecimalInt(ref mantissa, None) => {
                let i: Option<i64> = mantissa.parse().ok();
                i.map(|i| i as f64)
            }
            NumberLiteral::DecimalInt(ref mantissa, Some(Exp { ref sign, ref value, .. })) => {
                let s = format!("{}e{}{}", mantissa, format_sign(sign), value);
                let i: Option<i64> = s.parse().ok();
                i.map(|i| i as f64)
            }
            NumberLiteral::RadixInt(ref radix, ref src) => {
                i64::from_str_radix(&src[..], radix.value()).ok().map(|i| i as f64)
            }
            NumberLiteral::Float(ref ip, ref fp, None) => {
                format!("{}.{}", format_int(ip), format_int(fp)).parse().ok()
            }
            NumberLiteral::Float(ref ip, ref fp, Some(Exp { ref sign, ref value, .. })) => {
                format!("{}.{}e{}{}", format_int(ip), format_int(fp), format_sign(sign), value).parse().ok()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Exp {
    pub e: CharCase,
    pub sign: Option<Sign>,
    pub value: String
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Radix {
    Bin(CharCase),
    Oct(Option<CharCase>),
    Hex(CharCase)
}

impl Radix {
    pub fn value(&self) -> u32 {
        match *self {
            Radix::Bin(_) => 2,
            Radix::Oct(_) => 8,
            Radix::Hex(_) => 16
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum CharCase {
    LowerCase,
    UpperCase
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Sign {
    Plus,
    Minus
}
