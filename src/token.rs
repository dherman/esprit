use loc::*;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum ReservedWord {
    Null,
    True,
    False,

    Arguments,
    Eval,

    // 11.6.2.1 Keywords
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Export,
    Extends,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    Instanceof,
    Let,
    New,
    Return,
    Static,
    Super,
    Switch,
    This,
    Throw,
    Try,
    Typeof,
    Var,
    Void,
    While,
    With,
    Yield,

    // 11.6.2.2 Future Reserved Words
    Enum,
    Await,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public
}

pub type Token = Loc<TokenData>;

impl Token {
    pub fn new<T: HasSpan>(start: &T, end: &T, data: TokenData) -> Token {
        Token {
            span: span(start, end),
            data: data
        }
    }
}

#[derive(Debug, PartialEq)]
#[allow(dead_code)]
pub enum TokenData {
    Reserved(ReservedWord),

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

    String(String),
    RegExp(String, Vec<char>),

    Identifier(String),

    LineComment(String),
    BlockComment(String),
    Newline,
    EOF
}

#[derive(Debug, PartialEq)]
pub enum NumberLiteral {
    DecimalInt(String, Option<Exp>),
    RadixInt(Radix, String),
    Float(Option<String>, Option<String>, Option<Exp>)
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
