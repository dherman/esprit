#[derive(Debug, Copy, Eq, PartialEq)]
pub struct Posn {
    pub offset: u32,
    pub line: u32,
    pub column: u32
}

#[derive(Debug, Copy, Eq, PartialEq)]
pub struct Span {
    start: Posn,
    end: Posn
}

#[derive(Debug, Copy, Eq, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Token {
    pub span: Span,
    pub data: TokenData
}

impl Token {
    pub fn new(start: Posn, end: Posn, data: TokenData) -> Token {
        Token {
            span: Span {
                start: start,
                end: end
            },
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

    DecimalInt(String),
    BinaryInt(char, String),
    OctalInt(Option<char>, String),
    HexInt(char, String),
    Float(Option<String>, Option<String>, Option<String>),

    String(String),
    RegExp(String, Vec<char>),

    Identifier(String),

    LineComment(String),
    BlockComment(String),
    Newline,
    EOF
}
