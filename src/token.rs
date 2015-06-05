use track::*;

// Unconditionally reserved words.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum Reserved {
    // 11.6.2 Reserved Words
    Null,
    True,
    False,

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
    New,
    Return,
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

    // 11.6.2.2 Future Reserved Words
    Enum
}

// Contextually reserved words and special identifier names.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Atom {
    Arguments,
    Async,
    Await,
    Eval,
    From,
    Implements,
    Interface,
    Let,
    Of,
    Package,
    Private,
    Protected,
    Public,
    Static,
    Yield
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum Name {
    Atom(Atom),
    String(String)
}

impl Name {
    pub fn new(from: String) -> Name {
        match &from[..] {
            "arguments"  => Name::Atom(Atom::Arguments),
            "await"      => Name::Atom(Atom::Await),
            "eval"       => Name::Atom(Atom::Eval),
            "async"      => Name::Atom(Atom::Async),
            "from"       => Name::Atom(Atom::From),
            "implements" => Name::Atom(Atom::Implements),
            "interface"  => Name::Atom(Atom::Interface),
            "let"        => Name::Atom(Atom::Let),
            "of"         => Name::Atom(Atom::Of),
            "package"    => Name::Atom(Atom::Package),
            "private"    => Name::Atom(Atom::Private),
            "protected"  => Name::Atom(Atom::Protected),
            "public"     => Name::Atom(Atom::Public),
            "static"     => Name::Atom(Atom::Static),
            "yield"      => Name::Atom(Atom::Yield),
            _            => Name::String(from)
        }
    }
}

impl Atom {
    pub fn name(self) -> &'static str {
        match self {
            Atom::Arguments  => "arguments",
            Atom::Await      => "await",
            Atom::Eval       => "eval",
            Atom::Async      => "async",
            Atom::From       => "from",
            Atom::Implements => "implements",
            Atom::Interface  => "interface",
            Atom::Let        => "let",
            Atom::Of         => "of",
            Atom::Package    => "package",
            Atom::Private    => "private",
            Atom::Protected  => "protected",
            Atom::Public     => "public",
            Atom::Static     => "static",
            Atom::Yield      => "yield"
        }
    }
}

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
#[allow(dead_code)]
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

    String(String),
    RegExp(String, Vec<char>),

    Identifier(Name),

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
