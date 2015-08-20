use std::fmt;
use std::fmt::{Debug, Formatter};
use track::*;
use rustc_serialize::json::Json;

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

impl Reserved {
    pub fn name(&self) -> &'static str {
        match *self {
            // 11.6.2 Reserved Words
            Reserved::Null       => "null",
            Reserved::True       => "true",
            Reserved::False      => "false",

            // 11.6.2.1 Keywords
            Reserved::Break      => "break",
            Reserved::Case       => "case",
            Reserved::Catch      => "catch",
            Reserved::Class      => "class",
            Reserved::Const      => "const",
            Reserved::Continue   => "continue",
            Reserved::Debugger   => "debugger",
            Reserved::Default    => "default",
            Reserved::Delete     => "delete",
            Reserved::Do         => "do",
            Reserved::Else       => "else",
            Reserved::Export     => "export",
            Reserved::Extends    => "extends",
            Reserved::Finally    => "finally",
            Reserved::For        => "for",
            Reserved::Function   => "function",
            Reserved::If         => "if",
            Reserved::Import     => "import",
            Reserved::In         => "in",
            Reserved::Instanceof => "instanceof",
            Reserved::New        => "new",
            Reserved::Return     => "return",
            Reserved::Super      => "super",
            Reserved::Switch     => "switch",
            Reserved::This       => "this",
            Reserved::Throw      => "throw",
            Reserved::Try        => "try",
            Reserved::Typeof     => "typeof",
            Reserved::Var        => "var",
            Reserved::Void       => "void",
            Reserved::While      => "while",
            Reserved::With       => "with",

            // 11.6.2.2 Future Reserved Words
            Reserved::Enum       => "enum"
        }
    }

    pub fn into_string(self) -> String {
        self.name().to_string()
    }
}

// Contextually reserved words and special identifier names.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub enum Atom {
    Arguments,
    Async,
    Await,
    Eval,
    From,
    Get,
    Implements,
    Interface,
    Let,
    Of,
    Package,
    Private,
    Protected,
    Public,
    Set,
    Static,
    Target,
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
            "get"        => Name::Atom(Atom::Get),
            "implements" => Name::Atom(Atom::Implements),
            "interface"  => Name::Atom(Atom::Interface),
            "let"        => Name::Atom(Atom::Let),
            "of"         => Name::Atom(Atom::Of),
            "package"    => Name::Atom(Atom::Package),
            "private"    => Name::Atom(Atom::Private),
            "protected"  => Name::Atom(Atom::Protected),
            "public"     => Name::Atom(Atom::Public),
            "set"        => Name::Atom(Atom::Set),
            "static"     => Name::Atom(Atom::Static),
            "target"     => Name::Atom(Atom::Target),
            "yield"      => Name::Atom(Atom::Yield),
            _            => Name::String(from)
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Name::Atom(atom) => atom.name().to_string(),
            Name::String(s)  => s
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
            Atom::Get        => "get",
            Atom::Implements => "implements",
            Atom::Interface  => "interface",
            Atom::Let        => "let",
            Atom::Of         => "of",
            Atom::Package    => "package",
            Atom::Private    => "private",
            Atom::Protected  => "protected",
            Atom::Public     => "public",
            Atom::Set        => "set",
            Atom::Static     => "static",
            Atom::Target     => "target",
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

    String(StringLiteral),
    RegExp(String, Vec<char>),

    Identifier(Name),

    EOF
}

// FIXME: make saving source optional for memory savings

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
