use std::collections::HashMap;
use std::convert::{AsRef, From};
use token::TokenData;
use result::Result;
use error::Error;

// Word with potential escape sequences
pub struct Word {
    had_escape: bool,
    pub text: String,
}

impl Word {
    pub fn new() -> Self {
        Word {
            had_escape: false,
            text: String::new()
        }
    }

    pub fn had_escape(&self) -> bool {
        self.had_escape
    }

    pub fn set_had_escape(&mut self) {
        self.had_escape = true;
    }
}

// Unconditionally reserved words.
#[derive(Debug, Clone, Copy, Eq, PartialEq)]
#[cfg_attr(test, derive(Deserialize))]
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
    pub fn atom(&self) -> Option<Atom> {
        match *self {
            Name::Atom(ref atom) => Some(*atom),
            _ => None
        }
    }

    pub fn into_string(self) -> String {
        match self {
            Name::Atom(atom) => atom.name().to_string(),
            Name::String(s)  => s
        }
    }
}

impl AsRef<str> for Name {
    fn as_ref(&self) -> &str {
        match self {
            &Name::Atom(ref atom) => atom.name(),
            &Name::String(ref s)  => s.as_ref()
        }
    }
}

impl From<String> for Name {
    fn from(s: String) -> Name {
        match &s[..] {
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
            _            => Name::String(s)
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

macro_rules! wordmap {
    ($ns:ident, [ $( ( $key:expr, $val:ident ) ),* ]) => {
        {
            let mut temp_map = HashMap::new();
            $(
                temp_map.insert($key, $ns::$val);
            )*
            temp_map
        }
    };
}

pub struct Map {
    reserved: HashMap<&'static str, Reserved>,
    contextual: HashMap<&'static str, Atom>
}

impl Map {
    pub fn new() -> Map {
        Map {
            reserved: wordmap!(Reserved, [
                // ReservedWord
                ("null",       Null),       ("true",       True),       ("false",     False),

                // Keyword
                ("break",      Break),      ("case",       Case),       ("catch",     Catch),
                ("class",      Class),      ("const",      Const),      ("continue",  Continue),
                ("debugger",   Debugger),   ("default",    Default),    ("delete",    Delete),
                ("do",         Do),         ("else",       Else),       ("export",    Export),
                ("extends",    Extends),    ("finally",    Finally),    ("for",       For),
                ("function",   Function),   ("if",         If),         ("import",    Import),
                ("in",         In),         ("instanceof", Instanceof), ("new",       New),
                ("return",     Return),     ("super",      Super),      ("switch",    Switch),
                ("this",       This),       ("throw",      Throw),      ("try",       Try),
                ("typeof",     Typeof),     ("var",        Var),        ("void",      Void),
                ("while",      While),      ("with",       With),

                // FutureReservedWord
                ("enum",       Enum)
            ]),

            contextual: wordmap!(Atom, [
                // Restricted words in strict code
                ("arguments",  Arguments),  ("eval",       Eval),

                // Reserved in some contexts
                ("yield",      Yield),

                // Reserved words in module code
                ("await",      Await),

                // Reserved words in strict code
                ("implements", Implements), ("interface",  Interface),  ("let",       Let),
                ("package",    Package),    ("private",    Private),    ("protected", Protected),
                ("public",     Public),     ("static",     Static),

                // Purely contextual identifier names
                ("async",      Async),      ("from",       From),       ("of",       Of),
                ("get",        Get),        ("set",        Set),        ("target",   Target)
            ])
        }
    }

    pub fn tokenize(&self, s: Word) -> Result<TokenData> {
        Ok(match self.reserved.get(&s.text[..]) {
            Some(&word) if !s.had_escape() => TokenData::Reserved(word),
            Some(&word) => return Err(Error::ReservedWordWithEscapes(word)),
            None => match self.contextual.get(&s.text[..]) {
                Some(&atom) if !s.had_escape() => TokenData::Identifier(Name::Atom(atom)),
                _ => TokenData::Identifier(Name::String(s.text))
            }
        })
    }
}
