#![cfg(test)]

use std::collections::{HashMap, HashSet};
use rustc_serialize::json;
use rustc_serialize::json::{Json, Object, Array};
use rustc_serialize::{Decoder, Decodable};
use std::io::prelude::*;
use std::fs::File;
use token::{TokenData, ReservedWord};
use context::{Context, Mode};

pub struct ParserTest {
    pub source: String,
    pub expected: Result<Json, String>,
    pub options: Option<Json>
}

pub struct LexerTest {
    pub source: String,
    pub context: Context,
    pub expected: Result<TokenData, String>
}

fn deserialize_parser_test(test: &mut Object) -> ParserTest {
    ParserTest {
        source: test.remove("source").unwrap().into_string(),
        expected: if test.contains_key("error") {
            Err(test.remove("error").unwrap().into_string())
        } else {
            Ok(test.remove("expected").unwrap())
        },
        options: None
    }
}

fn deserialize_string_set(data: Array) -> HashSet<String> {
    let mut set = HashSet::new();
    for s in data.into_iter() {
        set.insert(s.into_string());
    }
    set
}

fn deserialize_lexer_context(data: &mut Object) -> Context {
    let mut cx = data.remove("context").unwrap();
    let set = deserialize_string_set(cx.into_array());
    Context {
        asi: set.contains("asi"),
        operator: set.contains("operator"),
        comment_tokens: set.contains("comments"),
        generator: set.contains("generator"),
        mode: Mode::Sloppy
    }
}

fn deserialize_lexer_test(mut test: Json) -> LexerTest {
    let mut obj = test.as_object_mut().unwrap();
    LexerTest {
        source: obj.remove("source").unwrap().into_string(),
        context: deserialize_lexer_context(obj),
        expected: if obj.contains_key("error") {
            Err(obj.remove("error").unwrap().into_string())
        } else {
            Ok(deserialize_token(obj.remove("expected").unwrap()))
        }
    }
}

trait JsonExt {
    fn into_array(self) -> json::Array;
    fn into_string(self) -> String;
    fn into_string_opt(self) -> Option<String>;
}

impl JsonExt for Json {
    fn into_array(self) -> json::Array {
        match self {
            Json::Array(array) => array,
            _ => panic!("expected array")
        }
    }
    fn into_string(self) -> String {
        match self {
            Json::String(string) => string,
            _ => panic!("expected string")
        }
    }
    fn into_string_opt(self) -> Option<String> {
        match self {
            Json::Null => None,
            Json::String(string) => Some(string),
            _ => panic!("expected string or null")
        }
    }
}

fn deserialize_reserved(word: &str) -> ReservedWord {
    match word {
        "Null"       => ReservedWord::Null,
        "True"       => ReservedWord::True,
        "False"      => ReservedWord::False,
        "Arguments"  => ReservedWord::Arguments,
        "Eval"       => ReservedWord::Eval,
        "Break"      => ReservedWord::Break,
        "Case"       => ReservedWord::Case,
        "Catch"      => ReservedWord::Catch,
        "Class"      => ReservedWord::Class,
        "Const"      => ReservedWord::Const,
        "Continue"   => ReservedWord::Continue,
        "Debugger"   => ReservedWord::Debugger,
        "Default"    => ReservedWord::Default,
        "Delete"     => ReservedWord::Delete,
        "Do"         => ReservedWord::Do,
        "Else"       => ReservedWord::Else,
        "Export"     => ReservedWord::Export,
        "Extends"    => ReservedWord::Extends,
        "Finally"    => ReservedWord::Finally,
        "For"        => ReservedWord::For,
        "Function"   => ReservedWord::Function,
        "If"         => ReservedWord::If,
        "Import"     => ReservedWord::Import,
        "In"         => ReservedWord::In,
        "Instanceof" => ReservedWord::Instanceof,
        "Let"        => ReservedWord::Let,
        "New"        => ReservedWord::New,
        "Return"     => ReservedWord::Return,
        "Static"     => ReservedWord::Static,
        "Super"      => ReservedWord::Super,
        "Switch"     => ReservedWord::Switch,
        "This"       => ReservedWord::This,
        "Throw"      => ReservedWord::Throw,
        "Try"        => ReservedWord::Try,
        "Typeof"     => ReservedWord::Typeof,
        "Var"        => ReservedWord::Var,
        "Void"       => ReservedWord::Void,
        "While"      => ReservedWord::While,
        "With"       => ReservedWord::With,
        "Yield"      => ReservedWord::Yield,
        "Enum"       => ReservedWord::Enum,
        "Await"      => ReservedWord::Await,
        "Implements" => ReservedWord::Implements,
        "Interface"  => ReservedWord::Interface,
        "Package"    => ReservedWord::Package,
        "Private"    => ReservedWord::Private,
        "Protected"  => ReservedWord::Protected,
        "Public"     => ReservedWord::Public,
        _            => panic!("invalid reserved word")
    }
}

macro_rules! right {
    ( $l:expr, $r:expr ) => { $r }
}

macro_rules! tuplify {
    ( $v:expr, ( $($dummy:tt),* ) ) => {
        {
            let mut t = $v.into_iter();
            ($(
                right!($dummy, t.next().unwrap())
            ),*)
        }
    };
}

fn deserialize_token(mut data: Json) -> TokenData {
    let mut arr = data.into_array();
    let ty = arr.remove(0).into_string();

    match &ty[..] {
        "Reserved"      => {
            let s = arr.remove(0).into_string();
            TokenData::Reserved(deserialize_reserved(&s[..]))
        }
        "LBrace"        => TokenData::LBrace,
        "RBrace"        => TokenData::RBrace,
        "LParen"        => TokenData::LParen,
        "RParen"        => TokenData::RParen,
        "LBrack"        => TokenData::LBrack,
        "RBrack"        => TokenData::RBrack,
        "Dot"           => TokenData::Dot,
        //"Ellipsis"    => TokenData::Ellipsis,
        "Semi"          => TokenData::Semi,
        "Comma"         => TokenData::Comma,
        "LAngle"        => TokenData::LAngle,
        "RAngle"        => TokenData::RAngle,
        "LEq"           => TokenData::LEq,
        "GEq"           => TokenData::GEq,
        "Eq"            => TokenData::Eq,
        "NEq"           => TokenData::NEq,
        "StrictEq"      => TokenData::StrictEq,
        "StrictNEq"     => TokenData::StrictNEq,
        "Plus"          => TokenData::Plus,
        "Minus"         => TokenData::Minus,
        "Star"          => TokenData::Star,
        "Mod"           => TokenData::Mod,
        "Slash"         => TokenData::Slash,
        "Inc"           => TokenData::Inc,
        "Dec"           => TokenData::Dec,
        "LShift"        => TokenData::LShift,
        "RShift"        => TokenData::RShift,
        "URShift"       => TokenData::URShift,
        "BitAnd"        => TokenData::BitAnd,
        "BitOr"         => TokenData::BitOr,
        "BitXor"        => TokenData::BitXor,
        "Bang"          => TokenData::Bang,
        "Tilde"         => TokenData::Tilde,
        "LogicalAnd"    => TokenData::LogicalAnd,
        "LogicalOr"     => TokenData::LogicalOr,
        "Question"      => TokenData::Question,
        "Colon"         => TokenData::Colon,
        "Assign"        => TokenData::Assign,
        "PlusAssign"    => TokenData::PlusAssign,
        "MinusAssign"   => TokenData::MinusAssign,
        "StarAssign"    => TokenData::StarAssign,
        "SlashAssign"   => TokenData::SlashAssign,
        "ModAssign"     => TokenData::ModAssign,
        "LShiftAssign"  => TokenData::LShiftAssign,
        "RShiftAssign"  => TokenData::RShiftAssign,
        "URShiftAssign" => TokenData::URShiftAssign,
        "BitAndAssign"  => TokenData::BitAndAssign,
        "BitOrAssign"   => TokenData::BitOrAssign,
        "BitXorAssign"  => TokenData::BitXorAssign,
        "Arrow"         => TokenData::Arrow,
        "Newline"       => TokenData::Newline,
        "EOF"           => TokenData::EOF,
        "DecimalInt"    => TokenData::DecimalInt(arr.remove(0).into_string()),
        "BinaryInt"     => {
            let (flag, value) = tuplify!(arr, ((), ()));
            TokenData::BinaryInt(flag.into_string().remove(0), value.into_string())
        }
        "OctalInt"      => {
            let (flag, value) = tuplify!(arr, ((), ()));
            TokenData::OctalInt(flag.into_string_opt().map(|mut str| str.remove(0)),
                            value.into_string())
        }
        "HexInt"        => {
            let (flag, value) = tuplify!(arr, ((), ()));
            TokenData::HexInt(flag.into_string().remove(0), value.into_string())
        }
        "Float"         => {
            let (int, frac, exp) = tuplify!(arr, ((), (), ()));
            TokenData::Float(int.into_string_opt(), frac.into_string_opt(), exp.into_string_opt())
        }
        "String"        => TokenData::String(arr.remove(0).into_string()),
        "RegExp"        => {
            let (pattern, flags) = tuplify!(arr, ((), ()));
            TokenData::RegExp(pattern.into_string(), flags.into_string().chars().collect())
        }
        "Identifier"    => TokenData::Identifier(arr.remove(0).into_string()),
        "LineComment"   => TokenData::LineComment(arr.remove(0).into_string()),
        "BlockComment"  => TokenData::BlockComment(arr.remove(0).into_string()),
        _               => panic!("invalid token")
    }
}

pub fn deserialize_lexer_tests(src: &str) -> Vec<LexerTest> {
    let data: Json = src.parse().unwrap();
    data.into_array()
        .into_iter()
        .map(deserialize_lexer_test)
        .collect()
}
