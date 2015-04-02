#![cfg(test)]

use std::collections::HashMap;
use rustc_serialize::json;
use rustc_serialize::json::{Json, Object};
use rustc_serialize::{Decoder, Decodable};
use std::io::prelude::*;
use std::fs::File;
use token::{Token, ReservedWord};

pub struct ExpectPass {
    source: String,
    expected: Json,
    options: Option<Json>
}

pub struct ExpectFail {
    source: String,
    error: String,
    options: Option<Json>
}

pub enum TestCase {
    Pass(ExpectPass),
    Fail(ExpectFail)
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

fn parse_expect_fail(test: &mut Object) -> ExpectFail {
    ExpectFail {
        source: test.remove("source").unwrap().into_string(),
        error: test.remove("error").unwrap().into_string(),
        options: None
    }
}

fn parse_expect_pass(test: &mut Object) -> ExpectPass {
    ExpectPass {
        source: test.remove("source").unwrap().into_string(),
        expected: test.remove("expected").unwrap(),
        options: None
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

fn deserialize_token(mut data: Json) -> Token {
    let mut obj = data.as_object_mut().unwrap();
    let ty = obj.remove("type").unwrap().into_string();

    match &ty[..] {
        "Reserved"      => {
            let s = obj.remove("value").unwrap().into_string();
            Token::Reserved(deserialize_reserved(&s[..]))
        }
        "LBrace"        => Token::LBrace,
        "RBrace"        => Token::RBrace,
        "LParen"        => Token::LParen,
        "RParen"        => Token::RParen,
        "LBrack"        => Token::LBrack,
        "RBrack"        => Token::RBrack,
        "Dot"           => Token::Dot,
        //"Ellipsis"    => Token::Ellipsis,
        "Semi"          => Token::Semi,
        "Comma"         => Token::Comma,
        "LAngle"        => Token::LAngle,
        "RAngle"        => Token::RAngle,
        "LEq"           => Token::LEq,
        "GEq"           => Token::GEq,
        "Eq"            => Token::Eq,
        "NEq"           => Token::NEq,
        "StrictEq"      => Token::StrictEq,
        "StrictNEq"     => Token::StrictNEq,
        "Plus"          => Token::Plus,
        "Minus"         => Token::Minus,
        "Star"          => Token::Star,
        "Mod"           => Token::Mod,
        "Slash"         => Token::Slash,
        "Inc"           => Token::Inc,
        "Dec"           => Token::Dec,
        "LShift"        => Token::LShift,
        "RShift"        => Token::RShift,
        "URShift"       => Token::URShift,
        "BitAnd"        => Token::BitAnd,
        "BitOr"         => Token::BitOr,
        "BitXor"        => Token::BitXor,
        "Bang"          => Token::Bang,
        "Tilde"         => Token::Tilde,
        "LogicalAnd"    => Token::LogicalAnd,
        "LogicalOr"     => Token::LogicalOr,
        "Question"      => Token::Question,
        "Colon"         => Token::Colon,
        "Assign"        => Token::Assign,
        "PlusAssign"    => Token::PlusAssign,
        "MinusAssign"   => Token::MinusAssign,
        "StarAssign"    => Token::StarAssign,
        "SlashAssign"   => Token::SlashAssign,
        "ModAssign"     => Token::ModAssign,
        "LShiftAssign"  => Token::LShiftAssign,
        "RShiftAssign"  => Token::RShiftAssign,
        "URShiftAssign" => Token::URShiftAssign,
        "BitAndAssign"  => Token::BitAndAssign,
        "BitOrAssign"   => Token::BitOrAssign,
        "BitXorAssign"  => Token::BitXorAssign,
        "Arrow"         => Token::Arrow,
        "Newline"       => Token::Newline,
        "EOF"           => Token::EOF,
        "DecimalInt"    => Token::DecimalInt(obj.remove("value").unwrap().into_string()),
        "BinaryInt"     => {
            let flag = obj.remove("flag").unwrap().into_string().remove(0);
            Token::BinaryInt(flag, obj.remove("value").unwrap().into_string())
        }
        "OctalInt"      => {
            let flag = obj.remove("flag").unwrap().into_string_opt().map(|mut str| str.remove(0));
            Token::OctalInt(flag, obj.remove("value").unwrap().into_string())
        }
        "HexInt"        => {
            let flag = obj.remove("flag").unwrap().into_string().remove(0);
            Token::HexInt(flag, obj.remove("value").unwrap().into_string())
        }
        "Float"         => {
            let int = obj.remove("int").unwrap().into_string_opt();
            let frac = obj.remove("frac").unwrap().into_string_opt();
            let exp = obj.remove("exp").unwrap().into_string_opt();
            Token::Float(int, frac, exp)
        }
        "String"        => Token::String(obj.remove("value").unwrap().into_string()),
        "RegExp"        => Token::RegExp(obj.remove("value").unwrap().into_string()),
        "Identifier"    => Token::Identifier(obj.remove("value").unwrap().into_string()),
        "LineComment"   => Token::LineComment(obj.remove("value").unwrap().into_string()),
        "BlockComment"  => Token::BlockComment(obj.remove("value").unwrap().into_string()),
        _               => panic!("invalid token")
    }
}

fn parse_test(mut test: Json) -> TestCase {
    let obj = test.as_object_mut().unwrap();
    if obj.contains_key("error") {
        TestCase::Fail(parse_expect_fail(obj))
    } else {
        TestCase::Pass(parse_expect_pass(obj))
    }
}

pub fn parse_tests(src: &str) -> Vec<TestCase> {
    let t = Json::from_str("{\"type\":\"DecimalInt\",\"value\":\"11.3\"}").unwrap();
    println!("yo: {:?}", deserialize_token(t));
    let data: Json = src.parse().unwrap();
    data.into_array()
        .into_iter()
        .map(parse_test)
        .collect()
}
