#![cfg(test)]

use std::collections::HashMap;
use rustc_serialize::json;
use rustc_serialize::json::{decode, Json, Object};
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

trait Matcher {
    fn test(self: Box<Self>, arg: &mut Object) -> Option<Token>;
}

impl<F> Matcher for F
    where F: FnOnce(&mut Object) -> Option<Token>
{
    fn test(self: Box<F>, arg: &mut Object) -> Option<Token> {
        self(arg)
    }
}

fn matcher<F:FnOnce(&mut Object) -> Option<Token> + 'static>(f: F) -> Box<Matcher> {
    Box::new(f)
}

fn nullary(token: Token) -> Box<Matcher> {
    matcher(|_| { Some(token) })
}

fn unary_string<F:Fn(String) -> Token + 'static>(f: F) -> Box<Matcher> {
    matcher(|data| {
        data.remove("value")
            .map(|str| f(str.into_string()))
    })
}

fn deserialize_token(mut data: Json) -> Token {
    let mut obj = data.as_object_mut().unwrap();
    let ty = obj.remove("type").unwrap().into_string();

    let mut matchers: HashMap<&str, Box<Matcher>> = HashMap::new();
    matchers.insert("LBrace", nullary(Token::LBrace));
    matchers.insert("DecimalInt", matcher(|data| -> Option<Token> {
        match data.remove("value") {
            None => None,
            Some(str) => Some(Token::DecimalInt(str.into_string()))
        }
    }));
    matchers.insert("String", unary_string(Token::String));

    let f = matchers.remove("DecimalInt").unwrap();
    let mut data = Json::from_str("{\"type\":\"DecimalInt\",\"value\":\"11.3\"}").unwrap();
    let obj = data.as_object_mut().unwrap();
    f.test(obj).unwrap()
}

fn parse_test(mut test: Json) -> TestCase {
    let obj = test.as_object_mut().unwrap();
    if obj.contains_key("error") {
        TestCase::Fail(parse_expect_fail(obj))
    } else {
        TestCase::Pass(parse_expect_pass(obj))
    }
}

pub fn read_tests(path: &str) -> Vec<TestCase> {
    let mut f: File = File::open(path).unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s);
    parse_tests(&s)
}

pub fn parse_tests(src: &str) -> Vec<TestCase> {
    let data: Json = src.parse().unwrap();
    data.into_array()
        .into_iter()
        .map(parse_test)
        .collect()
}
