#![cfg(test)]

use std::collections::HashSet;
use rustc_serialize::json::{Json, Object, Array};
use token::TokenData;
use context::{SharedContext, Mode};
use ast::*;
use estree::deserialize::{Deserialize, ExtractField, IntoNode, IntoToken, MatchJson};

pub struct ParserTest {
    pub source: String,
    pub expected: Option<Script>,
    pub options: Option<Json>
}

pub struct LexerTest {
    pub source: String,
    pub context: SharedContext,
    pub expected: Result<TokenData, String>
}

pub trait IntoTest {
    fn into_parser_test(self) -> Deserialize<ParserTest>;
    fn into_lexer_test(self) -> Deserialize<LexerTest>;
}

pub trait IntoTestSuite {
    fn into_parser_test_suite(self) -> Deserialize<Vec<ParserTest>>;
    fn into_lexer_test_suite(self) -> Deserialize<Vec<LexerTest>>;
}

impl IntoTestSuite for Array {
    fn into_parser_test_suite(self) -> Deserialize<Vec<ParserTest>> {
        let mut result = Vec::with_capacity(self.len());
        for data in self {
            result.push(try!(data.into_parser_test()));
        }
        Ok(result)
    }

    fn into_lexer_test_suite(self) -> Deserialize<Vec<LexerTest>> {
        let mut result = Vec::with_capacity(self.len());
        for data in self {
            result.push(try!(data.into_lexer_test()));
        }
        Ok(result)
    }
}

impl IntoTestSuite for Json {
    fn into_parser_test_suite(self) -> Deserialize<Vec<ParserTest>> {
        self.into_array().and_then(|arr| arr.into_parser_test_suite())
    }

    fn into_lexer_test_suite(self) -> Deserialize<Vec<LexerTest>> {
        self.into_array().and_then(|arr| arr.into_lexer_test_suite())
    }
}

impl IntoTest for Object {
    fn into_parser_test(mut self) -> Deserialize<ParserTest> {
        Ok(ParserTest {
            source: try!(self.extract_string("source")),
            expected: match try!(self.extract_object_opt("expected")) {
                None => None,
                Some(obj) => Some(try!(obj.into_program()))
            },
            options: None
        })
    }

    fn into_lexer_test(mut self) -> Deserialize<LexerTest> {
        let source = try!(self.extract_string("source"));
        let set = try!(self.extract_array("context").and_then(|arr| arr.into_string_set()));
        let expected = if self.contains_key("error") {
            Err(try!(self.extract_string("error")))
        } else {
            Ok(try!(self.extract("expected").and_then(|data| data.into_token())))
        };
        let mut context = SharedContext::new(Mode::Sloppy);
        context.operator = set.contains("operator");
        Ok(LexerTest {
            source: source,
            context: context,
            expected: expected
        })
    }
}

impl IntoTest for Json {
    fn into_parser_test(self) -> Deserialize<ParserTest> {
        self.into_object().and_then(|obj| obj.into_parser_test())
    }

    fn into_lexer_test(self) -> Deserialize<LexerTest> {
        self.into_object().and_then(|obj| obj.into_lexer_test())
    }
}

trait IntoStringSet {
    fn into_string_set(self) -> Deserialize<HashSet<String>>;
}

impl IntoStringSet for Array {
    fn into_string_set(self) -> Deserialize<HashSet<String>> {
        let mut set = HashSet::new();
        for data in self {
            set.insert(try!(data.into_string()));
        }
        Ok(set)
    }
}

pub fn deserialize_lexer_tests(src: &str) -> Vec<LexerTest> {
    let data: Json = src.parse().unwrap();
    match data.into_lexer_test_suite() {
        Ok(result) => result,
        Err(err) => panic!(format!("lexer test failed to deserialize: {:?}", err))
    }
}

pub fn deserialize_parser_tests(src: &str) -> Vec<ParserTest> {
    let data: Json = src.parse().unwrap();
    match data.into_parser_test_suite() {
        Ok(result) => result,
        Err(err) => panic!(format!("parser test failed to deserialize: {:?}", err))
    }
}
