#![cfg(test)]

use serde_json;
use unjson::ty::Array;
use unjson::{Unjson, ExtractField};
use easter::stmt::Script;
use estree::IntoScript;

pub struct ParserTest {
    pub filename: Option<String>,
    pub source: String,
    pub expected: Option<Script>
}

pub fn deserialize_parser_tests(src: &str) -> Vec<ParserTest> {
    let arr: Array = serde_json::from_str(src).unwrap();
    arr.into_iter().map(|v| {
        let mut obj = v.into_object().ok().unwrap();
        ParserTest {
            filename: if obj.contains_key("filename") {
                Some(obj.extract_string("filename").ok().unwrap())
            } else {
                None
            },
            source: obj.extract_string("source").ok().unwrap(),
            expected: match obj.extract_object_opt("expected").ok().unwrap() {
                None => None,
                Some(obj) => {
                    match obj.into_script() {
                        Ok(script) => Some(script),
                        Err(err)   => { panic!("failed to deserialize script: {}", err) }
                    }
                }
            }
        }
    }).collect()
}

/*
pub fn deserialize_parser_integration_tests(modpath: &str) -> () {
    
}
*/
