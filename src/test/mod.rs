#![cfg(test)]

use std::collections::HashMap;
use rustc_serialize::json;
use rustc_serialize::json::{Json, Object};
use rustc_serialize::{Decoder, Decodable};
use std::io::prelude::*;
use std::fs::File;
use token::{Token, ReservedWord};

pub struct DynJson {
    json: Json
}

impl Decodable for DynJson {
    fn decode<D: Decoder>(d: &mut D) -> Result<Self, u32> {
        unimplemented!()
    }
}

#[derive(RustcDecodable)]
pub struct ExpectPass {
    source: String,
    expected: DynJson,
    options: Option<DynJson>
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

