#![cfg(test)]

use std::collections::HashSet;
use serde::de::{Visitor, SeqVisitor, Deserialize, Deserializer, Error, Unexpected};
use serde::de::value::SeqVisitorDeserializer;
use serde_json::{self, Value};
use token::*;
use word::Name;
use std::fmt;

pub struct LexerTest {
    pub source: String,
    pub operator: bool,
    pub expected: Result<TokenData, String>
}

impl Deserialize for LexerTest {
    fn deserialize<D: Deserializer>(deserializer: D) -> Result<LexerTest, D::Error> {
        #[derive(Deserialize)]
        struct Repr {
            pub source: String,
            pub context: HashSet<String>,
            pub error: Option<String>,
            pub expected: Option<TokenData>
        }

        let repr: Repr = Deserialize::deserialize(deserializer)?;

        Ok(LexerTest {
            source: repr.source,
            operator: repr.context.contains("operator"),
            expected: match (repr.error, repr.expected) {
                (Some(_), Some(_)) => {
                    return Err(D::Error::duplicate_field("error|expected"));
                },
                (None, None) => {
                    return Err(D::Error::missing_field("error|expected"));
                },
                (Some(error), None) => Err(error),
                (None, Some(expected)) => Ok(expected)
            }
        })
    }
}

impl Deserialize for Name {
    fn deserialize<D: Deserializer>(deserializer: D) -> Result<Name, D::Error> {
        let word: String = Deserialize::deserialize(deserializer)?;
        Ok(Name::from(word))
    }
}

impl Deserialize for CharCase {
    fn deserialize<D: Deserializer>(deserializer: D) -> Result<CharCase, D::Error> {
        let ch: char = Deserialize::deserialize(deserializer)?;
        if ch.is_lowercase() {
            Ok(CharCase::LowerCase)
        } else if ch.is_uppercase() {
            Ok(CharCase::UpperCase)
        } else {
            Err(D::Error::invalid_value(Unexpected::Char(ch), &"lowercase or uppercase letter"))
        }
    }
}

impl Deserialize for Exp {
    fn deserialize<D: Deserializer>(deserializer: D) -> Result<Exp, D::Error> {
        let (e, sign, value) = Deserialize::deserialize(deserializer)?;
        Ok(Exp {
            e: e,
            sign: sign,
            value: value
        })
    }
}

static KNOWN_TOKEN_VARIANTS: [&'static str; 4] = ["Reserved", "LBrace", "RBrace", "..."];

impl Deserialize for TokenData {
    fn deserialize<D: Deserializer>(deserializer: D) -> Result<TokenData, D::Error> {
        struct TokenVisitor;

        impl Visitor for TokenVisitor {
            type Value = TokenData;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("a nonempty sequence")
            }

            fn visit_seq<V: SeqVisitor>(self, mut visitor: V) -> Result<TokenData, V::Error> {
                let ty: String = visitor.visit()?.unwrap();

                let inner = SeqVisitorDeserializer::new(visitor);

                Ok(match &ty[..] {
                    "Reserved"      => {
                        let (word,) = Deserialize::deserialize(inner)?;
                        TokenData::Reserved(word)
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
                    "EOF"           => TokenData::EOF,
                    "DecimalInt"    => {
                        let (value, exp) = Deserialize::deserialize(inner)?;
                        NumberSource::DecimalInt(value, exp).into_token_data()
                    }
                    "BinaryInt"     => {
                        let (flag, value) = Deserialize::deserialize(inner)?;
                        NumberSource::RadixInt(Radix::Bin(flag), value).into_token_data()
                    }
                    "OctalInt"      => {
                        let (flag, value) = Deserialize::deserialize(inner)?;
                        NumberSource::RadixInt(Radix::Oct(flag), value).into_token_data()
                    }
                    "HexInt"        => {
                        let (flag, value) = Deserialize::deserialize(inner)?;
                        NumberSource::RadixInt(Radix::Hex(flag), value).into_token_data()
                    }
                    "Float"         => {
                        let (int, frac, exp) = Deserialize::deserialize(inner)?;
                        NumberSource::Float(int, frac, exp).into_token_data()
                    }
                    "String"        => {
                        let (value,): (String,) = Deserialize::deserialize(inner)?;
                        TokenData::String(StringLiteral {
                            source: Some(format!("{:?}", Value::String(value.clone()))),
                            value: value
                        })
                    }
                    "RegExp"        => {
                        let (pattern, flags): (String, String) = Deserialize::deserialize(inner)?;
                        TokenData::RegExp(RegExpLiteral {
                            pattern: pattern,
                            flags: flags.chars().collect()
                        })
                    }
                    "Identifier"    => {
                        let (name,) = Deserialize::deserialize(inner)?;
                        TokenData::Identifier(name)
                    },
                    other => { return Err(V::Error::unknown_variant(other, &KNOWN_TOKEN_VARIANTS)); }
                })
            }
        }

        deserializer.deserialize_seq(TokenVisitor)
    }
}

pub fn deserialize_lexer_tests(src: &str) -> Vec<LexerTest> {
    serde_json::from_str(src).unwrap()
}
