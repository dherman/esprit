use serde::ser::*;
use serde_json::Number;
use joker::token::{ NumberLiteral, NumberSource, RegExpLiteral, StringLiteral };

use util::*;

pub trait IntoStringLiteral {
    fn into_string_literal(self) -> StringLiteral;
}

impl IntoStringLiteral for String {
    fn into_string_literal(self) -> StringLiteral {
        StringLiteral {
            source: None,
            value: self
        }
    }
}

pub trait IntoNumberLiteral {
    fn into_number_literal(self) -> NumberLiteral;
}

impl IntoNumberLiteral for Number {
    fn into_number_literal(self) -> NumberLiteral {
        // By definition of Serde_json, numbers are either f64, u64 or i64.
        // Unfortunately, we cannot pattern match on that.
        let value = if self.is_f64() {
            self.as_f64().unwrap()
        } else if self.is_u64() {
            self.as_u64().unwrap() as f64
        } else if self.is_i64() {
            self.as_i64().unwrap() as f64
        } else {
            panic!("Inconsistent numeric value.");
        };

        NumberLiteral {
            source: Some(NumberSource::DecimalInt(self.to_string(), None)),
            value
        }
    }
}

impl IntoNumberLiteral for i64 {
    fn into_number_literal(self) -> NumberLiteral {
        NumberLiteral {
            source: Some(NumberSource::DecimalInt(self.to_string(), None)),
            value: self as f64
        }
    }
}

impl IntoNumberLiteral for u64 {
    fn into_number_literal(self) -> NumberLiteral {
        NumberLiteral {
            source: Some(NumberSource::DecimalInt(self.to_string(), None)),
            value: self as f64
        }
    }
}

impl IntoNumberLiteral for f64 {
    fn into_number_literal(self) -> NumberLiteral {
        NumberLiteral {
            source: None,
            value: self
        }
    }
}

impl<'a> Serialize for Serialization<'a, NumberLiteral> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        self.data().value.serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, StringLiteral> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        self.data().value.serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, RegExpLiteral> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        let flags : String = self.data().flags.iter().collect();
        json!({
            "regex": {
                "pattern": self.data().pattern,
                "flags": flags
            }
        }).serialize(serializer)
    }
}
