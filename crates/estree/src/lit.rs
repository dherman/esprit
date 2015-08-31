use rustc_serialize::json::Json;
use joker::token::{StringLiteral, NumberLiteral};

trait ToSource {
    fn to_source(&self) -> String;
}

impl ToSource for String {
    fn to_source(&self) -> String {
        // FIXME: this hack is pretty close to correct but it's inefficient and the only dependency on rustc-serialize
        Json::String(self.to_string())
            .pretty()
            .to_string()
            .replace("\u{2028}", "\\u2028")
            .replace("\u{2029}", "\\u2029")
    }
}

pub trait IntoStringLiteral {
    fn into_string_literal(self) -> StringLiteral;
}

impl IntoStringLiteral for String {
    fn into_string_literal(self) -> StringLiteral {
        StringLiteral {
            source: self.to_source(),
            value: self
        }
    }
}

pub trait IntoNumberLiteral {
    fn into_number_literal(self) -> NumberLiteral;
}

impl IntoNumberLiteral for i64 {
    fn into_number_literal(self) -> NumberLiteral {
        NumberLiteral::DecimalInt(self.to_string(), None)
    }
}

impl IntoNumberLiteral for u64 {
    fn into_number_literal(self) -> NumberLiteral {
        NumberLiteral::DecimalInt(self.to_string(), None)
    }
}

impl IntoNumberLiteral for f64 {
    fn into_number_literal(self) -> NumberLiteral {
        let s = self.to_string();
        let v: Vec<&str> = s.split('.').collect();
        let int_part = Some(v[0].to_owned());
        let fract_part = if v.len() > 1 { Some(v[1].to_owned()) } else { None };
        NumberLiteral::Float(int_part, fract_part, None)
    }
}
