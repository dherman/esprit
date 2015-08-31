use joker::token::{StringLiteral, NumberLiteral, NumberSource};

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
