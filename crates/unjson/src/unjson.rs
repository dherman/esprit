use serde_json::value::Value;
use result::Result;
use error::*;
use ty::*;

trait OkType<T> {
    fn ok_type(self, &'static str, Ty) -> Result<T>;
}

impl<T> OkType<T> for Option<T> {
    fn ok_type(self, expected: &'static str, actual: Ty) -> Result<T> {
        match self {
            Some(v) => Ok(v),
            None    => Err(Error::TypeMismatch(expected, actual))
        }
    }
}

trait ValueEx {
    fn as_number(&self) -> Result<f64>;

    fn as_str_opt(&self) -> Result<Option<&str>>;
    fn as_array_opt(&self) -> Result<Option<&Vec<Value>>>;
    fn as_object_opt(&self) -> Result<Option<&Object>>;
    fn as_bool_opt(&self) -> Result<Option<bool>>;
    fn as_i64_opt(&self) -> Result<Option<i64>>;
    fn as_u64_opt(&self) -> Result<Option<u64>>;
    fn as_f64_opt(&self) -> Result<Option<f64>>;
    fn as_number_opt(&self) -> Result<Option<f64>>;
}

impl ValueEx for Value {
    fn as_number(&self) -> Result<f64> {
        match *self {
            Value::Number(ref v) if v.is_f64() => Ok(v.as_f64().unwrap()),
            Value::Number(ref v) if v.is_u64() => Ok(v.as_u64().unwrap() as f64),
            Value::Number(ref v) if v.is_i64() => Ok(v.as_i64().unwrap() as f64),
            _ => type_error("number", self.ty())
        }
    }

    fn as_str_opt(&self) -> Result<Option<&str>> {
        Ok(match self {
            &Value::Null          => None,
            &Value::String(ref s) => Some(&s[..]),
            _ => { return type_error("string or null", self.ty()); }
        })
    }

    fn as_array_opt(&self) -> Result<Option<&Vec<Value>>> {
        Ok(match self {
            &Value::Null         => None,
            &Value::Array(ref a) => Some(a),
            _ => { return type_error("array or null", self.ty()); }
        })
    }

    fn as_object_opt(&self) -> Result<Option<&Object>> {
        Ok(match self {
            &Value::Null          => None,
            &Value::Object(ref o) => Some(o),
            _ => { return type_error("object or null", self.ty()); }
        })
    }

    fn as_bool_opt(&self) -> Result<Option<bool>> {
        Ok(match self {
            &Value::Null        => None,
            &Value::Bool(ref b) => Some(*b),
            _ => { return type_error("boolean or null", self.ty()); }
        })
    }

    fn as_i64_opt(&self) -> Result<Option<i64>> {
        Ok(match self {
            &Value::Null          => None,
            &Value::Number(ref v) if v.is_i64() => Some(v.as_i64().unwrap()),
            _ => { return type_error("i64 or null", self.ty()); }
        })
    }

    fn as_u64_opt(&self) -> Result<Option<u64>> {
        Ok(match self {
            &Value::Null       => None,
            &Value::Number(ref v) if v.is_u64() => Some(v.as_u64().unwrap()),
            _ => { return type_error("u64 or null", self.ty()); }
        })
    }

    fn as_f64_opt(&self) -> Result<Option<f64>> {
        Ok(match self {
            &Value::Null       => None,
            &Value::Number(ref v) if v.is_f64() => Some(v.as_f64().unwrap()),
            _ => { return type_error("f64 or null", self.ty()); }
        })
    }

    fn as_number_opt(&self) -> Result<Option<f64>> {
        match *self {
            Value::Number(ref v) if v.is_f64() => Ok(Some(v.as_f64().unwrap())),
            Value::Number(ref v) if v.is_u64() => Ok(Some(v.as_u64().unwrap() as f64)),
            Value::Number(ref v) if v.is_i64() => Ok(Some(v.as_i64().unwrap() as f64)),
            Value::Null => Ok(None),
            _ => type_error("number or null", self.ty())
        }
    }

}

pub trait Unjson {
    fn into_array(self) -> Result<Array>;
    fn into_array_opt(self) -> Result<Option<Array>>;
    fn into_string(self) -> Result<String>;
    fn into_string_opt(self) -> Result<Option<String>>;
    fn into_object(self) -> Result<Object>;
    fn into_object_opt(self) -> Result<Option<Object>>;
    fn to_bool(&self) -> Result<bool>;
    fn to_bool_opt(&self) -> Result<Option<bool>>;
    fn to_i64(&self) -> Result<i64>;
    fn to_i64_opt(&self) -> Result<Option<i64>>;
    fn to_u64(&self) -> Result<u64>;
    fn to_u64_opt(&self) -> Result<Option<u64>>;
    fn to_f64(&self) -> Result<f64>;
    fn to_f64_opt(&self) -> Result<Option<f64>>;
    fn to_number(&self) -> Result<f64>;
    fn to_number_opt(&self) -> Result<Option<f64>>;
}

impl Unjson for Value {
    fn into_array(self) -> Result<Array> {
        match self {
            Value::Array(a) => Ok(a),
            _ => { return type_error("array", self.ty()); }
        }
    }

    fn into_array_opt(self) -> Result<Option<Array>> {
        match self {
            Value::Array(a) => Ok(Some(a)),
            Value::Null     => Ok(None),
            _ => { return type_error("array or null", self.ty()); }
        }
    }

    fn into_string(self) -> Result<String> {
        match self {
            Value::String(s) => Ok(s),
            _ => { return type_error("string", self.ty()); }
        }
    }

    fn into_string_opt(self) -> Result<Option<String>> {
        match self {
            Value::String(s) => Ok(Some(s)),
            Value::Null      => Ok(None),
            _ => { return type_error("string or null", self.ty()); }
        }
    }

    fn into_object(self) -> Result<Object> {
        match self {
            Value::Object(o) => Ok(o),
            _ => { return type_error("object", self.ty()); }
        }
    }

    fn into_object_opt(self) -> Result<Option<Object>> {
        match self {
            Value::Object(o) => Ok(Some(o)),
            Value::Null      => Ok(None),
            _ => { return type_error("object or null", self.ty()); }
        }
    }

    fn to_bool(&self) -> Result<bool> {
        match *self {
            Value::Bool(b) => Ok(b),
            _ => { return type_error("boolean", self.ty()); }
        }
    }

    fn to_bool_opt(&self) -> Result<Option<bool>> {
        match *self {
            Value::Bool(b) => Ok(Some(b)),
            Value::Null    => Ok(None),
            _ => { return type_error("boolean", self.ty()); }
        }
    }

    fn to_i64(&self) -> Result<i64> {
        match *self {
            Value::Number(ref v) if v.is_i64() => Ok(v.as_i64().unwrap()),
            _ => { return type_error("i64", self.ty()); }
        }
    }

    fn to_i64_opt(&self) -> Result<Option<i64>> {
        match *self {
            Value::Number(ref v) if v.is_i64() => Ok(Some(v.as_i64().unwrap())),
            Value::Null   => Ok(None),
            _ => { return type_error("i64", self.ty()); }
        }
    }

    fn to_u64(&self) -> Result<u64> {
        match *self {
            Value::Number(ref v) if v.is_u64() => Ok(v.as_u64().unwrap()),
            _ => { return type_error("u64", self.ty()); }
        }
    }

    fn to_u64_opt(&self) -> Result<Option<u64>> {
        match *self {
            Value::Number(ref v) if v.is_u64() => Ok(Some(v.as_u64().unwrap())),
            Value::Null   => Ok(None),
            _ => { return type_error("u64", self.ty()); }
        }
    }

    fn to_f64(&self) -> Result<f64> {
        match *self {
            Value::Number(ref v) if v.is_f64() => Ok(v.as_f64().unwrap()),
            _ => { return type_error("f64", self.ty()); }
        }
    }

    fn to_f64_opt(&self) -> Result<Option<f64>> {
        match *self {
            Value::Number(ref v) if v.is_f64() => Ok(Some(v.as_f64().unwrap())),
            Value::Null   => Ok(None),
            _ => { return type_error("f64", self.ty()); }
        }
    }

    fn to_number(&self) -> Result<f64> {
        self.as_number()
    }

    fn to_number_opt(&self) -> Result<Option<f64>> {
        self.as_number_opt()
    }
}

pub trait GetField {
    fn get_field(&self, &'static str) -> Result<&Value>;

    fn get_string(&self, &'static str) -> Result<&str>;
    fn get_string_opt(&self, &'static str) -> Result<Option<&str>>;

    fn get_array(&self, &'static str) -> Result<&Array>;
    fn get_array_opt(&self, &'static str) -> Result<Option<&Array>>;

    fn get_object(&self, &'static str) -> Result<&Object>;
    fn get_object_opt(&self, &'static str) -> Result<Option<&Object>>;

    fn get_bool(&self, &'static str) -> Result<bool>;
    fn get_bool_opt(&self, &'static str) -> Result<Option<bool>>;

    fn get_i64(&self, &'static str) -> Result<i64>;
    fn get_i64_opt(&self, &'static str) -> Result<Option<i64>>;

    fn get_u64(&self, &'static str) -> Result<u64>;
    fn get_u64_opt(&self, &'static str) -> Result<Option<u64>>;

    fn get_f64(&self, &'static str) -> Result<f64>;
    fn get_f64_opt(&self, &'static str) -> Result<Option<f64>>;

    fn get_number(&self, &'static str) -> Result<f64>;
    fn get_number_opt(&self, &'static str) -> Result<Option<f64>>;


    //fn get_object_array(&self, &'static str) -> Result<Vec<Object>>;
    //fn get_object_opt_array(&self, &'static str) -> Result<Vec<Option<Object>>>;
}

impl GetField for Object {
    fn get_field(&self, name: &'static str) -> Result<&Value> {
        match self.get(name) {
            Some(json) => Ok(json),
            None       => Err(Error::MissingField(name))
        }
    }

    fn get_string(&self, name: &'static str) -> Result<&str> {
        self.get_field(name).and_then(|v| v.as_str().ok_type("string", v.ty()))
    }

    fn get_string_opt(&self, name: &'static str) -> Result<Option<&str>> {
        self.get_field(name).and_then(|v| v.as_str_opt())
    }

    fn get_array(&self, name: &'static str) -> Result<&Array> {
        self.get_field(name).and_then(|v| v.as_array().ok_type("array", v.ty()))
    }

    fn get_array_opt(&self, name: &'static str) -> Result<Option<&Array>> {
        self.get_field(name).and_then(|v| v.as_array_opt())
    }

    fn get_object(&self, name: &'static str) -> Result<&Object> {
        self.get_field(name).and_then(|v| v.as_object().ok_type("object", v.ty()))
    }

    fn get_object_opt(&self, name: &'static str) -> Result<Option<&Object>> {
        self.get_field(name).and_then(|v| v.as_object_opt())
    }

    fn get_bool(&self, name: &'static str) -> Result<bool> {
        self.get_field(name).and_then(|v| v.as_bool().ok_type("boolean", v.ty()))
    }

    fn get_bool_opt(&self, name: &'static str) -> Result<Option<bool>> {
        self.get_field(name).and_then(|v| v.as_bool_opt())
    }

    fn get_i64(&self, name: &'static str) -> Result<i64> {
        self.get_field(name).and_then(|v| v.as_i64().ok_type("i64", v.ty()))
    }

    fn get_i64_opt(&self, name: &'static str) -> Result<Option<i64>> {
        self.get_field(name).and_then(|v| v.as_i64_opt())
    }

    fn get_u64(&self, name: &'static str) -> Result<u64> {
        self.get_field(name).and_then(|v| v.as_u64().ok_type("u64", v.ty()))
    }

    fn get_u64_opt(&self, name: &'static str) -> Result<Option<u64>> {
        self.get_field(name).and_then(|v| v.as_u64_opt())
    }

    fn get_f64(&self, name: &'static str) -> Result<f64> {
        self.get_field(name).and_then(|v| v.as_f64().ok_type("f64", v.ty()))
    }

    fn get_f64_opt(&self, name: &'static str) -> Result<Option<f64>> {
        self.get_field(name).and_then(|v| v.as_f64_opt())
    }

    fn get_number(&self, name: &'static str) -> Result<f64> {
        self.get_field(name).and_then(|v| v.as_number())
    }

    fn get_number_opt(&self, name: &'static str) -> Result<Option<f64>> {
        self.get_field(name).and_then(|v| v.as_number_opt())
    }
}

pub trait ExtractField {
    fn extract_field(&mut self, &'static str) -> Result<Value>;

    fn extract_string(&mut self, &'static str) -> Result<String>;
    fn extract_string_opt(&mut self, &'static str) -> Result<Option<String>>;

    fn extract_array(&mut self, &'static str) -> Result<Array>;
    fn extract_array_opt(&mut self, &'static str) -> Result<Option<Array>>;

    fn extract_object(&mut self, &'static str) -> Result<Object>;
    fn extract_object_opt(&mut self, &'static str) -> Result<Option<Object>>;

    fn extract_bool(&mut self, &'static str) -> Result<bool>;
    fn extract_bool_opt(&mut self, &'static str) -> Result<Option<bool>>;

    fn extract_i64(&mut self, &'static str) -> Result<i64>;
    fn extract_i64_opt(&mut self, &'static str) -> Result<Option<i64>>;

    fn extract_u64(&mut self, &'static str) -> Result<u64>;
    fn extract_u64_opt(&mut self, &'static str) -> Result<Option<u64>>;

    fn extract_f64(&mut self, &'static str) -> Result<f64>;
    fn extract_f64_opt(&mut self, &'static str) -> Result<Option<f64>>;

    fn extract_number(&mut self, &'static str) -> Result<f64>;
    fn extract_number_opt(&mut self, &'static str) -> Result<Option<f64>>;


    //fn extract_object_array(&mut self, &'static str) -> Result<&Vec<Object>>;
    //fn extract_object_opt_array(&mut self, &'static str) -> Result<&Vec<Option<Object>>>;
}

impl ExtractField for Object {
    fn extract_field(&mut self, name: &'static str) -> Result<Value> {
        match self.remove(name) {
            Some(json) => Ok(json),
            None       => Err(Error::MissingField(name))
        }
    }

    fn extract_string(&mut self, name: &'static str) -> Result<String> {
        self.extract_field(name).and_then(|v| v.into_string())
    }

    fn extract_string_opt(&mut self, name: &'static str) -> Result<Option<String>> {
        self.extract_field(name).and_then(|v| v.into_string_opt())
    }

    fn extract_array(&mut self, name: &'static str) -> Result<Array> {
        self.extract_field(name).and_then(|v| v.into_array())
    }

    fn extract_array_opt(&mut self, name: &'static str) -> Result<Option<Array>> {
        self.extract_field(name).and_then(|v| v.into_array_opt())
    }

    fn extract_object(&mut self, name: &'static str) -> Result<Object> {
        self.extract_field(name).and_then(|v| v.into_object())
    }

    fn extract_object_opt(&mut self, name: &'static str) -> Result<Option<Object>> {
        self.extract_field(name).and_then(|v| v.into_object_opt())
    }

    fn extract_bool(&mut self, name: &'static str) -> Result<bool> {
        self.extract_field(name).and_then(|v| v.to_bool())
    }

    fn extract_bool_opt(&mut self, name: &'static str) -> Result<Option<bool>> {
        self.extract_field(name).and_then(|v| v.to_bool_opt())
    }

    fn extract_i64(&mut self, name: &'static str) -> Result<i64> {
        self.extract_field(name).and_then(|v| v.to_i64())
    }

    fn extract_i64_opt(&mut self, name: &'static str) -> Result<Option<i64>> {
        self.extract_field(name).and_then(|v| v.to_i64_opt())
    }

    fn extract_u64(&mut self, name: &'static str) -> Result<u64> {
        self.extract_field(name).and_then(|v| v.to_u64())
    }

    fn extract_u64_opt(&mut self, name: &'static str) -> Result<Option<u64>> {
        self.extract_field(name).and_then(|v| v.to_u64_opt())
    }

    fn extract_f64(&mut self, name: &'static str) -> Result<f64> {
        self.extract_field(name).and_then(|v| v.to_f64())
    }

    fn extract_f64_opt(&mut self, name: &'static str) -> Result<Option<f64>> {
        self.extract_field(name).and_then(|v| v.to_f64_opt())
    }

    fn extract_number(&mut self, name: &'static str) -> Result<f64> {
        self.extract_field(name).and_then(|v| v.to_number())
    }

    fn extract_number_opt(&mut self, name: &'static str) -> Result<Option<f64>> {
        self.extract_field(name).and_then(|v| v.to_number_opt())
    }
}
