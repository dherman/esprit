use serde_json::Value;
use serde::ser::*;

pub enum Container {
    NothingMeaningful,
    FunctionDeclaration,
    FunctionExpression,
}

/// A container used to represent some piece of data being
/// serialized. Future versions may add options to the
/// serialization format.
pub struct Serialization<'a, T> where T: 'a {
    data: &'a T,
    container: Container,
}
impl<'a, T> Serialization<'a, T> where T: 'a {
    pub fn new(data: &'a T) -> Self {
        Serialization {
            data,
            container: Container::NothingMeaningful
        }
    }
    pub fn in_context(data: &'a T, context: Container) -> Self {
        Serialization {
            data,
            container: context
        }
    }
    pub fn data(&self) -> &'a T {
        self.data
    }
    pub fn container(&self) -> &Container {
        &self.container
    }
}
impl<'a, U, V> Serialization<'a, SerializationAppendOne<'a, U, V>> {
    pub fn append_one(first: &'a [U], second: &'a Option<V>) -> SerializationAppendOne<'a, U, V> {
        SerializationAppendOne {
            first,
            second,
        }
    }
}

pub struct SerializationAppendOne<'a, U, V> where U: 'a, V: 'a {
    first: &'a [U],
    second: &'a Option<V>,
}

impl<'a, U, V> Serialize for SerializationAppendOne<'a, U, V> where Serialization<'a, U>: Serialize, Serialization<'a, V>: Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.first.len() + if self.second.is_some() { 1 } else { 0 }))?;
        for item in self.first {
            seq.serialize_element(&Serialization::new(item))?;
        }
        if let Some(ref rest) = *self.second {
            seq.serialize_element(&Serialization::new(rest))?;
        }
        seq.end()
    }
}

/// `Vec<T>` is serialized as an array of T
impl<'a, T> Serialize for Serialization<'a, Vec<T>> where Serialization<'a, T>: Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        let mut seq = serializer.serialize_seq(Some(self.data().len()))?;
        for item in self.data() {
            seq.serialize_element(&Serialization::new(item))?;
        }
        seq.end()
    }
}

/// `Box<T>` is serialized as `&T`
impl<'a, T> Serialize for Serialization<'a, Box<T>> where Serialization<'a, T>: Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        Serialization::new(self.data().as_ref()).serialize(serializer)
    }
}

impl<'a, T> Serialize for Serialization<'a, Option<T>> where Serialization<'a, T>: Serialize {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        match *self.data() {
            None => serializer.serialize_none(),
            Some(ref data) => Serialization::new(data).serialize(serializer)
        }
    }
}

pub fn tag(mut v: Value) -> Value {
    {
        let mut obj = v.as_object_mut().unwrap();
        obj.insert("loc".to_string(), Value::Null);
    }
    v
}