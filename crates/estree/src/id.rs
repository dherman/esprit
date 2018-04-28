use easter::id::Id;
use unjson::ExtractField;
use unjson::ty::Object;
use joker::word::Name;
use serde::ser::*;

use tag::{Tag, TagOf};
use error::{node_type_error};
use result::Result;
use util::*;

pub trait IntoId {
    fn into_id(self) -> Result<Id>;
}

impl IntoId for Object {
    fn into_id(mut self) -> Result<Id> {
        let tag = self.tag()?;
        if tag != Tag::Identifier {
            return node_type_error("identifier", tag);
        }
        Ok(Id {
            location: None,
            name: Name::from(self.extract_string("name")?)
        })
    }
}

impl<'a> Serialize for Serialization<'a, Id> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        serializer.serialize_str(self.data().name.as_ref())
    }
}
