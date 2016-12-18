use easter::id::Id;
use unjson::ExtractField;
use unjson::ty::Object;
use joker::word::Name;

use tag::{Tag, TagOf};
use error::{Error, node_type_error};
use result::Result;

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
            name: Name::from(self.extract_string("name").map_err(Error::Json)?)
        })
    }
}
