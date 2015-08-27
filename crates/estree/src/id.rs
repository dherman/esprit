use easter::id::{IdData, Id};
use unjson::ExtractField;
use unjson::ty::Object;
use joker::word::Name;
use joker::track::*;

use tag::{Tag, TagOf};
use error::{Error, node_type_error};
use result::Result;

pub trait IntoId {
    fn into_id(self) -> Result<Id>;
}

impl IntoId for Object {
    fn into_id(mut self) -> Result<Id> {
        let tag = try!(self.tag());
        if tag != Tag::Identifier {
            return node_type_error("identifier", tag);
        }
        Ok(IdData {
            name: Name::from(try!(self.extract_string("name").map_err(Error::Json)))
        }.tracked(None))
    }
}
