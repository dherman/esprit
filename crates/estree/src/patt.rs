use easter::patt::Patt;
use easter::id::IdExt;
use unjson::ty::Object;

use id::IntoId;
use result::{Result, Map};

pub trait IntoPatt {
    fn into_patt(self) -> Result<Patt>;
}

impl IntoPatt for Object {
    fn into_patt(self) -> Result<Patt> {
        self.into_id().map(|id| id.into_patt())
    }
}
