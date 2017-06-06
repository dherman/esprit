use easter::patt::{ AssignTarget, CompoundPatt, Patt, PropPatt, RestPatt };
use easter::id::{Id, IdExt};
use unjson::ty::Object;
use serde::ser::*;

use id::IntoId;
use result::Result;
use util::*;

pub trait IntoPatt {
    fn into_patt(self) -> Result<Patt<Id>>;
}

impl IntoPatt for Object {
    fn into_patt(self) -> Result<Patt<Id>> {
        self.into_id().map(|id| id.into_patt())
    }
}

impl<'a> Serialize for Serialization<'a, Patt<Id>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::Patt::*;
        match *self.data() {
            Simple(ref t) => Serialization::new(t).serialize(serializer),
            Compound(ref c) => Serialization::new(c).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, Patt<AssignTarget>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::Patt::*;
        match *self.data() {
            Simple(ref t) => Serialization::new(t).serialize(serializer),
            Compound(ref c) => Serialization::new(c).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, AssignTarget> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::AssignTarget::*;
        match *self.data() {
            Id(ref id) => Serialization::new(id).serialize(serializer),
            Dot(_, ref obj, ref key) =>
                json!({
                    "type": "MemberExpression", // FIXME: That looks weird
                    "object": Serialization::new(obj),
                    "property": Serialization::new(key),
                    "computed": false
                }).serialize(serializer),
            Brack(_, ref obj, ref key) =>
                json!({
                    "type": "MemberExpression", // FIXME: That looks weird
                    "object": Serialization::new(obj),
                    "property": Serialization::new(key),
                    "computed": true
                }).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, RestPatt<Id>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        json!({
            "type": "RestElement",
            "argument": Serialization::new(&self.data().patt)
        }).serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, RestPatt<AssignTarget>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        json!({
            "type": "RestElement",
            "argument": Serialization::new(&self.data().patt)
        }).serialize(serializer)
    }
}

impl<'a> Serialize for Serialization<'a, PropPatt<Id>> { // FIXME: Factorize
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::PropPatt::*;
        match *self.data() {
            Regular(_, ref key, ref pat) =>
                tag(json!({
                    "type": "Property",
                    "kind": "init",
                    "method": false,
                    "key": Serialization::new(key),
                    "value": Serialization::new(pat)
                })).serialize(serializer),
            Shorthand(ref id) =>
                tag(json!({
                    "type": "Property",
                    "kind": "init",
                    "method": false,
                    "key": Serialization::new(id),
                    "value": Serialization::new(id)
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, PropPatt<AssignTarget>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::PropPatt::*;
        match *self.data() {
            Regular(_, ref key, ref pat) =>
                tag(json!({
                    "type": "Property",
                    "kind": "init",
                    "method": false,
                    "key": Serialization::new(key),
                    "value": Serialization::new(pat)
                })).serialize(serializer),
            Shorthand(ref id) =>
                tag(json!({
                    "type": "Property",
                    "kind": "init",
                    "method": false,
                    "key": Serialization::new(id),
                    "value": Serialization::new(id)
                })).serialize(serializer),
        }
    }
}

impl<'a> Serialize for Serialization<'a, CompoundPatt<Id>> {
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::CompoundPatt::*;
        match *self.data() {
            Arr(_, ref items, ref rest) => {
                tag(json!({
                    "type": "ArrayPattern",
                    "elements": Serialization::append_one(items, rest)
                })).serialize(serializer)
            }
            Obj(_, ref props) =>
                tag(json!({
                    "type": "ObjectPattern",
                    "properties": Serialization::new(props)
                })).serialize(serializer)
        }
    }
}

impl<'a> Serialize for Serialization<'a, CompoundPatt<AssignTarget>> { // FIXME: Find a way to factorize
    fn serialize<S: Serializer>(&self, serializer: S) -> ::std::result::Result<S::Ok, S::Error> {
        use easter::patt::CompoundPatt::*;
        match *self.data() {
            Arr(_, ref items, ref rest) => {
                tag(json!({
                    "type": "ArrayPattern",
                    "elements": Serialization::append_one(items, rest)
                })).serialize(serializer)
            }
            Obj(_, ref props) =>
                tag(json!({
                    "type": "ObjectPattern",
                    "properties": Serialization::new(props)
                })).serialize(serializer)
        }
    }
}
