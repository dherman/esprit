use std::fmt;
use std::fmt::{Display, Formatter};
use joker::track::{Span, IntoTracked};
use expr::{Expr, ExprData};
use patt::{Patt, AssignTarget, AssignTargetData, CompoundPattData, PropPatt, PropPattData};
use obj::{Prop, PropValData};

#[derive(Debug, PartialEq)]

pub enum Error {
    InvalidAssignTarget(Option<Span>),
    InvalidPropPatt(Option<Span>)
}

impl Display for Error {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Error::InvalidAssignTarget(_) => {
                fmt.write_str("invalid assignment pattern")
            }
            &Error::InvalidPropPatt(_) => {
                fmt.write_str("invalid object property in assignment pattern")
            }
        }
    }
}

pub trait IntoAssignPatt {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Error>;
}

impl IntoAssignPatt for Expr {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Error> {
        Ok(match self.value {
            ExprData::Id(id)           => Patt::Simple(AssignTargetData::Id(id).tracked(self.location)),
            ExprData::Dot(obj, key)    => Patt::Simple(AssignTargetData::Dot(obj, key).tracked(self.location)),
            ExprData::Brack(obj, prop) => Patt::Simple(AssignTargetData::Brack(obj, prop).tracked(self.location)),
            ExprData::Obj(props) => {
                let mut prop_patts = Vec::with_capacity(props.len());
                for prop in props {
                    prop_patts.push(try!(prop.into_assign_prop()));
                }
                Patt::Compound(CompoundPattData::Obj(prop_patts).tracked(self.location))
            }
            ExprData::Arr(exprs) => {
                let mut patts = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    patts.push(match expr {
                        Some(expr) => Some(try!(expr.into_assign_patt())),
                        None => None
                    });
                }
                Patt::Compound(CompoundPattData::Arr(patts).tracked(self.location))
            }
            _ => { return Err(Error::InvalidAssignTarget(self.location)); }
        })
    }
}

pub trait IntoAssignProp {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error>;
}

impl IntoAssignProp for Prop {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error> {
        let key = self.value.key;
        let patt = match self.value.val.value {
            PropValData::Init(expr) => try!(expr.into_assign_patt()),
            _ => { return Err(Error::InvalidPropPatt(self.value.val.location)); }
        };
        Ok(PropPattData { key: key, patt: patt }.tracked(self.location))
    }
}
