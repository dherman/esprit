use std::fmt;
use std::fmt::{Display, Formatter};
use joker::track::{Span, TrackingRef};
use expr::Expr;
use patt::{Patt, AssignTarget, CompoundPatt, PropPatt};
use obj::{Prop, PropVal};

#[derive(Debug, PartialEq, Clone)]
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
        Ok(match self {
            Expr::Id(id)                     => Patt::Simple(AssignTarget::Id(id)),
            Expr::Dot(location, obj, key)    => Patt::Simple(AssignTarget::Dot(location, obj, key)),
            Expr::Brack(location, obj, prop) => Patt::Simple(AssignTarget::Brack(location, obj, prop)),
            Expr::Obj(location, props) => {
                let mut prop_patts = Vec::with_capacity(props.len());
                for prop in props {
                    prop_patts.push(try!(prop.into_assign_prop()));
                }
                Patt::Compound(CompoundPatt::Obj(location, prop_patts))
            }
            Expr::Arr(location, exprs) => {
                let mut patts = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    patts.push(match expr {
                        Some(expr) => Some(try!(expr.into_assign_patt())),
                        None => None
                    });
                }
                Patt::Compound(CompoundPatt::Arr(location, patts))
            }
            _ => { return Err(Error::InvalidAssignTarget(*self.tracking_ref())); }
        })
    }
}

pub trait IntoAssignProp {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error>;
}

impl IntoAssignProp for Prop {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error> {
        let location = *self.tracking_ref();
        let key = self.key;
        let patt = match self.val {
            PropVal::Init(expr) => try!(expr.into_assign_patt()),
            _ => { return Err(Error::InvalidPropPatt(*self.val.tracking_ref())); }
        };
        Ok(PropPatt { location: location, key: key, patt: patt })
    }
}
