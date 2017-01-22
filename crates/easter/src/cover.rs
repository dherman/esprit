use std::fmt;
use std::fmt::{Display, Formatter};
use joker::track::{Span, TrackingRef};
use expr::{Expr, ExprListItem};
use patt::{Patt, RestPatt, AssignTarget, CompoundPatt, PropPatt};
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

pub trait IntoAssignTarget {
    fn into_assign_target(self) -> Result<AssignTarget, Error>;
}

impl IntoAssignTarget for Expr {
    fn into_assign_target(self) -> Result<AssignTarget, Error> {
        Ok(match self {
            Expr::Id(id)                     => AssignTarget::Id(id),
            Expr::Dot(location, obj, key)    => AssignTarget::Dot(location, obj, key),
            Expr::Brack(location, obj, prop) => AssignTarget::Brack(location, obj, prop),
            _ => { return Err(Error::InvalidAssignTarget(*self.tracking_ref())); }
        })
    }
}

pub trait IntoAssignPatt : IntoAssignTarget {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Error>;
}

impl IntoAssignPatt for Expr {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Error> {
        Ok(match self {
            Expr::Obj(location, props) => {
                let mut prop_patts = Vec::with_capacity(props.len());
                for prop in props {
                    prop_patts.push(prop.into_assign_prop()?);
                }
                Patt::Compound(CompoundPatt::Obj(location, prop_patts))
            }
            Expr::Arr(location, mut exprs) => {
                let mut patts = Vec::with_capacity(exprs.len());
                let mut rest = None;
                if let Some(last) = exprs.pop() {
                    if let Some(ExprListItem::Spread(None, expr)) = last {
                        rest = Some(Box::new(RestPatt {
                            location: None,
                            patt: expr.into_assign_patt()?
                        }));
                    } else {
                        exprs.push(last);
                    }
                }
                for expr in exprs {
                    patts.push(match expr {
                        Some(ExprListItem::Expr(expr)) => Some(expr.into_assign_patt()?),
                        Some(ExprListItem::Spread(loc, _)) => { return Err(Error::InvalidAssignTarget(loc)); }
                        None => None
                    });
                }
                Patt::Compound(CompoundPatt::Arr(location, patts, rest))
            }
            _ => { return self.into_assign_target().map(Patt::Simple); }
        })
    }
}

pub trait IntoAssignProp {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error>;
}

impl IntoAssignProp for Prop {
    fn into_assign_prop(self) -> Result<PropPatt<AssignTarget>, Error> {
        let location = *self.tracking_ref();
        Ok(match self {
            Prop::Regular(location, key, PropVal::Init(expr)) => {
                PropPatt::Regular(location, key, expr.into_assign_patt()?)
            }
            Prop::Shorthand(id) => {
                PropPatt::Shorthand(id)
            }
            _ => { return Err(Error::InvalidPropPatt(location)); }
        })
    }
}
