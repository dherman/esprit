use std::fmt::{Debug, Formatter, Result};

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Posn {
    pub offset: u32,
    pub line: u32,
    pub column: u32
}

impl Posn {
    pub fn origin() -> Posn {
        Posn {
            offset: 0,
            line: 0,
            column: 0
        }
    }
}

impl Debug for Posn {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        fmt.write_fmt(format_args!("{}:{}", self.line + 1, self.column + 1))
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: Posn,
    pub end: Posn
}

impl Debug for Span {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        fmt.write_fmt(format_args!("{:?}..{:?}", self.start, self.end))
    }
}

pub trait TrackingRef {
    fn tracking_ref(&self) -> &Option<Span>;
}

pub trait TrackingMut: TrackingRef {
    fn tracking_mut(&mut self) -> &mut Option<Span>;
}

impl<'a, T: TrackingRef> TrackingRef for &'a T {
    fn tracking_ref(&self) -> &Option<Span> { (*self).tracking_ref() }
}

impl TrackingRef for Option<Span> {
    fn tracking_ref(&self) -> &Option<Span> { self }
}

impl TrackingMut for Option<Span> {
    fn tracking_mut(&mut self) -> &mut Option<Span> { self }
}

pub trait Untrack {
    fn untrack(&mut self);
}

impl<T> Untrack for Box<T>
  where T: Untrack
{
    fn untrack(&mut self) {
        (**self).untrack();
    }
}

impl<T> Untrack for Option<T>
  where T: Untrack
{
    fn untrack(&mut self) {
        if let Some(ref mut x) = *self {
            x.untrack();
        }
    }
}

impl Untrack for Option<Span> {
    fn untrack(&mut self) {
        *self = None;
    }
}

impl<T> Untrack for Vec<T>
  where T: Untrack
{
    fn untrack(&mut self) {
        for x in self {
            x.untrack();
        }
    }
}

pub fn span<T, U>(left: &T, right: &U) -> Option<Span>
  where T: TrackingRef,
        U: TrackingRef
{
    match (*left.tracking_ref(), *right.tracking_ref()) {
        (Some(l), Some(r)) => Some(Span { start: l.start, end: r.end }),
        _ => None
    }
}
