use std::fmt;
use std::fmt::{Debug, Formatter};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: Posn,
    pub end: Posn
}

pub trait Track {
    fn location(&self) -> Option<Span>;
    fn track<U>(&self, other: U) -> Tracked<U> {
        Tracked {
            location: self.location(),
            value: other
        }
    }
}

impl Track for Posn {
    fn location(&self) -> Option<Span> {
        Some(Span {
            start: *self,
            end: *self
        })
    }
}

impl Track for Span {
    fn location(&self) -> Option<Span> {
        Some(*self)
    }
}

impl<T> Track for Option<T>
  where T: Track
{
    fn location(&self) -> Option<Span> {
        match self {
            &Some(ref x) => x.location(),
            &None        => None
        }
    }
}

#[derive(PartialEq, Eq)]
pub struct Tracked<T> {
    pub location: Option<Span>,
    pub value: T
}

impl<T: Debug> Debug for Tracked<T> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.debug_struct("Tracked")
            .field("value", &self.value)
            .finish()
    }
}

pub trait IntoTracked {
    fn tracked(self, Option<Span>) -> Tracked<Self>;
    //fn tracked<T: Track>(self, T) -> Tracked<Self>;
}

impl<T> IntoTracked for T {
    fn tracked(self, location: Option<Span>) -> Tracked<T> {
        Tracked { value: self, location: location }
    }
    // fn tracked<U: Track>(self, track: U) -> Tracked<T> {
    //     Tracked { value: self, location: track.location() }
    // }
}

pub trait Untrack {
    fn untrack(&mut self);
}

impl<T> Untrack for Tracked<T>
  where T: Untrack
{
    fn untrack(&mut self) {
        self.location = None;
        self.value.untrack();
    }
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
        match *self {
            Some(ref mut x) => { x.untrack(); }
            None => { }
        }
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

impl<T> Tracked<T> {
    pub fn map<U, F>(self, op: F) -> Tracked<U>
      where F: FnOnce(T) -> U
    {
        let Tracked { location, value } = self;
        Tracked { location: location, value: op(value) }
    }

    pub fn map_self<U, F>(self, op: F) -> Tracked<U>
      where F: FnOnce(Tracked<T>) -> U
    {
        let location = self.location;
        Tracked { location: location, value: op(self) }

    }
}

impl<T> Track for Tracked<T> {
    fn location(&self) -> Option<Span> {
        self.location
    }
}

pub fn span<T, U>(left: &T, right: &U) -> Option<Span>
  where T: Track,
        U: Track
{
    match (left.location(), right.location()) {
        (Some(l), Some(r)) => Some(Span { start: l.start, end: r.end }),
        _ => None
    }
}
