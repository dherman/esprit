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

#[derive(Debug, PartialEq, Eq)]
pub struct Tracked<T> {
    pub location: Option<Span>,
    pub value: T
}

pub trait IntoTracked {
    fn tracked(self, Option<Span>) -> Tracked<Self>;
}

impl<T> IntoTracked for T {
    fn tracked(self, location: Option<Span>) -> Tracked<T> {
        Tracked { value: self, location: location }
    }
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
