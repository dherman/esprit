#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Posn {
    pub offset: u32,
    pub line: u32,
    pub column: u32
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Span {
    pub start: Posn,
    pub end: Posn
}

// FIXME: rename to HasLoc
pub trait HasSpan {
    // FIXME: rename to loc
    fn span(&self) -> Option<Span>;
}

impl HasSpan for Posn {
    fn span(&self) -> Option<Span> {
        Some(Span {
            start: *self,
            end: *self
        })
    }
}

impl HasSpan for Span {
    fn span(&self) -> Option<Span> {
        Some(*self)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Loc<T> {
    pub span: Option<Span>,
    pub data: T
}

impl<T> Loc<T> {
    pub fn erase(mut self) -> Self {
        self.span = None;
        self
    }
}

pub trait IntoLoc {
    fn into_loc(self, Option<Span>) -> Loc<Self>;
    fn into_empty_loc(self) -> Loc<Self>;
}

impl<T> IntoLoc for T {
    fn into_loc(self, span: Option<Span>) -> Loc<T> {
        Loc { data: self, span: span }
    }

    fn into_empty_loc(self) -> Loc<T> {
        Loc { data: self, span: None }
    }
}

pub trait EraseLoc {
    fn erase_loc(self) -> Self;
}

impl<T> EraseLoc for Loc<T>
  where T: EraseLoc
{
    fn erase_loc(self) -> Self {
        Loc {
            span: None,
            data: self.data.erase_loc()
        }
    }
}

impl<T> EraseLoc for Box<T>
  where T: EraseLoc
{
    fn erase_loc(self) -> Self {
        Box::new((*self).erase_loc())
    }
}

impl<T> EraseLoc for Option<T>
  where T: EraseLoc
{
    fn erase_loc(self) -> Self {
        self.map(|x| x.erase_loc())
    }
}

impl<T> EraseLoc for Vec<T>
  where T: EraseLoc
{
    fn erase_loc(self) -> Self {
        self.into_iter()
            .map(|x| x.erase_loc())
            .collect()
    }
}

impl<T> Loc<T> {
    pub fn map<U, F>(self, op: F) -> Loc<U>
      where F: FnOnce(T) -> U
    {
        let Loc { span, data } = self;
        Loc { span: span, data: op(data) }
    }
}

impl<T> HasSpan for Loc<T> {
    fn span(&self) -> Option<Span> {
        self.span
    }
}

pub fn span<T, U>(left: &T, right: &U) -> Option<Span>
  where T: HasSpan,
        U: HasSpan
{
    match (left.span(), right.span()) {
        (Some(l), Some(r)) => Some(Span { start: l.start, end: r.end }),
        _ => None
    }
}
