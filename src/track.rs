use joker::track::{TrackingRef, TrackingMut, Posn, Span, span};
use joker::token::{Token, TokenData};
use easter::punc::Semi;
use parser::Parser;
use error::Error;
use result::Result;
use state::State;

pub trait Tracking {
    fn vec_span<T: TrackingRef>(&self, v: &Vec<T>) -> Option<Span>;
    fn posn(&self) -> Posn;
    fn start(&self) -> SpanTracker;
    fn span<F, T>(&mut self, parse: &mut F) -> Result<T>
      where F: FnMut(&mut Self) -> Result<T>,
            T: TrackingMut;
}

impl<I> Tracking for Parser<I> where I: Iterator<Item=char> {
    fn vec_span<T: TrackingRef>(&self, v: &Vec<T>) -> Option<Span> {
        let len = v.len();
        if len == 0 {
            let here = self.posn();
            return Some(Span { start: here, end: here });
        }
        span(&v[0], &v[len - 1])
    }

    fn posn(&self) -> Posn {
        self.lexer.posn()
    }

    fn start(&self) -> SpanTracker {
        SpanTracker { start: self.posn() }
    }

    fn span<F, T>(&mut self, parse: &mut F) -> Result<T>
      where F: FnMut(&mut Self) -> Result<T>,
            T: TrackingMut
    {
        let start = self.posn();
        let mut value = parse(self)?;
        let end = self.posn();
        *value.tracking_mut() = Some(Span { start: start, end: end });
        Ok(value)
    }
}

#[derive(Eq, PartialEq)]
pub enum Newline {
    Required,
    Optional
}

pub struct SpanTracker {
    start: Posn
}

impl SpanTracker {
/*
    pub fn end<I, T>(&self, parser: &Parser<I>, value: T) -> Tracked<T>
      where I: Iterator<Item=char>
    {
        Tracked { value: value, location: Some(Span { start: self.start, end: parser.posn() }) }
    }
*/

    pub fn end_with_auto_semi<I, T, F>(&self, parser: &mut Parser<I>, newline: Newline, cons: F)
        -> Result<T>
      where I: Iterator<Item=char>,
            F: FnOnce(Semi) -> T,
            T: TrackingMut
    {
        let before = parser.posn();
        match parser.peek()? {
            &Token { value: TokenData::Semi, location, .. } => {
                parser.reread(TokenData::Semi);
                let mut result = cons(Semi::Explicit(Some(location.start)));
                *result.tracking_mut() = Some(Span { start: self.start, end: parser.posn() });
                Ok(result)
            }
            &Token { value: TokenData::RBrace, .. }
          | &Token { value: TokenData::EOF, .. } => {
                let mut result = cons(Semi::Inserted);
                *result.tracking_mut() = Some(Span { start: self.start, end: before });
                Ok(result)
            }
            &Token { newline: found_newline, .. } => {
                if newline == Newline::Required && !found_newline {
                    let token = parser.read()?;
                    return Err(Error::FailedASI(token));
                }
                let mut result = cons(Semi::Inserted);
                *result.tracking_mut() = Some(Span { start: self.start, end: before });
                Ok(result)
            }
        }
    }
}
