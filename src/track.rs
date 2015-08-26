use joker::track::{Track, Tracked, Posn, Span, span};
use joker::token::{Token, TokenData};
use easter::punc::Semi;
use parser::Parser;
use error::Error;
use result::Result;
use state::State;

pub trait Tracking {
    fn vec_span<T: Track>(&self, v: &Vec<T>) -> Option<Span>;
    fn posn(&self) -> Posn;
    fn start(&self) -> SpanTracker;
    fn span<F, T>(&mut self, parse: &mut F) -> Result<Tracked<T>>
      where F: FnMut(&mut Self) -> Result<T>;
}

impl<I> Tracking for Parser<I> where I: Iterator<Item=char> {
    fn vec_span<T: Track>(&self, v: &Vec<T>) -> Option<Span> {
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

    fn span<F, T>(&mut self, parse: &mut F) -> Result<Tracked<T>>
      where F: FnMut(&mut Self) -> Result<T>
    {
        let start = self.posn();
        let value = try!(parse(self));
        let end = self.posn();
        Ok(Tracked { value: value, location: Some(Span { start: start, end: end }) })
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
        -> Result<Tracked<T>>
      where I: Iterator<Item=char>,
            F: FnOnce(Semi) -> T
    {
        let before = parser.posn();
        match try!(parser.peek()) {
            &Token { value: TokenData::Semi, location, .. } => {
                parser.reread(TokenData::Semi);
                Ok(Tracked {
                    value: cons(Semi::Explicit(Some(location.start))),
                    location: Some(Span { start: self.start, end: parser.posn() })
                })
            }
            &Token { value: TokenData::RBrace, .. }
          | &Token { value: TokenData::EOF, .. } => {
                Ok(Tracked {
                    value: cons(Semi::Inserted),
                    location: Some(Span { start: self.start, end: before })
                })
            }
            &Token { newline: found_newline, .. } => {
                if newline == Newline::Required && !found_newline {
                    let token = try!(parser.read());
                    return Err(Error::FailedASI(token));
                }
                Ok(Tracked {
                    value: cons(Semi::Inserted),
                    location: Some(Span { start: self.start, end: before })
                })
            }
        }
    }
}
