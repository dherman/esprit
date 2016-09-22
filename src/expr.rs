use joker::track::{Span, span};
use joker::token::Token;
use easter::punc::Unop;
use easter::expr::Expr;
use easter::obj::DotKey;

pub enum Prefix {
    Unop(Unop),
    Inc(Span),
    Dec(Span)
}

pub enum Postfix {
    Inc(Span),
    Dec(Span)
}

pub enum Deref {
    Brack(Expr, Token),
    Dot(DotKey)
}

impl Deref {
    pub fn append_to(self, expr: Expr) -> Expr {
        match self {
            Deref::Brack(deref, end) => {
                Expr::Brack(span(&expr, &Some(end.location)), Box::new(expr), Box::new(deref))
            }
            Deref::Dot(key) => {
                Expr::Dot(span(&expr, &key), Box::new(expr), key)
            }
        }
    }
}

pub enum Suffix {
    Deref(Deref),
    Arguments(Arguments)
}

pub struct Arguments {
    pub args: Vec<Expr>,
    pub end: Token
}

impl Arguments {
    pub fn append_to(self, expr: Expr) -> Expr {
        Expr::Call(span(&expr, &Some(self.end.location)), Box::new(expr), self.args)
    }

    pub fn append_to_new(self, new: Token, expr: Expr) -> Expr {
        Expr::New(span(&Some(new.location), &Some(self.end.location)), Box::new(expr), Some(self.args))
    }
}

impl Suffix {
    pub fn append_to(self, expr: Expr) -> Expr {
        match self {
            Suffix::Deref(deref) => deref.append_to(expr),
            Suffix::Arguments(args) => args.append_to(expr)
        }
    }
}
