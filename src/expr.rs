use joker::track::{Span, span, IntoTracked};
use joker::token::Token;
use easter::punc::Unop;
use easter::expr::{ExprData, Expr};
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
                let location = span(&expr, &end);
                ExprData::Brack(Box::new(expr), Box::new(deref)).tracked(location)
            }
            Deref::Dot(key) => {
                let location = span(&expr, &key);
                ExprData::Dot(Box::new(expr), key).tracked(location)
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
        let location = span(&expr, &self.end);
        ExprData::Call(Box::new(expr), self.args).tracked(location)
    }

    pub fn append_to_new(self, new: Token, expr: Expr) -> Expr {
        let location = span(&new, &self.end);
        ExprData::New(Box::new(expr), Some(self.args)).tracked(location)
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
