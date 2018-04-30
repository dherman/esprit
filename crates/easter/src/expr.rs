use std::fmt;
use std::fmt::{Debug, Formatter};
use joker::track::{TrackingRef, TrackingMut, Span, Untrack};
use joker::token::{NumberLiteral, StringLiteral, RegExpLiteral};

use obj::{DotKey, Prop};
use fun::Fun;
use punc::{Unop, Binop, Assop, Logop};
use id::Id;
use patt::{Patt, AssignTarget};

#[derive(PartialEq, Debug, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum ExprListItem {
    Expr(Expr),
    Spread(Option<Span>, Expr)
}

#[derive(Clone, TrackingRef, TrackingMut, Untrack, PartialEq)]
pub enum Expr {
    This(Option<Span>),
    Id(Id),
    Arr(Option<Span>, Vec<Option<ExprListItem>>),
    Obj(Option<Span>, Vec<Prop>),
    Fun(Fun<Option<Id>>),
    Seq(Option<Span>, Vec<Expr>),
    Unop(Option<Span>, Unop, Box<Expr>),
    Binop(Option<Span>, Binop, Box<Expr>, Box<Expr>),
    Logop(Option<Span>, Logop, Box<Expr>, Box<Expr>),
    PreInc(Option<Span>, Box<AssignTarget>),
    PostInc(Option<Span>, Box<AssignTarget>),
    PreDec(Option<Span>, Box<AssignTarget>),
    PostDec(Option<Span>, Box<AssignTarget>),
    Assign(Option<Span>, Patt<AssignTarget>, Box<Expr>),
    BinAssign(Option<Span>, Assop, AssignTarget, Box<Expr>),
    Cond(Option<Span>, Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Option<Span>, Box<Expr>, Vec<ExprListItem>),
    New(Option<Span>, Box<Expr>, Vec<ExprListItem>),
    Dot(Option<Span>, Box<Expr>, DotKey),
    Brack(Option<Span>, Box<Expr>, Box<Expr>),
    NewTarget(Option<Span>),
    True(Option<Span>),
    False(Option<Span>),
    Null(Option<Span>),
    Number(Option<Span>, NumberLiteral),
    RegExp(Option<Span>, RegExpLiteral),
    String(Option<Span>, StringLiteral)
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Expr::This(_)                                   => fmt.write_str("This"),
            &Expr::Id(ref id)                                => fmt.debug_tuple("Id").field(id).finish(),
            &Expr::Arr(_, ref elts)                          => fmt.debug_tuple("Arr").field(elts).finish(),
            &Expr::Obj(_, ref props)                         => fmt.debug_tuple("Obj").field(props).finish(),
            &Expr::Fun(ref fun)                              => fmt.debug_tuple("Fun").field(fun).finish(),
            &Expr::Seq(_, ref exprs)                         => fmt.debug_tuple("Seq").field(exprs).finish(),
            &Expr::Unop(_, ref op, ref arg)                  => fmt.debug_tuple("Unop").field(op).field(arg).finish(),
            &Expr::Binop(_, ref op, ref left, ref right)     => fmt.debug_tuple("Binop").field(op).field(left).field(right).finish(),
            &Expr::Logop(_, ref op, ref left, ref right)     => fmt.debug_tuple("Logop").field(op).field(left).field(right).finish(),
            &Expr::PreInc(_, ref arg)                        => fmt.debug_tuple("PreInc").field(arg).finish(),
            &Expr::PostInc(_, ref arg)                       => fmt.debug_tuple("PostInc").field(arg).finish(),
            &Expr::PreDec(_, ref arg)                        => fmt.debug_tuple("PreDec").field(arg).finish(),
            &Expr::PostDec(_, ref arg)                       => fmt.debug_tuple("PostDec").field(arg).finish(),
            &Expr::Assign(_, ref left, ref right)            => fmt.debug_tuple("Assign").field(left).field(right).finish(),
            &Expr::BinAssign(_, ref op, ref left, ref right) => fmt.debug_tuple("BinAssign").field(op).field(left).field(right).finish(),
            &Expr::Cond(_, ref test, ref cons, ref alt)      => fmt.debug_tuple("Cond").field(test).field(cons).field(alt).finish(),
            &Expr::Call(_, ref callee, ref args)             => fmt.debug_tuple("Call").field(callee).field(args).finish(),
            &Expr::New(_, ref ctor, ref args)                => fmt.debug_tuple("New").field(ctor).field(args).finish(),
            &Expr::Dot(_, ref expr, ref key)                 => fmt.debug_tuple("Dot").field(expr).field(key).finish(),
            &Expr::Brack(_, ref expr, ref prop)              => fmt.debug_tuple("Brack").field(expr).field(prop).finish(),
            &Expr::NewTarget(_)                              => fmt.write_str("NewTarget"),
            &Expr::True(_)                                   => fmt.write_str("True"),
            &Expr::False(_)                                  => fmt.write_str("False"),
            &Expr::Null(_)                                   => fmt.write_str("Null"),
            &Expr::Number(_, ref lit)                        => fmt.debug_tuple("Number").field(lit).finish(),
            &Expr::RegExp(_, ref lit)                        => fmt.debug_tuple("RegExp").field(lit).finish(),
            &Expr::String(_, ref lit)                        => fmt.debug_tuple("String").field(lit).finish()
        }
    }
}
