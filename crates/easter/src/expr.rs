use std::fmt;
use std::fmt::{Debug, Formatter};
use joker::track::{TrackingRef, TrackingMut, Span, Untrack};
use joker::token::{NumberLiteral, StringLiteral, RegExpLiteral};

use obj::{DotKey, Prop};
use fun::Fun;
use punc::{Unop, Binop, Assop, Logop};
use id::Id;
use patt::{Patt, AssignTarget};

#[derive(Clone)]
pub enum Expr {
    This(Option<Span>),
    Id(Id),
    Arr(Option<Span>, Vec<Option<Expr>>),
    Obj(Option<Span>, Vec<Prop>),
    Fun(Fun),
    Seq(Option<Span>, Vec<Expr>),
    Unop(Option<Span>, Unop, Box<Expr>),
    Binop(Option<Span>, Binop, Box<Expr>, Box<Expr>),
    Logop(Option<Span>, Logop, Box<Expr>, Box<Expr>),
    PreInc(Option<Span>, Box<AssignTarget>),
    PostInc(Option<Span>, Box<AssignTarget>),
    PreDec(Option<Span>, Box<AssignTarget>),
    PostDec(Option<Span>, Box<AssignTarget>),
    Assign(Option<Span>, Assop, Patt<AssignTarget>, Box<Expr>),
    Cond(Option<Span>, Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Option<Span>, Box<Expr>, Vec<Expr>),
    New(Option<Span>, Box<Expr>, Option<Vec<Expr>>),
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

impl TrackingRef for Expr {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Expr::This(ref location)
          | Expr::Arr(ref location, _)
          | Expr::Obj(ref location, _)
          | Expr::Seq(ref location, _)
          | Expr::Unop(ref location, _, _)
          | Expr::Binop(ref location, _, _, _)
          | Expr::Logop(ref location, _, _, _)
          | Expr::PreInc(ref location, _)
          | Expr::PostInc(ref location, _)
          | Expr::PreDec(ref location, _)
          | Expr::PostDec(ref location, _)
          | Expr::Assign(ref location, _, _, _)
          | Expr::Cond(ref location, _, _, _)
          | Expr::Call(ref location, _, _)
          | Expr::New(ref location, _, _)
          | Expr::Dot(ref location, _, _)
          | Expr::Brack(ref location, _, _)
          | Expr::NewTarget(ref location)
          | Expr::True(ref location)
          | Expr::False(ref location)
          | Expr::Null(ref location)
          | Expr::Number(ref location, _)
          | Expr::RegExp(ref location, _)
          | Expr::String(ref location, _) => location,
            Expr::Id(ref id) => id.tracking_ref(),
            Expr::Fun(ref fun) => fun.tracking_ref()
        }
    }
}

impl TrackingMut for Expr {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Expr::This(ref mut location)
          | Expr::Arr(ref mut location, _)
          | Expr::Obj(ref mut location, _)
          | Expr::Seq(ref mut location, _)
          | Expr::Unop(ref mut location, _, _)
          | Expr::Binop(ref mut location, _, _, _)
          | Expr::Logop(ref mut location, _, _, _)
          | Expr::PreInc(ref mut location, _)
          | Expr::PostInc(ref mut location, _)
          | Expr::PreDec(ref mut location, _)
          | Expr::PostDec(ref mut location, _)
          | Expr::Assign(ref mut location, _, _, _)
          | Expr::Cond(ref mut location, _, _, _)
          | Expr::Call(ref mut location, _, _)
          | Expr::New(ref mut location, _, _)
          | Expr::Dot(ref mut location, _, _)
          | Expr::Brack(ref mut location, _, _)
          | Expr::NewTarget(ref mut location)
          | Expr::True(ref mut location)
          | Expr::False(ref mut location)
          | Expr::Null(ref mut location)
          | Expr::Number(ref mut location, _)
          | Expr::RegExp(ref mut location, _)
          | Expr::String(ref mut location, _) => location,
            Expr::Id(ref mut id) => id.tracking_mut(),
            Expr::Fun(ref mut fun) => fun.tracking_mut()
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        if *self.tracking_ref() != *other.tracking_ref() {
            return false;
        }
        match (self, other) {
            (&Expr::This(_),                      &Expr::This(_))                      => true,
            (&Expr::Id(ref id_l),                 &Expr::Id(ref id_r))                 => id_l == id_r,
            (&Expr::Arr(_, ref elts_l),           &Expr::Arr(_, ref elts_r))           => elts_l == elts_r,
            (&Expr::Obj(_, ref props_l),          &Expr::Obj(_, ref props_r))          => props_l == props_r,
            (&Expr::Fun(ref fun_l),               &Expr::Fun(ref fun_r))               => fun_l == fun_r,
            (&Expr::Seq(_, ref exprs_l),          &Expr::Seq(_, ref exprs_r))          => exprs_l == exprs_r,
            (&Expr::Unop(_, ref op_l, ref arg_l), &Expr::Unop(_, ref op_r, ref arg_r)) => (op_l, arg_l) == (op_r, arg_r),
            (&Expr::Binop(_, ref op_l, ref arg1_l, ref arg2_l),
             &Expr::Binop(_, ref op_r, ref arg1_r, ref arg2_r))                        => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&Expr::Logop(_, ref op_l, ref arg1_l, ref arg2_l),
             &Expr::Logop(_, ref op_r, ref arg1_r, ref arg2_r))                        => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&Expr::PreInc(_, ref arg_l),         &Expr::PreInc(_, ref arg_r))
          | (&Expr::PostInc(_, ref arg_l),        &Expr::PostInc(_, ref arg_r))
          | (&Expr::PreDec(_, ref arg_l),         &Expr::PreDec(_, ref arg_r))
          | (&Expr::PostDec(_, ref arg_l),        &Expr::PostDec(_, ref arg_r))        => arg_l == arg_r,
            (&Expr::Assign(_, ref op_l, ref patt_l, ref arg_l),
             &Expr::Assign(_, ref op_r, ref patt_r, ref arg_r))                        => (op_l, patt_l, arg_l) == (op_r, patt_r, arg_r),
            (&Expr::Cond(_, ref test_l, ref cons_l, ref alt_l),
             &Expr::Cond(_, ref test_r, ref cons_r, ref alt_r))                        => (test_l, cons_l, alt_l) == (test_r, cons_r, alt_r),
            (&Expr::Call(_, ref callee_l, ref args_l),
             &Expr::Call(_, ref callee_r, ref args_r))                                 => (callee_l, args_l) == (callee_r, args_r),
            (&Expr::New(_, ref callee_l, None),   &Expr::New(_, ref callee_r, None))   => callee_l == callee_r,
            (&Expr::New(_, ref callee_l, None),   &Expr::New(_, ref callee_r, Some(ref args)))
          | (&Expr::New(_, ref callee_l, Some(ref args)),
             &Expr::New(_, ref callee_r, None))                                        => (callee_l == callee_r) && args.is_empty(),
            (&Expr::New(_, ref callee_l, Some(ref args_l)),
             &Expr::New(_, ref callee_r, Some(ref args_r)))                            => (callee_l, args_l) == (callee_r, args_r),
            (&Expr::Dot(_, ref obj_l, ref key_l), &Expr::Dot(_, ref obj_r, ref key_r)) => (obj_l, key_l) == (obj_r, key_r),
            (&Expr::Brack(_, ref obj_l, ref prop_l),
             &Expr::Brack(_, ref obj_r, ref prop_r))                                   => (obj_l, prop_l) == (obj_r, prop_r),
            (&Expr::NewTarget(_),          &Expr::NewTarget(_))                        => true,
            (&Expr::True(_),               &Expr::True(_))                             => true,
            (&Expr::False(_),              &Expr::False(_))                            => true,
            (&Expr::Null(_),               &Expr::Null(_))                             => true,
            (&Expr::Number(_, ref lit_l),  &Expr::Number(_, ref lit_r))                => lit_l == lit_r,
            (&Expr::RegExp(_, ref lit_l),  &Expr::RegExp(_, ref lit_r))                => lit_l == lit_r,
            (&Expr::String(_, ref lit_l),  &Expr::String(_, ref lit_r))                => lit_l == lit_r,
            _ => false
        }
    }
}

impl Debug for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &Expr::This(_)                                => fmt.write_str("This"),
            &Expr::Id(ref id)                             => fmt.debug_tuple("Id").field(id).finish(),
            &Expr::Arr(_, ref elts)                       => fmt.debug_tuple("Arr").field(elts).finish(),
            &Expr::Obj(_, ref props)                      => fmt.debug_tuple("Obj").field(props).finish(),
            &Expr::Fun(ref fun)                           => fmt.debug_tuple("Fun").field(fun).finish(),
            &Expr::Seq(_, ref exprs)                      => fmt.debug_tuple("Seq").field(exprs).finish(),
            &Expr::Unop(_, ref op, ref arg)               => fmt.debug_tuple("Unop").field(op).field(arg).finish(),
            &Expr::Binop(_, ref op, ref left, ref right)  => fmt.debug_tuple("Binop").field(op).field(left).field(right).finish(),
            &Expr::Logop(_, ref op, ref left, ref right)  => fmt.debug_tuple("Logop").field(op).field(left).field(right).finish(),
            &Expr::PreInc(_, ref arg)                     => fmt.debug_tuple("PreInc").field(arg).finish(),
            &Expr::PostInc(_, ref arg)                    => fmt.debug_tuple("PostInc").field(arg).finish(),
            &Expr::PreDec(_, ref arg)                     => fmt.debug_tuple("PreDec").field(arg).finish(),
            &Expr::PostDec(_, ref arg)                    => fmt.debug_tuple("PostDec").field(arg).finish(),
            &Expr::Assign(_, ref op, ref left, ref right) => fmt.debug_tuple("Assign").field(op).field(left).field(right).finish(),
            &Expr::Cond(_, ref test, ref cons, ref alt)   => fmt.debug_tuple("Cond").field(test).field(cons).field(alt).finish(),
            &Expr::Call(_, ref callee, ref args)          => fmt.debug_tuple("Call").field(callee).field(args).finish(),
            &Expr::New(_, ref ctor, None) => {
                let args: Vec<Expr> = vec![];
                fmt.debug_tuple("New").field(ctor).field(&args).finish()
            }
            &Expr::New(_, ref ctor, Some(ref args))       => fmt.debug_tuple("New").field(ctor).field(args).finish(),
            &Expr::Dot(_, ref expr, ref key)              => fmt.debug_tuple("Dot").field(expr).field(key).finish(),
            &Expr::Brack(_, ref expr, ref prop)           => fmt.debug_tuple("Brack").field(expr).field(prop).finish(),
            &Expr::NewTarget(_)                           => fmt.write_str("NewTarget"),
            &Expr::True(_)                                => fmt.write_str("True"),
            &Expr::False(_)                               => fmt.write_str("False"),
            &Expr::Null(_)                                => fmt.write_str("Null"),
            &Expr::Number(_, ref lit)                     => fmt.debug_tuple("Number").field(lit).finish(),
            &Expr::RegExp(_, ref lit)                     => fmt.debug_tuple("RegExp").field(lit).finish(),
            &Expr::String(_, ref lit)                     => fmt.debug_tuple("String").field(lit).finish()
        }
    }
}

impl Untrack for Expr {
    fn untrack(&mut self) {
        *self.tracking_mut() = None;
        match *self {
            Expr::This(_)                                           => { }
            Expr::Id(ref mut id)                                    => { id.untrack(); }
            Expr::Arr(_, ref mut exprs)                             => { exprs.untrack(); }
            Expr::Obj(_, ref mut props)                             => { props.untrack(); }
            Expr::Fun(ref mut fun)                                  => { fun.untrack(); }
            Expr::Seq(_, ref mut exprs)                             => { exprs.untrack(); }
            Expr::Unop(_, ref mut op, ref mut expr)                 => { op.untrack(); expr.untrack(); }
            Expr::Binop(_, ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            Expr::Logop(_, ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            Expr::PreInc(_, ref mut expr)                           => { expr.untrack(); }
            Expr::PostInc(_, ref mut expr)                          => { expr.untrack(); }
            Expr::PreDec(_, ref mut expr)                           => { expr.untrack(); }
            Expr::PostDec(_, ref mut expr)                          => { expr.untrack(); }
            Expr::Assign(_, ref mut op, ref mut patt, ref mut expr) => { op.untrack(); patt.untrack(); expr.untrack(); }
            Expr::Cond(_, ref mut test, ref mut cons, ref mut alt)  => { test.untrack(); cons.untrack(); alt.untrack(); }
            Expr::Call(_, ref mut callee, ref mut args)             => { callee.untrack(); args.untrack(); }
            Expr::New(_, ref mut ctor, ref mut args)                => { ctor.untrack(); args.untrack(); }
            Expr::Dot(_, ref mut obj, ref mut key)                  => { obj.untrack(); key.untrack(); }
            Expr::Brack(_, ref mut obj, ref mut prop)               => { obj.untrack(); prop.untrack(); }
            Expr::NewTarget(_)                                      => { }
            Expr::True(_)                                           => { }
            Expr::False(_)                                          => { }
            Expr::Null(_)                                           => { }
            Expr::Number(_, _)                                      => { }
            Expr::RegExp(_, _)                                      => { }
            Expr::String(_, _)                                      => { }
        }
    }
}
