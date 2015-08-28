use std::fmt;
use std::fmt::{Debug, Formatter};
use joker::track::*;
use joker::token::{NumberLiteral, StringLiteral};

use obj::{DotKey, Prop, IntoAssignProp};
use fun::Fun;
use punc::{Unop, Binop, Assop, Logop};
use id::Id;
use patt::{Patt, AssignTarget, AssignTargetData, CompoundPattData};

pub enum ExprData {
    This,
    Id(Id),
    Arr(Vec<Option<Expr>>),
    Obj(Vec<Prop>),
    Fun(Fun),
    Seq(Vec<Expr>),
    Unop(Unop, Box<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Logop(Logop, Box<Expr>, Box<Expr>),
    PreInc(Box<Expr>),
    PostInc(Box<Expr>),
    PreDec(Box<Expr>),
    PostDec(Box<Expr>),
    Assign(Assop, Patt<AssignTarget>, Box<Expr>),
    Cond(Box<Expr>, Box<Expr>, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    New(Box<Expr>, Option<Vec<Expr>>),
    Dot(Box<Expr>, DotKey),
    Brack(Box<Expr>, Box<Expr>),
    NewTarget,
    True,
    False,
    Null,
    Number(NumberLiteral),
    RegExp(String, Vec<char>),
    String(StringLiteral)
}

impl PartialEq for ExprData {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&ExprData::This,                      &ExprData::This)                      => true,
            (&ExprData::Id(ref id_l),              &ExprData::Id(ref id_r))              => id_l == id_r,
            (&ExprData::Arr(ref elts_l),           &ExprData::Arr(ref elts_r))           => elts_l == elts_r,
            (&ExprData::Obj(ref props_l),          &ExprData::Obj(ref props_r))          => props_l == props_r,
            (&ExprData::Fun(ref fun_l),            &ExprData::Fun(ref fun_r))            => fun_l == fun_r,
            (&ExprData::Seq(ref exprs_l),          &ExprData::Seq(ref exprs_r))          => exprs_l == exprs_r,
            (&ExprData::Unop(ref op_l, ref arg_l), &ExprData::Unop(ref op_r, ref arg_r)) => (op_l, arg_l) == (op_r, arg_r),
            (&ExprData::Binop(ref op_l, ref arg1_l, ref arg2_l),
             &ExprData::Binop(ref op_r, ref arg1_r, ref arg2_r))                         => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&ExprData::Logop(ref op_l, ref arg1_l, ref arg2_l),
             &ExprData::Logop(ref op_r, ref arg1_r, ref arg2_r))                         => (op_l, arg1_l, arg2_l) == (op_r, arg1_r, arg2_r),
            (&ExprData::PreInc(ref arg_l),         &ExprData::PreInc(ref arg_r))
          | (&ExprData::PostInc(ref arg_l),        &ExprData::PostInc(ref arg_r))
          | (&ExprData::PreDec(ref arg_l),         &ExprData::PreDec(ref arg_r))
          | (&ExprData::PostDec(ref arg_l),        &ExprData::PostDec(ref arg_r))        => arg_l == arg_r,
            (&ExprData::Assign(ref op_l, ref patt_l, ref arg_l),
             &ExprData::Assign(ref op_r, ref patt_r, ref arg_r))                         => (op_l, patt_l, arg_l) == (op_r, patt_r, arg_r),
            (&ExprData::Cond(ref test_l, ref cons_l, ref alt_l),
             &ExprData::Cond(ref test_r, ref cons_r, ref alt_r))                         => (test_l, cons_l, alt_l) == (test_r, cons_r, alt_r),
            (&ExprData::Call(ref callee_l, ref args_l),
             &ExprData::Call(ref callee_r, ref args_r))                                  => (callee_l, args_l) == (callee_r, args_r),
            (&ExprData::New(ref callee_l, None),   &ExprData::New(ref callee_r, None))   => callee_l == callee_r,
            (&ExprData::New(ref callee_l, None),   &ExprData::New(ref callee_r, Some(ref args)))
          | (&ExprData::New(ref callee_l, Some(ref args)),
             &ExprData::New(ref callee_r, None))                                         => (callee_l == callee_r) && args.is_empty(),
            (&ExprData::New(ref callee_l, Some(ref args_l)),
             &ExprData::New(ref callee_r, Some(ref args_r)))                             => (callee_l, args_l) == (callee_r, args_r),
            (&ExprData::Dot(ref obj_l, ref key_l), &ExprData::Dot(ref obj_r, ref key_r)) => (obj_l, key_l) == (obj_r, key_r),
            (&ExprData::Brack(ref obj_l, ref prop_l),
             &ExprData::Brack(ref obj_r, ref prop_r))                                    => (obj_l, prop_l) == (obj_r, prop_r),
            (&ExprData::NewTarget,          &ExprData::NewTarget)                        => true,
            (&ExprData::True,               &ExprData::True)                             => true,
            (&ExprData::False,              &ExprData::False)                            => true,
            (&ExprData::Null,               &ExprData::Null)                             => true,
            (&ExprData::Number(ref lit_l),  &ExprData::Number(ref lit_r))                => lit_l.value() == lit_r.value(),
            (&ExprData::RegExp(ref src_l, ref flags_l),
             &ExprData::RegExp(ref src_r, ref flags_r))                                  => (src_l, flags_l) == (src_r, flags_r),
            (&ExprData::String(ref lit_l),  &ExprData::String(ref lit_r))                => lit_l == lit_r,
            _ => false
        }
    }
}

impl Debug for ExprData {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match self {
            &ExprData::This                                => fmt.write_str("This"),
            &ExprData::Id(ref id)                          => fmt.debug_tuple("Id").field(id).finish(),
            &ExprData::Arr(ref elts)                       => fmt.debug_tuple("Arr").field(elts).finish(),
            &ExprData::Obj(ref props)                      => fmt.debug_tuple("Obj").field(props).finish(),
            &ExprData::Fun(ref fun)                        => fmt.debug_tuple("Fun").field(fun).finish(),
            &ExprData::Seq(ref exprs)                      => fmt.debug_tuple("Seq").field(exprs).finish(),
            &ExprData::Unop(ref op, ref arg)               => fmt.debug_tuple("Unop").field(op).field(arg).finish(),
            &ExprData::Binop(ref op, ref left, ref right)  => fmt.debug_tuple("Binop").field(op).field(left).field(right).finish(),
            &ExprData::Logop(ref op, ref left, ref right)  => fmt.debug_tuple("Logop").field(op).field(left).field(right).finish(),
            &ExprData::PreInc(ref arg)                     => fmt.debug_tuple("PreInc").field(arg).finish(),
            &ExprData::PostInc(ref arg)                    => fmt.debug_tuple("PostInc").field(arg).finish(),
            &ExprData::PreDec(ref arg)                     => fmt.debug_tuple("PreDec").field(arg).finish(),
            &ExprData::PostDec(ref arg)                    => fmt.debug_tuple("PostDec").field(arg).finish(),
            &ExprData::Assign(ref op, ref left, ref right) => fmt.debug_tuple("Assign").field(op).field(left).field(right).finish(),
            &ExprData::Cond(ref test, ref cons, ref alt)   => fmt.debug_tuple("Cond").field(test).field(cons).field(alt).finish(),
            &ExprData::Call(ref callee, ref args)          => fmt.debug_tuple("Call").field(callee).field(args).finish(),
            &ExprData::New(ref ctor, None) => {
                let args: Vec<Expr> = vec![];
                fmt.debug_tuple("New").field(ctor).field(&args).finish()
            }
            &ExprData::New(ref ctor, Some(ref args))       => fmt.debug_tuple("New").field(ctor).field(args).finish(),
            &ExprData::Dot(ref expr, ref key)              => fmt.debug_tuple("Dot").field(expr).field(key).finish(),
            &ExprData::Brack(ref expr, ref prop)           => fmt.debug_tuple("Brack").field(expr).field(prop).finish(),
            &ExprData::NewTarget                           => fmt.write_str("NewTarget"),
            &ExprData::True                                => fmt.write_str("True"),
            &ExprData::False                               => fmt.write_str("False"),
            &ExprData::Null                                => fmt.write_str("Null"),
            &ExprData::Number(ref lit)                     => fmt.debug_tuple("Number").field(lit).finish(),
            &ExprData::RegExp(ref source, ref flags)       => fmt.debug_tuple("RegExp").field(source).field(flags).finish(),
            &ExprData::String(ref lit)                     => fmt.debug_tuple("String").field(lit).finish()
        }
    }
}

// FIXME: should produce more detailed error information

pub trait IntoAssignPatt {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Option<Span>>;
}

impl IntoAssignPatt for Expr {
    fn into_assign_patt(self) -> Result<Patt<AssignTarget>, Option<Span>> {
        Ok(match self.value {
            ExprData::Id(id)           => Patt::Simple(AssignTargetData::Id(id).tracked(self.location)),
            ExprData::Dot(obj, key)    => Patt::Simple(AssignTargetData::Dot(obj, key).tracked(self.location)),
            ExprData::Brack(obj, prop) => Patt::Simple(AssignTargetData::Brack(obj, prop).tracked(self.location)),
            ExprData::Obj(props) => {
                let mut prop_patts = Vec::with_capacity(props.len());
                for prop in props {
                    prop_patts.push(try!(prop.into_assign_prop()));
                }
                Patt::Compound(CompoundPattData::Obj(prop_patts).tracked(self.location))
            }
            ExprData::Arr(exprs) => {
                let mut patts = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    patts.push(match expr {
                        Some(expr) => Some(try!(expr.into_assign_patt())),
                        None => None
                    });
                }
                Patt::Compound(CompoundPattData::Arr(patts).tracked(self.location))
            }
            _ => { return Err(self.location); }
        })
    }
}

impl Untrack for ExprData {
    fn untrack(&mut self) {
        match *self {
            ExprData::This                                           => { }
            ExprData::Id(ref mut id)                                 => { id.untrack(); }
            ExprData::Arr(ref mut exprs)                             => { exprs.untrack(); }
            ExprData::Obj(ref mut props)                             => { props.untrack(); }
            ExprData::Fun(ref mut fun)                               => { fun.untrack(); }
            ExprData::Seq(ref mut exprs)                             => { exprs.untrack(); }
            ExprData::Unop(ref mut op, ref mut expr)                 => { op.untrack(); expr.untrack(); }
            ExprData::Binop(ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            ExprData::Logop(ref mut op, ref mut left, ref mut right) => { op.untrack(); left.untrack(); right.untrack(); }
            ExprData::PreInc(ref mut expr)                           => { expr.untrack(); }
            ExprData::PostInc(ref mut expr)                          => { expr.untrack(); }
            ExprData::PreDec(ref mut expr)                           => { expr.untrack(); }
            ExprData::PostDec(ref mut expr)                          => { expr.untrack(); }
            ExprData::Assign(ref mut op, ref mut patt, ref mut expr) => { op.untrack(); patt.untrack(); expr.untrack(); }
            ExprData::Cond(ref mut test, ref mut cons, ref mut alt)  => { test.untrack(); cons.untrack(); alt.untrack(); }
            ExprData::Call(ref mut callee, ref mut args)             => { callee.untrack(); args.untrack(); }
            ExprData::New(ref mut ctor, ref mut args)                => { ctor.untrack(); args.untrack(); }
            ExprData::Dot(ref mut obj, ref mut key)                  => { obj.untrack(); key.untrack(); }
            ExprData::Brack(ref mut obj, ref mut prop)               => { obj.untrack(); prop.untrack(); }
            ExprData::NewTarget                                      => { }
            ExprData::True                                           => { }
            ExprData::False                                          => { }
            ExprData::Null                                           => { }
            ExprData::Number(_)                                      => { }
            ExprData::RegExp(_, _)                                   => { }
            ExprData::String(_)                                      => { }
        }
    }
}

pub type Expr = Tracked<ExprData>;
