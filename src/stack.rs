use ast::*;
use track::*;

#[derive(Debug)]
pub enum Infix {
    Binop(Binop),
    Assop(Assop),
    Logop(Logop),
    Cond(Expr)
}

impl Infix {
    fn debug(&self) -> String {
        match *self {
            Infix::Binop(ref op) => op.pretty(),
            Infix::Assop(ref op) => op.pretty(),
            Infix::Logop(ref op) => op.pretty(),
            Infix::Cond(_)       => format!("? _ :")
        }
    }

    fn right_associative(&self) -> bool {
        match *self {
            Infix::Assop(_) => true,
            _ => false
        }
    }

    fn groups_left(&self, right: &Infix) -> bool {
        self.precedence() >= right.precedence() &&
            !(self.precedence() == right.precedence() && self.right_associative())
    }
}

impl Precedence for Infix {
    fn precedence(&self) -> u32 {
        match *self {
            Infix::Binop(ref op) => op.precedence(),
            Infix::Assop(ref op) => op.precedence(),
            Infix::Logop(ref op) => op.precedence(),
            Infix::Cond(_)       => 1
        }
    }
}

#[derive(Debug)]
struct Frame {
    left: Expr,
    op: Infix
}

impl Precedence for Frame {
    fn precedence(&self) -> u32 {
        self.op.precedence()
    }
}

impl Frame {
    fn fill(self, right: Expr) -> Result<Expr, Option<Span>> {
        let location = span(&self.left, &right);
        Ok((match self.op {
            Infix::Binop(op)  => ExprData::Binop(op, Box::new(self.left), Box::new(right)),
            Infix::Assop(op)  => ExprData::Assign(op, try!(self.left.into_assignment_pattern()), Box::new(right)),
            Infix::Logop(op)  => ExprData::Logop(op, Box::new(self.left), Box::new(right)),
            Infix::Cond(cons) => ExprData::Cond(Box::new(self.left), Box::new(cons), Box::new(right))
        }).tracked(location))
    }

    fn debug(&self) -> String {
        format!("{} {} []", debug_expr(&self.left), self.op.debug())
    }
}

trait StringExt {
    fn repeat(&self, usize) -> String;
}

impl StringExt for String {
    fn repeat(&self, n: usize) -> String {
        let len = self.len() * n;
        let mut result = String::with_capacity(len);
        for _ in 0..n {
            result.push_str(&self[..]);
        }
        result
    }
}

use std::{cmp,usize};
fn max_len(x: &Vec<String>) -> usize {
    let mut result = usize::MIN;
    for s in x {
        result = cmp::max(s.len(), result);
    }
    result
}

fn debug_expr(expr: &Expr) -> String {
    match expr.value {
        ExprData::Binop(ref op, ref left, ref right) => {
            format!("({} {} {})", debug_expr(left), op.pretty(), debug_expr(right))
        }
        ExprData::Assign(ref op, _, ref right) => {
            format!("(_ {} {})", op.pretty(), debug_expr(right))
        }
        ExprData::Logop(ref op, ref left, ref right) => {
            format!("({} {} {})", debug_expr(left), op.pretty(), debug_expr(right))
        }
        ExprData::Cond(ref test, ref cons, ref alt) => {
            format!("({} ? {} : {})", debug_expr(test), debug_expr(cons), debug_expr(alt))
        }
        _ => format!("_")
    }
}

#[derive(Debug)]
pub struct Stack {
    frames: Vec<Frame>
}

impl Stack {
    pub fn new() -> Stack {
        Stack { frames: Vec::new() }
    }

    pub fn extend(&mut self, mut left: Expr, op: Infix) -> Result<(), Option<Span>> {
        let mut len;
        while { len = self.frames.len(); len > 0 } && self.frames[len - 1].op.groups_left(&op) {
            left = try!(self.frames.pop().unwrap().fill(left));
        }
        self.frames.push(Frame { left: left, op: op });
        Ok(())
    }

    pub fn finish(mut self, mut right: Expr) -> Result<Expr, Option<Span>> {
        while self.frames.len() > 0 {
            right = try!(self.frames.pop().unwrap().fill(right));
        }
        Ok(right)
    }

    pub fn debug(&self) -> String {
        let len = self.frames.len();
        if len == 0 {
            return format!("[]");
        }

        let mut frames = Vec::with_capacity(self.frames.len());
        for frame in self.frames.iter() {
            frames.push(frame.debug());
        }
        let width = max_len(&frames);
        let border = format!("+{}+", format!("-").repeat(width + 2));
        let mut result = String::new();
        for frame in frames {
            result.push_str(&border[..]);
            result.push_str("\n");
            result.push_str(&format!("| {}{} |\n", frame, format!(" ").repeat(width - frame.len()))[..]);
        }
        result.push_str(&border[..]);
        result.push_str("\n");
        result
    }
}
