use ast::*;
use track::*;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::{cmp, usize};

#[derive(Debug)]
pub enum Infix {
    Binop(Binop),
    Logop(Logop)
}

impl Infix {
    fn debug(&self) -> String {
        match *self {
            Infix::Binop(ref op) => op.to_string(),
            Infix::Logop(ref op) => op.to_string()
        }
    }

    fn groups_left(&self, right: &Infix) -> bool {
        self.precedence() >= right.precedence()
    }
}

impl Display for Infix {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match *self {
            Infix::Binop(ref op) => op.fmt(fmt),
            Infix::Logop(ref op) => op.fmt(fmt)
        }
    }
}

impl Precedence for Infix {
    fn precedence(&self) -> u32 {
        match *self {
            Infix::Binop(ref op) => op.precedence(),
            Infix::Logop(ref op) => op.precedence()
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
            Infix::Binop(op) => ExprData::Binop(op, Box::new(self.left), Box::new(right)),
            Infix::Logop(op) => ExprData::Logop(op, Box::new(self.left), Box::new(right))
        }).tracked(location))
    }
}

impl Frame {
    fn width(&self) -> usize {
        FrameExpr(&self.left).width() + 1 + self.op.to_string().len() + 1 + 2
    }
}

impl Display for Frame {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        fmt.write_fmt(format_args!("{} {} []", FrameExpr(&self.left), self.op))
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

struct FrameExpr<'a>(&'a Expr);

impl<'a> FrameExpr<'a> {
    fn width(&self) -> usize {
        match *self {
            FrameExpr(&Expr { value: ExprData::Binop(ref op, ref left, ref right), .. }) => {
                1 + FrameExpr(left).width() + 1 + op.to_string().len() + 1 + FrameExpr(right).width() + 1
            }
            FrameExpr(&Expr { value: ExprData::Logop(ref op, ref left, ref right), .. }) => {
                1 + FrameExpr(left).width() + 1 + op.to_string().len() + 1 + FrameExpr(right).width() + 1
            }
            _ => 1
        }
    }
}

impl<'a> Display for FrameExpr<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match *self {
            FrameExpr(&Expr { value: ExprData::Binop(ref op, ref left, ref right), .. }) => {
                fmt.write_fmt(format_args!("({} {} {})", FrameExpr(left), op, FrameExpr(right)))
            }
            FrameExpr(&Expr { value: ExprData::Logop(ref op, ref left, ref right), .. }) => {
                fmt.write_fmt(format_args!("({} {} {})", FrameExpr(left), op, FrameExpr(right)))
            }
            _ => fmt.write_str("_")
        }
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
}

impl Display for Stack {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        if self.frames.is_empty() {
            return fmt.write_str("[]");
        }
        let width = self.frames.iter().fold(usize::MIN, |max, f| cmp::max(max, f.width()));
        let border = format!("+{}+", "-".to_string().repeat(width + 2));
        for frame in self.frames.iter() {
            try!(fmt.write_str(&border[..]));
            try!(fmt.write_fmt(format_args!("\n| {}{} |\n", frame, " ".to_string().repeat(width - frame.width()))));
        }
        try!(fmt.write_str(&border[..]));
        try!(fmt.write_str("\n"));
        Ok(())
    }
}
