use std::fmt;
use std::fmt::{Display, Formatter};
use std::{cmp, usize};
use joker::track::span;
use easter::expr::Expr;
use easter::punc::{Binop, Logop, Precedence};

#[derive(Debug)]
pub enum Infix {
    Binop(Binop),
    Logop(Logop)
}

impl Infix {
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
    fn fill(self, right: Expr) -> Expr {
        let location = span(&self.left, &right);
        match self.op {
            Infix::Binop(op) => Expr::Binop(location, op, Box::new(self.left), Box::new(right)),
            Infix::Logop(op) => Expr::Logop(location, op, Box::new(self.left), Box::new(right))
        }
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
            FrameExpr(&Expr::Binop(_, ref op, ref left, ref right)) => {
                1 + FrameExpr(left).width() + 1 + op.to_string().len() + 1 + FrameExpr(right).width() + 1
            }
            FrameExpr(&Expr::Logop(_, ref op, ref left, ref right)) => {
                1 + FrameExpr(left).width() + 1 + op.to_string().len() + 1 + FrameExpr(right).width() + 1
            }
            _ => 1
        }
    }
}

impl<'a> Display for FrameExpr<'a> {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
        match *self {
            FrameExpr(&Expr::Binop(_, ref op, ref left, ref right)) => {
                fmt.write_fmt(format_args!("({} {} {})", FrameExpr(left), op, FrameExpr(right)))
            }
            FrameExpr(&Expr::Logop(_, ref op, ref left, ref right)) => {
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

    pub fn extend(&mut self, mut left: Expr, op: Infix) {
        let mut len;
        while { len = self.frames.len(); len > 0 } && self.frames[len - 1].op.groups_left(&op) {
            left = self.frames.pop().unwrap().fill(left);
        }
        self.frames.push(Frame { left: left, op: op });
    }

    pub fn finish(mut self, mut right: Expr) -> Expr {
        while self.frames.len() > 0 {
            right = self.frames.pop().unwrap().fill(right);
        }
        right
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
            fmt.write_str(&border[..])?;
            fmt.write_fmt(format_args!("\n| {}{} |\n", frame, " ".to_string().repeat(width - frame.width())))?;
        }
        fmt.write_str(&border[..])?;
        fmt.write_str("\n")?;
        Ok(())
    }
}
