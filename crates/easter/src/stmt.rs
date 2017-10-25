use joker::track::*;
use joker::token::StringLiteral;

use id::Id;
use expr::Expr;
use decl::{Decl, Dtor, ConstDtor, Import, Export};
use patt::{Patt, AssignTarget};
use punc::Semi;

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum Stmt {
    Empty(Option<Span>),
    Block(Block),
    Var(Option<Span>, Vec<Dtor>, Semi),
    Expr(Option<Span>, Expr, Semi),
    If(Option<Span>, Expr, Box<Stmt>, Option<Box<Stmt>>),
    Label(Option<Span>, Id, Box<Stmt>),
    Break(Option<Span>, Option<Id>, Semi),
    Cont(Option<Span>, Option<Id>, Semi),
    With(Option<Span>, Expr, Box<Stmt>),
    Switch(Option<Span>, Expr, Vec<Case>),
    Return(Option<Span>, Option<Expr>, Semi),
    Throw(Option<Span>, Expr, Semi),
    Try(Option<Span>, Block, Option<Box<Catch>>, Option<Block>),
    While(Option<Span>, Expr, Box<Stmt>),
    DoWhile(Option<Span>, Box<Stmt>, Expr, Semi),
    For(Option<Span>, Option<Box<ForHead>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForIn(Option<Span>, Box<ForInHead>, Expr, Box<Stmt>),
    ForOf(Option<Span>, Box<ForOfHead>, Expr, Box<Stmt>),
    Debugger(Option<Span>, Semi)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Body<Item> {
    pub location: Option<Span>,
    pub dirs: Vec<Dir>,
    pub items: Vec<Item>
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Block {
    pub location: Option<Span>,
    pub items: Vec<StmtListItem>
}

pub type Script = Body<StmtListItem>;

pub type Module = Body<ModItem>;

impl ModItem {
    pub fn is_directive(&self) -> bool {
        match *self {
            ModItem::StmtListItem(ref item) => item.is_directive(),
            _ => false
        }
    }

    pub fn to_directive(&self) -> Option<Dir> {
        match *self {
            ModItem::StmtListItem(ref item) => item.to_directive(),
            _ => None
        }
    }
}

impl Stmt {
    pub fn is_directive(&self) -> bool {
        match *self {
            Stmt::Expr(_, Expr::String(_, _), _) => true,
            _ => false
        }
    }

    pub fn to_directive(&self) -> Option<Dir> {
        match *self {
            Stmt::Expr(location, Expr::String(_, ref s@StringLiteral { .. }), semi) => Some(Dir {
                location: location,
                string: s.clone(),
                semi: semi
            }),
            _ => None
        }
    }

    pub fn into_directive(self) -> Result<Dir, Stmt> {
        match self {
            Stmt::Expr(location, Expr::String(_, s), semi) => {
                Ok(Dir {
                    location: location,
                    string: s,
                    semi: semi
                })
            }
            _ => Err(self)
        }
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum ForHead {
    Var(Option<Span>, Vec<Dtor>),
    Let(Option<Span>, Vec<Dtor>),
    Const(Option<Span>, Vec<ConstDtor>),
    Expr(Option<Span>, Expr)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum ForInHead {
    VarInit(Option<Span>, Id, Expr),
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Const(Option<Span>, Patt<Id>),
    Patt(Patt<AssignTarget>)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum ForOfHead {
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Const(Option<Span>, Patt<Id>),
    Patt(Patt<AssignTarget>)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Catch {
    pub location: Option<Span>,
    pub param: Patt<Id>,
    pub body: Block
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Case {
    pub location: Option<Span>,
    pub test: Option<Expr>,
    pub body: Vec<StmtListItem>
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub struct Dir {
    pub location: Option<Span>,
    pub string: StringLiteral,
    pub semi: Semi
}

impl Dir {
    pub fn pragma(&self) -> &str {
        if let Some(ref source) = self.string.source {
            source
        } else {
            &self.string.value
        }
    }
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum ModItem {
    Import(Import),
    Export(Export),
    StmtListItem(StmtListItem)
}

#[derive(Debug, PartialEq, Clone, TrackingRef, TrackingMut, Untrack)]
pub enum StmtListItem {
    Decl(Decl),
    Stmt(Stmt)
}

impl StmtListItem {
    pub fn is_directive(&self) -> bool {
        match *self {
            StmtListItem::Stmt(ref stmt) => stmt.is_directive(),
            _ => false
        }
    }

    pub fn to_directive(&self) -> Option<Dir> {
        match *self {
            StmtListItem::Stmt(ref stmt) => stmt.to_directive(),
            _ => None
        }
    }

    pub fn into_directive(self) -> Result<Dir, StmtListItem> {
        match self {
            StmtListItem::Stmt(stmt) => stmt.into_directive().map_err(StmtListItem::Stmt),
            StmtListItem::Decl(decl) => Err(StmtListItem::Decl(decl))
        }
    }

    pub fn into_mod_item(self) -> ModItem {
        ModItem::StmtListItem(self)
    }
}
