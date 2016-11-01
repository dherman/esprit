use joker::track::*;
use joker::token::StringLiteral;

use id::Id;
use expr::Expr;
use decl::{Decl, Dtor, Import, Export};
use patt::Patt;
use punc::Semi;

#[derive(Debug, PartialEq, Clone)]
pub enum Stmt {
    Empty(Option<Span>),
    Block(Option<Span>, Vec<StmtListItem>),
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
    Try(Option<Span>, Vec<StmtListItem>, Option<Box<Catch>>, Option<Vec<StmtListItem>>),
    While(Option<Span>, Expr, Box<Stmt>),
    DoWhile(Option<Span>, Box<Stmt>, Expr, Semi),
    For(Option<Span>, Option<Box<ForHead>>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForIn(Option<Span>, Box<ForInHead>, Expr, Box<Stmt>),
    ForOf(Option<Span>, Box<ForOfHead>, Expr, Box<Stmt>),
    Debugger(Option<Span>, Semi)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Body<Item> {
    pub location: Option<Span>,
    pub dirs: Vec<Dir>,
    pub items: Vec<Item>
}

impl<Item> TrackingRef for Body<Item> {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl<Item> TrackingMut for Body<Item> {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl<Item: Untrack> Untrack for Body<Item> {
    fn untrack(&mut self) {
        self.location = None;
        self.dirs.untrack();
        self.items.untrack();
    }
}

pub type Script = Body<StmtListItem>;

pub type Module = Body<ModItem>;

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

impl TrackingRef for Stmt {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            Stmt::Empty(ref location)
          | Stmt::Block(ref location, _)
          | Stmt::Var(ref location, _, _)
          | Stmt::Expr(ref location, _, _)
          | Stmt::If(ref location, _, _, _)
          | Stmt::Label(ref location, _, _)
          | Stmt::Break(ref location, _, _)
          | Stmt::Cont(ref location, _, _)
          | Stmt::With(ref location, _, _)
          | Stmt::Switch(ref location, _, _)
          | Stmt::Return(ref location, _, _)
          | Stmt::Throw(ref location, _, _)
          | Stmt::Try(ref location, _, _, _)
          | Stmt::While(ref location, _, _)
          | Stmt::DoWhile(ref location, _, _, _)
          | Stmt::For(ref location, _, _, _, _)
          | Stmt::ForIn(ref location, _, _, _)
          | Stmt::ForOf(ref location, _, _, _)
          | Stmt::Debugger(ref location, _) => location
        }
    }
}

impl TrackingMut for Stmt {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            Stmt::Empty(ref mut location)
          | Stmt::Block(ref mut location, _)
          | Stmt::Var(ref mut location, _, _)
          | Stmt::Expr(ref mut location, _, _)
          | Stmt::If(ref mut location, _, _, _)
          | Stmt::Label(ref mut location, _, _)
          | Stmt::Break(ref mut location, _, _)
          | Stmt::Cont(ref mut location, _, _)
          | Stmt::With(ref mut location, _, _)
          | Stmt::Switch(ref mut location, _, _)
          | Stmt::Return(ref mut location, _, _)
          | Stmt::Throw(ref mut location, _, _)
          | Stmt::Try(ref mut location, _, _, _)
          | Stmt::While(ref mut location, _, _)
          | Stmt::DoWhile(ref mut location, _, _, _)
          | Stmt::For(ref mut location, _, _, _, _)
          | Stmt::ForIn(ref mut location, _, _, _)
          | Stmt::ForOf(ref mut location, _, _, _)
          | Stmt::Debugger(ref mut location, _) => location
        }
    }
}

impl Untrack for Stmt {
    fn untrack(&mut self) {
        *self.tracking_mut() = None;
        match *self {
            Stmt::Empty(_)                                                       => { }
            Stmt::Block(_, ref mut items)                                        => { items.untrack(); }
            Stmt::Var(_, ref mut dtors, ref mut semi)                            => { dtors.untrack(); semi.untrack(); }
            Stmt::Expr(_, ref mut expr, ref mut semi)                            => { expr.untrack(); semi.untrack(); }
            Stmt::If(_, ref mut test, ref mut cons, ref mut alt)                 => { test.untrack(); cons.untrack(); alt.untrack(); }
            Stmt::Label(_, ref mut lab, ref mut stmt)                            => { lab.untrack(); stmt.untrack(); }
            Stmt::Break(_, ref mut lab, ref mut semi)                            => { lab.untrack(); semi.untrack(); }
            Stmt::Cont(_, ref mut lab, ref mut semi)                             => { lab.untrack(); semi.untrack(); }
            Stmt::With(_, ref mut expr, ref mut stmt)                            => { expr.untrack(); stmt.untrack(); }
            Stmt::Switch(_, ref mut expr, ref mut cases)                         => { expr.untrack(); cases.untrack(); }
            Stmt::Return(_, ref mut expr, ref mut semi)                          => { expr.untrack(); semi.untrack(); }
            Stmt::Throw(_, ref mut expr, ref mut semi)                           => { expr.untrack(); semi.untrack(); }
            Stmt::Try(_, ref mut body, ref mut catch, ref mut finally)           => { body.untrack(); catch.untrack(); finally.untrack(); }
            Stmt::While(_, ref mut expr, ref mut stmt)                           => { expr.untrack(); stmt.untrack(); }
            Stmt::DoWhile(_, ref mut stmt, ref mut expr, ref mut semi)           => { stmt.untrack(); expr.untrack(); semi.untrack(); }
            Stmt::For(_, ref mut init, ref mut test, ref mut incr, ref mut body) => { init.untrack(); test.untrack(); incr.untrack(); body.untrack(); }
            Stmt::ForIn(_, ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            Stmt::ForOf(_, ref mut lhs, ref mut rhs, ref mut body)               => { lhs.untrack(); rhs.untrack(); body.untrack(); }
            Stmt::Debugger(_, ref mut semi)                                      => { semi.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForHead {
    Var(Option<Span>, Vec<Dtor>),
    Let(Option<Span>, Vec<Dtor>),
    Expr(Option<Span>, Expr)
}

impl TrackingRef for ForHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForHead::Var(ref location, _)
          | ForHead::Let(ref location, _)
          | ForHead::Expr(ref location, _) => location
        }
    }
}

impl TrackingMut for ForHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForHead::Var(ref mut location, _)
          | ForHead::Let(ref mut location, _)
          | ForHead::Expr(ref mut location, _) => location
        }
    }
}

impl Untrack for ForHead {
    fn untrack(&mut self) {
        match *self {
            ForHead::Var(ref mut location, ref mut vec)
          | ForHead::Let(ref mut location, ref mut vec) => {
                *location = None;
                vec.untrack();
            }
            ForHead::Expr(ref mut location, ref mut expr) => {
                *location = None;
                expr.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForInHead {
    VarInit(Option<Span>, Id, Expr),
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Expr(Expr)
}

impl TrackingRef for ForInHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForInHead::VarInit(ref location, _, _)
          | ForInHead::Var(ref location, _)
          | ForInHead::Let(ref location, _) => location,
            ForInHead::Expr(ref expr) => expr.tracking_ref()
        }
    }
}

impl TrackingMut for ForInHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForInHead::VarInit(ref mut location, _, _)
          | ForInHead::Var(ref mut location, _)
          | ForInHead::Let(ref mut location, _) => location,
            ForInHead::Expr(ref mut expr) => expr.tracking_mut()
        }
    }
}

impl Untrack for ForInHead {
    fn untrack(&mut self) {
        match *self {
            ForInHead::VarInit(ref mut location, ref mut id, ref mut expr) => {
                *location = None;
                id.untrack();
                expr.untrack();
            }
            ForInHead::Var(ref mut location, ref mut patt)
          | ForInHead::Let(ref mut location, ref mut patt) => {
                *location = None;
                patt.untrack();
            }
            ForInHead::Expr(ref mut expr) => {
                expr.untrack();
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ForOfHead {
    Var(Option<Span>, Patt<Id>),
    Let(Option<Span>, Patt<Id>),
    Expr(Expr)
}

impl TrackingRef for ForOfHead {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ForOfHead::Var(ref location, _)
          | ForOfHead::Let(ref location, _) => location,
            ForOfHead::Expr(ref expr) => expr.tracking_ref()
        }
    }
}

impl TrackingMut for ForOfHead {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ForOfHead::Var(ref mut location, _)
          | ForOfHead::Let(ref mut location, _) => location,
            ForOfHead::Expr(ref mut expr) => expr.tracking_mut()
        }
    }
}

impl Untrack for ForOfHead {
    fn untrack(&mut self) {
        match *self {
            ForOfHead::Var(ref mut location, ref mut patt)
          | ForOfHead::Let(ref mut location, ref mut patt) => {
                *location = None;
                patt.untrack();
            }
            ForOfHead::Expr(ref mut expr) => { expr.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Catch {
    pub location: Option<Span>,
    pub param: Patt<Id>,
    pub body: Vec<StmtListItem>
}

impl TrackingRef for Catch {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Catch {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Catch {
    fn untrack(&mut self) {
        self.location = None;
        self.param.untrack();
        self.body.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Case {
    pub location: Option<Span>,
    pub test: Option<Expr>,
    pub body: Vec<StmtListItem>
}

impl TrackingRef for Case {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Case {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Case {
    fn untrack(&mut self) {
        self.location = None;
        self.test.untrack();
        self.body.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
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

impl TrackingRef for Dir {
    fn tracking_ref(&self) -> &Option<Span> { &self.location }
}

impl TrackingMut for Dir {
    fn tracking_mut(&mut self) -> &mut Option<Span> { &mut self.location }
}

impl Untrack for Dir {
    fn untrack(&mut self) {
        self.location = None;
        self.semi.untrack();
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum ModItem {
    Import(Import),
    Export(Export),
    Decl(Decl),
    Stmt(Stmt)
}

impl TrackingRef for ModItem {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            ModItem::Import(ref import) => import.tracking_ref(),
            ModItem::Export(ref export) => export.tracking_ref(),
            ModItem::Decl(ref decl) => decl.tracking_ref(),
            ModItem::Stmt(ref stmt) => stmt.tracking_ref()
        }
    }
}

impl TrackingMut for ModItem {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            ModItem::Import(ref mut import) => import.tracking_mut(),
            ModItem::Export(ref mut export) => export.tracking_mut(),
            ModItem::Decl(ref mut decl) => decl.tracking_mut(),
            ModItem::Stmt(ref mut stmt) => stmt.tracking_mut()
        }
    }
}

impl Untrack for ModItem {
    fn untrack(&mut self) {
        match *self {
            ModItem::Import(ref mut import) => { import.untrack(); }
            ModItem::Export(ref mut export) => { export.untrack(); }
            ModItem::Decl(ref mut decl) => { decl.untrack(); }
            ModItem::Stmt(ref mut stmt) => { stmt.untrack(); }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
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
        match self {
            StmtListItem::Stmt(stmt) => ModItem::Stmt(stmt),
            StmtListItem::Decl(decl) => ModItem::Decl(decl)
        }
    }
}

impl TrackingRef for StmtListItem {
    fn tracking_ref(&self) -> &Option<Span> {
        match *self {
            StmtListItem::Decl(ref decl) => decl.tracking_ref(),
            StmtListItem::Stmt(ref stmt) => stmt.tracking_ref()
        }
    }
}

impl TrackingMut for StmtListItem {
    fn tracking_mut(&mut self) -> &mut Option<Span> {
        match *self {
            StmtListItem::Decl(ref mut decl) => decl.tracking_mut(),
            StmtListItem::Stmt(ref mut stmt) => stmt.tracking_mut()
        }
    }
}

impl Untrack for StmtListItem {
    fn untrack(&mut self) {
        match *self {
            StmtListItem::Decl(ref mut decl) => { decl.untrack(); }
            StmtListItem::Stmt(ref mut stmt) => { stmt.untrack(); }
        }
    }
}
