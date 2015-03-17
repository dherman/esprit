const CX_ASI      : u32 = 0x01;
const CX_OPERATOR : u32 = 0x02;

#[derive(Debug, Copy, Eq, PartialEq)]
pub struct Context {
    asi: bool,
    operator: bool
}

impl Context {
    pub fn new() -> Context {
        Context { asi: false, operator: false }
    }
    pub fn is_asi_possible(&mut self) -> bool { self.asi }
    pub fn is_operator(&mut self) -> bool { self.operator }
}

/*
pub struct ParseContext {
    is_asi_possible: bool,
    is_operator: bool
}
*/

pub trait ParseContext {
    fn is_asi_possible(&mut self) -> bool;
    fn is_operator(&mut self) -> bool;
}

pub struct SimpleContext;

impl<'a> ParseContext for &'a SimpleContext {
    fn is_asi_possible(&mut self) -> bool { true }
    fn is_operator(&mut self) -> bool { true }
}

impl SimpleContext {
    pub fn new() -> SimpleContext {
        SimpleContext
    }
}
