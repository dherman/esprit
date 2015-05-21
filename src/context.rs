use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Mode {
    Sloppy,
    Strict,
    Module
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct SharedContext {
    pub mode: Mode,
    pub operator: bool,
    pub generator: bool
}

impl SharedContext {
    pub fn new(mode: Mode) -> SharedContext {
        SharedContext {
            mode: mode,
            operator: false,
            generator: false
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LabelType {
    Statement,
    Iteration
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParserContext {
    pub function: bool,
    pub iteration: bool,
    pub switch: bool,
    pub labels: HashMap<Rc<String>, LabelType>
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            function: false,
            iteration: false,
            switch: false,
            labels: HashMap::new()
        }
    }

    pub fn new_function() -> ParserContext {
        ParserContext {
            function: true,
            iteration: false,
            switch: false,
            labels: HashMap::new()
        }
    }
}
