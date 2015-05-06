use std::collections::HashSet;

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

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParserContext {
    pub function: bool,
    pub iteration: bool,
    pub switch: bool,
    pub label_set: HashSet<String>
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            function: false,
            iteration: false,
            switch: false,
            label_set: HashSet::new()
        }
    }

    pub fn new_function() -> ParserContext {
        ParserContext {
            function: true,
            iteration: false,
            switch: false,
            label_set: HashSet::new()
        }
    }
}
