#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Mode {
    Sloppy,
    Strict,
    Module
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Context {
    pub newlines: bool,
    pub operator: bool,
    pub generator: bool,
    pub mode: Mode
}
