#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Mode {
    Sloppy,
    Strict,
    Module
}

impl Mode {
    pub fn is_strict(self) -> bool {
        match self {
            Mode::Sloppy => false,
            Mode::Strict
          | Mode::Module => true
        }
    }
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Context {
    pub mode: Mode,
    pub operator: bool,
    pub generator: bool
}

impl Context {
    pub fn new(mode: Mode) -> Context {
        Context {
            mode: mode,
            operator: false,
            generator: false
        }
    }
}
