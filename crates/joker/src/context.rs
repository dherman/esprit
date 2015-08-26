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

// FIXME: this needs a better name

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
