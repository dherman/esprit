use joker::context::Mode;
use joker::token::{Atom, Name};

pub trait AtomExt {
    fn is_reserved(&self, Mode) -> bool;
    fn is_illegal_strict_binding(&self) -> bool;
}

impl AtomExt for Name {
    fn is_reserved(&self, mode: Mode) -> bool {
        match self {
            &Name::Atom(ref atom) => atom.is_reserved(mode),
            _ => false
        }
    }

    fn is_illegal_strict_binding(&self) -> bool {
        match *self {
            Name::Atom(ref atom) => atom.is_illegal_strict_binding(),
            _ => false
        }
    }
}

impl AtomExt for Atom {
    fn is_reserved(&self, mode: Mode) -> bool {
        // 12.1.1
        if mode.is_strict() {
            match *self {
                Atom::Implements
              | Atom::Interface
              | Atom::Let
              | Atom::Package
              | Atom::Private
              | Atom::Protected
              | Atom::Public
              | Atom::Static
              | Atom::Yield => true,
                _ => false
            }
        // 11.6.2.2
        } else {
            mode == Mode::Module && *self == Atom::Await
        }
    }

    // 12.1.1
    fn is_illegal_strict_binding(&self) -> bool {
        match *self {
            Atom::Arguments
          | Atom::Eval => true,
            _ => false
        }
    }
}
