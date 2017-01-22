use joker::word::{Atom, Name};
use tristate::TriState;

pub trait AtomExt {
    fn is_strict_reserved(&self) -> TriState;
    fn is_illegal_strict_binding(&self) -> bool;
}

impl AtomExt for Name {
    fn is_strict_reserved(&self) -> TriState {
        match self {
            &Name::Atom(ref atom) => atom.is_strict_reserved(),
            _ => TriState::No
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
    fn is_strict_reserved(&self) -> TriState {
        match *self {
            // 11.6.2.2
            Atom::Await => TriState::Unknown,

            // 12.1.1
            Atom::Implements
            | Atom::Interface
            | Atom::Let
            | Atom::Package
            | Atom::Private
            | Atom::Protected
            | Atom::Public
            | Atom::Static
            | Atom::Yield => TriState::Yes,
            _ => TriState::No
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
