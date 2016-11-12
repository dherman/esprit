use joker::word::{Atom, Name};
use context::{Goal, Mode};
use tristate::TriState;

pub trait AtomExt {
    fn is_reserved(&self, Goal) -> TriState;
    fn is_illegal_strict_binding(&self) -> bool;
}

impl AtomExt for Name {
    fn is_reserved(&self, goal: Goal) -> TriState {
        match self {
            &Name::Atom(ref atom) => atom.is_reserved(goal),
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
    fn is_reserved(&self, goal: Goal) -> TriState {
        if goal.definitely_strict() {
            match *self {
                // 11.6.2.2
                Atom::Await => if goal.definitely_module() {
                    TriState::Yes
                } else {
                    TriState::No
                },

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
        } else if goal.definitely_sloppy() {
            TriState::No
        } else {
            TriState::Unknown
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
