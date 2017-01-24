extern crate joker;
extern crate tristate;

#[cfg(feature = "nightly")]
#[macro_use]
extern crate derive;

#[cfg(feature = "nightly")]
macro_rules! pub_mod {
    ($name:ident) => (pub mod $name;)
}

#[cfg(not(feature = "nightly"))]
macro_rules! pub_mod {
    ($name:ident) => (pub mod $name {
        include!(concat!(env!("OUT_DIR"), "/", stringify!($name), ".rs"));
    })
}

pub_mod!(id);
pub_mod!(fun);
pub_mod!(obj);
pub_mod!(stmt);
pub_mod!(expr);
pub_mod!(decl);
pub_mod!(patt);
pub_mod!(punc);
pub_mod!(cover);
