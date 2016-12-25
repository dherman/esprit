extern crate joker;
extern crate tristate;

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
