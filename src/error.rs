use joker;
use joker::token::{Token, StringLiteral};
use joker::track::*;
use joker::word::Atom;
use easter::id::Id;
use easter::decl::{Import, Export};
use easter::cover;
use result::Result;
use context::Goal;
use parser::Strict;
use atom::AtomExt;

#[derive(Debug, PartialEq, Clone)]
pub enum Error {
    UnexpectedToken(Token),
    FailedASI(Token),
    LexError(joker::error::Error),
    TopLevelReturn(Span),
    IllegalBreak(Token),
    IllegalContinue(Token),
    InvalidLabel(Id),
    InvalidLabelType(Id),
    ContextualKeyword(Span, Atom),
    IllegalStrictBinding(Span, Atom),
    IllegalModuleBinding(Span, Atom),
    UnexpectedDirective(Option<Span>, StringLiteral),
    UnexpectedModule(Option<Span>),
    ImportInScript(Import),
    ExportInScript(Export),
    ForOfLetExpr(Span),
    DuplicateDefault(Token),
    StrictWith(Token),
    ThrowArgument(Token),
    OrphanTry(Token),
    InvalidLHS(Option<Span>, cover::Error),
    UnsupportedFeature(&'static str)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Check {
    Failed(Error),
    Strict(Error),
    Reserved(Span, Atom)
}

impl Check {
    pub fn perform(self, module: bool) -> Result<()> {
        match self {
            Check::Failed(error) => { return Err(error); }
            Check::Strict(error) => { return Err(error); }
            Check::Reserved(span, atom) => {
                if atom.is_reserved(Goal::Script(Strict::Yes)).definitely() {
                    return Err(Error::IllegalStrictBinding(span, atom));
                }
                if module && atom.is_reserved(Goal::Module).definitely() {
                    return Err(Error::IllegalModuleBinding(span, atom));
                }
            }
        }
        Ok(())
    }
}
