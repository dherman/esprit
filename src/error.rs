use joker;
use joker::token::{Token, StringLiteral};
use joker::track::*;
use joker::word::Atom;
use easter::id::Id;
use easter::decl::{Import, Export};
use easter::patt::CompoundPatt;
use easter::cover;
use result::Result;

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
    UnsupportedFeature(&'static str),
    CompoundParamWithUseStrict(CompoundPatt<Id>)
}

#[derive(Debug, PartialEq, Clone)]
pub enum Check {
    Strict(Error),
    Module(Error)
}

impl Check {
    pub fn perform(self, module: bool) -> Result<()> {
        match self {
            Check::Strict(error) => Err(error),
            Check::Module(error) => {
                if module {
                    Err(error)
                } else {
                    Ok(())
                }
            }
        }
    }
}
