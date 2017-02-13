use std::error;
use std::error::Error as StdError;
use std::fmt;

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

impl error::Error for Error {
    fn description(&self) -> &str {
        match *self {
            Error::UnexpectedToken(_) => "unexpected token",
            Error::FailedASI(_) => "failed ASI",
            Error::LexError(ref err) => err.description(),
            Error::TopLevelReturn(_) => "top level return",
            Error::IllegalBreak(_) => "illegal break",
            Error::IllegalContinue(_) => "illegal continue",
            Error::InvalidLabel(_) => "invalid label",
            Error::InvalidLabelType(_) => "invalid label type",
            Error::ContextualKeyword(_, _) => "contextual keyword",
            Error::IllegalStrictBinding(_, _) => "illegal strict binding",
            Error::UnexpectedDirective(_, _) => "unexpected directive",
            Error::UnexpectedModule(_) => "unexpected module",
            Error::ImportInScript(_) => "import in script",
            Error::ExportInScript(_) => "export in script",
            Error::ForOfLetExpr(_) => "for-of-let expr",
            Error::DuplicateDefault(_) => "duplicate default",
            Error::StrictWith(_) => "strict with",
            Error::ThrowArgument(_) => "throw argument",
            Error::OrphanTry(_) => "orphan try",
            Error::InvalidLHS(_, ref err) => err.description(),
            Error::UnsupportedFeature(_) => "unsupported feature",
            Error::CompoundParamWithUseStrict(_) => "compound param with use strict",
        }
    }

    fn cause(&self) -> Option<&error::Error> {
        match *self {
            Error::LexError(ref joker_err) => Some(joker_err),
            Error::InvalidLHS(_, ref cover_err) => Some(cover_err),
            _ => None,
        }
    }
}


impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.description())
    }
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

