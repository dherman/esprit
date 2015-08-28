use joker;
use joker::token::Token;
use joker::track::*;
use easter::id::Id;
use easter::cover;

#[derive(Debug, PartialEq)]
pub enum Error {
    UnexpectedToken(Token),
    FailedASI(Token),
    LexError(joker::error::Error),
    TopLevelReturn(Span),
    IllegalBreak(Token),
    IllegalContinue(Token),
    InvalidLabel(Id),
    InvalidLabelType(Id),
    ContextualKeyword(Id),
    IllegalStrictBinding(Id),
    ForOfLetExpr(Span),
    DuplicateDefault(Token),
    StrictWith(Token),
    ThrowArgument(Token),
    OrphanTry(Token),
    InvalidLHS(Option<Span>, cover::Error),
    UnsupportedFeature(&'static str)
}
