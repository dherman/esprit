use joker::token::{Token, TokenData, StringLiteral};
use joker::word::Reserved;
use context::LabelType;

pub trait First {
    fn first_binding(&self) -> bool;
    fn pragma(&self) -> Option<&str>;
}

pub trait Follows {
    fn follow_statement_list(&self) -> bool;
    fn expression_continuation(&self) -> bool;
}

impl First for Token {
    // first(LexicalBinding) =
    //   first(BindingIdentifier)
    // U first(BindingPattern)
    // = IdentifierName
    // U first(BindingPattern)
    // = IdentifierName
    // U first(ObjectBindingPattern)
    // U first(ArrayBindingPattern)
    // = IdentifierName U { '{', '[' }
    fn first_binding(&self) -> bool {
        match self.value {
            TokenData::LBrace
          | TokenData::LBrack
          | TokenData::Identifier(_) => true,
            _ => false
        }
    }

    fn pragma(&self) -> Option<&str> {
        match self.value {
            TokenData::String(StringLiteral { ref value, .. }) => Some(value),
            _ => None
        }
    }
}

impl Follows for Token {
    // follow(StatementList) =
    //   follow(CaseClause)
    // U follow(DefaultClause)
    // U follow(FunctionBody)
    // U follow(ScriptBody)
    // U follow(ModuleBody)
    // U { '}' }
    // = { '}', 'case', 'default', EOF }
    //
    // follow(CaseClause) =
    //   { '}' }
    // U first(CaseClause)
    // U first(DefaultClause)
    // = { '}', 'case', 'default' }
    //
    // follow(DefaultClause) =
    //   { '}' }
    // U first(CaseClause)
    // = { '}', 'case' }
    //
    // first(CaseClause) = { 'case' }
    // first(DefaultClause) = { 'default' }
    //
    // follow(ScriptBody) = { EOF }
    // follow(ModuleBody) = { EOF }
    fn follow_statement_list(&self) -> bool {
        match self.value {
              TokenData::Reserved(Reserved::Case)
            | TokenData::Reserved(Reserved::Default)
            | TokenData::EOF
            | TokenData::RBrace => true,
            _ => false
        }
    }

    fn expression_continuation(&self) -> bool {
        match self.value {
            // 1. Increment/decrement iff no preceding newline.
            TokenData::Inc
          | TokenData::Dec => !self.newline,

            // 2. Common non-continuations.
            TokenData::Semi
          | TokenData::RBrace
          | TokenData::EOF
            // 3. All other non-continuations.
          | TokenData::Reserved(_)
          | TokenData::LBrace
          | TokenData::RBrack
          | TokenData::RParen
          // | TokenData::Ellipsis
          | TokenData::Bang
          | TokenData::Tilde
          | TokenData::Colon
          | TokenData::Arrow
          | TokenData::Number(_)
          | TokenData::String(_)
          | TokenData::RegExp(_)
          | TokenData::Identifier(_) => false,

            // 5. All others are continuations.
            _ => true
        }
    }
}

pub trait HasLabelType {
    fn label_type(&self) -> LabelType;
}

impl HasLabelType for Token {
    fn label_type(&self) -> LabelType {
        match self.value {
            TokenData::Reserved(Reserved::Do)
          | TokenData::Reserved(Reserved::While)
          | TokenData::Reserved(Reserved::For) => LabelType::Iteration,
            _                                  => LabelType::Statement
        }
    }
}
