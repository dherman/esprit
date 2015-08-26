use joker::token::{Token, TokenData, Reserved};
use context::LabelType;

pub trait First {
    fn first_binding(&self) -> bool;
}

pub trait Follows {
    fn follow_statement_list(&self) -> bool;
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
