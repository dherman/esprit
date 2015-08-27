use std::collections::HashMap;
use std::rc::Rc;
use std::mem::replace;
use joker::word::Name;
use joker::track::{IntoTracked, span};
use easter::stmt::{StmtData, Stmt};
use easter::id::Id;
use result::Result;
use parser::Parser;

pub trait WithContext {
    fn with_labels<F>(&mut self, mut labels: Vec<Id>, label_type: LabelType, op: F) -> Result<Stmt>
      where F: FnOnce(&mut Self) -> Result<Stmt>;
    fn allow_in<F, T>(&mut self, allow_in: bool, parse: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T>;
}

impl<I: Iterator<Item=char>> WithContext for Parser<I> {
    fn with_labels<F>(&mut self, mut labels: Vec<Id>, label_type: LabelType, op: F) -> Result<Stmt>
      where F: FnOnce(&mut Self) -> Result<Stmt>
    {
        let mut label_strings = Vec::new();
        for id in labels.iter() {
            let label = Rc::new(id.value.name.clone());
            self.parser_cx.labels.insert(label.clone(), label_type);
            label_strings.push(label);
        }
        let result = op(self);
        for label in label_strings {
            self.parser_cx.labels.remove(&label);
        }
        let mut body = try!(result);
        labels.reverse();
        for id in labels {
            let location = span(&id, &body);
            body = StmtData::Label(id, Box::new(body)).tracked(location);
        }
        Ok(body)
    }

    fn allow_in<F, T>(&mut self, allow_in: bool, parse: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T>
    {
        let allow_in = replace(&mut self.parser_cx.allow_in, allow_in);
        let result = parse(self);
        replace(&mut self.parser_cx.allow_in, allow_in);
        result
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LabelType {
    Statement,
    Iteration
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct ParserContext {
    pub function: bool,
    pub iteration: bool,
    pub switch: bool,
    pub allow_in: bool,
    pub labels: HashMap<Rc<Name>, LabelType>
}

impl ParserContext {
    pub fn new() -> ParserContext {
        ParserContext {
            function: false,
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }

    pub fn new_function() -> ParserContext {
        ParserContext {
            function: true,
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }
}
