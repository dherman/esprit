use std::collections::HashMap;
use std::default::Default;
use std::rc::Rc;
use std::mem::replace;
use joker::word::Name;
use joker::track::span;
use easter::stmt::Stmt;
use easter::id::Id;
use result::Result;
use parser::{Parser, Strict};

pub trait WithContext {
    fn with_labels<F>(&mut self, labels: Vec<Id>, label_type: LabelType, op: F) -> Result<Stmt>
      where F: FnOnce(&mut Self) -> Result<Stmt>;
    fn allow_in<F, T>(&mut self, allow_in: bool, parse: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T>;
}

impl<I: Iterator<Item=char>> WithContext for Parser<I> {
    fn with_labels<F>(&mut self, mut labels: Vec<Id>, label_type: LabelType, op: F) -> Result<Stmt>
      where F: FnOnce(&mut Self) -> Result<Stmt>
    {
        let mut label_strings = Vec::new();
        for id in &labels {
            let label = Rc::new(id.name.clone());
            self.context.labels.insert(label.clone(), label_type);
            label_strings.push(label);
        }
        let result = op(self);
        for label in label_strings {
            self.context.labels.remove(&label);
        }
        let mut body = result?;
        labels.reverse();
        for id in labels {
            let location = span(&id, &body);
            body = Stmt::Label(location, id, Box::new(body));
        }
        Ok(body)
    }

    fn allow_in<F, T>(&mut self, allow_in: bool, parse: F) -> Result<T>
      where F: FnOnce(&mut Self) -> Result<T>
    {
        let allow_in = replace(&mut self.context.allow_in, allow_in);
        let result = parse(self);
        replace(&mut self.context.allow_in, allow_in);
        result
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum LabelType {
    Statement,
    Iteration
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum Goal {
    Script,
    Module,
    Unknown
}

impl Default for Goal {
    fn default() -> Goal { Goal::Unknown }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context {
    pub strict: Strict,
    pub function: bool,
    pub iteration: bool,
    pub switch: bool,
    pub allow_in: bool,
    pub labels: HashMap<Rc<Name>, LabelType>
}

impl Context {
    pub fn new() -> Context {
        Context {
            strict: Strict::Unknown,
            function: false,
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }

    pub fn new_function(&self) -> Context {
        Context {
            strict: self.strict,
            function: true,
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }
}
