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
        for id in labels.iter() {
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
    Script(Strict),
    Module,
    Unknown
}

impl Default for Goal {
    fn default() -> Goal { Goal::Unknown }
}

pub trait Mode {
    fn strict(&self) -> Strict;
    fn definitely_script(&self) -> bool;
    fn definitely_strict(&self) -> bool;
    fn definitely_sloppy(&self) -> bool;
    fn definitely_module(&self) -> bool;
}

impl Mode for Goal {
    fn strict(&self) -> Strict {
        match *self {
            Goal::Script(strict) => strict,
            Goal::Module         => Strict::Yes,
            Goal::Unknown        => Strict::Unknown
        }
    }

    fn definitely_script(&self) -> bool {
        match *self {
            Goal::Script(_) => true,
            _ => false
        }
    }

    fn definitely_strict(&self) -> bool {
        self.strict().definitely()
    }

    fn definitely_sloppy(&self) -> bool {
        self.strict().definitely_not()
    }

    fn definitely_module(&self) -> bool { *self == Goal::Module }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Context {
    pub function: Option<Strict>,
    pub iteration: bool,
    pub switch: bool,
    pub allow_in: bool,
    pub labels: HashMap<Rc<Name>, LabelType>
}

impl Context {
    pub fn new() -> Context {
        Context {
            function: None,
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }

    pub fn new_function(inherited: Strict) -> Context {
        Context {
            function: Some(inherited),
            iteration: false,
            switch: false,
            allow_in: true,
            labels: HashMap::new()
        }
    }
}
