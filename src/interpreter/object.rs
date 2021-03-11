use crate::interpreter::Environment;
use crate::parser::ast::Block;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    Integer(i32),
    Boolean(bool),
    String(String),
    Return(Box<Object>),
    Function {
        env: Rc<RefCell<Environment>>,
        params: Vec<String>,
        body: Block,
    },
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "Unit"),
            Object::Integer(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Return(obj) => write!(f, "Return {}", obj),
            Object::Function { params, body, .. } => {
                // Do not print `env` due to cyclic dependencies
                write!(f, "fn({}) {{ {} }}", params.iter().join(", "), body)
            }
        }
    }
}
