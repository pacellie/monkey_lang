use crate::interpreter::Environment;
use crate::parser::ast::Block;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Unit,
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
            Object::Integer(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Unit => write!(f, "Unit"),
            Object::Return(obj) => write!(f, "Return {}", obj),
            Object::Function { env, params, body } => write!(
                f,
                "{} :- fn({}) {{ {} }}",
                format!("{:p}", &(*env)), // cyclic printing leads to stackoverflow!
                params.iter().join(", "),
                body
            ),
        }
    }
}
