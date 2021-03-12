use crate::interpreter::Environment;
use crate::parser::ast::Block;

use std::cell::RefCell;
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BuiltinFunction {
    Len,
    First,
    Last,
    Rest,
    Push,
}

impl fmt::Display for BuiltinFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BuiltinFunction::Len => write!(f, "len"),
            BuiltinFunction::First => write!(f, "first"),
            BuiltinFunction::Last => write!(f, "last"),
            BuiltinFunction::Rest => write!(f, "rest"),
            BuiltinFunction::Push => write!(f, "push"),
        }
    }
}

impl BuiltinFunction {
    pub fn builtin_by_name(name: &str) -> Option<BuiltinFunction> {
        match name {
            "len" => Some(BuiltinFunction::Len),
            "first" => Some(BuiltinFunction::First),
            "last" => Some(BuiltinFunction::Last),
            "rest" => Some(BuiltinFunction::Rest),
            "push" => Some(BuiltinFunction::Push),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    Integer(i32),
    Boolean(bool),
    String(String),
    Array(Vec<Object>),
    Return(Box<Object>),
    Function {
        env: Rc<RefCell<Environment>>,
        params: Vec<String>,
        body: Block,
    },
    Builtin(BuiltinFunction),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "Unit"),
            Object::Integer(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Array(objs) => write!(f, "[{}]", objs.iter().join(", ")),
            Object::Return(obj) => write!(f, "Return {}", obj),
            Object::Function { params, body, .. } => {
                // Do not print `env` due to cyclic dependencies
                write!(f, "fn({}) {{ {} }}", params.iter().join(", "), body)
            }
            Object::Builtin(builtin) => write!(f, "built-in: {}", builtin),
        }
    }
}
