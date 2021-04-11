use crate::builtin::Builtin;
use crate::interpreter::Environment;
use crate::parser::ast::Block;

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Boolean(bool),
    Integer(i32),
    String(String),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Boolean(b) => write!(f, "{}", b),
            Primitive::Integer(i) => write!(f, "{}", i),
            Primitive::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    Primitive(Primitive),
    Array(Vec<Object>),
    Map(HashMap<Primitive, Object>),
    Return(Box<Object>),
    Function {
        env: Rc<RefCell<Environment>>,
        params: Vec<String>,
        body: Block,
    },
    Builtin(Builtin),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "Unit"),
            Object::Primitive(p) => write!(f, "{}", p),
            Object::Array(objs) => write!(f, "[{}]", objs.iter().join(", ")),
            Object::Map(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(key, value)| { format!("{}: {}", key, value) })
                    .join(", ")
            ),
            Object::Return(obj) => write!(f, "Return {}", obj),
            Object::Function { params, body, .. } => {
                // Do not print `env` due to cyclic dependencies
                write!(f, "fn({}) {{ {} }}", params.iter().join(", "), body)
            }
            Object::Builtin(builtin) => write!(f, "built-in: {}", builtin),
        }
    }
}

impl Object {
    pub fn boolean(b: bool) -> Object {
        Object::Primitive(Primitive::Boolean(b))
    }

    pub fn integer(i: i32) -> Object {
        Object::Primitive(Primitive::Integer(i))
    }

    pub fn string<S: Into<String>>(s: S) -> Object {
        Object::Primitive(Primitive::String(s.into()))
    }
}
