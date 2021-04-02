use std::fmt;

use crate::compiler::Reference;

use itertools::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    False,
    True,
    Integer(i32),
    String(String),
    Array(Vec<Reference>),
}

impl Object {
    pub fn string<S>(s: S) -> Object
    where
        S: Into<String>,
    {
        Object::String(s.into())
    }
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "unit"),
            Object::False => write!(f, "false"),
            Object::True => write!(f, "true"),
            Object::Integer(i) => write!(f, "{}", i),
            Object::String(s) => write!(f, "\"{}\"", s),
            Object::Array(vec) => write!(f, "[{}]", vec.iter().join(", ")),
        }
    }
}
