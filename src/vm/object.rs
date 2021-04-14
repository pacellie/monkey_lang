use crate::builtin::Builtin;
use crate::compiler::Reference;

use std::collections::HashMap;
use std::fmt;

use itertools::*;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Primitive {
    Unit,
    Boolean(bool),
    Integer(i32),
    String(String),
}

impl fmt::Display for Primitive {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Primitive::Unit => write!(f, "unit"),
            Primitive::Boolean(b) => write!(f, "{}", b),
            Primitive::Integer(i) => write!(f, "{}", i),
            Primitive::String(s) => write!(f, "\"{}\"", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Primitive(Primitive),
    Array(Vec<Reference>),
    Map(HashMap<Primitive, Reference>),
    Function {
        bytes: Vec<u8>,
        locals: usize,
        params: usize,
        free: Vec<Reference>,
    },
    Builtin(Builtin),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Primitive(p) => write!(f, "{}", p),
            Object::Array(vec) => write!(
                f,
                "[{}]",
                vec.iter()
                    .map(|reference| format!("{}", reference))
                    .join(", ")
            ),
            Object::Map(map) => write!(
                f,
                "{{{}}}",
                map.iter()
                    .map(|(key, value)| format!("{} -> {}", key, value))
                    .join(", ")
            ),
            Object::Function { .. } => write!(f, "function"),
            Object::Builtin(builtin) => write!(f, "{}", builtin),
        }
    }
}

impl Object {
    pub fn unit() -> Object {
        Object::Primitive(Primitive::Unit)
    }

    pub fn boolean(b: bool) -> Object {
        Object::Primitive(Primitive::Boolean(b))
    }

    pub fn integer(i: i32) -> Object {
        Object::Primitive(Primitive::Integer(i))
    }

    pub fn string<S>(s: S) -> Object
    where
        S: Into<String>,
    {
        Object::Primitive(Primitive::String(s.into()))
    }

    pub fn array(slice: &[Reference]) -> Object {
        Object::Array(slice.to_vec())
    }

    pub fn map(slice: &[(Primitive, Reference)]) -> Object {
        let mut hm = HashMap::new();
        for (key, value) in slice {
            hm.insert(key.clone(), *value);
        }
        Object::Map(hm)
    }
}
