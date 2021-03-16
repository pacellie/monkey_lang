use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
        }
    }
}
