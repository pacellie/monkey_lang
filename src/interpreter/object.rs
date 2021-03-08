use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    Boolean(bool),
    Unit,
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(n) => write!(f, "{}", n),
            Object::Boolean(b) => write!(f, "{}", b),
            Object::Unit => write!(f, "Unit"),
            Object::Return(obj) => write!(f, "Return {}", obj),
        }
    }
}
