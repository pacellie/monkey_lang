use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Unit,
    False,
    True,
    Integer(i32),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Unit => write!(f, "unit"),
            Object::False => write!(f, "false"),
            Object::True => write!(f, "true"),
            Object::Integer(i) => write!(f, "{}", i),
        }
    }
}
