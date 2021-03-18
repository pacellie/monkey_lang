use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Object {
    Integer(i32),
    True,
    False,
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Object::Integer(i) => write!(f, "{}", i),
            Object::True => write!(f, "true"),
            Object::False => write!(f, "false"),
        }
    }
}
