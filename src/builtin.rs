use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Builtin {
    Len,
    First,
    Last,
    Rest,
    Push,
    Puts,
}

impl fmt::Display for Builtin {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Builtin::Len => write!(f, "len"),
            Builtin::First => write!(f, "first"),
            Builtin::Last => write!(f, "last"),
            Builtin::Rest => write!(f, "rest"),
            Builtin::Push => write!(f, "push"),
            Builtin::Puts => write!(f, "puts"),
        }
    }
}

impl Builtin {
    pub const LEN: u8 = 0;
    pub const FIRST: u8 = 1;
    pub const LAST: u8 = 2;
    pub const REST: u8 = 3;
    pub const PUSH: u8 = 4;
    pub const PUTS: u8 = 5;

    pub fn builtins() -> Vec<String> {
        vec![
            "len".to_string(),
            "first".to_string(),
            "last".to_string(),
            "rest".to_string(),
            "push".to_string(),
            "puts".to_string(),
        ]
    }

    pub fn builtin_by_name(name: &str) -> Option<Builtin> {
        match name {
            "len" => Some(Builtin::Len),
            "first" => Some(Builtin::First),
            "last" => Some(Builtin::Last),
            "rest" => Some(Builtin::Rest),
            "push" => Some(Builtin::Push),
            "puts" => Some(Builtin::Puts),
            _ => None,
        }
    }
}
