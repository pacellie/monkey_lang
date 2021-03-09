use std::{fmt, num};

pub type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(pub String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<num::ParseIntError> for ParseError {
    fn from(error: num::ParseIntError) -> Self {
        ParseError(error.to_string())
    }
}
