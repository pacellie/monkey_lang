use std::error::Error;
use std::fmt;

pub type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, PartialEq, Eq)]
pub struct RuntimeError(pub String);

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for RuntimeError {}
