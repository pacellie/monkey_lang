use std::error::Error;
use std::fmt;

pub type Result<T> = std::result::Result<T, CompilerError>;

#[derive(Debug, PartialEq, Eq)]
pub struct CompilerError(pub String);

impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Error for CompilerError {}
