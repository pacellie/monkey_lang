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

impl RuntimeError {
    pub fn type_mismatch<S>(msg: S) -> RuntimeError
    where
        S: Into<String>,
    {
        RuntimeError(format!("type mismatch: `{}`", msg.into()))
    }

    pub fn unknown_name<S>(msg: S) -> RuntimeError
    where
        S: Into<String>,
    {
        RuntimeError(format!("unknown name: `{}`", msg.into()))
    }

    pub fn wrong_number_of_args(expected: usize, got: usize) -> RuntimeError {
        RuntimeError(format!(
            "wrong number of arguments: expected `{}`, got `{}`",
            expected, got
        ))
    }

    pub fn index_out_of_bounds(index: i32, upper_bound: usize) -> RuntimeError {
        RuntimeError(format!(
            "index out of bounds: `{}` not in `{{0..{}}}`",
            index, upper_bound
        ))
    }
}
