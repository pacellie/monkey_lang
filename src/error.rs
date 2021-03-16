use std::error::Error;
use std::{fmt, num};

pub type Result<T> = std::result::Result<T, MonkeyError>;

#[derive(Debug, PartialEq, Eq)]
pub enum MonkeyError {
    ParserError(String),
    RuntimeError(String),
    CompilerError(String),
}

impl MonkeyError {
    pub fn parser_error<S, T>(expected: S, got: T) -> MonkeyError
    where
        S: Into<String>,
        T: Into<String>,
    {
        MonkeyError::ParserError(format!(
            "Expected `{}`, got `{}` instead.",
            expected.into(),
            got.into()
        ))
    }

    pub fn type_mismatch<S>(msg: S) -> MonkeyError
    where
        S: Into<String>,
    {
        MonkeyError::RuntimeError(format!("type mismatch: `{}`", msg.into()))
    }

    pub fn unknown_name<S>(msg: S) -> MonkeyError
    where
        S: Into<String>,
    {
        MonkeyError::RuntimeError(format!("unknown name: `{}`", msg.into()))
    }

    pub fn wrong_number_of_args(expected: usize, got: usize) -> MonkeyError {
        MonkeyError::RuntimeError(format!(
            "wrong number of arguments: expected `{}`, got `{}`",
            expected, got
        ))
    }

    pub fn index_out_of_bounds(index: i32, upper_bound: usize) -> MonkeyError {
        MonkeyError::RuntimeError(format!(
            "index out of bounds: `{}` not in `{{0..{}}}`",
            index, upper_bound
        ))
    }

    pub fn missing_index<S>(msg: S) -> MonkeyError
    where
        S: Into<String>,
    {
        MonkeyError::RuntimeError(format!("missing index: `{}`", msg.into()))
    }
}

impl fmt::Display for MonkeyError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MonkeyError::ParserError(msg) => write!(f, "parser error: {}", msg),
            MonkeyError::RuntimeError(msg) => write!(f, "runtime error: {}", msg),
            MonkeyError::CompilerError(msg) => write!(f, "compiler error: {}", msg),
        }
    }
}

impl Error for MonkeyError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            MonkeyError::ParserError(_) => None,
            MonkeyError::RuntimeError(_) => None,
            MonkeyError::CompilerError(_) => None,
        }
    }
}

impl From<num::ParseIntError> for MonkeyError {
    fn from(error: num::ParseIntError) -> Self {
        MonkeyError::ParserError(error.to_string())
    }
}
