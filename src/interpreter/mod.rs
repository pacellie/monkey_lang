mod environment;
mod error;
mod interpreter;
mod object;

pub use environment::Environment;
pub use error::{Result, RuntimeError};
pub use interpreter::eval;
pub use object::{BuiltinFunction, Object};
