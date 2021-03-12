mod environment;
mod error;
mod interpreter;
mod object;

pub use environment::Environment;
pub use error::{Result, RuntimeError};
pub use interpreter::{eval, eval_program};
pub use object::{Builtin, Object, Primitive};
