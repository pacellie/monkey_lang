mod environment;
mod interpreter;
mod object;

pub use environment::Environment;
pub use interpreter::{eval, eval_program};
pub use object::{Builtin, Object, Primitive};
