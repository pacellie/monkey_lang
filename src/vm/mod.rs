mod error;
mod object;
mod vm;

pub use error::{Result, RuntimeError};
pub use object::Object;
pub use vm::VirtualMachine;
