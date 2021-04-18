use std::fmt;

pub type Reference = u16;
pub type Address = u16;
pub type GlobalBinding = u16;
pub type LocalBinding = u8;
pub type FreeBinding = u8;
pub type N = u16;
pub type M = u8;
pub type Builtin = u8;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Constant(Reference),
    Pop,

    // Unit
    Unit,

    // Boolean Constants
    False,
    True,

    // Binary Operations
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,

    // Unary Operations
    Minus,
    Bang,

    // Jumps
    JumpIfNot(Address),
    Jump(Address),

    // Global Bindings
    GetGlobal(GlobalBinding),
    SetGlobal(GlobalBinding),

    // Composite Data Types
    Array(N),
    Map(N),
    Index,

    // Functions
    Call(M),
    Return,

    // Local Bindings
    GetLocal(LocalBinding),
    SetLocal(LocalBinding),

    // Builtin
    GetBuiltin(Builtin),

    // Closures
    Closure(Reference, M),
    GetFree(FreeBinding),
    CurrentClosure,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Constant(reference) => write!(f, "Constant {}", reference),
            Op::Pop => write!(f, "Pop"),
            Op::Unit => write!(f, "unit"),
            Op::False => write!(f, "false"),
            Op::True => write!(f, "true"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::Eq => write!(f, "=="),
            Op::Neq => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Gt => write!(f, ">"),
            Op::Minus => write!(f, "-"),
            Op::Bang => write!(f, "!"),
            Op::JumpIfNot(address) => write!(f, "JumpIfNot {}", address),
            Op::Jump(address) => write!(f, "Jump {}", address),
            Op::GetGlobal(binding) => write!(f, "GetGlobal {}", binding),
            Op::SetGlobal(binding) => write!(f, "SetGlobal {}", binding),
            Op::Array(n) => write!(f, "Array[{}]", n),
            Op::Map(n) => write!(f, "Map[{}]", n),
            Op::Index => write!(f, "Index"),
            Op::Call(m) => write!(f, "Call {}", m),
            Op::Return => write!(f, "Return"),
            Op::GetLocal(binding) => write!(f, "GetLocal {}", binding),
            Op::SetLocal(binding) => write!(f, "SetLocal {}", binding),
            Op::GetBuiltin(builtin) => write!(f, "GetBuiltin {}", builtin),
            Op::Closure(reference, free) => write!(f, "Closure {} {}", reference, free),
            Op::GetFree(binding) => write!(f, "GetFree {}", binding),
            Op::CurrentClosure => write!(f, "Current"),
        }
    }
}
