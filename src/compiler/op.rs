use std::fmt;

pub type Reference = u16;
pub type Address = u16;
pub type GlobalBinding = u16;
pub type LocalBinding = u8;
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
}

#[rustfmt::skip]
impl Op {
    pub const CONSTANT  : u8 = 0;
    pub const POP       : u8 = 1;

    pub const UNIT      : u8 = 2;

    pub const FALSE     : u8 = 3;
    pub const TRUE      : u8 = 4;

    pub const ADD       : u8 = 5;
    pub const SUB       : u8 = 6;
    pub const MUL       : u8 = 7;
    pub const DIV       : u8 = 8;
    pub const EQ        : u8 = 9;
    pub const NEQ       : u8 = 10;
    pub const LT        : u8 = 11;
    pub const GT        : u8 = 12;

    pub const MINUS     : u8 = 13;
    pub const BANG      : u8 = 14;

    pub const JUMPIFNOT : u8 = 15;
    pub const JUMP      : u8 = 16;

    pub const GETGLOBAL : u8 = 17;
    pub const SETGLOBAL : u8 = 18;

    pub const ARRAY     : u8 = 19;
    pub const MAP       : u8 = 20;
    pub const INDEX     : u8 = 21;

    pub const CALL      : u8 = 22;
    pub const RETURN    : u8 = 23;

    pub const GETLOCAL  : u8 = 24;
    pub const SETLOCAL  : u8 = 25;

    pub const GETBUILTIN: u8 = 26;

    pub fn format(op: u8) -> String {
        match op {
            Op::CONSTANT => "constant",
            Op::POP => "pop",
            Op::UNIT => "unit",
            Op::FALSE => "false",
            Op::TRUE => "true",
            Op::ADD => "+",
            Op::SUB => "-",
            Op::MUL => "*",
            Op::DIV => "/",
            Op::EQ => "==",
            Op::NEQ => "!=",
            Op::LT => "<",
            Op::GT => ">",
            Op::MINUS => "-",
            Op::BANG => "!",
            Op::JUMPIFNOT => "jump_if_not",
            Op::JUMP => "jump",
            Op::GETGLOBAL => "get_global",
            Op::SETGLOBAL => "set_global",
            Op::ARRAY => "array",
            Op::MAP => "map",
            Op::CALL => "call",
            Op::RETURN => "return",
            Op::GETLOCAL => "get_local",
            Op::SETLOCAL => "set_local",
            Op::GETBUILTIN => "get_builtin",
            _ => "?",
        }.to_string()
    }
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
        }
    }
}
