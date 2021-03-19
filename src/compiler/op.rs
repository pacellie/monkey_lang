use std::fmt;

pub type Reference = u16;
pub type Address = u16;

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
}

#[rustfmt::skip]
impl Op {
    pub const CONSTANT : u8 = 0;
    pub const POP      : u8 = 1;

    pub const UNIT     : u8 = 2;

    pub const FALSE    : u8 = 3;
    pub const TRUE     : u8 = 4;

    pub const ADD      : u8 = 5;
    pub const SUB      : u8 = 6;
    pub const MUL      : u8 = 7;
    pub const DIV      : u8 = 8;
    pub const EQ       : u8 = 9;
    pub const NEQ      : u8 = 10;
    pub const LT       : u8 = 11;
    pub const GT       : u8 = 12;

    pub const MINUS    : u8 = 13;
    pub const BANG     : u8 = 14;

    pub const JUMPIFNOT: u8 = 15;
    pub const JUMP     : u8 = 16;
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Constant(reference) => write!(f, "Constant {}", reference),
            Op::Pop => write!(f, "Pop"),
            Op::Unit => write!(f, "Unit"),
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
        }
    }
}
