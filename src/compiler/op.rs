use std::fmt;

pub type Reference = u16;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Op {
    Constant(Reference),
    Pop,
    Add,
    Sub,
    Mul,
    Div,
    True,
    False,
    Eq,
    Neq,
    Lt,
    Gt,
    Minus,
    Bang,
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Op::Constant(reference) => write!(f, "Constant {}", reference),
            Op::Pop => write!(f, "Pop"),
            Op::Add => write!(f, "+"),
            Op::Sub => write!(f, "-"),
            Op::Mul => write!(f, "*"),
            Op::Div => write!(f, "/"),
            Op::True => write!(f, "true"),
            Op::False => write!(f, "false"),
            Op::Eq => write!(f, "=="),
            Op::Neq => write!(f, "!="),
            Op::Lt => write!(f, "<"),
            Op::Gt => write!(f, ">"),
            Op::Minus => write!(f, "-"),
            Op::Bang => write!(f, "!"),
        }
    }
}

impl Op {
    const CONSTANT: u8 = 0;
    const POP: u8 = 1;
    const ADD: u8 = 2;
    const SUB: u8 = 3;
    const MUL: u8 = 4;
    const DIV: u8 = 5;
    const TRUE: u8 = 6;
    const FALSE: u8 = 7;
    const EQ: u8 = 8;
    const NEQ: u8 = 9;
    const LT: u8 = 10;
    const GT: u8 = 11;
    const MINUS: u8 = 12;
    const BANG: u8 = 13;

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Op::Constant(reference) => {
                let mut bytes = Vec::with_capacity(3);
                bytes.push(Op::CONSTANT);
                bytes.extend_from_slice(&reference.to_be_bytes());
                bytes
            }
            Op::Pop => {
                vec![Op::POP]
            }
            Op::Add => {
                vec![Op::ADD]
            }
            Op::Sub => {
                vec![Op::SUB]
            }
            Op::Mul => {
                vec![Op::MUL]
            }
            Op::Div => {
                vec![Op::DIV]
            }
            Op::True => {
                vec![Op::TRUE]
            }
            Op::False => {
                vec![Op::FALSE]
            }
            Op::Eq => {
                vec![Op::EQ]
            }
            Op::Neq => {
                vec![Op::NEQ]
            }
            Op::Lt => {
                vec![Op::LT]
            }
            Op::Gt => {
                vec![Op::GT]
            }
            Op::Minus => {
                vec![Op::MINUS]
            }
            Op::Bang => {
                vec![Op::BANG]
            }
        }
    }

    pub fn decode(bytes: &[u8]) -> Option<(Op, usize)> {
        match bytes {
            [Op::CONSTANT, x, y, ..] => {
                let reference = u16::from_be_bytes([*x, *y]);
                Some((Op::Constant(reference), 3))
            }
            [Op::POP, ..] => Some((Op::Pop, 1)),
            [Op::ADD, ..] => Some((Op::Add, 1)),
            [Op::SUB, ..] => Some((Op::Sub, 1)),
            [Op::MUL, ..] => Some((Op::Mul, 1)),
            [Op::DIV, ..] => Some((Op::Div, 1)),
            [Op::TRUE, ..] => Some((Op::True, 1)),
            [Op::FALSE, ..] => Some((Op::False, 1)),
            [Op::EQ, ..] => Some((Op::Eq, 1)),
            [Op::NEQ, ..] => Some((Op::Neq, 1)),
            [Op::LT, ..] => Some((Op::Lt, 1)),
            [Op::GT, ..] => Some((Op::Gt, 1)),
            [Op::MINUS, ..] => Some((Op::Minus, 1)),
            [Op::BANG, ..] => Some((Op::Bang, 1)),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Binary {
    pub bytes: Vec<u8>,
}

impl Binary {
    pub fn len(&self) -> usize {
        self.bytes.len()
    }

    pub fn encode(instructions: Vec<Op>) -> Binary {
        let bytes = instructions
            .iter()
            .map(|instruction| instruction.encode())
            .flatten()
            .collect();

        Binary { bytes }
    }

    pub fn decode(&self) -> Option<Vec<Op>> {
        let mut ops = vec![];
        let mut i = 0;

        while i < self.bytes.len() {
            if let Some((op, j)) = Op::decode(&self.bytes[i..]) {
                ops.push(op);
                i += j;
            } else {
                return None;
            }
        }

        Some(ops)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode() {
        let ops = vec![
            Op::Constant(65534),
            Op::Pop,
            Op::Add,
            Op::Sub,
            Op::Mul,
            Op::Div,
            Op::True,
            Op::False,
            Op::Eq,
            Op::Neq,
            Op::Lt,
            Op::Gt,
            Op::Minus,
            Op::Bang,
        ];
        let bytes = Binary::encode(ops);
        let expected = Binary {
            bytes: vec![
                Op::CONSTANT,
                255,
                254,
                Op::POP,
                Op::ADD,
                Op::SUB,
                Op::MUL,
                Op::DIV,
                Op::TRUE,
                Op::FALSE,
                Op::EQ,
                Op::NEQ,
                Op::LT,
                Op::GT,
                Op::MINUS,
                Op::BANG,
            ],
        };

        assert_eq!(bytes, expected)
    }

    #[test]
    fn test_decode() {
        let binary = Binary {
            bytes: vec![
                Op::CONSTANT,
                255,
                254,
                Op::POP,
                Op::ADD,
                Op::SUB,
                Op::MUL,
                Op::DIV,
                Op::TRUE,
                Op::FALSE,
                Op::EQ,
                Op::NEQ,
                Op::LT,
                Op::GT,
                Op::MINUS,
                Op::BANG,
            ],
        };
        let ops = binary.decode().unwrap();
        let expected = vec![
            Op::Constant(65534),
            Op::Pop,
            Op::Add,
            Op::Sub,
            Op::Mul,
            Op::Div,
            Op::True,
            Op::False,
            Op::Eq,
            Op::Neq,
            Op::Lt,
            Op::Gt,
            Op::Minus,
            Op::Bang,
        ];

        assert_eq!(ops, expected)
    }
}
