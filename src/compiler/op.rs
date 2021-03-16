pub type Reference = u16;

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Constant(Reference),
    Add,
}

impl Op {
    const CONSTANT: u8 = 0;
    const ADD: u8 = 1;

    pub fn encode(&self) -> Vec<u8> {
        match self {
            Op::Constant(reference) => {
                let mut bytes = Vec::with_capacity(3);
                bytes.push(Op::CONSTANT);
                bytes.extend_from_slice(&reference.to_be_bytes());
                bytes
            }
            Op::Add => {
                vec![Op::ADD]
            }
        }
    }

    pub fn decode(bytes: &[u8]) -> Option<(Op, usize)> {
        match bytes {
            [Op::CONSTANT, x, y, ..] => {
                let reference = u16::from_be_bytes([*x, *y]);
                Some((Op::Constant(reference), 3))
            }
            [Op::ADD, ..] => Some((Op::Add, 1)),
            _ => None,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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
        let ops = vec![Op::Add, Op::Constant(65534)];
        let bytes = Binary::encode(ops);
        let expected = Binary {
            bytes: vec![1, 0, 255, 254],
        };

        assert_eq!(bytes, expected)
    }

    #[test]
    fn test_decode() {
        let binary = Binary {
            bytes: vec![1, 0, 255, 254],
        };
        let ops = binary.decode().unwrap();
        let expected = vec![Op::Add, Op::Constant(65534)];

        assert_eq!(ops, expected)
    }
}
