use crate::compiler::{ByteCode, Op, Reference};
use crate::error::{MonkeyError, Result};
use crate::vm::{Object, Primitive};

const STACK_SIZE: usize = 2048;
const GLOBALS_SIZE: usize = 65536;

const UNIT: u16 = 0;
const FALSE: u16 = 1;
const TRUE: u16 = 2;

pub struct VirtualMachine {
    pub bytes: Vec<u8>,
    pub heap: Vec<Object>,
    pub globals: Vec<Reference>,
    pub stack: Vec<Reference>,
    pub sp: usize,
    pub pc: usize,
    pub gp: usize,
}

impl VirtualMachine {
    pub fn new(byte_code: ByteCode) -> VirtualMachine {
        VirtualMachine {
            bytes: byte_code.bytes,
            heap: byte_code.constants,
            globals: vec![0; GLOBALS_SIZE],
            stack: vec![0; STACK_SIZE],
            sp: 0,
            pc: 0,
            gp: 0,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        while self.pc < self.bytes.len() {
            match self.bytes[self.pc] {
                Op::CONSTANT => {
                    let reference = self.u16();
                    self.push(reference)?;
                    self.pc += 2;
                }
                Op::POP => {
                    self.pop();
                }
                Op::UNIT => {
                    self.push(UNIT)?;
                }
                Op::FALSE => {
                    self.push(FALSE)?;
                }
                Op::TRUE => {
                    self.push(TRUE)?;
                }
                Op::ADD | Op::SUB | Op::MUL | Op::DIV | Op::EQ | Op::NEQ | Op::LT | Op::GT => {
                    self.execute_bin_op(self.bytes[self.pc])?;
                }
                Op::MINUS | Op::BANG => {
                    self.execute_un_op(self.bytes[self.pc])?;
                }
                Op::JUMPIFNOT => {
                    let reference = self.pop();

                    match reference {
                        FALSE => {
                            let address = self.u16();
                            self.pc = address as usize;
                            self.pc -= 1;
                        }
                        TRUE => {
                            self.pc += 2;
                        }
                        reference => {
                            return Err(MonkeyError::type_mismatch(format!(
                                "if ({}) {{...}}",
                                self.dereference(reference)?
                            )));
                        }
                    }
                }
                Op::JUMP => {
                    let address = self.u16();
                    self.pc = address as usize;
                    self.pc -= 1;
                }
                Op::SETGLOBAL => {
                    let index = self.u16() as usize;
                    self.gp = self.gp.max(index);
                    let reference = self.pop();
                    self.globals[index] = reference;
                    self.pc += 2;
                }
                Op::GETGLOBAL => {
                    let index = self.u16() as usize;
                    let reference = self.globals[index];
                    self.push(reference)?;
                    self.pc += 2;
                }
                Op::ARRAY => {
                    let n = self.u16() as usize;
                    self.pc += 2;

                    let references = &self.stack[self.sp - n..self.sp];
                    self.sp = self.sp - n;
                    let obj = Object::Array(references.to_vec());

                    let reference = self.reference(obj);
                    self.push(reference)?;
                }
                Op::MAP => {
                    let n = self.u16() as usize;
                    self.pc += 2;

                    let references = &self.stack[self.sp - 2 * n..self.sp];
                    self.sp = self.sp - 2 * n;

                    let pairs: Vec<(u16, u16)> = references
                        .chunks(2)
                        .into_iter()
                        .map(|chunk| (chunk[0], chunk[1]))
                        .collect();
                    let obj = Object::map(&pairs);

                    let reference = self.reference(obj);
                    self.push(reference)?;
                }
                _ => {
                    return Err(MonkeyError::RuntimeError(
                        "invalid bytecode format.".to_string(),
                    ));
                }
            }

            self.pc += 1;
        }

        Ok(())
    }

    fn execute_bin_op(&mut self, op: u8) -> Result<()> {
        let right = self.pop();
        let left = self.pop();

        #[rustfmt::skip]
        let reference = match (left, op, right) {
            (FALSE, Op::EQ, FALSE) => TRUE ,
            (FALSE, Op::EQ, TRUE ) => FALSE,
            (TRUE , Op::EQ, FALSE) => FALSE,
            (TRUE , Op::EQ, TRUE ) => TRUE ,

            (FALSE, Op::NEQ, FALSE) => FALSE,
            (FALSE, Op::NEQ, TRUE ) => TRUE ,
            (TRUE , Op::NEQ, FALSE) => TRUE ,
            (TRUE , Op::NEQ, TRUE ) => FALSE,

            (left, op, right) => {
                let left = self.dereference(left)?;
                let right = self.dereference(right)?;

                match (left, op, right) {
                    (Object::Primitive(Primitive::Integer(i)), Op::ADD, Object::Primitive(Primitive::Integer(j))) => {
                        let obj = Object::integer(i + j);
                        self.reference(obj)
                    }
                    (Object::Primitive(Primitive::Integer(i)), Op::SUB, Object::Primitive(Primitive::Integer(j))) => {
                        let obj = Object::integer(i - j);
                        self.reference(obj)
                    }
                    (Object::Primitive(Primitive::Integer(i)), Op::MUL, Object::Primitive(Primitive::Integer(j))) => {
                        let obj = Object::integer(i * j);
                        self.reference(obj)
                    }
                    (Object::Primitive(Primitive::Integer(i)), Op::DIV, Object::Primitive(Primitive::Integer(j))) => {
                        let obj = Object::integer(i / j);
                        self.reference(obj)
                    }
                    (Object::Primitive(Primitive::Integer(i)), Op::EQ , Object::Primitive(Primitive::Integer(j))) =>
                        if i == j { TRUE } else { FALSE },
                    (Object::Primitive(Primitive::Integer(i)), Op::NEQ, Object::Primitive(Primitive::Integer(j))) =>
                        if i != j { TRUE } else { FALSE },
                    (Object::Primitive(Primitive::Integer(i)), Op::LT , Object::Primitive(Primitive::Integer(j))) =>
                        if i < j  { TRUE } else { FALSE },
                    (Object::Primitive(Primitive::Integer(i)), Op::GT , Object::Primitive(Primitive::Integer(j))) =>
                        if i > j  { TRUE } else { FALSE },

                    (Object::Primitive(Primitive::String(s1)), Op::ADD, Object::Primitive(Primitive::String(s2))) => {
                        let obj = Object::string(s1.clone() + s2);
                        self.reference(obj)
                    }
                    (left, op, right) => {
                        return Err(MonkeyError::type_mismatch(format!(
                            "{} {} {}",
                            left, Op::format(op), right
                        )))
                    }
                }
            }
        };

        self.push(reference)
    }

    fn execute_un_op(&mut self, op: u8) -> Result<()> {
        let reference = self.pop();

        #[rustfmt::skip]
        let reference = match (op, reference) {
            (Op::BANG, FALSE) => TRUE ,
            (Op::BANG, TRUE ) => FALSE,
            (op, reference) => {
                let obj = self.dereference(reference)?;

                match (op, obj) {
                    (Op::MINUS, Object::Primitive(Primitive::Integer(i))) => {
                        let obj = Object::integer(-i);
                        self.reference(obj)
                    }
                    (op, obj) => {
                        return Err(MonkeyError::type_mismatch(format!(
                            "{} {}",
                            Op::format(op),
                            obj
                        )))
                    }
                }
            }
        };

        self.push(reference)
    }

    fn u16(&self) -> u16 {
        u16::from_be_bytes([self.bytes[self.pc + 1], self.bytes[self.pc + 2]])
    }

    fn reference(&mut self, obj: Object) -> Reference {
        self.heap.push(obj);
        (self.heap.len() - 1) as u16
    }

    fn dereference(&self, reference: Reference) -> Result<&Object> {
        if reference as usize >= self.heap.len() {
            Err(MonkeyError::RuntimeError("invalid heap access".to_string()))
        } else {
            Ok(&self.heap[reference as usize])
        }
    }

    fn push(&mut self, reference: Reference) -> Result<()> {
        if self.sp >= STACK_SIZE {
            Err(MonkeyError::RuntimeError("stack overflow".to_string()))
        } else {
            self.stack[self.sp] = reference;
            self.sp += 1;
            Ok(())
        }
    }

    fn pop(&mut self) -> Reference {
        if self.sp > 0 {
            self.sp -= 1;
        }
        self.stack[self.sp]
    }

    pub fn top(&self) -> Result<&Object> {
        let p = if self.sp > 0 { self.sp - 1 } else { 0 };
        self.dereference(self.stack[p])
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use test_case::test_case;

    #[test_case(
        "1",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1)
        ] ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "2",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(2)
        ] ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 + 2",
        1,
        vec![5],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ] ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "1 - 2",
        1,
        vec![5],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(-1)
        ] ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "1 * 2",
        1,
        vec![5],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2)
        ] ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "4 / 2",
        1,
        vec![5],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(4),
            Object::integer(2),
            Object::integer(2)
        ] ;
        "integer arithmetic 06"
    )]
    #[test_case(
        "50 / 2 * 2 + 10 - 5",
        1,
        vec![11],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(50),
            Object::integer(2),
            Object::integer(2),
            Object::integer(10),
            Object::integer(5),
            Object::integer(25),
            Object::integer(50),
            Object::integer(60),
            Object::integer(55),
        ] ;
        "integer arithmetic 07"
    )]
    #[test_case(
        "5 + 5 + 5 + 5 - 10",
        1,
        vec![11],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(5),
            Object::integer(5),
            Object::integer(5),
            Object::integer(10),
            Object::integer(10),
            Object::integer(15),
            Object::integer(20),
            Object::integer(10),
        ] ;
        "integer arithmetic 08"
    )]
    #[test_case(
        "2 * 2 * 2 * 2 * 2",
        1,
        vec![11],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(4),
            Object::integer(8),
            Object::integer(16),
            Object::integer(32),
        ] ;
        "integer arithmetic 09"
    )]
    #[test_case(
        "5 * 2 + 10",
        1,
        vec![7],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(10),
            Object::integer(20),
        ] ;
        "integer arithmetic 10"
    )]
    #[test_case(
        "5 + 2 * 10",
        1,
        vec![7],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(20),
            Object::integer(25),
        ] ;
        "integer arithmetic 11"
    )]
    #[test_case(
        "5 * (2 + 10)",
        1,
        vec![7],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(12),
            Object::integer(60),
        ] ;
        "integer arithmetic 12"
    )]
    #[test_case(
        "-5",
        1,
        vec![4],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(-5),
        ] ;
        "integer arithmetic 13"
    )]
    #[test_case(
        "-50 + 100 + -50",
        1,
        vec![9],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(50),
            Object::integer(100),
            Object::integer(50),
            Object::integer(-50),
            Object::integer(50),
            Object::integer(-50),
            Object::integer(0),
        ] ;
        "integer arithmetic 14"
    )]
    #[test_case(
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        1,
        vec![16],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(10),
            Object::integer(2),
            Object::integer(15),
            Object::integer(3),
            Object::integer(2),
            Object::integer(10),
            Object::integer(20),
            Object::integer(25),
            Object::integer(5),
            Object::integer(30),
            Object::integer(60),
            Object::integer(-10),
            Object::integer(50),
        ] ;
        "integer arithmetic 15"
    )]
    #[test_case(
        "false",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 1",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
        ] ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 1",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
        ] ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 == 2",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
        ] ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 != 2",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
        ] ;
        "boolean expression 06"
    )]
    #[test_case(
        "1 < 2",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
        ] ;
        "boolean expression 07"
    )]
    #[test_case(
        "1 > 2",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
        ] ;
        "boolean expression 08"
    )]
    #[test_case(
        "1 < 1",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
        ] ;
        "boolean expression 09"
    )]
    #[test_case(
        "1 > 1",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
        ] ;
        "boolean expression 10"
    )]
    #[test_case(
        "true == true",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 11"
    )]
    #[test_case(
        "false == false",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 12"
    )]
    #[test_case(
        "true != false",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 13"
    )]
    #[test_case(
        "false != true",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 14"
    )]
    #[test_case(
        "!true",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 15"
    )]
    #[test_case(
        "!false",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 16"
    )]
    #[test_case(
        "!!true",
        1,
        vec![2],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 17"
    )]
    #[test_case(
        "!!false",
        1,
        vec![1],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 18"
    )]
    #[test_case(
        "\"monkey\"",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::string("monkey")
        ] ;
        "string expression 01"
    )]
    #[test_case(
        "\"mon\" + \"key\"",
        1,
        vec![5],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::string("mon"),
            Object::string("key"),
            Object::string("monkey"),
        ] ;
        "string expression 02"
    )]
    #[test_case(
        "\"mon\" + \"key\" + \"banana\"",
        1,
        vec![7],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::string("mon"),
            Object::string("key"),
            Object::string("banana"),
            Object::string("monkey"),
            Object::string("monkeybanana"),
        ] ;
        "string expression 03"
    )]
    #[test_case(
        "[]",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::array(&[]),
        ] ;
        "array expression 01"
    )]
    #[test_case(
        "[1, 2, 3]",
        1,
        vec![6],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[3, 4, 5]),
        ] ;
        "array expression 02"
    )]
    #[test_case(
        "[1 + 2, 3 * 4, 5 + 6]",
        1,
        vec![12],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6),
            Object::integer(3),
            Object::integer(12),
            Object::integer(11),
            Object::array(&[9, 10, 11])
        ] ;
        "array expression 03"
    )]
    #[test_case(
        "{}",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::map(&[]),
        ] ;
        "map expression 01"
    )]
    #[test_case(
        "{1: 2, 3: 4}",
        1,
        vec![7],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::map(&[(3, 4), (5, 6)]),
        ] ;
        "map expression 02"
    )]
    #[test_case(
        "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
        1,
        vec![15],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(3),
            Object::integer(3),
            Object::integer(4),
            Object::integer(4),
            Object::integer(2),
            Object::integer(4),
            Object::integer(6),
            Object::integer(16),
            Object::map(&[(11, 12), (13, 14)]),
        ] ;
        "map expression 03"
    )]
    #[test_case(
        "if (true) { 10 }",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
        ] ;
        "if expr 01"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }",
        1,
        vec![3],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
            Object::integer(20),
        ] ;
        "if expr 02"
    )]
    #[test_case(
        "if (false) { 10 } else { 20 }",
        1,
        vec![4],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
            Object::integer(20),
        ] ;
        "if expr 03"
    )]
    #[test_case(
        "if (false) { 10 }",
        1,
        vec![0],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
        ] ;
        "if expr 04"
    )]
    #[test_case(
        "1; 2",
        1,
        vec![4],
        vec![0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "expr stmt 01"
    )]
    #[test_case(
        "let one = 1; one",
        1,
        vec![3],
        vec![3],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
        ] ;
        "global let stmt 01"
    )]
    #[test_case(
        "let one = 1; let two = 2; one + two",
        1,
        vec![5],
        vec![3, 4],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
        ] ;
        "global let stmt 02"
    )]
    #[test_case(
        "let one = 1; let two = one + one; one + two",
        1,
        vec![5],
        vec![3, 4],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
        ] ;
        "global let stmt 03"
    )]
    fn test(
        input: &str,
        sp: usize,
        stack: Vec<Reference>,
        globals: Vec<Reference>,
        heap: Vec<Object>,
    ) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast).unwrap();
        let mut vm = VirtualMachine::new(byte_code);

        vm.run().unwrap();

        assert_eq!(vm.sp, sp);
        assert_eq!(vm.stack[..vm.sp], stack[..]);
        assert_eq!(vm.globals[..vm.gp + 1], globals[..]);
        assert_eq!(vm.heap, heap)
    }
}
