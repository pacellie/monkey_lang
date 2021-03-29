use crate::compiler::{ByteCode, Op, Reference};
use crate::error::{MonkeyError, Result};
use crate::vm::Object;

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    pub bytes: Vec<u8>,
    pub heap: Vec<Object>,
    pub globals: Vec<Reference>,
    pub stack: Vec<Reference>,
    pub sp: usize,
    pub pc: usize,
}

impl VirtualMachine {
    pub fn new(byte_code: ByteCode) -> VirtualMachine {
        VirtualMachine {
            bytes: byte_code.bytes,
            heap: byte_code.constants,
            globals: vec![],
            stack: vec![],
            sp: 0,
            pc: 0,
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
                    self.pop()?;
                }
                Op::UNIT => {
                    self.push(0)?;
                }
                Op::FALSE => {
                    self.push(1)?;
                }
                Op::TRUE => {
                    self.push(2)?;
                }
                Op::ADD | Op::SUB | Op::MUL | Op::DIV | Op::EQ | Op::NEQ | Op::LT | Op::GT => {
                    self.execute_bin_op(self.bytes[self.pc])?;
                }
                Op::MINUS | Op::BANG => {
                    self.execute_un_op(self.bytes[self.pc])?;
                }
                Op::JUMPIFNOT => {
                    let reference = self.pop()?;
                    let obj = self.dereference(reference)?;

                    match obj {
                        Object::False => {
                            let address = self.u16();
                            self.pc = address as usize;
                            self.pc -= 1;
                        }
                        Object::True => {
                            self.pc += 2;
                        }
                        obj => {
                            return Err(MonkeyError::type_mismatch(format!(
                                "if ({}) {{...}}",
                                obj
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
                    let reference = self.pop()?;

                    while self.globals.len() <= index {
                        self.globals.push(0);
                    }

                    self.globals[index] = reference;
                    self.pc += 2;
                }
                Op::GETGLOBAL => {
                    let index = self.u16() as usize;
                    let reference = self.globals[index];
                    self.push(reference)?;
                    self.pc += 2;
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

    fn u16(&self) -> u16 {
        u16::from_be_bytes([self.bytes[self.pc + 1], self.bytes[self.pc + 2]])
    }

    fn execute_bin_op(&mut self, op: u8) -> Result<()> {
        let right = self.pop()?;
        let left = self.pop()?;

        let left = self.dereference(left)?;
        let right = self.dereference(right)?;

        #[rustfmt::skip]
        let reference = match (left, op, right) {
            (Object::Integer(i), Op::ADD, Object::Integer(j)) => {
                let obj = Object::Integer(i + j);
                self.reference(obj)
            }
            (Object::Integer(i), Op::SUB, Object::Integer(j)) => {
                let obj = Object::Integer(i - j);
                self.reference(obj)
            }
            (Object::Integer(i), Op::MUL, Object::Integer(j)) => {
                let obj = Object::Integer(i * j);
                self.reference(obj)
            }
            (Object::Integer(i), Op::DIV, Object::Integer(j)) => {
                let obj = Object::Integer(i / j);
                self.reference(obj)
            }

            (Object::Integer(i), Op::EQ , Object::Integer(j)) => if i == j { 2 } else { 1 },
            (Object::Integer(i), Op::NEQ, Object::Integer(j)) => if i != j { 2 } else { 1 },
            (Object::Integer(i), Op::LT , Object::Integer(j)) => if i < j  { 2 } else { 1 },
            (Object::Integer(i), Op::GT , Object::Integer(j)) => if i > j  { 2 } else { 1 },

            (Object::False, Op::EQ, Object::False) => 2,
            (Object::False, Op::EQ, Object::True ) => 1,
            (Object::True , Op::EQ, Object::False) => 1,
            (Object::True , Op::EQ, Object::True ) => 2,

            (Object::False, Op::NEQ, Object::False) => 1,
            (Object::False, Op::NEQ, Object::True ) => 2,
            (Object::True , Op::NEQ, Object::False) => 2,
            (Object::True , Op::NEQ, Object::True ) => 1,

            (left, op, right) => {
                return Err(MonkeyError::type_mismatch(format!(
                    "{} {} {}",
                    left, op, right
                )))
            }
        };

        self.push(reference)
    }

    fn execute_un_op(&mut self, op: u8) -> Result<()> {
        let reference = self.pop()?;
        let obj = self.dereference(reference)?;

        let reference = match (op, obj) {
            (Op::MINUS, Object::Integer(i)) => {
                let obj = Object::Integer(-i);
                self.reference(obj)
            }
            (Op::BANG, Object::False) => 2,
            (Op::BANG, Object::True) => 1,
            (op, obj) => return Err(MonkeyError::type_mismatch(format!("{} {}", op, obj))),
        };

        self.push(reference)
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
            if self.sp == self.stack.len() {
                self.stack.push(reference);
            } else {
                self.stack[self.sp] = reference;
            }
            self.sp += 1;
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<Reference> {
        if self.sp == 0 {
            Ok(0)
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp])
        }
    }

    pub fn top(&self) -> Result<&Object> {
        if self.sp == 0 {
            Ok(&self.heap[0])
        } else {
            self.dereference(self.stack[self.sp - 1])
        }
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
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1)
        ] ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "2",
        1,
        vec![3],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(2)
        ] ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 + 2",
        1,
        vec![5, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3)
        ] ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "1 - 2",
        1,
        vec![5, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(-1)
        ] ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "1 * 2",
        1,
        vec![5, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(2)
        ] ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "4 / 2",
        1,
        vec![5, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(4),
            Object::Integer(2),
            Object::Integer(2)
        ] ;
        "integer arithmetic 06"
    )]
    #[test_case(
        "50 / 2 * 2 + 10 - 5",
        1,
        vec![11, 7],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(50),
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(10),
            Object::Integer(5),
            Object::Integer(25),
            Object::Integer(50),
            Object::Integer(60),
            Object::Integer(55),
        ] ;
        "integer arithmetic 07"
    )]
    #[test_case(
        "5 + 5 + 5 + 5 - 10",
        1,
        vec![11, 7],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(5),
            Object::Integer(5),
            Object::Integer(5),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(15),
            Object::Integer(20),
            Object::Integer(10),
        ] ;
        "integer arithmetic 08"
    )]
    #[test_case(
        "2 * 2 * 2 * 2 * 2",
        1,
        vec![11, 7],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(2),
            Object::Integer(4),
            Object::Integer(8),
            Object::Integer(16),
            Object::Integer(32),
        ] ;
        "integer arithmetic 09"
    )]
    #[test_case(
        "5 * 2 + 10",
        1,
        vec![7, 5],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(2),
            Object::Integer(10),
            Object::Integer(10),
            Object::Integer(20),
        ] ;
        "integer arithmetic 10"
    )]
    #[test_case(
        "5 + 2 * 10",
        1,
        vec![7, 6, 5],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(2),
            Object::Integer(10),
            Object::Integer(20),
            Object::Integer(25),
        ] ;
        "integer arithmetic 11"
    )]
    #[test_case(
        "5 * (2 + 10)",
        1,
        vec![7, 6, 5],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(2),
            Object::Integer(10),
            Object::Integer(12),
            Object::Integer(60),
        ] ;
        "integer arithmetic 12"
    )]
    #[test_case(
        "-5",
        1,
        vec![4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(-5),
        ] ;
        "integer arithmetic 13"
    )]
    #[test_case(
        "-50 + 100 + -50",
        1,
        vec![9, 8],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(50),
            Object::Integer(100),
            Object::Integer(50),
            Object::Integer(-50),
            Object::Integer(50),
            Object::Integer(-50),
            Object::Integer(0),
        ] ;
        "integer arithmetic 14"
    )]
    #[test_case(
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        1,
        vec![16, 15, 7],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(5),
            Object::Integer(10),
            Object::Integer(2),
            Object::Integer(15),
            Object::Integer(3),
            Object::Integer(2),
            Object::Integer(10),
            Object::Integer(20),
            Object::Integer(25),
            Object::Integer(5),
            Object::Integer(30),
            Object::Integer(60),
            Object::Integer(-10),
            Object::Integer(50),
        ] ;
        "integer arithmetic 15"
    )]
    #[test_case(
        "false",
        1,
        vec![1],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        1,
        vec![2],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 1",
        1,
        vec![2, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(1),
        ] ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 1",
        1,
        vec![1, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(1),
        ] ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 == 2",
        1,
        vec![1, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
        ] ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 != 2",
        1,
        vec![2, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
        ] ;
        "boolean expression 06"
    )]
    #[test_case(
        "1 < 2",
        1,
        vec![2, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
        ] ;
        "boolean expression 07"
    )]
    #[test_case(
        "1 > 2",
        1,
        vec![1, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
        ] ;
        "boolean expression 08"
    )]
    #[test_case(
        "1 < 1",
        1,
        vec![1, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(1),
        ] ;
        "boolean expression 09"
    )]
    #[test_case(
        "1 > 1",
        1,
        vec![1, 4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(1),
        ] ;
        "boolean expression 10"
    )]
    #[test_case(
        "true == true",
        1,
        vec![2, 2],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 11"
    )]
    #[test_case(
        "false == false",
        1,
        vec![2, 1],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 12"
    )]
    #[test_case(
        "true != false",
        1,
        vec![2, 1],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 13"
    )]
    #[test_case(
        "false != true",
        1,
        vec![2, 2],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 14"
    )]
    #[test_case(
        "!true",
        1,
        vec![1],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 15"
    )]
    #[test_case(
        "!false",
        1,
        vec![2],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 16"
    )]
    #[test_case(
        "!!true",
        1,
        vec![2],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 17"
    )]
    #[test_case(
        "!!false",
        1,
        vec![1],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
        ] ;
        "boolean expression 18"
    )]
    #[test_case(
        "if (true) { 10 }",
        1,
        vec![3],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(10),
        ] ;
        "if expr 01"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }",
        1,
        vec![3],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(10),
            Object::Integer(20),
        ] ;
        "if expr 02"
    )]
    #[test_case(
        "if (false) { 10 } else { 20 }",
        1,
        vec![4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(10),
            Object::Integer(20),
        ] ;
        "if expr 03"
    )]
    #[test_case(
        "if (false) { 10 }",
        1,
        vec![0],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(10),
        ] ;
        "if expr 04"
    )]
    #[test_case(
        "1; 2",
        1,
        vec![4],
        vec![],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2)
        ] ;
        "expr stmt 01"
    )]
    #[test_case(
        "let one = 1; one",
        1,
        vec![3],
        vec![3],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
        ] ;
        "global let stmt 01"
    )]
    #[test_case(
        "let one = 1; let two = 2; one + two",
        1,
        vec![5, 4],
        vec![3, 4],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
        ] ;
        "global let stmt 02"
    )]
    #[test_case(
        "let one = 1; let two = one + one; one + two",
        1,
        vec![5, 4],
        vec![3, 4],
        vec![
            Object::Unit,
            Object::False,
            Object::True,
            Object::Integer(1),
            Object::Integer(2),
            Object::Integer(3),
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
        assert_eq!(vm.stack, stack);
        assert_eq!(vm.globals, globals);
        assert_eq!(vm.heap, heap)
    }
}
