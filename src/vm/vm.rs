use crate::compiler::{Binary, ByteCode, Op, Reference};
use crate::vm::{Object, Result, RuntimeError};

const STACK_SIZE: usize = 2048;

pub struct VirtualMachine {
    binary: Binary,
    constants: Vec<Object>,
    stack: Vec<Reference>,
    sp: usize,
}

impl VirtualMachine {
    pub fn new(byte_code: ByteCode) -> VirtualMachine {
        VirtualMachine {
            binary: byte_code.binary,
            constants: byte_code.constants,
            stack: vec![0; STACK_SIZE],
            sp: 0,
        }
    }

    pub fn run(&mut self) -> Result<()> {
        let mut pc = 0;

        while pc < self.binary.len() {
            if let Some((op, i)) = Op::decode(&self.binary.bytes[pc..]) {
                self.execute(op)?;
                pc += i;
            } else {
                return Err(RuntimeError("invalid bytecode format.".to_string()));
            }
        }

        Ok(())
    }

    fn execute(&mut self, op: Op) -> Result<()> {
        match op {
            Op::Constant(reference) => self.push(reference),
            Op::Add => {
                let right = self.pop()?;
                let left = self.pop()?;

                let left = self.dereference(left)?;
                let right = self.dereference(right)?;

                match (left, right) {
                    (Object::Integer(i), Object::Integer(j)) => {
                        let obj = Object::Integer(i + j);
                        let reference = self.reference(obj);
                        self.push(reference)
                    }
                }
            }
        }
    }

    fn reference(&mut self, obj: Object) -> Reference {
        self.constants.push(obj);
        (self.constants.len() - 1) as u16
    }

    fn dereference(&self, reference: Reference) -> Result<&Object> {
        if reference as usize >= self.constants.len() {
            Err(RuntimeError("invalid heap access".to_string()))
        } else {
            Ok(&self.constants[reference as usize])
        }
    }

    fn push(&mut self, reference: Reference) -> Result<()> {
        if self.sp >= STACK_SIZE {
            Err(RuntimeError("stack Overflow".to_string()))
        } else {
            self.stack[self.sp] = reference;
            self.sp += 1;
            Ok(())
        }
    }

    fn pop(&mut self) -> Result<Reference> {
        if self.sp == 0 {
            Err(RuntimeError("stack Underflow".to_string()))
        } else {
            self.sp -= 1;
            Ok(self.stack[self.sp])
        }
    }

    pub fn top(&self) -> Result<&Object> {
        if self.sp == 0 {
            Err(RuntimeError("stack Underflow".to_string()))
        } else {
            self.dereference(self.stack[self.sp])
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

    #[test_case("1"    , Object::Integer(1) ; "integer arithmetic 01")]
    #[test_case("2"    , Object::Integer(2) ; "integer arithmetic 02")]
    #[test_case("1 + 2", Object::Integer(3) ; "integer arithmetic 03")]
    fn test(input: &str, top: Object) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let (ast, _) = parser.parse();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast);
        let mut vm = VirtualMachine::new(byte_code);

        vm.run().unwrap();
        let obj = vm.top().unwrap();

        assert_eq!(obj, &top)
    }
}
