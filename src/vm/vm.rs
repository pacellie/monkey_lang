use crate::compiler::{ByteCode, Op, Reference};
use crate::error::{MonkeyError, Result};
use crate::vm::{Object, Primitive};

use std::collections::HashMap;

const STACK_SIZE: usize = 32;
const GLOBALS_SIZE: usize = 32;
const FRAMES_SIZE: usize = 32;

const UNIT: u16 = 0;
const FALSE: u16 = 1;
const TRUE: u16 = 2;

pub struct Frame {
    bytes: Vec<u8>,
    pc: i32,
    fp: usize,
}

impl Frame {
    pub fn new(bytes: Vec<u8>, fp: usize) -> Frame {
        Frame { bytes, pc: -1, fp }
    }
}

pub struct VirtualMachine {
    pub heap: Vec<Object>,
    pub globals: Vec<Reference>,
    pub frames: Vec<Frame>,
    pub fi: usize,
    pub stack: Vec<Reference>,
    pub sp: usize,
}

impl VirtualMachine {
    pub fn new(byte_code: ByteCode) -> VirtualMachine {
        let mut main_frame = Frame::new(byte_code.bytes, 0);
        main_frame.pc = 0;
        let mut frames: Vec<_> = vec![0; FRAMES_SIZE]
            .iter()
            .map(|_| Frame::new(vec![], 0))
            .collect();
        frames[0] = main_frame;

        VirtualMachine {
            heap: byte_code.constants,
            globals: vec![0; GLOBALS_SIZE],
            frames,
            fi: 1,
            stack: vec![0; STACK_SIZE],
            sp: 0,
        }
    }

    fn frame(&self) -> &Frame {
        &self.frames[self.fi - 1]
    }

    fn pc(&self) -> usize {
        self.frames[self.fi - 1].pc as usize
    }

    fn pc_mut(&mut self) -> &mut i32 {
        &mut self.frames[self.fi - 1].pc
    }

    fn push_frame(&mut self, frame: Frame) {
        self.frames[self.fi] = frame;
        self.fi += 1;
    }

    fn pop_frame(&mut self) -> &Frame {
        self.fi -= 1;
        &self.frames[self.fi]
    }

    fn read_u8(&self) -> u8 {
        self.frame().bytes[self.pc() + 1]
    }

    fn read_u16(&self) -> u16 {
        u16::from_be_bytes([
            self.frame().bytes[self.pc() + 1],
            self.frame().bytes[self.pc() + 2],
        ])
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

    pub fn run(&mut self) -> Result<()> {
        while self.pc() < self.frame().bytes.len() {
            let op = self.frame().bytes[self.pc()];

            print!("{} ", Op::format(op));

            match op {
                Op::CONSTANT => self.constant()?,
                Op::POP => {
                    self.pop();
                }
                Op::UNIT => self.push(UNIT)?,
                Op::FALSE => self.push(FALSE)?,
                Op::TRUE => self.push(TRUE)?,
                Op::ADD | Op::SUB | Op::MUL | Op::DIV | Op::EQ | Op::NEQ | Op::LT | Op::GT => {
                    self.bin_op(self.frame().bytes[self.pc()])?
                }
                Op::MINUS | Op::BANG => self.un_op(self.frame().bytes[self.pc()])?,
                Op::JUMPIFNOT => self.jump_if_not()?,
                Op::JUMP => self.jump(),
                Op::SETGLOBAL => self.set_global(),
                Op::GETGLOBAL => self.get_global()?,
                Op::ARRAY => self.array()?,
                Op::MAP => self.map()?,
                Op::INDEX => self.index()?,
                Op::CALL => self.call()?,
                Op::RETURN => self.ret()?,
                Op::SETLOCAL => self.set_local(),
                Op::GETLOCAL => self.get_local()?,
                _ => {
                    return Err(MonkeyError::RuntimeError(
                        "invalid bytecode format.".to_string(),
                    ));
                }
            }

            println!("{:?}", self.stack);

            *self.pc_mut() += 1;
        }

        Ok(())
    }

    fn constant(&mut self) -> Result<()> {
        let reference = self.read_u16();
        self.push(reference)?;
        *self.pc_mut() += 2;

        Ok(())
    }

    fn bin_op(&mut self, op: u8) -> Result<()> {
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

    fn un_op(&mut self, op: u8) -> Result<()> {
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
                            "{}{}",
                            Op::format(op),
                            obj
                        )))
                    }
                }
            }
        };

        self.push(reference)
    }

    fn jump_if_not(&mut self) -> Result<()> {
        let reference = self.pop();

        match reference {
            FALSE => {
                let address = self.read_u16();
                *self.pc_mut() = address as i32;
                *self.pc_mut() -= 1;
            }
            TRUE => {
                *self.pc_mut() += 2;
            }
            reference => {
                return Err(MonkeyError::type_mismatch(format!(
                    "if ({}) {{...}}",
                    self.dereference(reference)?
                )));
            }
        }

        Ok(())
    }

    fn jump(&mut self) {
        let address = self.read_u16();
        *self.pc_mut() = address as i32;
        *self.pc_mut() -= 1;
    }

    fn set_global(&mut self) {
        let index = self.read_u16() as usize;
        let reference = self.pop();
        self.globals[index] = reference;
        *self.pc_mut() += 2;
    }

    fn get_global(&mut self) -> Result<()> {
        let index = self.read_u16() as usize;
        let reference = self.globals[index];
        self.push(reference)?;

        *self.pc_mut() += 2;
        Ok(())
    }

    fn array(&mut self) -> Result<()> {
        let n = self.read_u16() as usize;

        let references = &self.stack[self.sp - n..self.sp];
        self.sp = self.sp - n;
        let obj = Object::Array(references.to_vec());

        let reference = self.reference(obj);

        *self.pc_mut() += 2;
        self.push(reference)
    }

    fn map(&mut self) -> Result<()> {
        let n = self.read_u16() as usize;

        let references = &self.stack[self.sp - 2 * n..self.sp];
        self.sp = self.sp - 2 * n;

        let mut hm = HashMap::new();
        for pair in references.chunks(2) {
            match self.dereference(pair[0])? {
                Object::Primitive(p) => {
                    hm.insert(p.clone(), pair[1]);
                }
                obj => return Err(MonkeyError::type_mismatch(format!("{{{}: ... }}", obj))),
            }
        }

        let reference = self.reference(Object::Map(hm));

        *self.pc_mut() += 2;
        self.push(reference)
    }

    fn index(&mut self) -> Result<()> {
        let index = self.pop();
        let indexable = self.pop();

        let index = self.dereference(index)?;
        let indexable = self.dereference(indexable)?;

        let reference = match (indexable, index) {
            (Object::Array(vec), Object::Primitive(Primitive::Integer(i))) => {
                if 0 <= *i && (*i as usize) < vec.len() {
                    vec[*i as usize]
                } else {
                    return Err(MonkeyError::index_out_of_bounds(*i, vec.len() - 1));
                }
            }
            (Object::Map(map), Object::Primitive(key)) => match map.get(key) {
                Some(value) => *value,
                None => return Err(MonkeyError::missing_index(format!("{}", key))),
            },
            (indexable, index) => {
                return Err(MonkeyError::type_mismatch(format!(
                    "{}[{}]",
                    indexable, index
                )))
            }
        };

        self.push(reference)
    }

    fn call(&mut self) -> Result<()> {
        let reference = self.stack[self.sp - 1];
        let obj = self.dereference(reference)?;

        if let Object::Function { bytes, locals } = obj {
            let frame = Frame::new(bytes.clone(), self.sp);
            self.sp = frame.fp + locals;
            self.push_frame(frame);
        } else {
            return Err(MonkeyError::type_mismatch(format!("{}(...)", obj)));
        }

        Ok(())
    }

    fn ret(&mut self) -> Result<()> {
        let reference = self.pop();

        let fp = self.pop_frame().fp;
        self.sp = fp - 1;

        self.push(reference)
    }

    fn set_local(&mut self) {
        let index = self.read_u8() as usize;
        let fp = self.frame().fp;

        self.stack[fp + index] = self.pop();

        *self.pc_mut() += 1;
    }

    fn get_local(&mut self) -> Result<()> {
        let index = self.read_u8() as usize;
        let fp = self.frame().fp;

        let reference = self.stack[fp + index];

        *self.pc_mut() += 1;

        self.push(reference)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::Compiler;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use test_case::test_case;

    fn encode(ops: Vec<Op>) -> Vec<u8> {
        let mut compiler = Compiler::new();

        for op in ops {
            compiler.encode(op);
        }

        compiler.scopes[0].bytes.clone()
    }

    #[test_case(
        "1",
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![4],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![16],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 1",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 11"
    )]
    #[test_case(
        "false == false",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 12"
    )]
    #[test_case(
        "true != false",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 13"
    )]
    #[test_case(
        "false != true",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 14"
    )]
    #[test_case(
        "!true",
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 15"
    )]
    #[test_case(
        "!false",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 16"
    )]
    #[test_case(
        "!!true",
        vec![2],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 17"
    )]
    #[test_case(
        "!!false",
        vec![1],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 18"
    )]
    #[test_case(
        "\"monkey\"",
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![6],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![12],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::map(&[
                (Primitive::Integer(1), 4),
                (Primitive::Integer(3), 6)
            ]),
        ] ;
        "map expression 02"
    )]
    #[test_case(
        "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
        vec![15],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
            Object::map(&[
                (Primitive::Integer(2), 12),
                (Primitive::Integer(6), 14),
            ]),
        ] ;
        "map expression 03"
    )]
    #[test_case(
        "[1, 2, 3][1]",
        vec![4],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(1),
            Object::array(&[3, 4, 5]),
        ] ;
        "index expression 01"
    )]
    #[test_case(
        "[1, 2, 3][0 + 2]",
        vec![5],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(0),
            Object::integer(2),
            Object::array(&[3, 4, 5]),
            Object::integer(2),
        ] ;
        "index expression 02"
    )]
    #[test_case(
        "[[1, 1, 1]][0][0]",
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
            Object::integer(1),
            Object::integer(0),
            Object::integer(0),
            Object::array(&[3, 4, 5]),
            Object::array(&[8]),
        ] ;
        "index expression 03"
    )]
    #[test_case(
        "{1: 1, 2: 2}[1]",
        vec![4],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(1),
            Object::map(&[
                (Primitive::Integer(1), 4),
                (Primitive::Integer(2), 6),
            ]),
        ] ;
        "index expression 04"
    )]
    #[test_case(
        "{1: 1, 2: 2}[2]",
        vec![6],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::map(&[
                (Primitive::Integer(1), 4),
                (Primitive::Integer(2), 6),
            ]),
        ] ;
        "index expression 05"
    )]
    #[test_case(
        "if (true) { 10 }",
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![3],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![4],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![0],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![4],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![3],
        vec![3, 0, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![3, 4, 0, 0, 0, 0, 0, 0, 0, 0],
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
        vec![5],
        vec![3, 4, 0, 0, 0, 0, 0, 0, 0, 0],
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
    #[test_case(
        "let f = fn() { 5 + 10 }; f()",
        vec![6],
        vec![5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(5),
            Object::integer(10),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Constant(4),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(15),
        ] ;
        "function call 01"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { 2 }; f() + g()",
        vec![7],
        vec![4, 6, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(5),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(3),
        ] ;
        "function call 02"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { f() + 2 }; let h = fn() { g() + 3 }; h()",
        vec![10],
        vec![4, 6, 8, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Call,
                    Op::Constant(5),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(3),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(1),
                    Op::Call,
                    Op::Constant(7),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0
            },
            Object::integer(3),
            Object::integer(6),
        ] ;
        "function call 03"
    )]
    #[test_case(
        "let f = fn() { return 1; 2 }; f()",
        vec![3],
        vec![5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                    Op::Constant(4),
                    Op::Return,
                ]),
                locals: 0
            },
        ] ;
        "function call 04"
    )]
    #[test_case(
        "let f = fn() { return 1; return 2; }; f()",
        vec![3],
        vec![5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                    Op::Constant(4),
                    Op::Return,
                ]),
                locals: 0
            },
        ] ;
        "function call 05"
    )]
    #[test_case(
        "let f = fn() { }; f()",
        vec![0],
        vec![3, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(0),
                    Op::Return,
                ]),
                locals: 0
            },
        ] ;
        "function call 06"
    )]
    #[test_case(
        "let f = fn() { }; let g = fn() { f() }; g()",
        vec![0],
        vec![3, 4, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(0),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Call,
                    Op::Return,
                ]),
                locals: 0
            },
        ] ;
        "function call 07"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { f }; g()()",
        vec![3],
        vec![4, 5, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Return,
                ]),
                locals: 0
            },
        ] ;
        "function call 08"
    )]
    #[test_case(
        "let f = fn() { let x = 1; x }; f()",
        vec![3],
        vec![4, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1
            },
        ] ;
        "function call 09"
    )]
    #[test_case(
        "let f = fn() { let x = 1; let y = 2; x + y }; f()",
        vec![6],
        vec![5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::SetLocal(0),
                    Op::Constant(4),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2
            },
            Object::integer(3),
        ] ;
        "function call 10"
    )]
    #[test_case(
        "let f = fn() { let x = 1; let y = 2; x + y }; let g = fn() { let a = 3; let b = 4; a + b }; f() + g()",
        vec![11],
        vec![5, 8, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::SetLocal(0),
                    Op::Constant(4),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2
            },
            Object::integer(3),
            Object::integer(4),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(6),
                    Op::SetLocal(0),
                    Op::Constant(7),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2
            },
            Object::integer(3),
            Object::integer(7),
            Object::integer(10),
        ] ;
        "function call 11"
    )]
    #[test_case(
        "let a = 42; let f = fn() { let x = 1; a - x }; let g = fn() { let x = 2; a - x }; f() + g()",
        vec![10],
        vec![3, 5, 7, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(42),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(4),
                    Op::SetLocal(0),
                    Op::GetGlobal(0),
                    Op::GetLocal(0),
                    Op::Sub,
                    Op::Return,
                ]),
                locals: 1
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(6),
                    Op::SetLocal(0),
                    Op::GetGlobal(0),
                    Op::GetLocal(0),
                    Op::Sub,
                    Op::Return,
                ]),
                locals: 1
            },
            Object::integer(41),
            Object::integer(40),
            Object::integer(81),
        ] ;
        "function call 12"
    )]
    #[test_case(
        "let f = fn() { let g = fn() { 1 }; g }; f()()",
        vec![3],
        vec![5, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(3),
                    Op::Return,
                ]),
                locals: 0
            },
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(4),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1
            },
        ] ;
        "function call 13"
    )]
    fn test(input: &str, stack: Vec<Reference>, globals: Vec<Reference>, heap: Vec<Object>) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast).unwrap();
        let mut vm = VirtualMachine::new(byte_code);

        vm.run().unwrap();

        assert_eq!(vm.stack[..vm.sp], stack[..]);
        assert_eq!(vm.globals[0..10], globals[..]);
        assert_eq!(vm.heap, heap)
    }

    #[test_case("5 + true"              , MonkeyError::type_mismatch("5 + true")        ; "error 01")]
    #[test_case("5 + true; 5;"          , MonkeyError::type_mismatch("5 + true")        ; "error 02")]
    #[test_case("-true"                 , MonkeyError::type_mismatch("-true")           ; "error 03")]
    #[test_case("!5"                    , MonkeyError::type_mismatch("!5")              ; "error 04")]
    #[test_case("5; true + false; 5"    , MonkeyError::type_mismatch("true + false")    ; "error 05")]
    #[test_case("if (1) { 10 }"         , MonkeyError::type_mismatch("if (1) {...}")    ; "error 06")]
    // #[test_case("len(1)"                , MonkeyError::type_mismatch("len(1)")          ; "error 07")]
    // #[test_case("len(\"one\", \"two\")" , MonkeyError::wrong_number_of_args(1, 2)       ; "error 08")]
    #[test_case("[1, 2, 3][3]"          , MonkeyError::index_out_of_bounds(3, 2)        ; "error 09")]
    #[test_case("[1, 2, 3][-1]"         , MonkeyError::index_out_of_bounds(-1, 2)       ; "error 10")]
    #[test_case("{\"foo\": 5}[\"bar\"]" , MonkeyError::missing_index("\"bar\"")         ; "error 11")]
    #[test_case("{}[\"foo\"]"           , MonkeyError::missing_index("\"foo\"")         ; "error 12")]
    // #[test_case("{}[fn(x) { x }]"       , MonkeyError::type_mismatch("{}[fn(x) { x }]") ; "error 13")]
    fn test_error(input: &str, expected: MonkeyError) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast).unwrap();
        let mut vm = VirtualMachine::new(byte_code);
        let error = vm.run().unwrap_err();

        assert_eq!(error, expected)
    }
}
