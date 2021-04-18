use crate::builtin::Builtin;
use crate::compiler::{ByteCode, Op, Reference};
use crate::error::{MonkeyError, Result};
use crate::vm::{Object, Primitive};

use std::collections::HashMap;

const STACK_SIZE: usize = 2 << 11;
const GLOBALS_SIZE: usize = 2 << 16;
const FRAMES_SIZE: usize = 2 << 10;

const UNIT: u16 = 6;
const FALSE: u16 = 7;
const TRUE: u16 = 8;

pub struct Frame {
    closure: Reference,
    bytes: Vec<u8>,
    frees: Vec<Reference>,
    pc: i32,
    fp: usize,
}

impl Frame {
    pub fn new(closure: Reference, bytes: Vec<u8>, frees: Vec<Reference>, fp: usize) -> Frame {
        Frame {
            closure,
            bytes,
            frees,
            pc: -1,
            fp,
        }
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
        let mut main_frame = Frame::new(0, byte_code.bytes, vec![], 0);
        main_frame.pc = 0;
        let mut frames: Vec<_> = vec![0; FRAMES_SIZE]
            .iter()
            .map(|_| Frame::new(0, vec![], vec![], 0))
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
                Op::GETBUILTIN => self.get_builtin()?,
                Op::CLOSURE => self.closure()?,
                Op::GETFREE => self.get_free()?,
                Op::CURRENT => self.current()?,
                _ => {
                    return Err(MonkeyError::RuntimeError(
                        "invalid bytecode format.".to_string(),
                    ))
                }
            }

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

                match (left, right) {
                    (Object::Primitive(left), Object::Primitive(right)) =>
                        match (left, op, right) {
                            (Primitive::Integer(i), Op::ADD, Primitive::Integer(j)) => {
                                let obj = Object::integer(i + j);
                                self.reference(obj)
                            }
                            (Primitive::Integer(i), Op::SUB, Primitive::Integer(j)) => {
                                let obj = Object::integer(i - j);
                                self.reference(obj)
                            }
                            (Primitive::Integer(i), Op::MUL, Primitive::Integer(j)) => {
                                let obj = Object::integer(i * j);
                                self.reference(obj)
                            }
                            (Primitive::Integer(i), Op::DIV, Primitive::Integer(j)) => {
                                let obj = Object::integer(i / j);
                                self.reference(obj)
                            }
                            (Primitive::Integer(i), Op::EQ , Primitive::Integer(j)) =>
                                if i == j { TRUE } else { FALSE },
                            (Primitive::Integer(i), Op::NEQ, Primitive::Integer(j)) =>
                                if i != j { TRUE } else { FALSE },
                            (Primitive::Integer(i), Op::LT , Primitive::Integer(j)) =>
                                if i < j  { TRUE } else { FALSE },
                            (Primitive::Integer(i), Op::GT , Primitive::Integer(j)) =>
                                if i > j  { TRUE } else { FALSE },

                            (Primitive::String(s1), Op::ADD, Primitive::String(s2)) => {
                                let obj = Object::string(s1.clone() + s2);
                                self.reference(obj)
                            }
                            (left, op, right) =>
                                return Err(MonkeyError::type_mismatch(format!(
                                    "{} {} {}",
                                    left, Op::format(op), right
                                ))),
                        }
                    (left, right) =>
                        return Err(MonkeyError::type_mismatch(format!(
                            "{} {} {}",
                            left, Op::format(op), right
                        ))),
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
                    (op, obj) =>
                        return Err(MonkeyError::type_mismatch(format!(
                            "{}{}",
                            Op::format(op),
                            obj
                        ))),
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
                )))
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
        let n_args = self.read_u8() as usize;
        *self.pc_mut() += 1;

        let reference = self.stack[self.sp - 1 - n_args];
        let obj = self.dereference(reference)?;

        match obj.clone() {
            Object::Closure { fun, frees } => self.call_closure(reference, fun, frees, n_args),
            Object::Builtin(builtin) => self.call_builtin(builtin, n_args),
            _ => Err(MonkeyError::type_mismatch(format!("{}(...)", obj))),
        }
    }

    fn call_closure(
        &mut self,
        closure: Reference,
        fun: Reference,
        frees: Vec<Reference>,
        n_args: usize,
    ) -> Result<()> {
        match self.dereference(fun)? {
            Object::Function {
                bytes,
                locals,
                params,
            } => {
                if n_args == *params {
                    let frame = Frame::new(closure, bytes.clone(), frees, self.sp - n_args);
                    self.sp = frame.fp + locals;
                    self.push_frame(frame);
                    Ok(())
                } else {
                    Err(MonkeyError::wrong_number_of_args(*params, n_args))
                }
            }
            obj => Err(MonkeyError::type_mismatch(format!("{}(...)", obj))),
        }
    }

    fn call_builtin(&mut self, builtin: Builtin, n_args: usize) -> Result<()> {
        let args = &self.stack[self.sp - n_args..self.sp];
        self.sp = self.sp - n_args - 1;

        let reference = match builtin {
            Builtin::Len => match args {
                [reference] => {
                    let obj = self.dereference(*reference)?;
                    let len = match obj {
                        Object::Primitive(Primitive::String(s)) => s.len() as i32,
                        Object::Array(vec) => vec.len() as i32,
                        _ => return Err(MonkeyError::type_mismatch(format!("len({})", obj))),
                    };
                    self.reference(Object::integer(len))
                }
                _ => return Err(MonkeyError::wrong_number_of_args(1, n_args)),
            },
            Builtin::First => match args {
                [reference] => {
                    let obj = self.dereference(*reference)?;
                    match obj {
                        Object::Array(vec) => {
                            if vec.len() != 0 {
                                vec[0]
                            } else {
                                return Err(MonkeyError::runtime_error("first([])"));
                            }
                        }
                        _ => return Err(MonkeyError::type_mismatch(format!("first({})", obj))),
                    }
                }
                _ => return Err(MonkeyError::wrong_number_of_args(1, n_args)),
            },
            Builtin::Last => match args {
                [reference] => {
                    let obj = self.dereference(*reference)?;
                    match obj {
                        Object::Array(vec) => {
                            if vec.len() != 0 {
                                vec[vec.len() - 1]
                            } else {
                                return Err(MonkeyError::runtime_error("last([])"));
                            }
                        }
                        _ => return Err(MonkeyError::type_mismatch(format!("last({})", obj))),
                    }
                }
                _ => return Err(MonkeyError::wrong_number_of_args(1, n_args)),
            },
            Builtin::Rest => match args {
                [reference] => {
                    let obj = self.dereference(*reference)?;
                    match obj {
                        Object::Array(vec) => {
                            if vec.len() != 0 {
                                let obj = Object::Array(vec[1..].to_vec());
                                self.reference(obj)
                            } else {
                                return Err(MonkeyError::runtime_error("rest([])"));
                            }
                        }
                        _ => return Err(MonkeyError::type_mismatch(format!("rest({})", obj))),
                    }
                }
                _ => return Err(MonkeyError::wrong_number_of_args(1, n_args)),
            },
            Builtin::Push => match args {
                [array, reference] => {
                    let obj = self.dereference(*array)?;
                    match obj {
                        Object::Array(vec) => {
                            let mut vec = vec.clone();
                            vec.push(*reference);
                            let obj = Object::Array(vec);
                            self.reference(obj)
                        }
                        _ => return Err(MonkeyError::type_mismatch(format!("push({}, ...)", obj))),
                    }
                }
                _ => return Err(MonkeyError::wrong_number_of_args(2, n_args)),
            },
            Builtin::Puts => {
                for arg in args {
                    let obj = self.dereference(*arg)?;
                    print!("{}", obj);
                }
                UNIT
            }
        };

        self.push(reference)
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

    fn get_builtin(&mut self) -> Result<()> {
        let reference = self.read_u8() as u16;

        *self.pc_mut() += 1;

        self.push(reference)
    }

    fn closure(&mut self) -> Result<()> {
        let fun = self.read_u16();
        *self.pc_mut() += 2;

        let n_free = self.read_u8() as usize;
        *self.pc_mut() += 1;

        let mut frees = vec![0; n_free];
        for i in 0..n_free {
            frees[i] = self.stack[self.sp - n_free + i];
        }
        self.sp = self.sp - n_free;

        let closure = Object::Closure { fun, frees };
        let reference = self.reference(closure);

        self.push(reference)
    }

    fn get_free(&mut self) -> Result<()> {
        let index = self.read_u8() as usize;
        *self.pc_mut() += 1;

        let reference = self.frame().frees[index];
        self.push(reference)
    }

    fn current(&mut self) -> Result<()> {
        let current_closure = self.frame().closure;
        self.push(current_closure)
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

    fn concat<A: Clone>(xs: Vec<A>, ys: Vec<A>) -> Vec<A> {
        let zs: Vec<A> = xs.iter().cloned().chain(ys.iter().cloned()).collect();
        zs
    }

    fn heap(objs: Vec<Object>) -> Vec<Object> {
        concat(Compiler::static_constants(), objs)
    }

    #[test_case(
        "1",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1)
        ]) ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "2",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(2)
        ]) ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 + 2",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ]) ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "1 - 2",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(-1)
        ]) ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "1 * 2",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(2)
        ]) ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "4 / 2",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(4),
            Object::integer(2),
            Object::integer(2)
        ]) ;
        "integer arithmetic 06"
    )]
    #[test_case(
        "50 / 2 * 2 + 10 - 5",
        vec![17],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(50),
            Object::integer(2),
            Object::integer(2),
            Object::integer(10),
            Object::integer(5),
            Object::integer(25),
            Object::integer(50),
            Object::integer(60),
            Object::integer(55),
        ]) ;
        "integer arithmetic 07"
    )]
    #[test_case(
        "5 + 5 + 5 + 5 - 10",
        vec![17],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(5),
            Object::integer(5),
            Object::integer(5),
            Object::integer(10),
            Object::integer(10),
            Object::integer(15),
            Object::integer(20),
            Object::integer(10),
        ]) ;
        "integer arithmetic 08"
    )]
    #[test_case(
        "2 * 2 * 2 * 2 * 2",
        vec![17],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::integer(4),
            Object::integer(8),
            Object::integer(16),
            Object::integer(32),
        ]) ;
        "integer arithmetic 09"
    )]
    #[test_case(
        "5 * 2 + 10",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(10),
            Object::integer(20),
        ]) ;
        "integer arithmetic 10"
    )]
    #[test_case(
        "5 + 2 * 10",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(20),
            Object::integer(25),
        ]) ;
        "integer arithmetic 11"
    )]
    #[test_case(
        "5 * (2 + 10)",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(2),
            Object::integer(10),
            Object::integer(12),
            Object::integer(60),
        ]) ;
        "integer arithmetic 12"
    )]
    #[test_case(
        "-5",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(-5),
        ]) ;
        "integer arithmetic 13"
    )]
    #[test_case(
        "-50 + 100 + -50",
        vec![15],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(50),
            Object::integer(100),
            Object::integer(50),
            Object::integer(-50),
            Object::integer(50),
            Object::integer(-50),
            Object::integer(0),
        ]) ;
        "integer arithmetic 14"
    )]
    #[test_case(
        "(5 + 10 * 2 + 15 / 3) * 2 + -10",
        vec![22],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
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
        ]) ;
        "integer arithmetic 15"
    )]
    #[test_case(
        "false",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 1",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
        ]) ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 1",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
        ]) ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 == 2",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
        ]) ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 != 2",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
        ]) ;
        "boolean expression 06"
    )]
    #[test_case(
        "1 < 2",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
        ]) ;
        "boolean expression 07"
    )]
    #[test_case(
        "1 > 2",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
        ]) ;
        "boolean expression 08"
    )]
    #[test_case(
        "1 < 1",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
        ]) ;
        "boolean expression 09"
    )]
    #[test_case(
        "1 > 1",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
        ]) ;
        "boolean expression 10"
    )]
    #[test_case(
        "true == true",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 11"
    )]
    #[test_case(
        "false == false",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 12"
    )]
    #[test_case(
        "true != false",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 13"
    )]
    #[test_case(
        "false != true",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 14"
    )]
    #[test_case(
        "!true",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 15"
    )]
    #[test_case(
        "!false",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 16"
    )]
    #[test_case(
        "!!true",
        vec![8],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 17"
    )]
    #[test_case(
        "!!false",
        vec![7],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![]) ;
        "boolean expression 18"
    )]
    #[test_case(
        "\"monkey\"",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string("monkey")
        ]) ;
        "string expression 01"
    )]
    #[test_case(
        "\"mon\" + \"key\"",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string("mon"),
            Object::string("key"),
            Object::string("monkey"),
        ]) ;
        "string expression 02"
    )]
    #[test_case(
        "\"mon\" + \"key\" + \"banana\"",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string("mon"),
            Object::string("key"),
            Object::string("banana"),
            Object::string("monkey"),
            Object::string("monkeybanana"),
        ]) ;
        "string expression 03"
    )]
    #[test_case(
        "[]",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::array(&[]),
        ]) ;
        "array expression 01"
    )]
    #[test_case(
        "[1, 2, 3]",
        vec![12],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[9, 10, 11]),
        ]) ;
        "array expression 02"
    )]
    #[test_case(
        "[1 + 2, 3 * 4, 5 + 6]",
        vec![18],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6),
            Object::integer(3),
            Object::integer(12),
            Object::integer(11),
            Object::array(&[15, 16, 17])
        ]) ;
        "array expression 03"
    )]
    #[test_case(
        "{}",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::map(&[]),
        ]) ;
        "map expression 01"
    )]
    #[test_case(
        "{1: 2, 3: 4}",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::map(&[
                (Primitive::Integer(1), 10),
                (Primitive::Integer(3), 12)
            ]),
        ]) ;
        "map expression 02"
    )]
    #[test_case(
        "{1 + 1: 2 * 2, 3 + 3: 4 * 4}",
        vec![21],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
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
                (Primitive::Integer(2), 18),
                (Primitive::Integer(6), 20),
            ]),
        ]) ;
        "map expression 03"
    )]
    #[test_case(
        "[1, 2, 3][1]",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(1),
            Object::array(&[9, 10, 11]),
        ]) ;
        "index expression 01"
    )]
    #[test_case(
        "[1, 2, 3][0 + 2]",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(0),
            Object::integer(2),
            Object::array(&[9, 10, 11]),
            Object::integer(2),
        ]) ;
        "index expression 02"
    )]
    #[test_case(
        "[[1, 1, 1]][0][0]",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
            Object::integer(1),
            Object::integer(0),
            Object::integer(0),
            Object::array(&[9, 10, 11]),
            Object::array(&[14]),
        ]) ;
        "index expression 03"
    )]
    #[test_case(
        "{1: 1, 2: 2}[1]",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(1),
            Object::map(&[
                (Primitive::Integer(1), 10),
                (Primitive::Integer(2), 12),
            ]),
        ]) ;
        "index expression 04"
    )]
    #[test_case(
        "{1: 1, 2: 2}[2]",
        vec![12],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(2),
            Object::map(&[
                (Primitive::Integer(1), 10),
                (Primitive::Integer(2), 12),
            ]),
        ]) ;
        "index expression 05"
    )]
    #[test_case(
        "if (true) { 10 }",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(10),
        ]) ;
        "if expr 01"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(10),
            Object::integer(20),
        ]) ;
        "if expr 02"
    )]
    #[test_case(
        "if (false) { 10 } else { 20 }",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(10),
            Object::integer(20),
        ]) ;
        "if expr 03"
    )]
    #[test_case(
        "if (false) { 10 }",
        vec![6],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(10),
        ]) ;
        "if expr 04"
    )]
    #[test_case(
        "1; 2",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "expr stmt 01"
    )]
    #[test_case(
        "let one = 1; one",
        vec![9],
        vec![9, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
        ]) ;
        "global let stmt 01"
    )]
    #[test_case(
        "let one = 1; let two = 2; one + two",
        vec![11],
        vec![9, 10, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
        ]) ;
        "global let stmt 02"
    )]
    #[test_case(
        "let one = 1; let two = one + one; one + two",
        vec![11],
        vec![9, 10, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
        ]) ;
        "global let stmt 03"
    )]
    #[test_case(
        "let f = fn() { 5 + 10 }; f()",
        vec![13],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(5),
            Object::integer(10),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Constant(10),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::integer(15),
        ]) ;
        "function call 01"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { 2 }; f() + g()",
        vec![15],
        vec![13, 14, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(11),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::integer(3),
        ]) ;
        "function call 02"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { f() + 2 }; let h = fn() { g() + 3 }; h()",
        vec![19],
        vec![15, 16, 17, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Call(0),
                    Op::Constant(11),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::integer(3),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(1),
                    Op::Call(0),
                    Op::Constant(13),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::Closure {
                fun: 14,
                frees: vec![],
            },
            Object::integer(3),
            Object::integer(6),
        ]) ;
        "function call 03"
    )]
    #[test_case(
        "let f = fn() { return 1; 2 }; f()",
        vec![9],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                    Op::Constant(10),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
        ]) ;
        "function call 04"
    )]
    #[test_case(
        "let f = fn() { return 1; return 2; }; f()",
        vec![9],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                    Op::Constant(10),
                    Op::Return,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
        ]) ;
        "function call 05"
    )]
    #[test_case(
        "let f = fn() { }; f()",
        vec![6],
        vec![10, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(6),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
        ]) ;
        "function call 06"
    )]
    #[test_case(
        "let f = fn() { }; let g = fn() { f() }; g()",
        vec![6],
        vec![11, 12, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(6),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Call(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
        ]) ;
        "function call 07"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { f }; g()()",
        vec![9],
        vec![12, 13, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
        ]) ;
        "function call 08"
    )]
    #[test_case(
        "let f = fn() { let x = 1; x }; f()",
        vec![9],
        vec![11, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
        ]) ;
        "function call 09"
    )]
    #[test_case(
        "let f = fn() { let x = 1; let y = 2; x + y }; f()",
        vec![13],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::SetLocal(0),
                    Op::Constant(10),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::integer(3),
        ]) ;
        "function call 10"
    )]
    #[test_case(
        "let f = fn() { let x = 1; let y = 2; x + y }; let g = fn() { let a = 3; let b = 4; a + b }; f() + g()",
        vec![19],
        vec![15, 16, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::SetLocal(0),
                    Op::Constant(10),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2,
                params: 0,
            },
            Object::integer(3),
            Object::integer(4),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(12),
                    Op::SetLocal(0),
                    Op::Constant(13),
                    Op::SetLocal(1),
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::Closure {
                fun: 14,
                frees: vec![],
            },
            Object::integer(3),
            Object::integer(7),
            Object::integer(10),
        ]) ;
        "function call 11"
    )]
    #[test_case(
        "let a = 42; let f = fn() { let x = 1; a - x }; let g = fn() { let x = 2; a - x }; f() + g()",
        vec![18],
        vec![9, 14, 15, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(42),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(10),
                    Op::SetLocal(0),
                    Op::GetGlobal(0),
                    Op::GetLocal(0),
                    Op::Sub,
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(12),
                    Op::SetLocal(0),
                    Op::GetGlobal(0),
                    Op::GetLocal(0),
                    Op::Sub,
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::Closure {
                fun: 13,
                frees: vec![],
            },
            Object::integer(41),
            Object::integer(40),
            Object::integer(81),
        ]) ;
        "function call 12"
    )]
    #[test_case(
        "let f = fn() { let g = fn() { 1 }; g }; f()()",
        vec![9],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::Closure(10, 0),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
            },
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
        ]) ;
        "function call 13"
    )]
    #[test_case(
        "let f = fn(x) { x }; f(42)",
        vec![10],
        vec![11, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::integer(42),
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
        ]) ;
        "function call 14"
    )]
    #[test_case(
        "let f = fn(x, y) { x + y }; f(1, 2)",
        vec![13],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 2,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
            Object::integer(3),
        ]) ;
        "function call 15"
    )]
    #[test_case(
        "let f = fn(x, y) { let z = x + y; z }; f(1, 2)",
        vec![13],
        vec![12, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
            Object::integer(3),
        ]) ;
        "function call 16"
    )]
    #[test_case(
        "let f = fn(x, y) { let z = x + y; z }; f(1, 2) + f(3, 4)",
        vec![17],
        vec![14, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
            Object::integer(3),
            Object::integer(7),
            Object::integer(10),
        ]) ;
        "function call 17"
    )]
    #[test_case(
        "let f = fn(x, y) { let z = x + y; z }; let g = fn() { f(1, 2) + f(3, 4) }; g()",
        vec![19],
        vec![15, 16, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Constant(10),
                    Op::Constant(11),
                    Op::Call(2),
                    Op::GetGlobal(0),
                    Op::Constant(12),
                    Op::Constant(13),
                    Op::Call(2),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 9,
                frees: vec![],
            },
            Object::Closure {
                fun: 14,
                frees: vec![],
            },
            Object::integer(3),
            Object::integer(7),
            Object::integer(10),
        ]) ;
        "function call 18"
    )]
    #[test_case(
        "let a = 42;\n\
         let f = fn(x, y) {\n\
             let z = x + y;\n\
             z + a\n\
         };\n\
         let g = fn() {\n\
             f(1, 2) + f(3, 4) + a\n\
         };\n\
         g() + a",
        vec![24],
        vec![9, 16, 17, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(42),
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::GetGlobal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(1),
                    Op::Constant(11),
                    Op::Constant(12),
                    Op::Call(2),
                    Op::GetGlobal(1),
                    Op::Constant(13),
                    Op::Constant(14),
                    Op::Call(2),
                    Op::Add,
                    Op::GetGlobal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 15,
                frees: vec![],
            },
            Object::integer(3),
            Object::integer(45),
            Object::integer(7),
            Object::integer(49),
            Object::integer(94),
            Object::integer(136),
            Object::integer(178),
        ]) ;
        "function call 19"
    )]
    #[test_case(
        "let a = 42; let f = fn() { if(false) { 1 } else { 0 } }; f()",
        vec![11],
        vec![9, 13, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(42),
            Object::integer(1),
            Object::integer(0),
            Object::Function {
                bytes: encode(vec![
                    Op::False,
                    Op::JumpIfNot(10),
                    Op::Constant(10),
                    Op::Jump(13),
                    Op::Constant(11),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
        ]) ;
        "function call 20"
    )]
    #[test_case(
        "len(\"\")",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string(""),
            Object::integer(0),
        ]) ;
        "builtins 01"
    )]
    #[test_case(
        "len(\"hello world\")",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string("hello world"),
            Object::integer(11),
        ]) ;
        "builtins 03"
    )]
    #[test_case(
        "len([])",
        vec![10],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::array(&[]),
            Object::integer(0),
        ]) ;
        "builtins 04"
    )]
    #[test_case(
        "len([1, 2, 3])",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[9, 10, 11]),
            Object::integer(3),
        ]) ;
        "builtins 05"
    )]
    #[test_case(
        "first([1, 2, 3])",
        vec![9],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[9, 10, 11]),
        ]) ;
        "builtins 06"
    )]
    #[test_case(
        "last([1, 2, 3])",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[9, 10, 11]),
        ]) ;
        "builtins 07"
    )]
    #[test_case(
        "rest([1, 2, 3])",
        vec![13],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::array(&[9, 10, 11]),
            Object::array(&[10, 11]),
        ]) ;
        "builtins 08"
    )]
    #[test_case(
        "push([], 1)",
        vec![11],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::array(&[]),
            Object::array(&[9]),
        ]) ;
        "builtins 09"
    )]
    #[test_case(
        "puts(\"hello \", \"world!\")",
        vec![6],
        vec![0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::string("hello "),
            Object::string("world!"),
        ]) ;
        "builtins 10"
    )]
    #[test_case(
        "let f = fn(a) {\n\
            fn() { a }\n\
        };\n\
        let g = f(42);\n\
        g()",
        vec![11],
        vec![12, 13, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Closure(9, 1),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::integer(42),
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 9,
                frees: vec![11],
            },
        ]) ;
        "closures 01"
    )]
    #[test_case(
        "let f = fn(a, b) {\n\
            fn(c) { a + b + c }\n\
        };\n\
        let g = f(1, 2);\n\
        g(3)",
        vec![17],
        vec![14, 15, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::GetFree(1),
                    Op::Add,
                    Op::GetLocal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Closure(9, 2),
                    Op::Return,
                ]),
                locals: 2,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::Closure {
                fun: 9,
                frees: vec![11, 12],
            },
            Object::integer(3),
            Object::integer(6),
        ]) ;
        "closures 02"
    )]
    #[test_case(
        "let f = fn(a, b) {\n\
            let c = a + b;
            fn(d) { c + d }\n\
        };\n\
        let g = f(1, 2);\n\
        g(3)",
        vec![17],
        vec![14, 16, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::GetLocal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::Closure(9, 1),
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::Closure {
                fun: 10,
                frees: vec![],
            },
            Object::integer(3),
            Object::Closure {
                fun: 9,
                frees: vec![15],
            },
            Object::integer(6),
        ]) ;
        "closures 03"
    )]
    #[test_case(
        "let g = fn(a, b) {
            let c = a + b;
            fn(d) {
                let e = d + c;
                fn(f) { e + f }
            }
        };
        let h = g(1, 2);
        let i = h(3);
        i(8)",
        vec![21],
        vec![16, 18, 20, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::GetLocal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetFree(0),
                    Op::Add,
                    Op::SetLocal(1),
                    Op::GetLocal(1),
                    Op::Closure(9, 1),
                    Op::Return,
                ]),
                locals: 2,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::GetLocal(1),
                    Op::Add,
                    Op::SetLocal(2),
                    Op::GetLocal(2),
                    Op::Closure(10, 1),
                    Op::Return,
                ]),
                locals: 3,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(8),
            Object::Closure {
                fun: 11,
                frees: vec![],
            },
            Object::integer(3),
            Object::Closure {
                fun: 10,
                frees: vec![17],
            },
            Object::integer(6),
            Object::Closure {
                fun: 9,
                frees: vec![19],
            },
            Object::integer(14),
        ]) ;
        "closures 04"
    )]
    #[test_case(
        "let a = 1;\n\
        let f = fn(b) {\n\
            fn(c) {\n\
                fn(d) { a + b + c + d }\n\
            }\n\
        };\n\
        let g = f(2);\n\
        let h = g(3);\n\
        h(8)",
        vec![21],
        vec![9, 16, 17, 18, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::GetFree(0),
                    Op::Add,
                    Op::GetFree(1),
                    Op::Add,
                    Op::GetLocal(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::GetLocal(0),
                    Op::Closure(10, 2),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Closure(11, 1),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::integer(2),
            Object::integer(3),
            Object::integer(8),
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::Closure {
                fun: 11,
                frees: vec![13],
            },
            Object::Closure {
                fun: 10,
                frees: vec![13, 14],
            },
            Object::integer(3),
            Object::integer(6),
            Object::integer(14),
        ]) ;
        "closures 05"
    )]
    #[test_case(
        "let f = fn(a, b) {\n\
            let g = fn() { a };\n\
            let h = fn() { b };\n\
            fn() { g() + h() }\n\
        };\n\
        let i = f(1, 2);\n\
        i()",
        vec![19],
        vec![15, 18, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetFree(0),
                    Op::Call(0),
                    Op::GetFree(1),
                    Op::Call(0),
                    Op::Add,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
            },
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Closure(9, 1),
                    Op::SetLocal(2),
                    Op::GetLocal(1),
                    Op::Closure(10, 1),
                    Op::SetLocal(3),
                    Op::GetLocal(2),
                    Op::GetLocal(3),
                    Op::Closure(11, 2),
                    Op::Return,
                ]),
                locals: 4,
                params: 2,
            },
            Object::integer(1),
            Object::integer(2),
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::Closure {
                fun: 9,
                frees: vec![13],
            },
            Object::Closure {
                fun: 10,
                frees: vec![14],
            },
            Object::Closure {
                fun: 11,
                frees: vec![16, 17],
            },
            Object::integer(3),
        ]) ;
        "closures 06"
    )]
    #[test_case(
        "let f = fn(x) {\n\
            if (x == 0) {\n\
                0\n\
            } else {\n\
                f(x - 1)\n\
            }\n\
        };\n\
        f(1)",
        vec![10],
        vec![14, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(0),
            Object::integer(0),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Constant(9),
                    Op::Eq,
                    Op::JumpIfNot(15),
                    Op::Constant(10),
                    Op::Jump(24),
                    Op::Current,
                    Op::GetLocal(0),
                    Op::Constant(11),
                    Op::Sub,
                    Op::Call(1),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::integer(1),
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::integer(0),
        ]) ;
        "recursive closures 01"
    )]
    #[test_case(
        "let f = fn() {\n\
            let g = fn(x) {\n\
                if (x == 0) {\n\
                    0\n\
                } else {\n\
                    g(x - 1)\n\
                }\n\
            };\n\
            g(1)\n\
        };\n\
        f()",
        vec![10],
        vec![15, 0, 0, 0, 0, 0, 0, 0, 0, 0],
        heap(vec![
            Object::integer(0),
            Object::integer(0),
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Constant(9),
                    Op::Eq,
                    Op::JumpIfNot(15),
                    Op::Constant(10),
                    Op::Jump(24),
                    Op::Current,
                    Op::GetLocal(0),
                    Op::Constant(11),
                    Op::Sub,
                    Op::Call(1),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
            },
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Closure(12, 0),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Constant(13),
                    Op::Call(1),
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
            },
            Object::Closure {
                fun: 14,
                frees: vec![],
            },
            Object::Closure {
                fun: 12,
                frees: vec![],
            },
            Object::integer(0),
        ]) ;
        "recursive closures 02"
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

    #[test_case("5 + true"              , MonkeyError::type_mismatch("5 + true")      ; "error 01")]
    #[test_case("5 + true; 5;"          , MonkeyError::type_mismatch("5 + true")      ; "error 02")]
    #[test_case("-true"                 , MonkeyError::type_mismatch("-true")         ; "error 03")]
    #[test_case("!5"                    , MonkeyError::type_mismatch("!5")            ; "error 04")]
    #[test_case("5; true + false; 5"    , MonkeyError::type_mismatch("true + false")  ; "error 05")]
    #[test_case("if (1) { 10 }"         , MonkeyError::type_mismatch("if (1) {...}")  ; "error 06")]
    #[test_case("len(1)"                , MonkeyError::type_mismatch("len(1)")        ; "error 07")]
    #[test_case("len(\"one\", \"two\")" , MonkeyError::wrong_number_of_args(1, 2)     ; "error 08")]
    #[test_case("[1, 2, 3][3]"          , MonkeyError::index_out_of_bounds(3, 2)      ; "error 09")]
    #[test_case("[1, 2, 3][-1]"         , MonkeyError::index_out_of_bounds(-1, 2)     ; "error 10")]
    #[test_case("{\"foo\": 5}[\"bar\"]" , MonkeyError::missing_index("\"bar\"")       ; "error 11")]
    #[test_case("{}[\"foo\"]"           , MonkeyError::missing_index("\"foo\"")       ; "error 12")]
    #[test_case("{}[fn(x) { x }]"       , MonkeyError::type_mismatch("{}[closure()]") ; "error 13")]
    #[test_case("fn() { 1 }(1)"         , MonkeyError::wrong_number_of_args(0, 1)     ; "error 14")]
    #[test_case("fn(x) { x }()"         , MonkeyError::wrong_number_of_args(1, 0)     ; "error 15")]
    #[test_case("fn(x, y) { x + y }(1)" , MonkeyError::wrong_number_of_args(2, 1)     ; "error 16")]
    #[test_case("first([])"             , MonkeyError::runtime_error("first([])")     ; "error 17")]
    #[test_case("first(1)"              , MonkeyError::type_mismatch("first(1)")      ; "error 18")]
    #[test_case("last([])"              , MonkeyError::runtime_error("last([])")      ; "error 19")]
    #[test_case("last(1)"               , MonkeyError::type_mismatch("last(1)")       ; "error 20")]
    #[test_case("rest([])"              , MonkeyError::runtime_error("rest([])")      ; "error 21")]
    #[test_case("rest(1)"               , MonkeyError::type_mismatch("rest(1)")       ; "error 22")]
    #[test_case("push(1, 1)"            , MonkeyError::type_mismatch("push(1, ...)")  ; "error 23")]
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
