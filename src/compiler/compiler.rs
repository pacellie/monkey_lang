use crate::compiler::{Op, Reference};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::vm::Object;

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub bytes: Vec<u8>,
    pub constants: Vec<Object>,
}

pub struct Compiler {
    bytes: Vec<u8>,
    constants: Vec<Object>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            bytes: vec![],
            constants: vec![Object::Unit, Object::False, Object::True],
        }
    }

    pub fn compile(&mut self, ast: &Program) -> ByteCode {
        self.compile_block_stmt(ast);

        ByteCode {
            bytes: self.bytes.clone(),
            constants: self.constants.clone(),
        }
    }

    fn compile_block_stmt(&mut self, block: &Block) {
        block.0.iter().for_each(|stmt| self.compile_stmt(stmt));
    }

    fn compile_stmt(&mut self, stmt: &Statement) {
        match stmt {
            Statement::Expr(expr) => self.compile_expr(expr),
            Statement::Stmt(expr) => {
                self.compile_expr(expr);
                self.emit(Op::Pop);
            }
            _ => panic!(),
        }
    }

    fn compile_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Integer(i) => {
                let reference = self.allocate(Object::Integer(*i));
                self.emit(Op::Constant(reference));
            }
            Expression::Boolean(false) => {
                self.emit(Op::False);
            }
            Expression::Boolean(true) => {
                self.emit(Op::True);
            }
            Expression::Prefix { operator, expr } => {
                self.compile_expr(expr);
                match operator {
                    Token::Minus => self.emit(Op::Minus),
                    Token::Bang => self.emit(Op::Bang),
                    _ => panic!(),
                };
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match operator {
                    Token::Plus => self.emit(Op::Add),
                    Token::Minus => self.emit(Op::Sub),
                    Token::Asterisk => self.emit(Op::Mul),
                    Token::Slash => self.emit(Op::Div),
                    Token::Eq => self.emit(Op::Eq),
                    Token::Neq => self.emit(Op::Neq),
                    Token::Lt => self.emit(Op::Lt),
                    Token::Gt => self.emit(Op::Gt),
                    _ => panic!(),
                };
            }
            Expression::If { cond, yes, no } => {
                self.compile_expr(cond);
                let jump_if_not = self.emit(Op::JumpIfNot(65535));
                self.compile_block_stmt(yes);
                let jump = self.emit(Op::Jump(65535));
                self.patch(jump_if_not);

                match no {
                    Some(no) => {
                        self.compile_block_stmt(no);
                    }
                    None => {
                        self.emit(Op::Unit);
                    }
                };

                self.patch(jump);
            }
            _ => panic!(),
        }
    }

    fn encode(&mut self, op: Op) {
        match op {
            Op::Constant(reference) => {
                self.bytes.push(Op::CONSTANT);
                self.bytes.extend_from_slice(&reference.to_be_bytes());
            }
            Op::Pop => {
                self.bytes.push(Op::POP);
            }
            Op::Unit => {
                self.bytes.push(Op::UNIT);
            }
            Op::False => {
                self.bytes.push(Op::FALSE);
            }
            Op::True => {
                self.bytes.push(Op::TRUE);
            }
            Op::Add => {
                self.bytes.push(Op::ADD);
            }
            Op::Sub => {
                self.bytes.push(Op::SUB);
            }
            Op::Mul => {
                self.bytes.push(Op::MUL);
            }
            Op::Div => {
                self.bytes.push(Op::DIV);
            }
            Op::Eq => {
                self.bytes.push(Op::EQ);
            }
            Op::Neq => {
                self.bytes.push(Op::NEQ);
            }
            Op::Lt => {
                self.bytes.push(Op::LT);
            }
            Op::Gt => {
                self.bytes.push(Op::GT);
            }
            Op::Minus => {
                self.bytes.push(Op::MINUS);
            }
            Op::Bang => {
                self.bytes.push(Op::BANG);
            }
            Op::JumpIfNot(address) => {
                self.bytes.push(Op::JUMPIFNOT);
                self.bytes.extend_from_slice(&address.to_be_bytes());
            }
            Op::Jump(address) => {
                self.bytes.push(Op::JUMP);
                self.bytes.extend_from_slice(&address.to_be_bytes());
            }
        }
    }

    fn emit(&mut self, op: Op) -> usize {
        let index = self.bytes.len();
        self.encode(op);
        index
    }

    fn allocate(&mut self, obj: Object) -> Reference {
        let reference = self.constants.len() as u16;
        self.constants.push(obj);
        reference
    }

    fn patch(&mut self, index: usize) {
        let [x, y] = (self.bytes.len() as u16).to_be_bytes();
        self.bytes[index + 1] = x;
        self.bytes[index + 2] = y;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::Op;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use test_case::test_case;

    trait Decode {
        fn decode(&self) -> Option<Vec<Op>>;
    }

    impl Decode for &[u8] {
        fn decode(&self) -> Option<Vec<Op>> {
            let mut ops = vec![];
            let mut i = 0;

            while i < self.len() {
                match self[i] {
                    Op::CONSTANT => {
                        let reference = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::Constant(reference));
                        i += 2;
                    }
                    Op::POP => ops.push(Op::Pop),
                    Op::UNIT => ops.push(Op::Unit),
                    Op::FALSE => ops.push(Op::False),
                    Op::TRUE => ops.push(Op::True),
                    Op::ADD => ops.push(Op::Add),
                    Op::SUB => ops.push(Op::Sub),
                    Op::MUL => ops.push(Op::Mul),
                    Op::DIV => ops.push(Op::Div),
                    Op::EQ => ops.push(Op::Eq),
                    Op::NEQ => ops.push(Op::Neq),
                    Op::LT => ops.push(Op::Lt),
                    Op::GT => ops.push(Op::Gt),
                    Op::MINUS => ops.push(Op::Minus),
                    Op::BANG => ops.push(Op::Bang),
                    Op::JUMPIFNOT => {
                        let address = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::JumpIfNot(address));
                        i += 2;
                    }
                    Op::JUMP => {
                        let address = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::Jump(address));
                        i += 2;
                    }
                    _ => {
                        return None;
                    }
                }

                i += 1;
            }

            Some(ops)
        }
    }

    #[test_case(
        "1 + 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Add],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "1 - 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Sub],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 * 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Mul],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "2 / 1",
        vec![Op::Constant(3), Op::Constant(4), Op::Div],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(2), Object::Integer(1)] ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "-1",
        vec![Op::Constant(3), Op::Minus],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1)] ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "false",
        vec![Op::False],
        vec![Object::Unit, Object::False, Object::True] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![Op::True],
        vec![Object::Unit, Object::False, Object::True] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Eq],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Neq],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 < 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Lt],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 > 2",
        vec![Op::Constant(3), Op::Constant(4), Op::Gt],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 06"
    )]
    #[test_case(
        "true == false",
        vec![Op::True, Op::False, Op::Eq],
        vec![Object::Unit, Object::False, Object::True] ;
        "boolean expression 07"
    )]
    #[test_case(
        "true != false",
        vec![Op::True, Op::False, Op::Neq],
        vec![Object::Unit, Object::False, Object::True] ;
        "boolean expression 08"
    )]
    #[test_case(
        "!true",
        vec![Op::True, Op::Bang],
        vec![Object::Unit, Object::False, Object::True] ;
        "boolean expression 09"
    )]
    #[test_case(
        "if (true) { 10 }; 20;",
        vec![Op::True, Op::JumpIfNot(10), Op::Constant(3), Op::Jump(11), Op::Unit, Op::Pop, Op::Constant(4), Op::Pop],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(10), Object::Integer(20)] ;
        "if expression 01"
    )]
    #[test_case(
        "if (true) { 1; 2; 3 }",
        vec![Op::True, Op::JumpIfNot(18), Op::Constant(3), Op::Pop, Op::Constant(4), Op::Pop, Op::Constant(5), Op::Jump(19), Op::Unit],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2), Object::Integer(3)] ;
        "if expression 02"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }; 30;",
        vec![Op::True, Op::JumpIfNot(10), Op::Constant(3), Op::Jump(13), Op::Constant(4), Op::Pop, Op::Constant(5), Op::Pop],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(10), Object::Integer(20), Object::Integer(30)] ;
        "if expression 03"
    )]
    #[test_case(
        "1; 2",
        vec![Op::Constant(3), Op::Pop, Op::Constant(4)],
        vec![Object::Unit, Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "expr stmt 01"
    )]
    fn test_compile(input: &str, ops: Vec<Op>, constants: Vec<Object>) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast);

        assert_eq!((&byte_code.bytes[..]).decode().unwrap(), ops);
        assert_eq!(byte_code.constants, constants);
    }
}
