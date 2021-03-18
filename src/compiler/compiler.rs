use crate::compiler::{Binary, Op, Reference};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::vm::Object;

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub binary: Binary,
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
            constants: vec![Object::False, Object::True],
        }
    }

    pub fn compile(&mut self, ast: &Program) -> ByteCode {
        ast.0.iter().for_each(|stmt| self.compile_stmt(stmt));

        ByteCode {
            binary: Binary {
                bytes: self.bytes.clone(),
            },
            constants: self.constants.clone(),
        }
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
            Expression::Boolean(false) => self.emit(Op::False),
            Expression::Boolean(true) => self.emit(Op::True),
            Expression::Prefix { operator, expr } => {
                self.compile_expr(expr);
                match operator {
                    Token::Minus => self.emit(Op::Minus),
                    Token::Bang => self.emit(Op::Bang),
                    _ => panic!(),
                }
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
                }
            }
            _ => panic!(),
        }
    }

    fn emit(&mut self, op: Op) {
        self.bytes.append(&mut op.encode());
    }

    fn allocate(&mut self, obj: Object) -> Reference {
        let reference = self.constants.len() as u16;
        self.constants.push(obj);
        reference
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::Op;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use test_case::test_case;

    #[test_case(
        "1 + 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Add],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "1 - 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Sub],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 * 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Mul],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "2 / 1",
        vec![Op::Constant(2), Op::Constant(3), Op::Div],
        vec![Object::False, Object::True, Object::Integer(2), Object::Integer(1)] ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "-1",
        vec![Op::Constant(2), Op::Minus],
        vec![Object::False, Object::True, Object::Integer(1)] ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "false",
        vec![Op::False],
        vec![Object::False, Object::True] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![Op::True],
        vec![Object::False, Object::True] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Eq],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Neq],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 < 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Lt],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 > 2",
        vec![Op::Constant(2), Op::Constant(3), Op::Gt],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "boolean expression 06"
    )]
    #[test_case(
        "true == false",
        vec![Op::True, Op::False, Op::Eq],
        vec![Object::False, Object::True] ;
        "boolean expression 07"
    )]
    #[test_case(
        "true != false",
        vec![Op::True, Op::False, Op::Neq],
        vec![Object::False, Object::True] ;
        "boolean expression 08"
    )]
    #[test_case(
        "!true",
        vec![Op::True, Op::Bang],
        vec![Object::False, Object::True] ;
        "boolean expression 09"
    )]
    #[test_case(
        "1; 2",
        vec![Op::Constant(2), Op::Pop, Op::Constant(3)],
        vec![Object::False, Object::True, Object::Integer(1), Object::Integer(2)] ;
        "expr stmt 01"
    )]
    fn test_compile(input: &str, ops: Vec<Op>, constants: Vec<Object>) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast);

        assert_eq!(byte_code.binary.decode().unwrap(), ops);
        assert_eq!(byte_code.constants, constants);
    }
}
