use crate::compiler::{Binary, Op};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::vm::Object;

#[derive(Debug)]
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
            constants: vec![],
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
            _ => panic!(),
        }
    }

    fn compile_expr(&mut self, expr: &Expression) {
        match expr {
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left);
                self.compile_expr(right);
                match operator {
                    Token::Plus => self.bytes.append(&mut Op::Add.encode()),
                    _ => (),
                }
            }
            Expression::Integer(i) => {
                let reference = self.constants.len() as u16;

                let obj = Object::Integer(*i);
                self.constants.push(obj);

                let op = Op::Constant(reference);
                self.bytes.append(&mut op.encode());
            }
            _ => panic!(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::compiler::Op;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    #[test]
    fn test_compile() {
        let input = b"1 + 2";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast);

        let ops = vec![Op::Constant(0), Op::Constant(1), Op::Add];
        let constants = vec![Object::Integer(1), Object::Integer(2)];

        assert_eq!(byte_code.binary.decode().unwrap(), ops);
        assert_eq!(byte_code.constants, constants);
    }
}
