use crate::compiler::{Op, Reference};
use crate::error::{MonkeyError, Result};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::symbol::*;
use crate::vm::Object;

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub bytes: Vec<u8>,
    pub constants: Vec<Object>,
}

pub struct Compiler {
    bytes: Vec<u8>,
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            bytes: vec![],
            constants: vec![
                Object::unit(),
                Object::boolean(false),
                Object::boolean(true),
            ],
            symbol_table: SymbolTable::new(),
        }
    }

    pub fn compile(&mut self, ast: &Program) -> Result<ByteCode> {
        self.compile_block_stmt(ast)?;

        Ok(ByteCode {
            bytes: self.bytes.clone(),
            constants: self.constants.clone(),
        })
    }

    fn compile_block_stmt(&mut self, block: &Block) -> Result<()> {
        for stmt in &block.0 {
            self.compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn compile_stmt(&mut self, stmt: &Statement) -> Result<()> {
        match stmt {
            Statement::Let { name, expr } => {
                self.compile_expr(expr)?;
                let index = self.symbol_table.define(name).index;
                self.emit(Op::SetGlobal(index));
            }
            Statement::Stmt(expr) => {
                self.compile_expr(expr)?;
                self.emit(Op::Pop);
            }
            Statement::Expr(expr) => self.compile_expr(expr)?,
            _ => panic!(),
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Name(name) => {
                let index = self
                    .symbol_table
                    .resolve(name)
                    .map_or(Err(MonkeyError::undefined_variable(name)), |symbol| {
                        Ok(symbol)
                    })?
                    .index;

                self.emit(Op::GetGlobal(index));
            }
            Expression::Integer(i) => {
                let reference = self.allocate(Object::integer(*i));
                self.emit(Op::Constant(reference));
            }
            Expression::Boolean(false) => {
                self.emit(Op::False);
            }
            Expression::Boolean(true) => {
                self.emit(Op::True);
            }
            Expression::String(s) => {
                let reference = self.allocate(Object::string(s));
                self.emit(Op::Constant(reference));
            }
            Expression::Array(vec) => {
                for expr in vec {
                    self.compile_expr(expr)?;
                }

                self.emit(Op::Array(vec.len() as u16));
            }
            Expression::Map(vec) => {
                for (key, value) in vec {
                    self.compile_expr(key)?;
                    self.compile_expr(value)?;
                }

                self.emit(Op::Map(vec.len() as u16));
            }
            Expression::Prefix { operator, expr } => {
                self.compile_expr(expr)?;
                match operator {
                    Token::Minus => self.emit(Op::Minus),
                    Token::Bang => self.emit(Op::Bang),
                    _ => {
                        return Err(MonkeyError::type_mismatch(format!("{}{}", operator, expr)));
                    }
                };
            }
            Expression::Infix {
                left,
                operator,
                right,
            } => {
                self.compile_expr(left)?;
                self.compile_expr(right)?;
                match operator {
                    Token::Plus => self.emit(Op::Add),
                    Token::Minus => self.emit(Op::Sub),
                    Token::Asterisk => self.emit(Op::Mul),
                    Token::Slash => self.emit(Op::Div),
                    Token::Eq => self.emit(Op::Eq),
                    Token::Neq => self.emit(Op::Neq),
                    Token::Lt => self.emit(Op::Lt),
                    Token::Gt => self.emit(Op::Gt),
                    _ => {
                        return Err(MonkeyError::type_mismatch(format!(
                            "{} {} {}",
                            left, operator, right
                        )));
                    }
                };
            }
            Expression::If { cond, yes, no } => {
                self.compile_expr(cond)?;
                let jump_if_not = self.emit(Op::JumpIfNot(65535));
                self.compile_block_stmt(yes)?;
                let jump = self.emit(Op::Jump(65535));
                self.patch(jump_if_not);

                match no {
                    Some(no) => {
                        self.compile_block_stmt(no)?;
                    }
                    None => {
                        self.emit(Op::Unit);
                    }
                };

                self.patch(jump);
            }
            Expression::Index { expr, index } => {
                self.compile_expr(expr)?;
                self.compile_expr(index)?;
                self.emit(Op::Index);
            }
            _ => panic!(),
        }

        Ok(())
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
            Op::GetGlobal(binding) => {
                self.bytes.push(Op::GETGLOBAL);
                self.bytes.extend_from_slice(&binding.to_be_bytes());
            }
            Op::SetGlobal(binding) => {
                self.bytes.push(Op::SETGLOBAL);
                self.bytes.extend_from_slice(&binding.to_be_bytes());
            }
            Op::Array(n) => {
                self.bytes.push(Op::ARRAY);
                self.bytes.extend_from_slice(&n.to_be_bytes());
            }
            Op::Map(n) => {
                self.bytes.push(Op::MAP);
                self.bytes.extend_from_slice(&n.to_be_bytes());
            }
            Op::Index => {
                self.bytes.push(Op::INDEX);
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
                    Op::GETGLOBAL => {
                        let binding = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::GetGlobal(binding));
                        i += 2;
                    }
                    Op::SETGLOBAL => {
                        let binding = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::SetGlobal(binding));
                        i += 2;
                    }
                    Op::ARRAY => {
                        let n = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::Array(n));
                        i += 2;
                    }
                    Op::MAP => {
                        let n = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        ops.push(Op::Map(n));
                        i += 2;
                    }
                    Op::INDEX => ops.push(Op::Index),
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
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Add
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "1 - 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Sub
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 * 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Mul
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "2 / 1",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Div
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(2),
            Object::integer(1)
        ] ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "-1",
        vec![
            Op::Constant(3),
            Op::Minus
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1)
        ] ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "false",
        vec![
            Op::False
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![
            Op::True
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Eq
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Neq
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 < 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Lt
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 > 2",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Gt
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "boolean expression 06"
    )]
    #[test_case(
        "true == false",
        vec![
            Op::True,
            Op::False,
            Op::Eq
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 07"
    )]
    #[test_case(
        "true != false",
        vec![
            Op::True,
            Op::False,
            Op::Neq
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 08"
    )]
    #[test_case(
        "!true",
        vec![

            Op::True,
            Op::Bang],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "boolean expression 09"
    )]
    #[test_case(
        "\"monkey\"",
        vec![
            Op::Constant(3)
        ],
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
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Add
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::string("mon"),
            Object::string("key")
        ] ;
        "string expression 02"
    )]
    #[test_case(
        "[]",
        vec![
            Op::Array(0)
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "array expression 01"
    )]
    #[test_case(
        "[1, 2, 3]",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Constant(5),
            Op::Array(3)
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ] ;
        "array expression 02"
    )]
    #[test_case(
        "[1 + 2, 3 - 4, 5 * 6]",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Add, Op::Constant(5),
            Op::Constant(6),
            Op::Sub,
            Op::Constant(7),
            Op::Constant(8),
            Op::Mul,
            Op::Array(3)
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6)
        ] ;
        "array expression 03"
    )]
    #[test_case(
        "{}",
        vec![
            Op::Map(0),
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ] ;
        "map expression 01"
    )]
    #[test_case(
        "{1: 2, 3: 4, 5: 6}",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Constant(5),
            Op::Constant(6),
            Op::Constant(7),
            Op::Constant(8),
            Op::Map(3),
        ],
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
        ] ;
        "map expression 02"
    )]
    #[test_case(
        "{1: 2 + 3, 4: 5 * 6}",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Constant(5),
            Op::Add,
            Op::Constant(6),
            Op::Constant(7),
            Op::Constant(8),
            Op::Mul,
            Op::Map(2),
        ],
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
        ] ;
        "map expression 03"
    )]
    #[test_case(
        "[1, 2, 3][1 + 1]",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Constant(5),
            Op::Array(3),
            Op::Constant(6),
            Op::Constant(7),
            Op::Add,
            Op::Index,
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(1),
            Object::integer(1),
        ] ;
        "index expression 01"
    )]
    #[test_case(
        "{1: 2}[2 - 1]",
        vec![
            Op::Constant(3),
            Op::Constant(4),
            Op::Map(1),
            Op::Constant(5),
            Op::Constant(6),
            Op::Sub,
            Op::Index,
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(1),
        ] ;
        "index expression 02"
    )]
    #[test_case(
        "if (true) { 10 }; 20;",
        vec![
            Op::True,
            Op::JumpIfNot(10),
            Op::Constant(3),
            Op::Jump(11),
            Op::Unit,
            Op::Pop,
            Op::Constant(4),
            Op::Pop
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
            Object::integer(20)
        ] ;
        "if expression 01"
    )]
    #[test_case(
        "if (true) { 1; 2; 3 }",
        vec![
            Op::True,
            Op::JumpIfNot(18),
            Op::Constant(3),
            Op::Pop,
            Op::Constant(4),
            Op::Pop,
            Op::Constant(5),
            Op::Jump(19),
            Op::Unit
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ] ;
        "if expression 02"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }; 30;",
        vec![
            Op::True,
            Op::JumpIfNot(10),
            Op::Constant(3),
            Op::Jump(13),
            Op::Constant(4),
            Op::Pop,
            Op::Constant(5),
            Op::Pop
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(10),
            Object::integer(20),
            Object::integer(30)
        ] ;
        "if expression 03"
    )]
    #[test_case(
        "1; 2",
        vec![
            Op::Constant(3),
            Op::Pop,
            Op::Constant(4)
        ],
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
        "let one = 1; let two = 2;",
        vec![
            Op::Constant(3),
            Op::SetGlobal(0),
            Op::Constant(4),
            Op::SetGlobal(1)
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1),
            Object::integer(2)
        ] ;
        "let stmt 01"
    )]
    #[test_case(
        "let one = 1; one;",
        vec![
            Op::Constant(3),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::Pop
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1)
        ] ;
        "let stmt 02"
    )]
    #[test_case(
        "let one = 1; let two = one; two",
        vec![
            Op::Constant(3),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::SetGlobal(1),
            Op::GetGlobal(1)
        ],
        vec![
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
            Object::integer(1)
        ] ;
        "let stmt 03"
    )]
    fn test_compile(input: &str, ops: Vec<Op>, constants: Vec<Object>) {
        let lexer = Lexer::new(input.as_bytes());
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let mut compiler = Compiler::new();
        let byte_code = compiler.compile(&ast).unwrap();

        assert_eq!((&byte_code.bytes[..]).decode().unwrap(), ops);
        assert_eq!(byte_code.constants, constants);
    }
}
