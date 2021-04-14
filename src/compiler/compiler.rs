use crate::builtin::Builtin;
use crate::compiler::{symbol, Op, Reference, SymbolTable};
use crate::error::{MonkeyError, Result};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::vm::Object;

use std::mem;

#[derive(Debug, Clone)]
pub struct ByteCode {
    pub bytes: Vec<u8>,
    pub constants: Vec<Object>,
}

pub struct Scope {
    pub bytes: Vec<u8>,
}

pub struct Compiler {
    pub scopes: Vec<Scope>,
    pub scope: usize,
    pub constants: Vec<Object>,
    pub symbol_table: SymbolTable,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            scopes: vec![Scope { bytes: vec![] }],
            scope: 0,
            constants: Compiler::static_constants(),
            symbol_table: SymbolTable::toplevel(),
        }
    }

    pub fn static_constants() -> Vec<Object> {
        vec![
            Object::Builtin(Builtin::Len),
            Object::Builtin(Builtin::First),
            Object::Builtin(Builtin::Last),
            Object::Builtin(Builtin::Rest),
            Object::Builtin(Builtin::Push),
            Object::Builtin(Builtin::Puts),
            Object::unit(),
            Object::boolean(false),
            Object::boolean(true),
        ]
    }

    fn current_scope(&mut self) -> &mut Scope {
        &mut self.scopes[self.scope]
    }

    fn enter_scope(&mut self) {
        self.symbol_table =
            SymbolTable::new(mem::replace(&mut self.symbol_table, SymbolTable::empty()));
        self.scopes.push(Scope { bytes: vec![] });
        self.scope += 1;
    }

    fn leave_scope(&mut self) -> Vec<u8> {
        self.symbol_table = *self.symbol_table.outer.take().unwrap();
        self.scope -= 1;
        self.scopes.pop().unwrap().bytes
    }

    fn emit(&mut self, op: Op) -> usize {
        let index = self.current_scope().bytes.len();
        self.encode(op);
        index
    }

    fn allocate(&mut self, obj: Object) -> Reference {
        let reference = self.constants.len() as u16;
        self.constants.push(obj);
        reference
    }

    fn patch(&mut self, index: usize) {
        let [x, y] = (self.current_scope().bytes.len() as u16).to_be_bytes();
        self.current_scope().bytes[index + 1] = x;
        self.current_scope().bytes[index + 2] = y;
    }

    pub fn compile(&mut self, ast: &Program) -> Result<ByteCode> {
        self.compile_block_stmt(ast)?;

        Ok(ByteCode {
            bytes: self.scopes[0].bytes.clone(),
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

                let symbol = self.symbol_table.define(name);
                let scope = symbol.scope;
                let index = symbol.index;

                match scope {
                    symbol::Scope::Local => {
                        self.emit(Op::SetLocal(index as u8));
                    }
                    symbol::Scope::Global => {
                        self.emit(Op::SetGlobal(index));
                    }
                    symbol::Scope::Builtin => (),
                }
            }
            Statement::Return(expr) => {
                self.compile_expr(expr)?;
                self.emit(Op::Return);
            }
            Statement::Stmt(expr) => {
                self.compile_expr(expr)?;
                self.emit(Op::Pop);
            }
            Statement::Expr(expr) => self.compile_expr(expr)?,
        }

        Ok(())
    }

    fn compile_expr(&mut self, expr: &Expression) -> Result<()> {
        match expr {
            Expression::Name(name) => {
                let symbol = self
                    .symbol_table
                    .resolve(name)
                    .map_or(Err(MonkeyError::undefined_variable(name)), |symbol| {
                        Ok(symbol)
                    })?;

                let scope = symbol.scope;
                let index = symbol.index;

                match scope {
                    symbol::Scope::Local => {
                        self.emit(Op::GetLocal(index as u8));
                    }
                    symbol::Scope::Global => {
                        self.emit(Op::GetGlobal(index));
                    }
                    symbol::Scope::Builtin => {
                        self.emit(Op::GetBuiltin(index as u8));
                    }
                }
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
                    _ => return Err(MonkeyError::type_mismatch(format!("{}{}", operator, expr))),
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
                        )))
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
            Expression::Function { params, body } => {
                self.enter_scope();

                for param in params {
                    self.symbol_table.define(param);
                }

                self.compile_block_stmt(body)?;

                let locals = self.symbol_table.size();

                let mut bytes = self.leave_scope();

                if bytes.len() == 0 {
                    bytes.push(Op::CONSTANT);
                    bytes.extend_from_slice(&6u16.to_be_bytes());
                }
                bytes.push(Op::RETURN);

                let reference = self.allocate(Object::Function {
                    bytes,
                    locals,
                    params: params.len(),
                    free: vec![],
                });
                self.emit(Op::Closure(reference, 0));
            }
            Expression::Call { expr, args } => {
                self.compile_expr(expr)?;

                for arg in args {
                    self.compile_expr(arg)?;
                }

                self.emit(Op::Call(args.len() as u8));
            }
            Expression::Index { expr, index } => {
                self.compile_expr(expr)?;
                self.compile_expr(index)?;
                self.emit(Op::Index);
            }
        }

        Ok(())
    }

    pub fn encode(&mut self, op: Op) {
        match op {
            Op::Constant(reference) => {
                self.current_scope().bytes.push(Op::CONSTANT);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&reference.to_be_bytes());
            }
            Op::Pop => {
                self.current_scope().bytes.push(Op::POP);
            }
            Op::Unit => {
                self.current_scope().bytes.push(Op::UNIT);
            }
            Op::False => {
                self.current_scope().bytes.push(Op::FALSE);
            }
            Op::True => {
                self.current_scope().bytes.push(Op::TRUE);
            }
            Op::Add => {
                self.current_scope().bytes.push(Op::ADD);
            }
            Op::Sub => {
                self.current_scope().bytes.push(Op::SUB);
            }
            Op::Mul => {
                self.current_scope().bytes.push(Op::MUL);
            }
            Op::Div => {
                self.current_scope().bytes.push(Op::DIV);
            }
            Op::Eq => {
                self.current_scope().bytes.push(Op::EQ);
            }
            Op::Neq => {
                self.current_scope().bytes.push(Op::NEQ);
            }
            Op::Lt => {
                self.current_scope().bytes.push(Op::LT);
            }
            Op::Gt => {
                self.current_scope().bytes.push(Op::GT);
            }
            Op::Minus => {
                self.current_scope().bytes.push(Op::MINUS);
            }
            Op::Bang => {
                self.current_scope().bytes.push(Op::BANG);
            }
            Op::JumpIfNot(address) => {
                self.current_scope().bytes.push(Op::JUMPIFNOT);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&address.to_be_bytes());
            }
            Op::Jump(address) => {
                self.current_scope().bytes.push(Op::JUMP);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&address.to_be_bytes());
            }
            Op::GetGlobal(binding) => {
                self.current_scope().bytes.push(Op::GETGLOBAL);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&binding.to_be_bytes());
            }
            Op::SetGlobal(binding) => {
                self.current_scope().bytes.push(Op::SETGLOBAL);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&binding.to_be_bytes());
            }
            Op::Array(n) => {
                self.current_scope().bytes.push(Op::ARRAY);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&n.to_be_bytes());
            }
            Op::Map(n) => {
                self.current_scope().bytes.push(Op::MAP);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&n.to_be_bytes());
            }
            Op::Index => {
                self.current_scope().bytes.push(Op::INDEX);
            }
            Op::Call(m) => {
                self.current_scope().bytes.push(Op::CALL);
                self.current_scope().bytes.push(m);
            }
            Op::Return => {
                self.current_scope().bytes.push(Op::RETURN);
            }
            Op::GetLocal(binding) => {
                self.current_scope().bytes.push(Op::GETLOCAL);
                self.current_scope().bytes.push(binding);
            }
            Op::SetLocal(binding) => {
                self.current_scope().bytes.push(Op::SETLOCAL);
                self.current_scope().bytes.push(binding);
            }
            Op::GetBuiltin(builtin) => {
                self.current_scope().bytes.push(Op::GETBUILTIN);
                self.current_scope().bytes.push(builtin);
            }
            Op::Closure(reference, free) => {
                self.current_scope().bytes.push(Op::CLOSURE);
                self.current_scope()
                    .bytes
                    .extend_from_slice(&reference.to_be_bytes());
                self.current_scope().bytes.push(free);
            }
        }
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
                    Op::CALL => {
                        let m = self[i + 1];
                        ops.push(Op::Call(m));
                        i += 1;
                    }
                    Op::RETURN => ops.push(Op::Return),
                    Op::GETLOCAL => {
                        let binding = self[i + 1];
                        ops.push(Op::GetLocal(binding));
                        i += 1;
                    }
                    Op::SETLOCAL => {
                        let binding = self[i + 1];
                        ops.push(Op::SetLocal(binding));
                        i += 1;
                    }
                    Op::GETBUILTIN => {
                        let builtin = self[i + 1];
                        ops.push(Op::GetBuiltin(builtin));
                        i += 1;
                    }
                    Op::CLOSURE => {
                        let reference = u16::from_be_bytes([self[i + 1], self[i + 2]]);
                        let free = self[i + 3];
                        ops.push(Op::Closure(reference, free));
                        i += 3;
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
        "1 + 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Add
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "integer arithmetic 01"
    )]
    #[test_case(
        "1 - 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Sub
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "integer arithmetic 02"
    )]
    #[test_case(
        "1 * 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Mul
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "integer arithmetic 03"
    )]
    #[test_case(
        "2 / 1",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Div
        ],
        heap(vec![
            Object::integer(2),
            Object::integer(1)
        ]) ;
        "integer arithmetic 04"
    )]
    #[test_case(
        "-1",
        vec![
            Op::Constant(9),
            Op::Minus
        ],
        heap(vec![
            Object::integer(1)
        ]) ;
        "integer arithmetic 05"
    )]
    #[test_case(
        "false",
        vec![
            Op::False
        ],
        heap(vec![]) ;
        "boolean expression 01"
    )]
    #[test_case(
        "true",
        vec![
            Op::True
        ],
        heap(vec![]) ;
        "boolean expression 02"
    )]
    #[test_case(
        "1 == 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Eq
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "boolean expression 03"
    )]
    #[test_case(
        "1 != 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Neq
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "boolean expression 04"
    )]
    #[test_case(
        "1 < 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Lt
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "boolean expression 05"
    )]
    #[test_case(
        "1 > 2",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Gt
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "boolean expression 06"
    )]
    #[test_case(
        "true == false",
        vec![
            Op::True,
            Op::False,
            Op::Eq
        ],
        heap(vec![]) ;
        "boolean expression 07"
    )]
    #[test_case(
        "true != false",
        vec![
            Op::True,
            Op::False,
            Op::Neq
        ],
        heap(vec![]) ;
        "boolean expression 08"
    )]
    #[test_case(
        "!true",
        vec![
            Op::True,
            Op::Bang
        ],
        heap(vec![]) ;
        "boolean expression 09"
    )]
    #[test_case(
        "\"monkey\"",
        vec![
            Op::Constant(9)
        ],
        heap(vec![
            Object::string("monkey")
        ]) ;
        "string expression 01"
    )]
    #[test_case(
        "\"mon\" + \"key\"",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Add
        ],
        heap(vec![
            Object::string("mon"),
            Object::string("key")
        ]) ;
        "string expression 02"
    )]
    #[test_case(
        "[]",
        vec![
            Op::Array(0)
        ],
        heap(vec![]) ;
        "array expression 01"
    )]
    #[test_case(
        "[1, 2, 3]",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Constant(11),
            Op::Array(3)
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ]) ;
        "array expression 02"
    )]
    #[test_case(
        "[1 + 2, 3 - 4, 5 * 6]",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Add,
            Op::Constant(11),
            Op::Constant(12),
            Op::Sub,
            Op::Constant(13),
            Op::Constant(14),
            Op::Mul,
            Op::Array(3)
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6)
        ]) ;
        "array expression 03"
    )]
    #[test_case(
        "{}",
        vec![
            Op::Map(0),
        ],
        heap(vec![]) ;
        "map expression 01"
    )]
    #[test_case(
        "{1: 2, 3: 4, 5: 6}",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Constant(11),
            Op::Constant(12),
            Op::Constant(13),
            Op::Constant(14),
            Op::Map(3),
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6),
        ]) ;
        "map expression 02"
    )]
    #[test_case(
        "{1: 2 + 3, 4: 5 * 6}",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Constant(11),
            Op::Add,
            Op::Constant(12),
            Op::Constant(13),
            Op::Constant(14),
            Op::Mul,
            Op::Map(2),
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(4),
            Object::integer(5),
            Object::integer(6),
        ]) ;
        "map expression 03"
    )]
    #[test_case(
        "[1, 2, 3][1 + 1]",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Constant(11),
            Op::Array(3),
            Op::Constant(12),
            Op::Constant(13),
            Op::Add,
            Op::Index,
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
            Object::integer(1),
            Object::integer(1),
        ]) ;
        "index expression 01"
    )]
    #[test_case(
        "{1: 2}[2 - 1]",
        vec![
            Op::Constant(9),
            Op::Constant(10),
            Op::Map(1),
            Op::Constant(11),
            Op::Constant(12),
            Op::Sub,
            Op::Index,
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(2),
            Object::integer(1),
        ]) ;
        "index expression 02"
    )]
    #[test_case(
        "if (true) { 10 }; 20;",
        vec![
            Op::True,
            Op::JumpIfNot(10),
            Op::Constant(9),
            Op::Jump(11),
            Op::Unit,
            Op::Pop,
            Op::Constant(10),
            Op::Pop
        ],
        heap(vec![
            Object::integer(10),
            Object::integer(20)
        ]) ;
        "if expression 01"
    )]
    #[test_case(
        "if (true) { 1; 2; 3 }",
        vec![
            Op::True,
            Op::JumpIfNot(18),
            Op::Constant(9),
            Op::Pop,
            Op::Constant(10),
            Op::Pop,
            Op::Constant(11),
            Op::Jump(19),
            Op::Unit
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2),
            Object::integer(3)
        ]) ;
        "if expression 02"
    )]
    #[test_case(
        "if (true) { 10 } else { 20 }; 30;",
        vec![
            Op::True,
            Op::JumpIfNot(10),
            Op::Constant(9),
            Op::Jump(13),
            Op::Constant(10),
            Op::Pop,
            Op::Constant(11),
            Op::Pop
        ],
        heap(vec![
            Object::integer(10),
            Object::integer(20),
            Object::integer(30)
        ]) ;
        "if expression 03"
    )]
    #[test_case(
        "1; 2",
        vec![
            Op::Constant(9),
            Op::Pop,
            Op::Constant(10)
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "expr stmt 01"
    )]
    #[test_case(
        "let one = 1; let two = 2;",
        vec![
            Op::Constant(9),
            Op::SetGlobal(0),
            Op::Constant(10),
            Op::SetGlobal(1)
        ],
        heap(vec![
            Object::integer(1),
            Object::integer(2)
        ]) ;
        "global let stmt 01"
    )]
    #[test_case(
        "let one = 1; one;",
        vec![
            Op::Constant(9),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::Pop
        ],
        heap(vec![
            Object::integer(1)
        ]) ;
        "global let stmt 02"
    )]
    #[test_case(
        "let one = 1; let two = one; two",
        vec![
            Op::Constant(9),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::SetGlobal(1),
            Op::GetGlobal(1)
        ],
        heap(vec![
            Object::integer(1)
        ]) ;
        "global let stmt 03"
    )]
    #[test_case(
        "fn() { return 5 + 10; }",
        vec![
            Op::Closure(11, 0),
        ],
        heap(vec![
            Object::integer(5),
            Object::integer(10),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Constant(10),
                    Op::Add,
                    Op::Return,
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "function literal 01"
    )]
    #[test_case(
        "fn() { 5 + 10 }",
        vec![
            Op::Closure(11, 0),
        ],
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
                free: vec![],
            },
        ]) ;
        "function literal 02"
    )]
    #[test_case(
        "fn() { 5; 10 }",
        vec![
            Op::Closure(11, 0),
        ],
        heap(vec![
            Object::integer(5),
            Object::integer(10),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Pop,
                    Op::Constant(10),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "function literal 03"
    )]
    #[test_case(
        "fn() { }",
        vec![
            Op::Closure(9, 0),
        ],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(6),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "function literal 04"
    )]
    #[test_case(
        "fn() { 42 }()",
        vec![
            Op::Closure(10, 0),
            Op::Call(0),
        ],
        heap(vec![
            Object::integer(42),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "call expr 01"
    )]
    #[test_case(
        "let f = fn() { 42 }; f()",
        vec![
            Op::Closure(10, 0),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::Call(0),
        ],
        heap(vec![
            Object::integer(42),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "call expr 02"
    )]
    #[test_case(
        "let f = fn() { 1 }; let g = fn() { 2 }; f() + g()",
        vec![
            Op::Closure(10, 0),
            Op::SetGlobal(0),
            Op::Closure(12, 0),
            Op::SetGlobal(1),
            Op::GetGlobal(0),
            Op::Call(0),
            Op::GetGlobal(1),
            Op::Call(0),
            Op::Add,
        ],
        heap(vec![
            Object::integer(1),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
            Object::integer(2),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(11),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "call expr 03"
    )]
    #[test_case(
        "let f = fn(x) { x }; f(42)",
        vec![
            Op::Closure(9, 0),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::Constant(10),
            Op::Call(1),
        ],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1,
                params: 1,
                free: vec![],
            },
            Object::integer(42),
        ]) ;
        "call expr 04"
    )]
    #[test_case(
        "let f = fn(x, y, z) { x; y; z }; f(1, 2, 3)",
        vec![
            Op::Closure(9, 0),
            Op::SetGlobal(0),
            Op::GetGlobal(0),
            Op::Constant(10),
            Op::Constant(11),
            Op::Constant(12),
            Op::Call(3),
        ],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetLocal(0),
                    Op::Pop,
                    Op::GetLocal(1),
                    Op::Pop,
                    Op::GetLocal(2),
                    Op::Return,
                ]),
                locals: 3,
                params: 3,
                free: vec![],
            },
            Object::integer(1),
            Object::integer(2),
            Object::integer(3),
        ]) ;
        "call expr 05"
    )]
    #[test_case(
        "let x = 42; fn() { x }",
        vec![
            Op::Constant(9),
            Op::SetGlobal(0),
            Op::Closure(10, 0),
        ],
        heap(vec![
            Object::integer(42),
            Object::Function {
                bytes: encode(vec![
                    Op::GetGlobal(0),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            },
        ]) ;
        "let stmt scopes 01"
    )]
    #[test_case(
        "fn() { let x = 42; x }",
        vec![
            Op::Closure(10, 0),
        ],
        heap(vec![
            Object::integer(42),
            Object::Function {
                bytes: encode(vec![
                    Op::Constant(9),
                    Op::SetLocal(0),
                    Op::GetLocal(0),
                    Op::Return,
                ]),
                locals: 1,
                params: 0,
                free: vec![],
            },
        ]) ;
        "let stmt scopes 02"
    )]
    #[test_case(
        "fn() { let x = 1; let y = 2; x + y }",
        vec![
            Op::Closure(11, 0),
        ],
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
                free: vec![],
            },
        ]) ;
        "let stmt scopes 03"
    )]
    #[test_case(
        "len([]); push([], 1);",
        vec![
            Op::GetBuiltin(Builtin::LEN),
            Op::Array(0),
            Op::Call(1),
            Op::Pop,
            Op::GetBuiltin(Builtin::PUSH),
            Op::Array(0),
            Op::Constant(9),
            Op::Call(2),
            Op::Pop,
        ],
        heap(vec![
            Object::integer(1),
        ]);
        "builtins 01"
    )]
    #[test_case(
        "fn() { len([]) }",
        vec![
            Op::Closure(9, 0),
        ],
        heap(vec![
            Object::Function {
                bytes: encode(vec![
                    Op::GetBuiltin(Builtin::LEN),
                    Op::Array(0),
                    Op::Call(1),
                    Op::Return,
                ]),
                locals: 0,
                params: 0,
                free: vec![],
            }
        ]) ;
        "builtins 02"
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
