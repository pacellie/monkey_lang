use crate::lexer::Token;

use std::fmt::{self, Display};

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return(Expression),
    Stmt(Expression),
    Expr(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Stmt(expr) => write!(f, "{};", expr),
            Statement::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block(pub Vec<Statement>);

fn fmt_slice<A, B>(slice: &[A], sep: B) -> String
where
    A: Display,
    B: Display,
{
    let mut result = String::new();

    for stmt in &slice[0..slice.len() - 1] {
        result.push_str(&stmt.to_string());
        result.push_str(&sep.to_string());
    }
    result.push_str(&slice[slice.len() - 1].to_string());

    result
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", fmt_slice(&self.0[..], " "))
    }
}

pub type Program = Block;

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Name(String),
    Integer(i32),
    Boolean(bool),
    Prefix {
        operator: Token,
        expr: Box<Expression>,
    },
    Infix {
        left: Box<Expression>,
        operator: Token,
        right: Box<Expression>,
    },
    If {
        cond: Box<Expression>,
        yes: Block,
        no: Option<Block>,
    },
    Function {
        params: Vec<String>,
        body: Block,
    },
    Call {
        function: Box<Expression>,
        args: Vec<Expression>,
    },
    Dummy,
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Name(string) => write!(f, "{}", string),
            Expression::Integer(int) => write!(f, "{}", int),
            Expression::Boolean(boolean) => write!(f, "{}", boolean),
            Expression::Prefix { operator, expr } => write!(f, "({}{})", operator, expr),
            Expression::Infix {
                left,
                operator,
                right,
            } => write!(f, "({} {} {})", left, operator, right),
            Expression::If { cond, yes, no } => {
                let no = no
                    .as_ref()
                    .map_or("".to_string(), |block| format!(" else {{ {} }}", block));
                write!(f, "if ({}) {{ {} }}{}", cond, yes, no)
            }
            Expression::Function { params, body } => {
                write!(f, "fn ({}) {{ {} }}", fmt_slice(params, ", "), body)
            }
            Expression::Call { function, args } => {
                write!(f, "{}({})", function, fmt_slice(args, ", "))
            }
            Expression::Dummy => write!(f, "DUMMY"),
        }
    }
}

// Statement
pub fn let_stmt<S: Into<String>>(name: S, value: Expression) -> Statement {
    Statement::Let {
        name: name.into(),
        value,
    }
}

pub fn return_stmt(value: Expression) -> Statement {
    Statement::Return(value)
}

pub fn stmt(value: Expression) -> Statement {
    Statement::Stmt(value)
}

pub fn expr_stmt(value: Expression) -> Statement {
    Statement::Expr(value)
}

// Block / Program
pub fn block(stmts: Vec<Statement>) -> Block {
    Block(stmts)
}

// Expression
pub fn name<S: Into<String>>(name: S) -> Expression {
    Expression::Name(name.into())
}

pub fn integer(n: i32) -> Expression {
    Expression::Integer(n)
}

pub fn boolean(b: bool) -> Expression {
    Expression::Boolean(b)
}

pub fn prefix(operator: Token, expr: Expression) -> Expression {
    Expression::Prefix {
        operator,
        expr: Box::new(expr),
    }
}

pub fn infix(left: Expression, operator: Token, right: Expression) -> Expression {
    Expression::Infix {
        left: Box::new(left),
        operator,
        right: Box::new(right),
    }
}

pub fn if_expr(cond: Expression, yes: Block, no: Option<Block>) -> Expression {
    Expression::If {
        cond: Box::new(cond),
        yes,
        no,
    }
}

pub fn function<S: Into<String> + Clone>(params: Vec<S>, body: Block) -> Expression {
    Expression::Function {
        params: params.iter().map(|param| param.clone().into()).collect(),
        body,
    }
}

pub fn call(function: Expression, args: Vec<Expression>) -> Expression {
    Expression::Call {
        function: Box::new(function),
        args,
    }
}
