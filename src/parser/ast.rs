use crate::lexer::Token;

use std::fmt;

use itertools::Itertools;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, expr: Expression },
    Return(Expression),
    Stmt(Expression),
    Expr(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let { name, expr } => write!(f, "let {} = {};", name, expr),
            Statement::Return(expr) => write!(f, "return {};", expr),
            Statement::Stmt(expr) => write!(f, "{};", expr),
            Statement::Expr(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Block(pub Vec<Statement>);

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.iter().join(" "))
    }
}

pub type Program = Block;

#[derive(Debug, Clone, PartialEq, Eq)]
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
                write!(f, "fn ({}) {{ {} }}", params.iter().join(", "), body)
            }
            Expression::Call { function, args } => {
                write!(f, "{}({})", function, args.iter().join(", "))
            }
        }
    }
}

pub fn let_stmt<S: Into<String>>(name: S, expr: Expression) -> Statement {
    Statement::Let {
        name: name.into(),
        expr,
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

pub fn block(stmts: Vec<Statement>) -> Block {
    Block(stmts)
}

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