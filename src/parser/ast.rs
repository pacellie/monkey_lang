use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return(Expression),
    Stmt(Expression),
    Expr(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block(Vec<Statement>);

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
    Dummy,
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
