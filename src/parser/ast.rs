use crate::lexer::Token;

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: Expression, value: Expression },
    Return(Expression),
    Stmt(Expression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Expression {
    Name(String),
    Integer(i32),
    Prefix {
        operator: Token,
        expr: Box<Expression>,
    },
    Dummy,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    stmts: Vec<Statement>,
}

impl Statement {
    pub fn let_stmt<S: Into<String>>(name: S, value: Expression) -> Statement {
        Statement::Let {
            name: Expression::Name(name.into()),
            value,
        }
    }

    pub fn return_stmt(value: Expression) -> Statement {
        Statement::Return(value)
    }

    pub fn stmt(value: Expression) -> Statement {
        Statement::Stmt(value)
    }
}

impl Expression {
    pub fn name<S: Into<String>>(name: S) -> Expression {
        Expression::Name(name.into())
    }

    pub fn integer(n: i32) -> Expression {
        Expression::Integer(n)
    }

    pub fn prefix(operator: Token, expr: Expression) -> Expression {
        Expression::Prefix {
            operator,
            expr: Box::new(expr),
        }
    }
}

impl Program {
    pub fn new(stmts: Vec<Statement>) -> Program {
        Program { stmts }
    }
}
