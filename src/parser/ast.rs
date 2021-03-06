use crate::lexer::Token;

use std::fmt;

trait Pretty {
    fn pretty(&self, indent: usize) -> String;
}

#[derive(Debug, PartialEq, Eq)]
pub enum Statement {
    Let { name: Expression, value: Expression },
    Return(Expression),
    Stmt(Expression),
    Expr(Expression),
}

impl Pretty for Statement {
    fn pretty(&self, indent: usize) -> String {
        let indent = " ".repeat(indent);

        match self {
            Statement::Let { name, value } => format!("{}let {} = {};", indent, name, value),
            Statement::Return(expr) => format!("{}return {};", indent, expr),
            Statement::Stmt(expr) => format!("{}{};", indent, expr),
            Statement::Expr(expr) => format!("{}{}", indent, expr),
        }
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty(0))
    }
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

    pub fn expr(value: Expression) -> Statement {
        Statement::Expr(value)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Block(Vec<Statement>);

pub type Program = Block;

impl Pretty for Block {
    fn pretty(&self, indent: usize) -> String {
        let spacing = " ".repeat(indent);
        let mut formatted = String::new();

        for stmt in &self.0[0..self.0.len() - 1] {
            formatted.push_str(&format!("{}{}", spacing, stmt.pretty(indent)));
            formatted.push_str("\n");
        }
        formatted.push_str(&format!(
            "{}{}",
            spacing,
            self.0[self.0.len() - 1].pretty(indent)
        ));

        formatted
    }
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &self.pretty(0))
    }
}

impl Block {
    pub fn new(stmts: Vec<Statement>) -> Block {
        Block(stmts)
    }
}

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

impl Pretty for Expression {
    fn pretty(&self, indent: usize) -> String {
        match self {
            Expression::Name(name) => format!("{}", name),
            Expression::Integer(n) => format!("{}", n),
            Expression::Boolean(b) => format!("{}", b),
            Expression::Prefix { operator, expr } => format!("({}{})", operator, expr),
            Expression::Infix {
                left,
                operator,
                right,
            } => format!("({} {} {})", left, operator, right),
            Expression::If { cond, yes, no } => {
                let cond = cond.pretty(0);
                let yes = yes.pretty(indent + 1);
                let pretty_no = no.as_ref().map_or("".to_string(), |block| {
                    format!("else {{\n{}\n}}", block.pretty(indent + 1))
                });
                format!("if ({}) {{\n{}\n}} {}", cond, yes, pretty_no)
            }
            Expression::Dummy => format!("{}", "DUMMY"),
        }
    }
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.pretty(0))
    }
}

impl Expression {
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
}
