use crate::lexer::{Lexer, Token};
use crate::parser::ast::*;

use std::{fmt, num};

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<num::ParseIntError> for ParseError {
    fn from(error: num::ParseIntError) -> Self {
        ParseError(error.to_string())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // >, <
    Sum,         // +
    Product,     // *
    Prefix,      //-, !
    Call,        // f()
}

#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peek: Token,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer,
            current: Token::Eof,
            peek: Token::Eof,
        };

        parser.advance();
        parser.advance();

        parser
    }

    fn advance(&mut self) {
        std::mem::swap(&mut self.current, &mut self.peek);
        self.peek = self.lexer.next().unwrap();
    }

    fn peek_identifer(&mut self) -> Result<String> {
        match self.peek.clone() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError(format!(
                "Expected identifier, got {} instead.",
                self.peek
            ))),
        }
    }

    fn peek_acknowledge(&mut self, expected: Token) -> Result<()> {
        if self.peek == expected {
            self.advance();
            Ok(())
        } else {
            Err(ParseError(format!(
                "Expected {}, got {} instead.",
                expected, self.peek
            )))
        }
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut stmts = vec![];

        while self.current != Token::Eof {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);

            self.advance();
        }

        Ok(Program::new(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Statement> {
        match self.current {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Statement> {
        let name = self.peek_identifer()?;
        self.peek_acknowledge(Token::Assign)?;

        while self.current != Token::Semicolon {
            self.advance();
        }

        Ok(Statement::let_stmt(name, Expression::Dummy))
    }

    fn parse_return_stmt(&mut self) -> Result<Statement> {
        self.advance();

        while self.current != Token::Semicolon {
            self.advance();
        }

        Ok(Statement::return_stmt(Expression::Dummy))
    }

    fn parse_expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.parse_expr(Precedence::Lowest)?;
        self.peek_acknowledge(Token::Semicolon)?;

        Ok(Statement::stmt(expr))
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression> {
        let left = match &self.current {
            Token::Ident(name) => Expression::name(name),
            Token::Int(n) => n.parse::<i32>().map(|n| Expression::integer(n))?,
            Token::Bang => self.parse_prefix_expr()?,
            Token::Minus => self.parse_prefix_expr()?,
            _ => {
                return Err(ParseError("Oops.".to_string()));
            }
        };

        Ok(left)
    }

    fn parse_prefix_expr(&mut self) -> Result<Expression> {
        let operator = self.current.clone();

        self.advance();

        let right = self.parse_expr(Precedence::Prefix)?;

        Ok(Expression::prefix(operator, right))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case(
        b"let x = 5;\n\
          let y = 10;\n\
          let foobar = 838383;\n\
        ",
        vec![
            Statement::let_stmt("x", Expression::Dummy),
            Statement::let_stmt("y", Expression::Dummy),
            Statement::let_stmt("foobar", Expression::Dummy),
        ] ;
        "let stmt"
    )]
    #[test_case(
        b"return 5;\n\
          return 10;\n\
          return 993322;\n\
        ",
        vec![
            Statement::return_stmt(Expression::Dummy),
            Statement::return_stmt(Expression::Dummy),
            Statement::return_stmt(Expression::Dummy),
        ] ;
        "return stmt"
    )]
    #[test_case(
        b"foobar;",
        vec![
            Statement::stmt(Expression::name("foobar")),
        ] ;
        "identifer stmt"
    )]
    #[test_case(
        b"5;",
        vec![
            Statement::stmt(Expression::integer(5)),
        ] ;
        "integer stmt"
    )]
    #[test_case(
        b"!5;",
        vec![
            Statement::stmt(Expression::prefix(Token::Bang , Expression::integer(5)))
        ] ;
        "prefix bang"
    )]
    #[test_case(
        b"-15;",
        vec![
            Statement::stmt(Expression::prefix(Token::Minus, Expression::integer(15)))
        ] ;
        "prefix minus"
    )]
    fn test(input: &[u8], expected: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let output = parser.parse();

        let expected = Program::new(expected);

        assert_eq!(output.unwrap(), expected)
    }
}
