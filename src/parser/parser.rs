use crate::lexer::{Lexer, Token};
use crate::parser::ast::*;

use std::fmt;

type Result<T> = std::result::Result<T, ParseError>;

#[derive(Debug)]
pub struct ParseError(String);

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
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
            Token::Let => self
                .parse_let_stmt()
                .map(|let_stmt| Statement::LetKind(let_stmt)),
            _ => Err(ParseError("Oops".to_string())),
        }
    }

    fn parse_let_stmt(&mut self) -> Result<Let> {
        let name = self.peek_identifer()?;
        self.peek_acknowledge(Token::Assign)?;

        while self.current != Token::Semicolon {
            self.advance();
        }

        Ok(Let::new(name, Expression::Dummy))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_let_stmt() {
        let input = b"let x = 5;\n\
            let y = 10;\n\
            let foobar = 838383;\n\
            ";

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let output = parser.parse();

        let expected = Program::new(vec![
            Statement::let_stmt("x", Expression::Dummy),
            Statement::let_stmt("y", Expression::Dummy),
            Statement::let_stmt("foobar", Expression::Dummy),
        ]);

        assert_eq!(output.unwrap(), expected)
    }
}
