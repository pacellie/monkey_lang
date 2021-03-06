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

impl Precedence {
    pub fn precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            _ => Precedence::Lowest,
        }
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
                "Expected `identifier`, got `{}` instead.",
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
                "Expected `{}`, got `{}` instead.",
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

        Ok(Block::new(stmts))
    }

    fn parse_stmt(&mut self) -> Result<Statement> {
        match self.current {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    // let <expr> = <expr>;
    fn parse_let_stmt(&mut self) -> Result<Statement> {
        let name = self.peek_identifer()?;
        self.peek_acknowledge(Token::Assign)?;

        while self.current != Token::Semicolon {
            self.advance();
        }

        Ok(Statement::let_stmt(name, Expression::Dummy))
    }

    // return <epxr>;
    fn parse_return_stmt(&mut self) -> Result<Statement> {
        self.advance();

        while self.current != Token::Semicolon {
            self.advance();
        }

        Ok(Statement::return_stmt(Expression::Dummy))
    }

    // <expr>; | <expr>
    fn parse_expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.parse_expr(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.advance();
            Ok(Statement::stmt(expr))
        } else {
            Ok(Statement::expr(expr))
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression> {
        #[rustfmt::skip]
        let mut expr = match &self.current {
            Token::Ident(name) => Expression::name(name),
            Token::Int(n)      => n.parse::<i32>().map(|n| Expression::integer(n))?,
            Token::True                => Expression::boolean(true),
            Token::False               => Expression::boolean(false),
            Token::LParen              => self.parse_group_expr()?,
            Token::Bang | Token::Minus => self.parse_prefix_expr()?,
            _ => {
                return Err(ParseError(format!(
                    "`{}` is not a valid start of an expression.",
                    self.current
                )));
            }
        };

        while self.peek != Token::Semicolon && precedence < Precedence::precedence(&self.peek) {
            expr = match &self.peek {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::Neq
                | Token::Lt
                | Token::Gt => {
                    self.advance();
                    self.parse_infix_expr(expr)?
                }
                _ => expr,
            }
        }

        Ok(expr)
    }

    // <un_op><expr>
    fn parse_prefix_expr(&mut self) -> Result<Expression> {
        let operator = self.current.clone();

        self.advance();

        let right = self.parse_expr(Precedence::Prefix)?;

        Ok(Expression::prefix(operator, right))
    }

    // <expr><bin_op><expr>
    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression> {
        let operator = self.current.clone();
        let precedence = Precedence::precedence(&operator);

        self.advance();

        let right = self.parse_expr(precedence)?;

        Ok(Expression::infix(left, operator, right))
    }

    // (<expr>)
    fn parse_group_expr(&mut self) -> Result<Expression> {
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.peek_acknowledge(Token::RParen)?;

        Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case(b"let x = 5;", "let x = DUMMY;" ; "let stmt"      )]
    #[test_case(b"return 5;" , "return DUMMY;"  ; "return stmt"   )]
    #[test_case(b"foobar;"   , "foobar;"        ; "identifer stmt")]
    #[test_case(b"5;"        , "5;"             ; "integer stmt"  )]
    #[test_case(b"true;"     , "true;"          ; "true stmt"     )]
    #[test_case(b"false;"    , "false;"         ; "false stmt"    )]
    #[test_case(b"!5"    , "(!5)"     ; "prefix bang 01" )]
    #[test_case(b"!true" , "(!true)"  ; "prefix bang 02" )]
    #[test_case(b"!false", "(!false)" ; "prefix bang 03" )]
    #[test_case(b"-15"   , "(-15)"    ; "prefix minus"   )]
    #[test_case(b"5 + 5" , "(5 + 5)"  ; "infix plus"    )]
    #[test_case(b"5 - 5" , "(5 - 5)"  ; "infix minus"   )]
    #[test_case(b"5 * 5" , "(5 * 5)"  ; "infix asterisk")]
    #[test_case(b"5 / 5" , "(5 / 5)"  ; "infix slash"   )]
    #[test_case(b"5 > 5" , "(5 > 5)"  ; "infix gt"      )]
    #[test_case(b"5 < 5" , "(5 < 5)"  ; "infix lt"      )]
    #[test_case(b"5 == 5", "(5 == 5)" ; "infix eq"      )]
    #[test_case(b"5 != 5", "(5 != 5)" ; "infix neq"     )]
    #[test_case(b"-a * b"                     , "((-a) * b)"                             ; "precedence 01")]
    #[test_case(b"!-a"                        , "(!(-a))"                                ; "precedence 02")]
    #[test_case(b"a + b + c"                  , "((a + b) + c)"                          ; "precedence 03")]
    #[test_case(b"a + b - c"                  , "((a + b) - c)"                          ; "precedence 04")]
    #[test_case(b"a * b * c"                  , "((a * b) * c)"                          ; "precedence 05")]
    #[test_case(b"a * b / c"                  , "((a * b) / c)"                          ; "precedence 06")]
    #[test_case(b"a + b / c"                  , "(a + (b / c))"                          ; "precedence 07")]
    #[test_case(b"a + b * c + d / e - f"      , "(((a + (b * c)) + (d / e)) - f)"        ; "precedence 08")]
    #[test_case(b"3 + 4; -5 * 5"              , "(3 + 4);\n((-5) * 5)"                   ; "precedence 09")]
    #[test_case(b"5 > 4 == 3 < 4"             , "((5 > 4) == (3 < 4))"                   ; "precedence 10")]
    #[test_case(b"5 < 4 != 3 > 4"             , "((5 < 4) != (3 > 4))"                   ; "precedence 11")]
    #[test_case(b"3 + 4 * 5 == 3 * 1 + 4 * 5" , "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))" ; "precedence 12")]
    #[test_case(b"3 > 5 == false"             , "((3 > 5) == false)"                     ; "precedence 13")]
    #[test_case(b"3 < 5 == true"              , "((3 < 5) == true)"                      ; "precedence 14")]
    #[test_case(b"true == true"               , "(true == true)"                         ; "precedence 15")]
    #[test_case(b"true != false"              , "(true != false)"                        ; "precedence 16")]
    #[test_case(b"false == false"             , "(false == false)"                       ; "precedence 17")]
    #[test_case(b"1 + (2 + 3) + 4"            , "((1 + (2 + 3)) + 4)"                    ; "precedence 18")]
    #[test_case(b"(5 + 5) * 2"                , "((5 + 5) * 2)"                          ; "precedence 19")]
    #[test_case(b"2 / (5 + 5)"                , "(2 / (5 + 5))"                          ; "precedence 20")]
    #[test_case(b"-(5 + 5)"                   , "(-(5 + 5))"                             ; "precedence 21")]
    #[test_case(b"!(true == true)"            , "(!(true == true))"                      ; "precedence 22")]
    fn test(input: &[u8], expected: &str) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let output = parser.parse();
        let output: String = output.unwrap().to_string();

        assert_eq!(output, expected)
    }
}
