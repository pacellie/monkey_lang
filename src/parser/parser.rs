use crate::lexer::{Lexer, Token};
use crate::parser::ast::*;
use crate::parser::error::{ParseError, Result};

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
            Token::LParen => Precedence::Call,
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

        Ok(block(stmts))
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
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.peek_acknowledge(Token::Semicolon)?;

        Ok(let_stmt(name, expr))
    }

    // return <epxr>;
    fn parse_return_stmt(&mut self) -> Result<Statement> {
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.peek_acknowledge(Token::Semicolon)?;

        Ok(return_stmt(expr))
    }

    // <stmt>*
    fn parse_block_stmt(&mut self) -> Result<Block> {
        self.advance();

        let mut stmts = vec![];

        while self.current != Token::RBrace && self.current != Token::Eof {
            let stmt = self.parse_stmt()?;
            stmts.push(stmt);
            self.advance();
        }

        Ok(block(stmts))
    }

    // <expr>; | <expr>
    fn parse_expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.parse_expr(Precedence::Lowest)?;

        if self.peek == Token::Semicolon {
            self.advance();
            Ok(stmt(expr))
        } else {
            Ok(expr_stmt(expr))
        }
    }

    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression> {
        #[rustfmt::skip]
        let mut expr = match &self.current {
            Token::Ident(s)    => name(s),
            Token::Int(n)      => n.parse::<i32>().map(|n| integer(n))?,
            Token::True                => boolean(true),
            Token::False               => boolean(false),
            Token::Function            => self.parse_function_expr()?,
            Token::LParen              => self.parse_group_expr()?,
            Token::Bang | Token::Minus => self.parse_prefix_expr()?,
            Token::If                  => self.parse_if_expr()?,
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
                Token::LParen => {
                    self.advance();
                    self.parse_call_expr(expr)?
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

        Ok(prefix(operator, right))
    }

    // <expr><bin_op><expr>
    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression> {
        let operator = self.current.clone();
        let precedence = Precedence::precedence(&operator);

        self.advance();

        let right = self.parse_expr(precedence)?;

        Ok(infix(left, operator, right))
    }

    // (<expr>)
    fn parse_group_expr(&mut self) -> Result<Expression> {
        self.advance();

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.peek_acknowledge(Token::RParen)?;

        Ok(expr)
    }

    // if (<expr>) { <block> } else { <block> } | if (<expr>) { <block> }
    fn parse_if_expr(&mut self) -> Result<Expression> {
        self.peek_acknowledge(Token::LParen)?;
        self.advance();

        let cond = self.parse_expr(Precedence::Lowest)?;

        self.peek_acknowledge(Token::RParen)?;
        self.peek_acknowledge(Token::LBrace)?;

        let yes = self.parse_block_stmt()?;

        if self.peek == Token::Else {
            self.advance();
            self.peek_acknowledge(Token::LBrace)?;

            let no = self.parse_block_stmt()?;

            Ok(if_expr(cond, yes, Some(no)))
        } else {
            Ok(if_expr(cond, yes, None))
        }
    }

    // fn(<params>)<block>
    fn parse_function_expr(&mut self) -> Result<Expression> {
        self.peek_acknowledge(Token::LParen)?;

        let params = self.parse_params()?;

        self.peek_acknowledge(Token::LBrace)?;

        let body = self.parse_block_stmt()?;

        Ok(function(params, body))
    }

    // (<param>(,<param>)*)*
    fn parse_params(&mut self) -> Result<Vec<String>> {
        let mut params = vec![];

        while self.peek != Token::RParen && self.peek != Token::Eof {
            let param = self.peek_identifer()?;
            params.push(param);

            if self.peek != Token::RParen {
                self.peek_acknowledge(Token::Comma)?;
            }
        }
        self.advance();

        Ok(params)
    }

    // <expr>(<args>)
    fn parse_call_expr(&mut self, function: Expression) -> Result<Expression> {
        let args = self.parse_args()?;

        Ok(call(function, args))
    }

    // (<expr>(,<expr>)*)*
    fn parse_args(&mut self) -> Result<Vec<Expression>> {
        let mut args = vec![];

        while self.peek != Token::RParen && self.peek != Token::Eof {
            self.advance();

            let arg = self.parse_expr(Precedence::Lowest)?;
            args.push(arg);

            if self.peek != Token::RParen {
                self.peek_acknowledge(Token::Comma)?;
            }
        }
        self.advance();

        Ok(args)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use lazy_static::lazy_static;
    use test_case::test_case;

    #[rustfmt::skip]
    lazy_static! {
        // Statement
        static ref LET_STMT: (&'static [u8], Program) =
        (
            b"let x = 5;",
            block(vec![
                let_stmt(
                    "x",
                    integer(5)
                )

            ])
        );

        static ref RETURN_STMT: (&'static [u8], Program) =
        (
            b"return 5;",
            block(vec![
                return_stmt(integer(5))
            ])
        );

        // Literal
        static ref LITERAL_01: (&'static [u8], Program) =
        (
            b"foobar",
            block(vec![expr_stmt(name("foobar"))])
        );

        static ref LITERAL_02: (&'static [u8], Program) =
        (
            b"5",
            block(vec![expr_stmt(integer(5))])
        );

        static ref LITERAL_03: (&'static [u8], Program) =
        (
            b"true",
            block(vec![expr_stmt(boolean(true))])
        );

        static ref LITERAL_04: (&'static [u8], Program) =
        (
            b"false",
            block(vec![expr_stmt(boolean(false))])
        );

        static ref LITERAL_05: (&'static [u8], Program) =
        (
            b"fn() {}",
            block(vec![expr_stmt(
                function::<String>(
                    vec![],
                    block(vec![])
                )
            )])
        );

        static ref LITERAL_06: (&'static [u8], Program) =
        (
            b"fn(x) {}",
            block(vec![expr_stmt(
                function(
                    vec![
                        "x",
                    ],
                    block(vec![])
                )
            )])
        );

        static ref LITERAL_07: (&'static [u8], Program) =
        (
            b"fn(x, y) { x + y; }",
            block(vec![expr_stmt(
                function(
                    vec![
                        "x",
                        "y",
                    ],
                    block(vec![
                        stmt(
                            infix(
                                name("x"),
                                Token::Plus,
                                name("y"),
                            )
                        )
                    ])
                )
            )])
        );

        static ref LITERAL_08: (&'static [u8], Program) =
        (
            b"fn(x, y, z) {}",
            block(vec![expr_stmt(
                function(
                    vec![
                        "x",
                        "y",
                        "z",
                    ],
                    block(vec![])
                )
            )])
        );

        // Prefix Expression
        static ref PREFIX_01: (&'static [u8], Program) =
        (
            b"!true",
            block(vec![expr_stmt(prefix(Token::Bang, boolean(true)))])
        );

        static ref PREFIX_02: (&'static [u8], Program) =
        (
            b"-15",
            block(vec![expr_stmt(prefix(Token::Minus, integer(15)))])
        );

        // Infix Expression
        static ref INFIX_01: (&'static [u8], Program) =
        (
            b"5 + 5",
            block(vec![expr_stmt(infix(integer(5), Token::Plus, integer(5)))])
        );

        static ref INFIX_02: (&'static [u8], Program) =
        (
            b"5 - 5",
            block(vec![expr_stmt(infix(integer(5), Token::Minus, integer(5)))])
        );

        static ref INFIX_03: (&'static [u8], Program) =
        (
            b"5 * 5",
            block(vec![expr_stmt(infix(integer(5), Token::Asterisk, integer(5)))])
        );

        static ref INFIX_04: (&'static [u8], Program) =
        (
            b"5 / 5",
            block(vec![expr_stmt(infix(integer(5), Token::Slash, integer(5)))])
        );

        static ref INFIX_05: (&'static [u8], Program) =
        (
            b"5 > 5",
            block(vec![expr_stmt(infix(integer(5), Token::Gt, integer(5)))])
        );

        static ref INFIX_06: (&'static [u8], Program) =
        (
            b"5 < 5",
            block(vec![expr_stmt(infix(integer(5), Token::Lt, integer(5)))])
        );

        static ref INFIX_07: (&'static [u8], Program) =
        (
            b"5 == 5",
            block(vec![expr_stmt(infix(integer(5), Token::Eq, integer(5)))])
        );

        static ref INFIX_08: (&'static [u8], Program) =
        (
            b"5 != 5",
            block(vec![expr_stmt(infix(integer(5), Token::Neq, integer(5)))])
        );

        // Precedence
        static ref PRECEDENCE_01: (&'static [u8], Program) =
        (
            b"-a * b",
            block(vec![expr_stmt(
                infix(
                    prefix(Token::Minus, name("a")),
                    Token::Asterisk,
                    name("b")
                )
            )])
        );

        static ref PRECEDENCE_02: (&'static [u8], Program) =
        (
            b"!-a",
            block(vec![expr_stmt(
                prefix(
                    Token::Bang,
                    prefix(Token::Minus, name("a"))
                )
            )])
        );

        static ref PRECEDENCE_03: (&'static [u8], Program) =
        (
            b"a + b + c",
            block(vec![expr_stmt(
                infix(
                    infix(
                        name("a"),
                        Token::Plus,
                        name("b"),
                    ),
                    Token::Plus,
                    name("c")
                )
            )])
        );

        static ref PRECEDENCE_04: (&'static [u8], Program) =
        (
            b"a + b - c",
            block(vec![expr_stmt(
                infix(
                    infix(
                        name("a"),
                        Token::Plus,
                        name("b"),
                    ),
                    Token::Minus,
                    name("c")
                )
            )])
        );

        static ref PRECEDENCE_05: (&'static [u8], Program) =
        (
            b"a * b * c",
            block(vec![expr_stmt(
                infix(
                    infix(
                        name("a"),
                        Token::Asterisk,
                        name("b"),
                    ),
                    Token::Asterisk,
                    name("c")
                )
            )])
        );

        static ref PRECEDENCE_06: (&'static [u8], Program) =
        (
            b"a * b / c",
            block(vec![expr_stmt(
                infix(
                    infix(
                        name("a"),
                        Token::Asterisk,
                        name("b"),
                    ),
                    Token::Slash,
                    name("c")
                )
            )])
        );

        static ref PRECEDENCE_07: (&'static [u8], Program) =
        (
            b"a + b / c",
            block(vec![expr_stmt(
                infix(
                    name("a"),
                    Token::Plus,
                    infix(
                        name("b"),
                        Token::Slash,
                        name("c"),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_08: (&'static [u8], Program) =
        (
            b"a + b * c + d / e - f",
            block(vec![expr_stmt(
                infix(
                    infix(
                        infix(
                            name("a"),
                            Token::Plus,
                            infix(
                                name("b"),
                                Token::Asterisk,
                                name("c"),
                            ),
                        ),
                        Token::Plus,
                        infix(
                            name("d"),
                            Token::Slash,
                            name("e"),
                        ),
                    ),
                    Token::Minus,
                    name("f")
                )
            )])
        );

        static ref PRECEDENCE_09: (&'static [u8], Program) =
        (
            b"3 + 4; -5 * 5",
            block(vec![
                stmt(
                    infix(
                        integer(3),
                        Token::Plus,
                        integer(4),
                    )
                ),
                expr_stmt(
                    infix(
                        prefix(Token::Minus, integer(5)),
                        Token::Asterisk,
                        integer(5),
                    )
                ),
            ])
        );

        static ref PRECEDENCE_10: (&'static [u8], Program) =
        (
            b"5 > 4 == 3 < 4",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(5),
                        Token::Gt,
                        integer(4),
                    ),
                    Token::Eq,
                    infix(
                        integer(3),
                        Token::Lt,
                        integer(4),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_11: (&'static [u8], Program) =
        (
            b"5 < 4 != 3 > 4",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(5),
                        Token::Lt,
                        integer(4),
                    ),
                    Token::Neq,
                    infix(
                        integer(3),
                        Token::Gt,
                        integer(4),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_12: (&'static [u8], Program) =
        (
            b"3 + 4 * 5 == 3 * 1 + 4 * 5",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(3),
                        Token::Plus,
                        infix(
                            integer(4),
                            Token::Asterisk,
                            integer(5),
                        ),
                    ),
                    Token::Eq,
                    infix(
                        infix(
                            integer(3),
                            Token::Asterisk,
                            integer(1),
                        ),
                        Token::Plus,
                        infix(
                            integer(4),
                            Token::Asterisk,
                            integer(5),
                        )
                    ),
                )
            )])
        );

        static ref PRECEDENCE_13: (&'static [u8], Program) =
        (
            b"3 > 5 == false",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(3),
                        Token::Gt,
                        integer(5),
                    ),
                    Token::Eq,
                    boolean(false),
                )
            )])
        );

        static ref PRECEDENCE_14: (&'static [u8], Program) =
        (
            b"3 < 5 == true",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(3),
                        Token::Lt,
                        integer(5),
                    ),
                    Token::Eq,
                    boolean(true),
                )
            )])
        );

        static ref PRECEDENCE_15: (&'static [u8], Program) =
        (
            b"true == true",
            block(vec![expr_stmt(
                infix(
                    boolean(true),
                    Token::Eq,
                    boolean(true)
                )
            )])
        );

        static ref PRECEDENCE_16: (&'static [u8], Program) =
        (
            b"true != false",
            block(vec![expr_stmt(
                infix(
                    boolean(true),
                    Token::Neq,
                    boolean(false)
                )
            )])
        );

        static ref PRECEDENCE_17: (&'static [u8], Program) =
        (
            b"false == false",
            block(vec![expr_stmt(
                infix(
                    boolean(false),
                    Token::Eq,
                    boolean(false)
                )
            )])
        );

        static ref PRECEDENCE_18: (&'static [u8], Program) =
        (
            b"1 + (2 + 3) + 4",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(1),
                        Token::Plus,
                        infix(
                            integer(2),
                            Token::Plus,
                            integer(3),
                        )
                    ),
                    Token::Plus,
                    integer(4),
                )
            )])
        );

        static ref PRECEDENCE_19: (&'static [u8], Program) =
        (
            b"(5 + 5) * 2",
            block(vec![expr_stmt(
                infix(
                    infix(
                        integer(5),
                        Token::Plus,
                        integer(5),
                    ),
                    Token::Asterisk,
                    integer(2),
                )
            )])
        );

        static ref PRECEDENCE_20: (&'static [u8], Program) =
        (
            b"2 / (5 + 5)",
            block(vec![expr_stmt(
                infix(
                    integer(2),
                    Token::Slash,
                    infix(
                        integer(5),
                        Token::Plus,
                        integer(5),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_21: (&'static [u8], Program) =
        (
            b"-(5 + 5)",
            block(vec![expr_stmt(
                prefix(
                    Token::Minus,
                    infix(
                        integer(5),
                        Token::Plus,
                        integer(5),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_22: (&'static [u8], Program) =
        (
            b"!(true == true)",
            block(vec![expr_stmt(
                prefix(
                    Token::Bang,
                    infix(
                        boolean(true),
                        Token::Eq,
                        boolean(true),
                    ),
                )
            )])
        );

        static ref PRECEDENCE_23: (&'static [u8], Program) =
        (
            b"a + add(b * c) + d",
            block(vec![expr_stmt(
                infix(
                    infix(
                        name("a"),
                        Token::Plus,
                        call(
                            name("add"),
                            vec![
                                infix(
                                    name("b"),
                                    Token::Asterisk,
                                    name("c"),
                                )
                            ]
                        )
                    ),
                    Token::Plus,
                    name("d")
                )
            )])
        );

        static ref PRECEDENCE_24: (&'static [u8], Program) =
        (
            b"add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            block(vec![expr_stmt(
                call(
                    name("add"),
                    vec![
                        name("a"),
                        name("b"),
                        integer(1),
                        infix(
                            integer(2),
                            Token::Asterisk,
                            integer(3),
                        ),
                        infix(
                            integer(4),
                            Token::Plus,
                            integer(5),
                        ),
                        call(
                            name("add"),
                            vec![
                                integer(6),
                                infix(
                                    integer(7),
                                    Token::Asterisk,
                                    integer(8),
                                )
                            ]
                        )
                    ]
                )
            )])
        );

        static ref PRECEDENCE_25: (&'static [u8], Program) =
        (
            b"add(a + b + c * d / f + g)",
            block(vec![expr_stmt(
                call(
                    name("add"),
                    vec![
                        infix(
                            infix(
                                infix(
                                    name("a"),
                                    Token::Plus,
                                    name("b"),
                                ),
                                Token::Plus,
                                infix(
                                    infix(
                                        name("c"),
                                        Token::Asterisk,
                                        name("d"),
                                    ),
                                    Token::Slash,
                                    name("f")
                                ),
                            ),
                            Token::Plus,
                            name("g")
                        )
                    ]
                )
            )])
        );

        // If Expression
        static ref IF_EXPR_01: (&'static [u8], Program) =
        (
            b"if (x < y) { x }",
            block(vec![
                expr_stmt(
                    if_expr(
                        infix(name("x"), Token::Lt, name("y")),
                        block(vec![expr_stmt(name("x"))]),
                        None
                    )
                )
            ])
        );

        static ref IF_EXPR_02: (&'static [u8], Program) =
        (
            b"if (x < y) { x } else { y }",
            block(vec![
                expr_stmt(
                    if_expr(
                        infix(name("x"), Token::Lt, name("y")),
                        block(vec![expr_stmt(name("x"))]),
                        Some(block(vec![expr_stmt(name("y"))])),
                    )
                )
            ])
        );

        // Call Expression
        static ref CALL_EXPR_01: (&'static [u8], Program) =
        (
            b"add()",
            block(vec![
                expr_stmt(
                    call(
                        name("add"),
                        vec![],
                    )
                ),
            ])
        );

        static ref CALL_EXPR_02: (&'static [u8], Program) =
        (
            b"add(1)",
            block(vec![
                expr_stmt(
                    call(
                        name("add"),
                        vec![
                            integer(1),
                        ]
                    )
                ),
            ])
        );

        static ref CALL_EXPR_03: (&'static [u8], Program) =
        (
            b"add(1, 2 * 3)",
            block(vec![
                expr_stmt(
                    call(
                        name("add"),
                        vec![
                            integer(1),
                            infix(
                                integer(2),
                                Token::Asterisk,
                                integer(3),
                            ),
                        ]
                    )
                ),
            ])
        );

        static ref CALL_EXPR_04: (&'static [u8], Program) =
        (
            b"add(1, 2 * 3, 4 + 5)",
            block(vec![
                expr_stmt(
                    call(
                        name("add"),
                        vec![
                            integer(1),
                            infix(
                                integer(2),
                                Token::Asterisk,
                                integer(3),
                            ),
                            infix(
                                integer(4),
                                Token::Plus,
                                integer(5),
                            ),
                        ]
                    )
                ),
            ])
        );
    }

    #[test_case(LET_STMT.0     , &LET_STMT.1      ; "let stmt"       )]
    #[test_case(RETURN_STMT.0  , &RETURN_STMT.1   ; "return stmt"    )]
    #[test_case(LITERAL_01.0   , &LITERAL_01.1    ; "literal ident"  )]
    #[test_case(LITERAL_02.0   , &LITERAL_02.1    ; "literal integer")]
    #[test_case(LITERAL_03.0   , &LITERAL_03.1    ; "literal true"   )]
    #[test_case(LITERAL_04.0   , &LITERAL_04.1    ; "literal false"  )]
    #[test_case(LITERAL_05.0   , &LITERAL_05.1    ; "literal fn 0"   )]
    #[test_case(LITERAL_06.0   , &LITERAL_06.1    ; "literal fn 1"   )]
    #[test_case(LITERAL_07.0   , &LITERAL_07.1    ; "literal fn 2"   )]
    #[test_case(LITERAL_08.0   , &LITERAL_08.1    ; "literal fn 3"   )]
    #[test_case(PREFIX_01.0    , &PREFIX_01.1     ; "prefix bang"    )]
    #[test_case(PREFIX_02.0    , &PREFIX_02.1     ; "prefix minus"   )]
    #[test_case(INFIX_01.0     , &INFIX_01.1      ; "infix plus"     )]
    #[test_case(INFIX_02.0     , &INFIX_02.1      ; "infix minus"    )]
    #[test_case(INFIX_03.0     , &INFIX_03.1      ; "infix asterisk" )]
    #[test_case(INFIX_04.0     , &INFIX_04.1      ; "infix slash"    )]
    #[test_case(INFIX_05.0     , &INFIX_05.1      ; "infix gt"       )]
    #[test_case(INFIX_06.0     , &INFIX_06.1      ; "infix lt"       )]
    #[test_case(INFIX_07.0     , &INFIX_07.1      ; "infix eq"       )]
    #[test_case(INFIX_08.0     , &INFIX_08.1      ; "infix neq"      )]
    #[test_case(PRECEDENCE_01.0, &PRECEDENCE_01.1 ; "precedence 01"  )]
    #[test_case(PRECEDENCE_02.0, &PRECEDENCE_02.1 ; "precedence 02"  )]
    #[test_case(PRECEDENCE_03.0, &PRECEDENCE_03.1 ; "precedence 03"  )]
    #[test_case(PRECEDENCE_04.0, &PRECEDENCE_04.1 ; "precedence 04"  )]
    #[test_case(PRECEDENCE_05.0, &PRECEDENCE_05.1 ; "precedence 05"  )]
    #[test_case(PRECEDENCE_06.0, &PRECEDENCE_06.1 ; "precedence 06"  )]
    #[test_case(PRECEDENCE_07.0, &PRECEDENCE_07.1 ; "precedence 07"  )]
    #[test_case(PRECEDENCE_08.0, &PRECEDENCE_08.1 ; "precedence 08"  )]
    #[test_case(PRECEDENCE_09.0, &PRECEDENCE_09.1 ; "precedence 09"  )]
    #[test_case(PRECEDENCE_10.0, &PRECEDENCE_10.1 ; "precedence 10"  )]
    #[test_case(PRECEDENCE_11.0, &PRECEDENCE_11.1 ; "precedence 11"  )]
    #[test_case(PRECEDENCE_12.0, &PRECEDENCE_12.1 ; "precedence 12"  )]
    #[test_case(PRECEDENCE_13.0, &PRECEDENCE_13.1 ; "precedence 13"  )]
    #[test_case(PRECEDENCE_14.0, &PRECEDENCE_14.1 ; "precedence 14"  )]
    #[test_case(PRECEDENCE_15.0, &PRECEDENCE_15.1 ; "precedence 15"  )]
    #[test_case(PRECEDENCE_16.0, &PRECEDENCE_16.1 ; "precedence 16"  )]
    #[test_case(PRECEDENCE_17.0, &PRECEDENCE_17.1 ; "precedence 17"  )]
    #[test_case(PRECEDENCE_18.0, &PRECEDENCE_18.1 ; "precedence 18"  )]
    #[test_case(PRECEDENCE_19.0, &PRECEDENCE_19.1 ; "precedence 19"  )]
    #[test_case(PRECEDENCE_20.0, &PRECEDENCE_20.1 ; "precedence 20"  )]
    #[test_case(PRECEDENCE_21.0, &PRECEDENCE_21.1 ; "precedence 21"  )]
    #[test_case(PRECEDENCE_22.0, &PRECEDENCE_22.1 ; "precedence 22"  )]
    #[test_case(PRECEDENCE_23.0, &PRECEDENCE_23.1 ; "precedence 23"  )]
    #[test_case(PRECEDENCE_24.0, &PRECEDENCE_24.1 ; "precedence 24"  )]
    #[test_case(PRECEDENCE_25.0, &PRECEDENCE_25.1 ; "precedence 25"  )]
    #[test_case(IF_EXPR_01.0   , &IF_EXPR_01.1    ; "if expr 01"     )]
    #[test_case(IF_EXPR_02.0   , &IF_EXPR_02.1    ; "if expr 02"     )]
    #[test_case(CALL_EXPR_01.0 , &CALL_EXPR_01.1  ; "call expr 01"   )]
    #[test_case(CALL_EXPR_02.0 , &CALL_EXPR_02.1  ; "call expr 02"   )]
    #[test_case(CALL_EXPR_03.0 , &CALL_EXPR_03.1  ; "call expr 03"   )]
    #[test_case(CALL_EXPR_04.0 , &CALL_EXPR_04.1  ; "call expr 04"   )]
    fn test(input: &[u8], expected: &Program) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let output = parser.parse().unwrap();

        assert_eq!(&output, expected)
    }
}
