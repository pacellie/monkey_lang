use crate::error::{MonkeyError, Result};
use crate::lexer::{Lexer, Token};
use crate::parser::ast::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    Lowest,
    Equals,      // ==
    LessGreater, // >, <
    Sum,         // +
    Product,     // *
    Prefix,      // -, !
    Call,        // f()
    Index,       // a[]
}

impl Precedence {
    pub fn precedence(token: &Token) -> Precedence {
        match token {
            Token::Eq | Token::Neq => Precedence::Equals,
            Token::Lt | Token::Gt => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LParen => Precedence::Call,
            Token::LBracket => Precedence::Index,
            _ => Precedence::Lowest,
        }
    }
}

/*
   Invariant:
   (1) A parsing function is only called if `current` contains a valid
     first token according to the associated grammar rule.

   (2) If a parsing function returns Ok(...), the token `current` contains
     the first token `after` the associated grammer rule.

   E.g. parse_let_stmt: let <name> = <expr> ;
   (1) parse_let_stmt will only be called if `current` contains the `Token::Let`
   (2) if parse_let_stmt returns successfully `current` contains the first token
       after `Token::Semicolon`
*/
#[derive(Debug)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    current: Token,
    peek: Token, // TODO: peek not necessary?
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

    fn advance_if(&mut self, token: &Token) -> Result<()> {
        if &self.current == token {
            self.advance();
            Ok(())
        } else {
            Err(MonkeyError::parser_error(
                token.to_string(),
                self.current.to_string(),
            ))
        }
    }

    // <stmt>*
    pub fn parse(&mut self) -> std::result::Result<Program, Vec<MonkeyError>> {
        let mut stmts = vec![];
        let mut errors = vec![];

        while self.current != Token::Eof {
            self.parse_stmt().map_or_else(
                |err| {
                    self.advance();
                    errors.push(err);
                },
                |stmt| {
                    stmts.push(stmt);
                },
            );
        }

        if errors.len() == 0 {
            Ok(block(stmts))
        } else {
            Err(errors)
        }
    }

    // <let_stmt> | <return_stmt> | <stmt> | <expr>
    fn parse_stmt(&mut self) -> Result<Statement> {
        match self.current {
            Token::Let => self.parse_let_stmt(),
            Token::Return => self.parse_return_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    // <let><name><=><expr><;>
    fn parse_let_stmt(&mut self) -> Result<Statement> {
        self.advance(); // advance over `Token::Let`

        let name = self.parse_name()?;

        self.advance_if(&Token::Assign)?; // advance over `Token::Assign`

        let mut expr = self.parse_expr(Precedence::Lowest)?;

        if let Expression::Function { params, body, .. } = expr {
            expr = function(&name, params, body);
        }

        self.advance_if(&Token::Semicolon)?; // advance over `Token::Semicolon`

        Ok(let_stmt(name, expr))
    }

    // <return><epxr><;>
    fn parse_return_stmt(&mut self) -> Result<Statement> {
        self.advance(); // advance over `Token::Return`

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.advance_if(&Token::Semicolon)?; // advance over `Token::Semicolon`

        Ok(return_stmt(expr))
    }

    // <stmt>*
    fn parse_block_stmt(&mut self) -> Result<Block> {
        self.parse_many(|parser| parser.parse_stmt())
            .map(|stmts| block(stmts))
    }

    // <expr><;> | <expr>
    fn parse_expr_stmt(&mut self) -> Result<Statement> {
        let expr = self.parse_expr(Precedence::Lowest)?;

        if self.current == Token::Semicolon {
            self.advance(); // advance over `Token::Semicolon`
            Ok(stmt(expr))
        } else {
            Ok(expr_stmt(expr))
        }
    }

    // <name>
    fn parse_name(&mut self) -> Result<String> {
        match self.current.clone() {
            Token::Ident(name) => {
                self.advance(); // advance over `Token::Ident(...)`
                Ok(name)
            }
            _ => {
                return Err(MonkeyError::parser_error("name", self.current.to_string()));
            }
        }
    }

    // <expr>
    fn parse_expr(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut expr = match self.current.clone() {
            Token::Ident(s) => {
                self.advance(); // advance over `Token::Ident(...)`
                name(s)
            }
            Token::Int(n) => {
                self.advance(); // advance over `Token::Int(...)`
                n.parse::<i32>().map(|n| integer(n))?
            }
            Token::True => {
                self.advance(); // advance over `Token::True`
                boolean(true)
            }
            Token::False => {
                self.advance(); // advance over `Token::False`
                boolean(false)
            }
            Token::String(s) => {
                self.advance(); // advance over `Token::String(...)`
                string(s)
            }
            Token::Function => self.parse_function_expr()?,
            Token::LParen => self.parse_group_expr()?,
            Token::LBracket => self.parse_array_expr()?,
            Token::LBrace => self.parse_map_expr()?,
            Token::Bang | Token::Minus => self.parse_prefix_expr()?,
            Token::If => self.parse_if_expr()?,
            _ => {
                return Err(MonkeyError::parser_error(
                    "start of an expression",
                    self.current.to_string(),
                ));
            }
        };

        while self.current != Token::Semicolon && precedence < Precedence::precedence(&self.current)
        {
            expr = match &self.current {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Eq
                | Token::Neq
                | Token::Lt
                | Token::Gt => self.parse_infix_expr(expr)?,
                Token::LParen => self.parse_call_expr(expr)?,
                Token::LBracket => self.parse_index_expr(expr)?,
                _ => expr,
            }
        }

        Ok(expr)
    }

    // <un_op><expr>
    fn parse_prefix_expr(&mut self) -> Result<Expression> {
        let operator = self.current.clone();
        self.advance(); // advance over `operator`

        let right = self.parse_expr(Precedence::Prefix)?;

        Ok(prefix(operator, right))
    }

    // <bin_op><expr>
    fn parse_infix_expr(&mut self, left: Expression) -> Result<Expression> {
        let operator = self.current.clone();
        self.advance(); // advance over `operator`

        let precedence = Precedence::precedence(&operator);

        let right = self.parse_expr(precedence)?;

        Ok(infix(left, operator, right))
    }

    // <(><expr><)>
    fn parse_group_expr(&mut self) -> Result<Expression> {
        self.advance(); // advance over `Token::LParen`

        let expr = self.parse_expr(Precedence::Lowest)?;

        self.advance_if(&Token::RParen)?; // advance over `Token::RParen`

        Ok(expr)
    }

    // <[><expr>*<]>
    fn parse_array_expr(&mut self) -> Result<Expression> {
        self.advance(); // advance over `Token::LBracket`

        let exprs = self.parse_exprs()?;

        self.advance_if(&Token::RBracket)?; // advance over `Token::RBracket`

        Ok(array(exprs))
    }

    // <[><expr><]>
    fn parse_index_expr(&mut self, expr: Expression) -> Result<Expression> {
        self.advance(); // advance over `Token::LBracket`

        let idx = self.parse_expr(Precedence::Lowest)?;

        self.advance_if(&Token::RBracket)?; // advance over `Token::RBracket`

        Ok(index(expr, idx))
    }

    // <{>(<expr><:><expr>(<,><expr><:><expr>)*)*<}>
    fn parse_map_expr(&mut self) -> Result<Expression> {
        self.advance(); // advance over `Token::LBrace`

        let pairs = self.parse_many_sep_by(|parser| parser.parse_key_value_pair(), Token::Comma)?;

        self.advance_if(&Token::RBrace)?; // advance over `Token::RBrace`

        Ok(map(pairs))
    }

    // <expr><:><expr>
    fn parse_key_value_pair(&mut self) -> Result<(Expression, Expression)> {
        let key = self.parse_expr(Precedence::Lowest)?;

        self.advance_if(&Token::Colon)?; // advance over `Token::Colon`

        let value = self.parse_expr(Precedence::Lowest)?;

        Ok((key, value))
    }

    // <if><(><expr><)><{><block><}><else><{><block><}> | <if><(><expr><)><{><block><}>
    fn parse_if_expr(&mut self) -> Result<Expression> {
        self.advance(); // advance over `Token::If`

        let cond = self.between(
            &Token::LParen,
            |parser| parser.parse_expr(Precedence::Lowest),
            &Token::RParen,
        )?;

        let yes = self.between(
            &Token::LBrace,
            |parser| parser.parse_block_stmt(),
            &Token::RBrace,
        )?;

        if self.current == Token::Else {
            self.advance(); // advance over `Token::Else`

            let no = self.between(
                &Token::LBrace,
                |parser| parser.parse_block_stmt(),
                &Token::RBrace,
            )?;

            Ok(if_expr(cond, yes, Some(no)))
        } else {
            Ok(if_expr(cond, yes, None))
        }
    }

    // <fn><(><params><)><{><block><}>
    fn parse_function_expr(&mut self) -> Result<Expression> {
        self.advance(); // advance over `Token::Function`

        let params = self.between(
            &Token::LParen,
            |parser| parser.parse_names(),
            &Token::RParen,
        )?;

        let body = self.between(
            &Token::LBrace,
            |parser| parser.parse_block_stmt(),
            &Token::RBrace,
        )?;

        Ok(function("anonymous", params, body))
    }

    // (<param>(<,><param>)*)*
    fn parse_names(&mut self) -> Result<Vec<String>> {
        self.parse_many_sep_by(|parser| parser.parse_name(), Token::Comma)
    }

    // <(><args><)>
    fn parse_call_expr(&mut self, expr: Expression) -> Result<Expression> {
        self.advance(); // advance over `Token::LParen`
        let args = self.parse_exprs()?;
        self.advance_if(&Token::RParen)?; // advance over `Token::RParen`

        Ok(call(expr, args))
    }

    // (<expr>(<,><expr>)*)*
    fn parse_exprs(&mut self) -> Result<Vec<Expression>> {
        self.parse_many_sep_by(|parser| parser.parse_expr(Precedence::Lowest), Token::Comma)
    }

    // <start><f><end>
    fn between<A, F>(&mut self, start: &Token, f: F, end: &Token) -> Result<A>
    where
        F: Fn(&mut Parser) -> Result<A>,
    {
        self.advance_if(&start)?; // advance over `start`

        let value = f(self)?;

        self.advance_if(&end)?; // advance over `end`

        Ok(value)
    }

    // <f>*
    fn parse_many<A, F>(&mut self, f: F) -> Result<Vec<A>>
    where
        F: Fn(&mut Parser) -> Result<A>,
    {
        let mut xs = vec![];

        while let Ok(x) = f(self) {
            xs.push(x);
        }

        Ok(xs)
    }

    // (<f>(<sep><f>)*)*
    fn parse_many_sep_by<A, F>(&mut self, f: F, sep: Token) -> Result<Vec<A>>
    where
        F: Fn(&mut Parser) -> Result<A>,
    {
        let mut xs = vec![];

        // try to parse the first element, return `vec![]` is this fails
        if let Ok(x) = f(self) {
            xs.push(x);
        } else {
            return Ok(xs);
        }

        // as long as there is a valid separator continue parsing elements
        while let Ok(_) = self.advance_if(&sep) {
            xs.push(f(self)?);
        }

        Ok(xs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use test_case::test_case;

    #[test_case(
        b"let x = 5;",
        block(vec![
            let_stmt(
                "x",
                integer(5)
            )
        ]) ;
        "let stmt 01"
    )]
    #[test_case(
        b"return 5;",
        block(vec![
            return_stmt(integer(5))
        ]) ;
        "return stmt 01"
    )]
    #[test_case(
        b"foobar",
        block(vec![expr_stmt(name("foobar"))]) ;
        "name literal"
    )]
    #[test_case(
        b"5",
        block(vec![expr_stmt(integer(5))]) ;
        "integer literal"
    )]
    #[test_case(
        b"true",
        block(vec![expr_stmt(boolean(true))]) ;
        "true literal"
    )]
    #[test_case(
        b"false",
        block(vec![expr_stmt(boolean(false))]) ;
        "false literal"
    )]
    #[test_case(
        b"fn() {}",
        block(vec![expr_stmt(
            function::<String, &str>(
                "anonymous",
                vec![],
                block(vec![])
            )
        )]) ;
        "fn literal 01"
    )]
    #[test_case(
        b"fn(x) {}",
        block(vec![expr_stmt(
            function(
                "anonymous",
                vec![
                    "x",
                ],
                block(vec![])
            )
        )]) ;
        "fn literal 02"
    )]
    #[test_case(
        b"fn(x, y) { x + y; }",
        block(vec![expr_stmt(
            function(
                "anonymous",
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
        )]) ;
        "fn literal 03"
    )]
    #[test_case(
        b"fn(x, y, z) {}",
        block(vec![expr_stmt(
            function(
                "anonymous",
                vec![
                    "x",
                    "y",
                    "z",
                ],
                block(vec![])
            )
        )]) ;
        "fn literal 04"
    )]
    #[test_case(
        b"\"hello world\"",
        block(vec![expr_stmt(string("hello world".to_string()))]) ;
        "string literal"
    )]
    #[test_case(
        b"[1, 2 * 2, 3 + 3]",
        block(vec![expr_stmt(
            array(vec![
                integer(1),
                infix(
                    integer(2),
                    Token::Asterisk,
                    integer(2),
                ),
                infix(
                    integer(3),
                    Token::Plus,
                    integer(3),
                ),
            ])
        )]) ;
        "array literal"
    )]
    #[test_case(
        b"{}",
        block(vec![expr_stmt(
            map(vec![])
        )]) ;
        "map literal 01"
    )]
    #[test_case(
        b"{\"one\": 1, \"two\": 2, \"three\": 3}",
        block(vec![expr_stmt(
            map(vec![
                (string("one"), integer(1)),
                (string("two"), integer(2)),
                (string("three"), integer(3)),
            ])
        )]) ;
        "map literal 02"
    )]
    #[test_case(
        b"{true: 0 + 1, 1: 10 - 8, false: 15 / 5}",
        block(vec![expr_stmt(
            map(vec![
                (
                    boolean(true),
                    infix(
                        integer(0),
                        Token::Plus,
                        integer(1),
                    )
                ),
                (
                    integer(1),
                    infix(
                        integer(10),
                        Token::Minus,
                        integer(8),
                    )
                ),
                (
                    boolean(false),
                    infix(
                        integer(15),
                        Token::Slash,
                        integer(5),
                    )
                ),
            ])
        )]) ;
        "map literal 03"
    )]
    #[test_case(
        b"!true",
        block(vec![expr_stmt(prefix(Token::Bang, boolean(true)))]) ;
        "prefix 01"
    )]
    #[test_case(
        b"-15",
        block(vec![expr_stmt(prefix(Token::Minus, integer(15)))]) ;
        "prefix 02"
    )]
    #[test_case(
        b"5 + 5",
        block(vec![expr_stmt(infix(integer(5), Token::Plus, integer(5)))]) ;
        "infix 01"
    )]
    #[test_case(
        b"5 - 5",
        block(vec![expr_stmt(infix(integer(5), Token::Minus, integer(5)))]) ;
        "infix 02"
    )]
    #[test_case(
        b"5 * 5",
        block(vec![expr_stmt(infix(integer(5), Token::Asterisk, integer(5)))]) ;
        "infix 03"
    )]
    #[test_case(
        b"5 / 5",
        block(vec![expr_stmt(infix(integer(5), Token::Slash, integer(5)))]) ;
        "infix 04"
    )]
    #[test_case(
        b"5 > 5",
        block(vec![expr_stmt(infix(integer(5), Token::Gt, integer(5)))]) ;
        "infix 05"
    )]
    #[test_case(
        b"5 < 5",
        block(vec![expr_stmt(infix(integer(5), Token::Lt, integer(5)))]) ;
        "infix 06"
    )]
    #[test_case(
        b"5 == 5",
        block(vec![expr_stmt(infix(integer(5), Token::Eq, integer(5)))]) ;
        "infix 07"
    )]
    #[test_case(
        b"5 != 5",
        block(vec![expr_stmt(infix(integer(5), Token::Neq, integer(5)))]) ;
        "infix 08"
    )]
    #[test_case(
        b"-a * b",
        block(vec![expr_stmt(
            infix(
                prefix(Token::Minus, name("a")),
                Token::Asterisk,
                name("b")
            )
        )]) ;
        "precedence 01"
    )]
    #[test_case(
        b"!-a",
        block(vec![expr_stmt(
            prefix(
                Token::Bang,
                prefix(Token::Minus, name("a"))
            )
        )]) ;
        "precedence 02"
    )]
    #[test_case(
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
        )]) ;
        "precedence 03"
    )]
    #[test_case(
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
        )]) ;
        "precedence 04"
    )]
    #[test_case(
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
        )]) ;
        "precedence 05"
    )]
    #[test_case(
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
        )]) ;
        "precedence 06"
    )]
    #[test_case(
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
        )]) ;
        "precedence 07"
    )]
    #[test_case(
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
        )]) ;
        "precedence 08"
    )]
    #[test_case(
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
        ]) ;
        "precedence 09"
    )]
    #[test_case(
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
        )]) ;
        "precedence 10"
    )]
    #[test_case(
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
        )]) ;
        "precedence 11"
    )]
    #[test_case(
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
        )]) ;
        "precedence 12"
    )]
    #[test_case(
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
        )]) ;
        "precedence 13"
    )]
    #[test_case(
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
        )]) ;
        "precedence 14"
    )]
    #[test_case(
        b"true == true",
        block(vec![expr_stmt(
            infix(
                boolean(true),
                Token::Eq,
                boolean(true)
            )
        )]) ;
        "precedence 15"
    )]
    #[test_case(
        b"true != false",
        block(vec![expr_stmt(
            infix(
                boolean(true),
                Token::Neq,
                boolean(false)
            )
        )]) ;
        "precedence 16"
    )]
    #[test_case(
        b"false == false",
        block(vec![expr_stmt(
            infix(
                boolean(false),
                Token::Eq,
                boolean(false)
            )
        )]) ;
        "precedence 17"
    )]
    #[test_case(
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
        )]) ;
        "precedence 18"
    )]
    #[test_case(
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
        )]) ;
        "precedence 19"
    )]
    #[test_case(
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
        )]) ;
        "precedence 20"
    )]
    #[test_case(
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
        )]) ;
        "precedence 21"
    )]
    #[test_case(
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
        )]) ;
        "precedence 22"
    )]
    #[test_case(
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
        )]) ;
        "precedence 23"
    )]
    #[test_case(
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
        )]) ;
        "precedence 24"
    )]
    #[test_case(
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
        )]) ;
        "precedence 25"
    )]
    #[test_case(
        b"a * [1, 2, 3, 4][b * c] * d",
        block(vec![expr_stmt(
            infix(
                infix(
                    name("a"),
                    Token::Asterisk,
                    index(
                        array(vec![
                            integer(1),
                            integer(2),
                            integer(3),
                            integer(4),
                        ]),
                        infix(
                            name("b"),
                            Token::Asterisk,
                            name("c"),
                        )
                    )
                ),
                Token::Asterisk,
                name("d"),
            )
        )]) ;
        "precedence 26"
    )]
    #[test_case(
        b"add(a * b[2], b[1], 2 * [1, 2][1])",
        block(vec![expr_stmt(
            call(
                name("add"),
                vec![
                    infix(
                        name("a"),
                        Token::Asterisk,
                        index(
                            name("b"),
                            integer(2),
                        ),
                    ),
                    index(
                        name("b"),
                        integer(1),
                    ),
                    infix(
                        integer(2),
                        Token::Asterisk,
                        index(
                            array(vec![
                                integer(1),
                                integer(2),
                            ]),
                            integer(1),
                        ),
                    )
                ]
            )
        )]) ;
        "precedence 27"
    )]
    #[test_case(
        b"if (x < y) { x }",
        block(vec![
            expr_stmt(
                if_expr(
                    infix(name("x"), Token::Lt, name("y")),
                    block(vec![expr_stmt(name("x"))]),
                    None
                )
            )
        ]) ;
        "if expr 01"
    )]
    #[test_case(
        b"if (x < y) { x } else { y }",
        block(vec![
            expr_stmt(
                if_expr(
                    infix(name("x"), Token::Lt, name("y")),
                    block(vec![expr_stmt(name("x"))]),
                    Some(block(vec![expr_stmt(name("y"))])),
                )
            )
        ]) ;
        "if expr 02"
    )]
    #[test_case(
        b"add()",
        block(vec![
            expr_stmt(
                call(
                    name("add"),
                    vec![],
                )
            ),
        ]) ;
        "call expr 01"
    )]
    #[test_case(
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
        ]) ;
        "call expr 02"
    )]
    #[test_case(
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
        ]) ;
        "call expr 03"
    )]
    #[test_case(
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
        ]) ;
        "call expr 04"
    )]
    #[test_case(
        b"myArray[1 + 1]",
        block(vec![
            expr_stmt(
                index(
                    name("myArray"),
                    infix(
                        integer(1),
                        Token::Plus,
                        integer(1),
                    )
                )
            ),
        ]) ;
        "index expr 01"
    )]
    fn test(input: &[u8], expected: Program) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();

        assert_eq!(ast, expected)
    }
}
