use crate::interpreter::object::Object;
use crate::lexer::Token;
use crate::parser::ast::*;

use std::fmt;

type Result<T> = std::result::Result<T, RuntimeError>;

#[derive(Debug, PartialEq, Eq)]
pub struct RuntimeError(String);

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub fn eval(ast: &Program) -> Result<Object> {
    match eval_block(ast)? {
        Object::Return(obj) => {
            return Ok(*obj);
        }
        obj => Ok(obj),
    }
}

pub fn eval_block(block: &Block) -> Result<Object> {
    let mut obj = Object::Unit;

    for stmt in &block.0 {
        obj = match eval_stmt(stmt)? {
            Object::Return(obj) => {
                return Ok(Object::Return(obj));
            }
            obj => obj,
        }
    }

    Ok(obj)
}

fn eval_stmt(stmt: &Statement) -> Result<Object> {
    match stmt {
        Statement::Return(expr) => {
            let obj = eval_expr(expr)?;
            Ok(Object::Return(Box::new(obj)))
        }
        Statement::Stmt(expr) => {
            eval_expr(expr)?;
            Ok(Object::Unit)
        }
        Statement::Expr(expr) => eval_expr(expr),
        _ => panic!(),
    }
}

fn eval_expr(expr: &Expression) -> Result<Object> {
    match expr {
        Expression::Integer(n) => Ok(Object::Integer(*n)),
        Expression::Boolean(b) => Ok(Object::Boolean(*b)),
        Expression::Prefix { operator, expr } => eval_prefix_expr(operator, expr),
        Expression::Infix {
            left,
            operator,
            right,
        } => eval_infix_expr(left, operator, right),
        Expression::If { cond, yes, no } => eval_if_expr(cond, yes, no),
        _ => panic!(),
    }
}

fn eval_prefix_expr(operator: &Token, expr: &Expression) -> Result<Object> {
    let object = eval_expr(expr)?;
    match (operator, object) {
        (Token::Bang, Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        (Token::Minus, Object::Integer(n)) => Ok(Object::Integer(-n)),
        (operator, object) => Err(RuntimeError(format!(
            "type mismatch: `{}{}`",
            operator, object
        ))),
    }
}

fn eval_infix_expr(left: &Expression, operator: &Token, right: &Expression) -> Result<Object> {
    let left = eval_expr(left)?;
    let right = eval_expr(right)?;

    #[rustfmt::skip]
    let object = match (left, operator, right) {
        (Object::Integer(n), Token::Plus    , Object::Integer(m)) => Object::Integer(n + m),
        (Object::Integer(n), Token::Minus   , Object::Integer(m)) => Object::Integer(n - m),
        (Object::Integer(n), Token::Asterisk, Object::Integer(m)) => Object::Integer(n * m),
        (Object::Integer(n), Token::Slash   , Object::Integer(m)) => Object::Integer(n / m),
        (Object::Integer(n), Token::Eq      , Object::Integer(m)) => Object::Boolean(n == m),
        (Object::Integer(n), Token::Neq     , Object::Integer(m)) => Object::Boolean(n != m),
        (Object::Integer(n), Token::Gt      , Object::Integer(m)) => Object::Boolean(n > m),
        (Object::Integer(n), Token::Lt      , Object::Integer(m)) => Object::Boolean(n < m),

        (Object::Boolean(b), Token::Eq , Object::Boolean(p)) => Object::Boolean(b == p),
        (Object::Boolean(b), Token::Neq, Object::Boolean(p)) => Object::Boolean(b != p),

        (left, operator, right) => {
            return Err(RuntimeError(format!("type mismatch: `{} {} {}`", left, operator, right)));
        }
    };

    Ok(object)
}

fn eval_if_expr(cond: &Expression, yes: &Block, no: &Option<Block>) -> Result<Object> {
    let cond = eval_expr(cond)?;
    if let Object::Boolean(cond) = cond {
        if cond {
            eval_block(yes)
        } else {
            if let Some(no) = no {
                eval_block(no)
            } else {
                Ok(Object::Unit)
            }
        }
    } else {
        Err(RuntimeError(format!(
            "type mismatch: `if ({}) {{...}}`",
            cond
        )))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use lazy_static::lazy_static;
    use test_case::test_case;

    #[rustfmt::skip]
    lazy_static! {
        // Literal
        static ref LITERAL_01: (&'static [u8], Object) =
        (
            b"5",
            Object::Integer(5)
        );

        static ref LITERAL_02: (&'static [u8], Object) =
        (
            b"true",
            Object::Boolean(true)
        );

        static ref LITERAL_03: (&'static [u8], Object) =
        (
            b"false",
            Object::Boolean(false)
        );

        // Prefix Expression
        static ref PREFIX_01: (&'static [u8], Object) =
        (
            b"!true",
            Object::Boolean(false)
        );

        static ref PREFIX_02: (&'static [u8], Object) =
        (
            b"!false",
            Object::Boolean(true)
        );

        static ref PREFIX_03: (&'static [u8], Object) =
        (
            b"!!true",
            Object::Boolean(true)
        );

        static ref PREFIX_04: (&'static [u8], Object) =
        (
            b"!!false",
            Object::Boolean(false)
        );

        static ref PREFIX_05: (&'static [u8], Object) =
        (
            b"-5",
            Object::Integer(-5)
        );

        static ref PREFIX_06: (&'static [u8], Object) =
        (
            b"--5",
            Object::Integer(5)
        );

        // Infix Expression
        static ref INFIX_01: (&'static [u8], Object) =
        (
            b"5 + 5 + 5 + 5 - 10",
            Object::Integer(10)
        );

        static ref INFIX_02: (&'static [u8], Object) =
        (
            b"2 * 2 * 2 * 2 * 2",
            Object::Integer(32)
        );

        static ref INFIX_03: (&'static [u8], Object) =
        (
            b"-50 + 100 + -50",
            Object::Integer(0)
        );

        static ref INFIX_04: (&'static [u8], Object) =
        (
            b"5 * 2 + 10",
            Object::Integer(20)
        );

        static ref INFIX_05: (&'static [u8], Object) =
        (
            b"5 + 2 * 10",
            Object::Integer(25)
        );

        static ref INFIX_06: (&'static [u8], Object) =
        (
            b"20 + 2 * -10",
            Object::Integer(0)
        );

        static ref INFIX_07: (&'static [u8], Object) =
        (
            b"50 / 2 * 2 + 10",
            Object::Integer(60)
        );

        static ref INFIX_08: (&'static [u8], Object) =
        (
            b"2 * (5 + 10)",
            Object::Integer(30)
        );

        static ref INFIX_09: (&'static [u8], Object) =
        (
            b"3 * 3 * 3 + 10",
            Object::Integer(37)
        );

        static ref INFIX_10: (&'static [u8], Object) =
        (
            b"3 * (3 * 3) + 10",
            Object::Integer(37)
        );

        static ref INFIX_11: (&'static [u8], Object) =
        (
            b"(5 + 10 * 2 + 15 / 3) * 2 + -10",
            Object::Integer(50)
        );

        static ref INFIX_12: (&'static [u8], Object) =
        (
            b"1 < 2",
            Object::Boolean(true)
        );

        static ref INFIX_13: (&'static [u8], Object) =
        (
            b"1 > 2",
            Object::Boolean(false)
        );

        static ref INFIX_14: (&'static [u8], Object) =
        (
            b"1 < 1",
            Object::Boolean(false)
        );

        static ref INFIX_15: (&'static [u8], Object) =
        (
            b"1 > 1",
            Object::Boolean(false)
        );

        static ref INFIX_16: (&'static [u8], Object) =
        (
            b"1 == 1",
            Object::Boolean(true)
        );

        static ref INFIX_17: (&'static [u8], Object) =
        (
            b"1 != 1",
            Object::Boolean(false)
        );

        static ref INFIX_18: (&'static [u8], Object) =
        (
            b"1 == 2",
            Object::Boolean(false)
        );

        static ref INFIX_19: (&'static [u8], Object) =
        (
            b"1 != 2",
            Object::Boolean(true)
        );

        static ref INFIX_20: (&'static [u8], Object) =
        (
            b"true == true",
            Object::Boolean(true)
        );

        static ref INFIX_21: (&'static [u8], Object) =
        (
            b"false == true",
            Object::Boolean(false)
        );

        static ref INFIX_22: (&'static [u8], Object) =
        (
            b"true == false",
            Object::Boolean(false)
        );

        static ref INFIX_23: (&'static [u8], Object) =
        (
            b"true != false",
            Object::Boolean(true)
        );

        static ref INFIX_24: (&'static [u8], Object) =
        (
            b"false != true",
            Object::Boolean(true)
        );

        // If Expr
        static ref IF_EXPR_01: (&'static [u8], Object) =
        (
            b"if (true) { 10 }",
            Object::Integer(10)
        );

        static ref IF_EXPR_02: (&'static [u8], Object) =
        (
            b"if (false) { 10 }",
            Object::Unit
        );

        static ref IF_EXPR_03: (&'static [u8], Object) =
        (
            b"if (1 < 2) { 10 }",
            Object::Integer(10)
        );

        static ref IF_EXPR_04: (&'static [u8], Object) =
        (
            b"if (1 > 2) { 10 }",
            Object::Unit
        );

        static ref IF_EXPR_05: (&'static [u8], Object) =
        (
            b"if (1 > 2) { 10 } else { 20 }",
            Object::Integer(20)
        );

        static ref IF_EXPR_06: (&'static [u8], Object) =
        (
            b"if (1 < 2) { 10 } else { 20 }",
            Object::Integer(10)
        );

        // Return Statement
        static ref RETURN_STMT_01: (&'static [u8], Object) =
        (
            b"return 10;",
            Object::Integer(10)
        );

        static ref RETURN_STMT_02: (&'static [u8], Object) =
        (
            b"return 10; 9;",
            Object::Integer(10)
        );

        static ref RETURN_STMT_03: (&'static [u8], Object) =
        (
            b"return 2 * 5; 9;",
            Object::Integer(10)
        );

        static ref RETURN_STMT_04: (&'static [u8], Object) =
        (
            b"9; return 2 * 5; 9;",
            Object::Integer(10)
        );

        static ref RETURN_STMT_05: (&'static [u8], Object) =
        (
            b"if (10 > 1) {\n\
                  if(10 > 1) {\n\
                      return 10;\n\
                  }\n\
                  \n\
                  return 1;\n\
              }",
            Object::Integer(10)
        );

        // Error Handling
        static ref ERROR_01: (&'static [u8], RuntimeError) =
        (
            b"5 + true",
            RuntimeError("type mismatch: `5 + true`".to_string())
        );

        static ref ERROR_02: (&'static [u8], RuntimeError) =
        (
            b"5 + true; 5;",
            RuntimeError("type mismatch: `5 + true`".to_string())
        );

        static ref ERROR_03: (&'static [u8], RuntimeError) =
        (
            b"-true",
            RuntimeError("type mismatch: `-true`".to_string())
        );

        static ref ERROR_04: (&'static [u8], RuntimeError) =
        (
            b"!5",
            RuntimeError("type mismatch: `!5`".to_string())
        );

        static ref ERROR_05: (&'static [u8], RuntimeError) =
        (
            b"5; true + false; 5",
            RuntimeError("type mismatch: `true + false`".to_string())
        );

        static ref ERROR_06: (&'static [u8], RuntimeError) =
        (
            b"if (1) { 10 }",
            RuntimeError("type mismatch: `if (1) {...}`".to_string())
        );
    }

    #[test_case(LITERAL_01.0    , &LITERAL_01.1     ; "integer literal")]
    #[test_case(LITERAL_02.0    , &LITERAL_02.1     ; "true literal"   )]
    #[test_case(LITERAL_03.0    , &LITERAL_03.1     ; "false literal"  )]
    #[test_case(PREFIX_01.0     , &PREFIX_01.1      ; "prefix bang 01" )]
    #[test_case(PREFIX_02.0     , &PREFIX_02.1      ; "prefix bang 02" )]
    #[test_case(PREFIX_03.0     , &PREFIX_03.1      ; "prefix bang 03" )]
    #[test_case(PREFIX_04.0     , &PREFIX_04.1      ; "prefix bang 04" )]
    #[test_case(PREFIX_05.0     , &PREFIX_05.1      ; "prefix minus 01")]
    #[test_case(PREFIX_06.0     , &PREFIX_06.1      ; "prefix minus 02")]
    #[test_case(INFIX_01.0      , &INFIX_01.1       ; "infix 01"       )]
    #[test_case(INFIX_02.0      , &INFIX_02.1       ; "infix 02"       )]
    #[test_case(INFIX_03.0      , &INFIX_03.1       ; "infix 03"       )]
    #[test_case(INFIX_04.0      , &INFIX_04.1       ; "infix 04"       )]
    #[test_case(INFIX_05.0      , &INFIX_05.1       ; "infix 05"       )]
    #[test_case(INFIX_06.0      , &INFIX_06.1       ; "infix 06"       )]
    #[test_case(INFIX_07.0      , &INFIX_07.1       ; "infix 07"       )]
    #[test_case(INFIX_08.0      , &INFIX_08.1       ; "infix 08"       )]
    #[test_case(INFIX_09.0      , &INFIX_09.1       ; "infix 09"       )]
    #[test_case(INFIX_10.0      , &INFIX_10.1       ; "infix 10"       )]
    #[test_case(INFIX_11.0      , &INFIX_11.1       ; "infix 11"       )]
    #[test_case(INFIX_12.0      , &INFIX_12.1       ; "infix 12"       )]
    #[test_case(INFIX_13.0      , &INFIX_13.1       ; "infix 13"       )]
    #[test_case(INFIX_14.0      , &INFIX_14.1       ; "infix 14"       )]
    #[test_case(INFIX_15.0      , &INFIX_15.1       ; "infix 15"       )]
    #[test_case(INFIX_16.0      , &INFIX_16.1       ; "infix 16"       )]
    #[test_case(INFIX_17.0      , &INFIX_17.1       ; "infix 17"       )]
    #[test_case(INFIX_18.0      , &INFIX_18.1       ; "infix 18"       )]
    #[test_case(INFIX_19.0      , &INFIX_19.1       ; "infix 19"       )]
    #[test_case(INFIX_20.0      , &INFIX_20.1       ; "infix 20"       )]
    #[test_case(INFIX_21.0      , &INFIX_21.1       ; "infix 21"       )]
    #[test_case(INFIX_22.0      , &INFIX_22.1       ; "infix 22"       )]
    #[test_case(INFIX_23.0      , &INFIX_23.1       ; "infix 23"       )]
    #[test_case(INFIX_24.0      , &INFIX_24.1       ; "infix 24"       )]
    #[test_case(IF_EXPR_01.0    , &IF_EXPR_01.1     ; "if expr 01"     )]
    #[test_case(IF_EXPR_02.0    , &IF_EXPR_02.1     ; "if expr 02"     )]
    #[test_case(IF_EXPR_03.0    , &IF_EXPR_03.1     ; "if expr 03"     )]
    #[test_case(IF_EXPR_04.0    , &IF_EXPR_04.1     ; "if expr 04"     )]
    #[test_case(IF_EXPR_05.0    , &IF_EXPR_05.1     ; "if expr 05"     )]
    #[test_case(IF_EXPR_06.0    , &IF_EXPR_06.1     ; "if expr 06"     )]
    #[test_case(RETURN_STMT_01.0, &RETURN_STMT_01.1 ; "return stmt 01" )]
    #[test_case(RETURN_STMT_02.0, &RETURN_STMT_02.1 ; "return stmt 02" )]
    #[test_case(RETURN_STMT_03.0, &RETURN_STMT_03.1 ; "return stmt 03" )]
    #[test_case(RETURN_STMT_04.0, &RETURN_STMT_04.1 ; "return stmt 04" )]
    #[test_case(RETURN_STMT_05.0, &RETURN_STMT_05.1 ; "return stmt 05" )]
    fn test(input: &[u8], expected: &Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let object = eval(&ast).unwrap();

        assert_eq!(&object, expected)
    }

    #[test_case(ERROR_01.0, &ERROR_01.1 ; "error 01" )]
    #[test_case(ERROR_02.0, &ERROR_02.1 ; "error 02" )]
    #[test_case(ERROR_03.0, &ERROR_03.1 ; "error 03" )]
    #[test_case(ERROR_04.0, &ERROR_04.1 ; "error 04" )]
    #[test_case(ERROR_05.0, &ERROR_05.1 ; "error 05" )]
    #[test_case(ERROR_06.0, &ERROR_06.1 ; "error 06" )]
    fn test_error(input: &[u8], expected: &RuntimeError) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let error = eval(&ast).unwrap_err();

        assert_eq!(&error, expected)
    }
}
