use crate::interpreter::Environment;
use crate::interpreter::Object;
use crate::interpreter::{Result, RuntimeError};
use crate::lexer::Token;
use crate::parser::ast::*;

use std::cell::RefCell;
use std::rc::Rc;

type Env = Rc<RefCell<Environment>>;

pub fn eval(env: Env, ast: Program) -> Result<Object> {
    match eval_block(env, ast)? {
        Object::Return(obj) => {
            return Ok(*obj);
        }
        obj => Ok(obj),
    }
}

pub fn eval_block(env: Env, block: Block) -> Result<Object> {
    let mut obj = Object::Unit;

    for stmt in block.0 {
        obj = match eval_stmt(env.clone(), stmt)? {
            Object::Return(obj) => {
                return Ok(Object::Return(obj));
            }
            obj => obj,
        }
    }

    Ok(obj)
}

fn eval_stmt(env: Env, stmt: Statement) -> Result<Object> {
    match stmt {
        Statement::Let { name, expr } => {
            let obj = eval_expr(env.clone(), expr)?;
            env.borrow_mut().set(name, obj);
            Ok(Object::Unit)
        }
        Statement::Return(expr) => {
            let obj = eval_expr(env, expr)?;
            Ok(Object::Return(Box::new(obj)))
        }
        Statement::Stmt(expr) => {
            eval_expr(env, expr)?;
            Ok(Object::Unit)
        }
        Statement::Expr(expr) => eval_expr(env, expr),
    }
}

fn eval_expr(env: Env, expr: Expression) -> Result<Object> {
    match expr {
        Expression::Name(name) => env
            .borrow()
            .get(&name)
            .ok_or(RuntimeError(format!("unknown identifier: `{}`", name))),
        Expression::Integer(n) => Ok(Object::Integer(n)),
        Expression::Boolean(b) => Ok(Object::Boolean(b)),
        Expression::Prefix { operator, expr } => eval_prefix_expr(env, operator, *expr),
        Expression::Infix {
            left,
            operator,
            right,
        } => eval_infix_expr(env, *left, operator, *right),
        Expression::If { cond, yes, no } => eval_if_expr(env, *cond, yes, no),
        Expression::Function { params, body } => Ok(Object::Function {
            env: env.clone(),
            params,
            body,
        }),
        Expression::Call { function, args } => {
            let function = eval_expr(env.clone(), *function)?;
            let args: Result<Vec<Object>> = args
                .iter()
                .cloned()
                .map(|expr| eval_expr(env.clone(), expr))
                .collect();
            let args = args?;

            match function {
                Object::Function { env, params, body } => {
                    let mut env = Environment::new(env);

                    for (name, arg) in params.iter().zip(args) {
                        env.set(name.clone(), arg);
                    }

                    let obj = eval_block(Rc::new(RefCell::new(env)), body)?;
                    if let Object::Return(obj) = obj {
                        Ok(*obj)
                    } else {
                        Ok(obj)
                    }
                }
                _ => Err(RuntimeError(format!("type mismatch: `{}(...)`", function))),
            }
        }
    }
}

fn eval_prefix_expr(env: Env, operator: Token, expr: Expression) -> Result<Object> {
    let object = eval_expr(env, expr)?;
    match (operator, object) {
        (Token::Bang, Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        (Token::Minus, Object::Integer(n)) => Ok(Object::Integer(-n)),
        (operator, object) => Err(RuntimeError(format!(
            "type mismatch: `{}{}`",
            operator, object
        ))),
    }
}

fn eval_infix_expr(
    env: Env,
    left: Expression,
    operator: Token,
    right: Expression,
) -> Result<Object> {
    let left = eval_expr(env.clone(), left)?;
    let right = eval_expr(env, right)?;

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

fn eval_if_expr(env: Env, cond: Expression, yes: Block, no: Option<Block>) -> Result<Object> {
    let cond = eval_expr(env.clone(), cond)?;
    if let Object::Boolean(cond) = cond {
        if cond {
            eval_block(env, yes)
        } else {
            if let Some(no) = no {
                eval_block(env, no)
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

    use test_case::test_case;

    #[test_case(b"5"    , Object::Integer(5)     ; "integer literal")]
    #[test_case(b"true" , Object::Boolean(true)  ; "true literal"   )]
    #[test_case(b"false", Object::Boolean(false) ; "false literal"  )]
    #[test_case(
        b"fn(x) { x + 2 }",
        Object::Function {
            env: Rc::new(RefCell::new(Environment::empty())),
            params: vec![
                "x".to_string()
            ],
            body: block(vec![
                expr_stmt(
                    infix(
                        name("x"),
                        Token::Plus,
                        integer(2),
                    )
                )
            ])
        } ;
        "function literal"
    )]
    #[test_case(b"!true"  , Object::Boolean(false); "prefix bang 01")]
    #[test_case(b"!false" , Object::Boolean(true) ; "prefix bang 02")]
    #[test_case(b"!!true" , Object::Boolean(true) ; "prefix bang 03")]
    #[test_case(b"!!false", Object::Boolean(false); "prefix bang 04")]
    #[test_case(b"-5" , Object::Integer(-5); "prefix minus 01")]
    #[test_case(b"--5", Object::Integer(5) ; "prefix minus 02")]
    #[test_case(b"5 + 5 + 5 + 5 - 10"             , Object::Integer(10)    ; "infix 01")]
    #[test_case(b"2 * 2 * 2 * 2 * 2"              , Object::Integer(32)    ; "infix 02")]
    #[test_case(b"-50 + 100 + -50"                , Object::Integer(0)     ; "infix 03")]
    #[test_case(b"5 * 2 + 10"                     , Object::Integer(20)    ; "infix 04")]
    #[test_case(b"5 + 2 * 10"                     , Object::Integer(25)    ; "infix 05")]
    #[test_case(b"20 + 2 * -10"                   , Object::Integer(0)     ; "infix 06")]
    #[test_case(b"50 / 2 * 2 + 10"                , Object::Integer(60)    ; "infix 07")]
    #[test_case(b"2 * (5 + 10)"                   , Object::Integer(30)    ; "infix 08")]
    #[test_case(b"3 * 3 * 3 + 10"                 , Object::Integer(37)    ; "infix 09")]
    #[test_case(b"3 * (3 * 3) + 10"               , Object::Integer(37)    ; "infix 10")]
    #[test_case(b"(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Integer(50)    ; "infix 11")]
    #[test_case(b"1 < 2"                          , Object::Boolean(true)  ; "infix 12")]
    #[test_case(b"1 > 2"                          , Object::Boolean(false) ; "infix 13")]
    #[test_case(b"1 < 1"                          , Object::Boolean(false) ; "infix 14")]
    #[test_case(b"1 > 1"                          , Object::Boolean(false) ; "infix 15")]
    #[test_case(b"1 == 1"                         , Object::Boolean(true)  ; "infix 16")]
    #[test_case(b"1 != 1"                         , Object::Boolean(false) ; "infix 17")]
    #[test_case(b"1 == 2"                         , Object::Boolean(false) ; "infix 18")]
    #[test_case(b"1 != 2"                         , Object::Boolean(true)  ; "infix 19")]
    #[test_case(b"true == true"                   , Object::Boolean(true)  ; "infix 20")]
    #[test_case(b"false == true"                  , Object::Boolean(false) ; "infix 21")]
    #[test_case(b"true == false"                  , Object::Boolean(false) ; "infix 22")]
    #[test_case(b"true != false"                  , Object::Boolean(true)  ; "infix 23")]
    #[test_case(b"false != true"                  , Object::Boolean(true)  ; "infix 24")]
    #[test_case(b"if (true) { 10 }"             , Object::Integer(10) ; "if expr 01")]
    #[test_case(b"if (false) { 10 }"            , Object::Unit        ; "if expr 02")]
    #[test_case(b"if (1 < 2) { 10 }"            , Object::Integer(10) ; "if expr 03")]
    #[test_case(b"if (1 > 2) { 10 }"            , Object::Unit        ; "if expr 04")]
    #[test_case(b"if (1 > 2) { 10 } else { 20 }", Object::Integer(20) ; "if expr 05")]
    #[test_case(b"if (1 < 2) { 10 } else { 20 }", Object::Integer(10) ; "if expr 06")]
    #[test_case(b"return 10;"      , Object::Integer(10) ; "return stmt 01")]
    #[test_case(b"return 10; 9;"   , Object::Integer(10) ; "return stmt 02")]
    #[test_case(b"return 2 * 5; 9;", Object::Integer(10) ; "return stmt 03")]
    #[test_case(b"9; return 10; 9;", Object::Integer(10) ; "return stmt 04")]
    #[test_case(
        b"if (10 > 1) {\n\
                if(10 > 1) {\n\
                    return 10;\n\
                }\n\
                \n\
                return 1;\n\
            }",
        Object::Integer(10) ;
        "return stmt 05"
    )]
    #[test_case(b"let a = 5; a"                              , Object::Integer(5)  ; "let stmt 01")]
    #[test_case(b"let a = 5 * 5; a"                          , Object::Integer(25) ; "let stmt 02")]
    #[test_case(b"let a = 5; let b = a; b"                   , Object::Integer(5)  ; "let stmt 03")]
    #[test_case(b"let a = 5; let b = a; let c = a + b + 5; c", Object::Integer(15) ; "let stmt 04")]
    #[test_case(b"let identity = fn(x) { x }; identity(5)"            , Object::Integer(5)  ; "call expr 01")]
    #[test_case(b"let identity = fn(x) { return x; }; identity(5)"    , Object::Integer(5)  ; "call expr 02")]
    #[test_case(b"let double = fn(x) { x * 2 }; double(5)"            , Object::Integer(10) ; "call expr 03")]
    #[test_case(b"let add = fn(x, y) { x + y }; add(5, 5)"            , Object::Integer(10) ; "call expr 04")]
    #[test_case(b"let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))", Object::Integer(20) ; "call expr 05")]
    #[test_case(b"fn(x) { x }(5)"                                     , Object::Integer(5)  ; "call expr 06")]
    #[test_case(
        b"let adder = fn(x) {\n\
              fn(y) { x + y}\n\
          };\n\
          \n\
          let add_two = adder(2);\n\
          add_two(2)"
        ,
        Object::Integer(4) ;
        "closure"
    )]
    fn test(input: &[u8], expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let env = Rc::new(RefCell::new(Environment::empty()));
        let object = eval(env, ast).unwrap();

        assert_eq!(object, expected)
    }

    #[test_case(b"5 + true"          , RuntimeError("type mismatch: `5 + true`".to_string())     ; "error 01")]
    #[test_case(b"5 + true; 5;"      , RuntimeError("type mismatch: `5 + true`".to_string())     ; "error 02")]
    #[test_case(b"-true"             , RuntimeError("type mismatch: `-true`".to_string())        ; "error 03")]
    #[test_case(b"!5"                , RuntimeError("type mismatch: `!5`".to_string())           ; "error 04")]
    #[test_case(b"5; true + false; 5", RuntimeError("type mismatch: `true + false`".to_string()) ; "error 05")]
    #[test_case(b"if (1) { 10 }"     , RuntimeError("type mismatch: `if (1) {...}`".to_string()) ; "error 06")]
    #[test_case(b"foobar"            , RuntimeError("unknown identifier: `foobar`".to_string())  ; "error 07")]
    fn test_error(input: &[u8], expected: RuntimeError) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let env = Rc::new(RefCell::new(Environment::empty()));
        let error = eval(env, ast).unwrap_err();

        assert_eq!(error, expected)
    }
}
