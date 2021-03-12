use crate::interpreter::Environment;
use crate::interpreter::{BuiltinFunction, Object};
use crate::interpreter::{Result, RuntimeError};
use crate::lexer::Token;
use crate::parser::ast::*;

use std::cell::RefCell;
use std::rc::Rc;

use itertools::Itertools;

type Env = Rc<RefCell<Environment>>;

pub fn eval(ast: Program) -> Result<Object> {
    eval_program(Rc::new(RefCell::new(Environment::empty())), ast)
}

pub fn eval_program(env: Env, ast: Program) -> Result<Object> {
    match eval_block(env, ast)? {
        Object::Return(obj) => {
            return Ok(*obj);
        }
        obj => Ok(obj),
    }
}

fn eval_block(env: Env, block: Block) -> Result<Object> {
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
        Expression::Name(name) => eval_name(env, name),
        Expression::Integer(n) => Ok(Object::Integer(n)),
        Expression::Boolean(b) => Ok(Object::Boolean(b)),
        Expression::String(s) => Ok(Object::String(s)),
        Expression::Array(exprs) => eval_exprs(env, exprs).map(|objs| Object::Array(objs)),
        Expression::Prefix { operator, expr } => eval_prefix_expr(env, operator, *expr),
        Expression::Infix {
            left,
            operator,
            right,
        } => eval_infix_expr(env, *left, operator, *right),
        Expression::If { cond, yes, no } => eval_if_expr(env, *cond, yes, no),
        Expression::Function { params, body } => Ok(Object::Function { env, params, body }),
        Expression::Call { expr, args } => eval_call_expr(env, *expr, args),
        Expression::Index { expr, index } => eval_index_expr(env, *expr, *index),
    }
}

fn eval_name(env: Env, name: String) -> Result<Object> {
    env.borrow()
        .get(&name)
        .or_else(|| BuiltinFunction::builtin_by_name(&name).map(|builtin| Object::Builtin(builtin)))
        .ok_or(RuntimeError::unknown_name(format!("{}", name)))
}

fn eval_prefix_expr(env: Env, operator: Token, expr: Expression) -> Result<Object> {
    let object = eval_expr(env, expr)?;
    match (operator, object) {
        (Token::Bang, Object::Boolean(b)) => Ok(Object::Boolean(!b)),
        (Token::Minus, Object::Integer(n)) => Ok(Object::Integer(-n)),
        (operator, object) => Err(RuntimeError::type_mismatch(format!(
            "{}{}",
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

        (Object::String(s), Token::Plus, Object::String(t)) => Object::String(format!("{}{}", s, t)),

        (left, operator, right) => {
            return Err(RuntimeError::type_mismatch(format!("{} {} {}", left, operator, right)));
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
        Err(RuntimeError::type_mismatch(format!(
            "if ({}) {{...}}",
            cond
        )))
    }
}

fn eval_call_expr(env: Env, expr: Expression, args: Vec<Expression>) -> Result<Object> {
    let obj = eval_expr(env.clone(), expr)?;
    let args = eval_exprs(env.clone(), args)?;

    match obj {
        Object::Function { env, params, body } => eval_function_call_expr(env, params, body, args),
        Object::Builtin(builtin) => eval_builtin_call_expr(builtin, args),
        _ => Err(RuntimeError::type_mismatch(format!(
            "{}({})",
            obj,
            args.iter().join(", ")
        ))),
    }
}

fn eval_exprs(env: Env, exprs: Vec<Expression>) -> Result<Vec<Object>> {
    exprs
        .iter()
        .cloned()
        .map(|expr| eval_expr(env.clone(), expr))
        .collect()
}

fn eval_function_call_expr(
    env: Env,
    params: Vec<String>,
    body: Block,
    args: Vec<Object>,
) -> Result<Object> {
    if params.len() != args.len() {
        return Err(RuntimeError::wrong_number_of_args(params.len(), args.len()));
    }

    let mut env = Environment::new(env);

    params
        .iter()
        .cloned()
        .zip(args)
        .for_each(|(name, arg)| env.set(name, arg));

    let obj = eval_block(Rc::new(RefCell::new(env)), body)?;
    if let Object::Return(obj) = obj {
        Ok(*obj)
    } else {
        Ok(obj)
    }
}

fn eval_builtin_call_expr(builtin: BuiltinFunction, args: Vec<Object>) -> Result<Object> {
    match builtin {
        BuiltinFunction::Len => match &args[..] {
            [obj] => match obj {
                Object::String(s) => Ok(Object::Integer(s.len() as i32)),
                Object::Array(vec) => Ok(Object::Integer(vec.len() as i32)),
                _ => Err(RuntimeError::type_mismatch(format!("len({})", obj))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(1, args.len())),
        },
        BuiltinFunction::First => match &args[..] {
            [obj] => match obj {
                Object::Array(vec) => {
                    if vec.len() != 0 {
                        Ok(vec[0].clone())
                    } else {
                        Err(RuntimeError("first([])".to_string()))
                    }
                }
                _ => Err(RuntimeError::type_mismatch(format!("first({})", obj))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(1, args.len())),
        },
        BuiltinFunction::Last => match &args[..] {
            [obj] => match obj {
                Object::Array(vec) => {
                    if vec.len() != 0 {
                        Ok(vec[vec.len() - 1].clone())
                    } else {
                        Err(RuntimeError("last([])".to_string()))
                    }
                }
                _ => Err(RuntimeError::type_mismatch(format!("last({})", obj))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(1, args.len())),
        },
        BuiltinFunction::Rest => match &args[..] {
            [obj] => match obj {
                Object::Array(vec) => {
                    if vec.len() != 0 {
                        Ok(Object::Array(vec[1..].to_vec()))
                    } else {
                        Err(RuntimeError("rest([])".to_string()))
                    }
                }
                _ => Err(RuntimeError::type_mismatch(format!("rest({})", obj))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(1, args.len())),
        },
        BuiltinFunction::Push => match &args[..] {
            [array, obj] => match array {
                Object::Array(vec) => {
                    let mut vec = vec.clone();
                    vec.push(obj.clone());
                    Ok(Object::Array(vec))
                }
                _ => Err(RuntimeError::type_mismatch(format!(
                    "push({}, {})",
                    array, obj
                ))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(2, args.len())),
        },
    }
}

fn eval_index_expr(env: Env, expr: Expression, index: Expression) -> Result<Object> {
    let array = eval_expr(env.clone(), expr)?;
    let index = eval_expr(env.clone(), index)?;

    match (array, index) {
        (Object::Array(vec), Object::Integer(i)) => {
            if 0 <= i && (i as usize) < vec.len() {
                Ok(vec[i as usize].clone())
            } else {
                Err(RuntimeError::index_out_of_bounds(i, vec.len() - 1))
            }
        }
        (array, index) => Err(RuntimeError::type_mismatch(format!("{}[{}]", array, index))),
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
    #[test_case(b"\"Hello World!\"", Object::String("Hello World!".to_string()) ; "string literal")]
    #[test_case(
        b"[1, 2 * 2, 3 + 3]",
        Object::Array(vec![
            Object::Integer(1),
            Object::Integer(4),
            Object::Integer(6),
        ]) ;
        "array literal"
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
    #[test_case(
        b"\"Hello\" + \" \" + \"World!\"",
        Object::String("Hello World!".to_string()) ;
        "infix 25"
    )]
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
    #[test_case(b"len(\"\")"           , Object::Integer(0)  ; "builtin len 01")]
    #[test_case(b"len(\"four\")"       , Object::Integer(4)  ; "builtin len 02")]
    #[test_case(b"len(\"hello world\")", Object::Integer(11) ; "builtin len 03")]
    #[test_case(b"[1, 2, 3][0]", Object::Integer(1) ; "index expr 01")]
    #[test_case(b"[1, 2, 3][1]", Object::Integer(2) ; "index expr 02")]
    #[test_case(b"[1, 2, 3][2]", Object::Integer(3) ; "index expr 03")]
    #[test_case(b"let i = 0; [1][i]", Object::Integer(1) ; "index expr 04")]
    #[test_case(b"[1, 2, 3][1 + 1]", Object::Integer(3) ; "index expr 05")]
    #[test_case(b"let a = [1, 2, 3]; a[2]", Object::Integer(3) ; "index expr 06")]
    #[test_case(b"let a = [1, 2, 3]; a[0] + a[1] + a[2]", Object::Integer(6) ; "index expr 07")]
    #[test_case(b"let a = [1, 2, 3]; let i = a[0]; a[i]", Object::Integer(2) ; "index expr 08")]
    fn test(input: &[u8], expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let object = eval(ast).unwrap();

        assert_eq!(object, expected)
    }

    #[test_case(b"5 + true"              , RuntimeError::type_mismatch("5 + true")     ; "error 01")]
    #[test_case(b"5 + true; 5;"          , RuntimeError::type_mismatch("5 + true")     ; "error 02")]
    #[test_case(b"-true"                 , RuntimeError::type_mismatch("-true")        ; "error 03")]
    #[test_case(b"!5"                    , RuntimeError::type_mismatch("!5")           ; "error 04")]
    #[test_case(b"5; true + false; 5"    , RuntimeError::type_mismatch("true + false") ; "error 05")]
    #[test_case(b"if (1) { 10 }"         , RuntimeError::type_mismatch("if (1) {...}") ; "error 06")]
    #[test_case(b"foobar"                , RuntimeError::unknown_name("foobar")        ; "error 07")]
    #[test_case(b"len(1)"                , RuntimeError::type_mismatch("len(1)")       ; "error 08")]
    #[test_case(b"len(\"one\", \"two\")" , RuntimeError::wrong_number_of_args(1, 2)    ; "error 09")]
    #[test_case(b"[1, 2, 3][3]"          , RuntimeError::index_out_of_bounds(3, 2)     ; "error 10")]
    #[test_case(b"[1, 2, 3][-1]"         , RuntimeError::index_out_of_bounds(-1, 2)    ; "error 11")]
    fn test_error(input: &[u8], expected: RuntimeError) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let error = eval(ast).unwrap_err();

        assert_eq!(error, expected)
    }
}
