use crate::interpreter::Environment;
use crate::interpreter::{Builtin, Object, Primitive};
use crate::interpreter::{Result, RuntimeError};
use crate::lexer::Token;
use crate::parser::ast::*;

use std::cell::RefCell;
use std::collections::HashMap;
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
        Expression::Integer(n) => Ok(Object::integer(n)),
        Expression::Boolean(b) => Ok(Object::boolean(b)),
        Expression::String(s) => Ok(Object::string(s)),
        Expression::Array(exprs) => {
            eval_many(env, exprs, eval_expr).map(|objs| Object::Array(objs))
        }
        Expression::Map(pairs) => eval_map_expr(env, pairs),
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
        .or_else(|| Builtin::builtin_by_name(&name).map(|builtin| Object::Builtin(builtin)))
        .ok_or(RuntimeError::unknown_name(format!("{}", name)))
}

fn eval_prefix_expr(env: Env, operator: Token, expr: Expression) -> Result<Object> {
    let object = eval_expr(env, expr)?;
    match (operator, object) {
        (Token::Bang, Object::Primitive(Primitive::Boolean(b))) => Ok(Object::boolean(!b)),
        (Token::Minus, Object::Primitive(Primitive::Integer(n))) => Ok(Object::integer(-n)),
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

    let (left, right) = match (left, right) {
        (Object::Primitive(l), Object::Primitive(r)) => (l, r),
        (left, right) => {
            return Err(RuntimeError::type_mismatch(format!(
                "{} {} {}",
                left, operator, right
            )));
        }
    };

    #[rustfmt::skip]
    let object = match (left, operator, right) {
        (Primitive::Integer(n), Token::Plus    , Primitive::Integer(m)) => Object::integer(n + m),
        (Primitive::Integer(n), Token::Minus   , Primitive::Integer(m)) => Object::integer(n - m),
        (Primitive::Integer(n), Token::Asterisk, Primitive::Integer(m)) => Object::integer(n * m),
        (Primitive::Integer(n), Token::Slash   , Primitive::Integer(m)) => Object::integer(n / m),
        (Primitive::Integer(n), Token::Eq      , Primitive::Integer(m)) => Object::boolean(n == m),
        (Primitive::Integer(n), Token::Neq     , Primitive::Integer(m)) => Object::boolean(n != m),
        (Primitive::Integer(n), Token::Gt      , Primitive::Integer(m)) => Object::boolean(n > m),
        (Primitive::Integer(n), Token::Lt      , Primitive::Integer(m)) => Object::boolean(n < m),

        (Primitive::Boolean(b), Token::Eq , Primitive::Boolean(p)) => Object::boolean(b == p),
        (Primitive::Boolean(b), Token::Neq, Primitive::Boolean(p)) => Object::boolean(b != p),

        (Primitive::String(s), Token::Plus, Primitive::String(t)) => Object::string(format!("{}{}", s, t)),

        (left, operator, right) => {
            return Err(RuntimeError::type_mismatch(format!("{} {} {}", left, operator, right)));
        }
    };

    Ok(object)
}

fn eval_if_expr(env: Env, cond: Expression, yes: Block, no: Option<Block>) -> Result<Object> {
    let cond = eval_expr(env.clone(), cond)?;
    if let Object::Primitive(Primitive::Boolean(cond)) = cond {
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
    let args = eval_many(env, args, eval_expr)?;

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

fn eval_many<A, B, F>(env: Env, xs: Vec<A>, f: F) -> Result<Vec<B>>
where
    A: Clone,
    F: Fn(Env, A) -> Result<B>,
{
    xs.iter().cloned().map(|a| f(env.clone(), a)).collect()
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

fn eval_builtin_call_expr(builtin: Builtin, args: Vec<Object>) -> Result<Object> {
    match builtin {
        Builtin::Len => match &args[..] {
            [obj] => match obj {
                Object::Primitive(Primitive::String(s)) => Ok(Object::integer(s.len() as i32)),
                Object::Array(vec) => Ok(Object::integer(vec.len() as i32)),
                _ => Err(RuntimeError::type_mismatch(format!("len({})", obj))),
            },
            _ => Err(RuntimeError::wrong_number_of_args(1, args.len())),
        },
        Builtin::First => match &args[..] {
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
        Builtin::Last => match &args[..] {
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
        Builtin::Rest => match &args[..] {
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
        Builtin::Push => match &args[..] {
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
        Builtin::Puts => {
            args.iter().for_each(|obj| println!("{}", obj));
            Ok(Object::Unit)
        }
    }
}

fn eval_index_expr(env: Env, expr: Expression, index: Expression) -> Result<Object> {
    let array = eval_expr(env.clone(), expr)?;
    let index = eval_expr(env.clone(), index)?;

    match (array, index) {
        (Object::Array(vec), Object::Primitive(Primitive::Integer(i))) => {
            if 0 <= i && (i as usize) < vec.len() {
                Ok(vec[i as usize].clone())
            } else {
                Err(RuntimeError::index_out_of_bounds(i, vec.len() - 1))
            }
        }
        (Object::Map(map), Object::Primitive(key)) => map
            .get(&key)
            .map(|value| value.clone())
            .ok_or(RuntimeError::missing_index(format!("{}", key))),
        (obj, index) => Err(RuntimeError::type_mismatch(format!("{}[{}]", obj, index))),
    }
}

fn eval_map_expr(env: Env, pairs: Vec<(Expression, Expression)>) -> Result<Object> {
    let pairs = eval_many(env, pairs, |env, (key, value)| {
        match eval_expr(env.clone(), key)? {
            Object::Primitive(key) => {
                let value = eval_expr(env, value)?;
                Ok((key, value))
            }
            obj => Err(RuntimeError::type_mismatch(format!("{{{}: (...)}}", obj))),
        }
    })?;

    let map: HashMap<Primitive, Object> = pairs.iter().cloned().collect();

    Ok(Object::Map(map))
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::lexer::Lexer;
    use crate::parser::Parser;

    use std::collections::HashMap;

    use test_case::test_case;

    #[test_case(b"5"    , Object::integer(5)     ; "integer literal")]
    #[test_case(b"true" , Object::boolean(true)  ; "true literal"   )]
    #[test_case(b"false", Object::boolean(false) ; "false literal"  )]
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
    #[test_case(b"\"Hello World!\"", Object::string("Hello World!".to_string()) ; "string literal")]
    #[test_case(
        b"[1, 2 * 2, 3 + 3]",
        Object::Array(vec![
            Object::integer(1),
            Object::integer(4),
            Object::integer(6),
        ]) ;
        "array literal"
    )]
    #[test_case(b"!true"  , Object::boolean(false); "prefix bang 01")]
    #[test_case(b"!false" , Object::boolean(true) ; "prefix bang 02")]
    #[test_case(b"!!true" , Object::boolean(true) ; "prefix bang 03")]
    #[test_case(b"!!false", Object::boolean(false); "prefix bang 04")]
    #[test_case(b"-5" , Object::integer(-5); "prefix minus 01")]
    #[test_case(b"--5", Object::integer(5) ; "prefix minus 02")]
    #[test_case(b"5 + 5 + 5 + 5 - 10"             , Object::integer(10)    ; "infix 01")]
    #[test_case(b"2 * 2 * 2 * 2 * 2"              , Object::integer(32)    ; "infix 02")]
    #[test_case(b"-50 + 100 + -50"                , Object::integer(0)     ; "infix 03")]
    #[test_case(b"5 * 2 + 10"                     , Object::integer(20)    ; "infix 04")]
    #[test_case(b"5 + 2 * 10"                     , Object::integer(25)    ; "infix 05")]
    #[test_case(b"20 + 2 * -10"                   , Object::integer(0)     ; "infix 06")]
    #[test_case(b"50 / 2 * 2 + 10"                , Object::integer(60)    ; "infix 07")]
    #[test_case(b"2 * (5 + 10)"                   , Object::integer(30)    ; "infix 08")]
    #[test_case(b"3 * 3 * 3 + 10"                 , Object::integer(37)    ; "infix 09")]
    #[test_case(b"3 * (3 * 3) + 10"               , Object::integer(37)    ; "infix 10")]
    #[test_case(b"(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::integer(50)    ; "infix 11")]
    #[test_case(b"1 < 2"                          , Object::boolean(true)  ; "infix 12")]
    #[test_case(b"1 > 2"                          , Object::boolean(false) ; "infix 13")]
    #[test_case(b"1 < 1"                          , Object::boolean(false) ; "infix 14")]
    #[test_case(b"1 > 1"                          , Object::boolean(false) ; "infix 15")]
    #[test_case(b"1 == 1"                         , Object::boolean(true)  ; "infix 16")]
    #[test_case(b"1 != 1"                         , Object::boolean(false) ; "infix 17")]
    #[test_case(b"1 == 2"                         , Object::boolean(false) ; "infix 18")]
    #[test_case(b"1 != 2"                         , Object::boolean(true)  ; "infix 19")]
    #[test_case(b"true == true"                   , Object::boolean(true)  ; "infix 20")]
    #[test_case(b"false == true"                  , Object::boolean(false) ; "infix 21")]
    #[test_case(b"true == false"                  , Object::boolean(false) ; "infix 22")]
    #[test_case(b"true != false"                  , Object::boolean(true)  ; "infix 23")]
    #[test_case(b"false != true"                  , Object::boolean(true)  ; "infix 24")]
    #[test_case(
        b"\"Hello\" + \" \" + \"World!\"",
        Object::string("Hello World!".to_string()) ;
        "infix 25"
    )]
    #[test_case(b"if (true) { 10 }"             , Object::integer(10) ; "if expr 01")]
    #[test_case(b"if (false) { 10 }"            , Object::Unit        ; "if expr 02")]
    #[test_case(b"if (1 < 2) { 10 }"            , Object::integer(10) ; "if expr 03")]
    #[test_case(b"if (1 > 2) { 10 }"            , Object::Unit        ; "if expr 04")]
    #[test_case(b"if (1 > 2) { 10 } else { 20 }", Object::integer(20) ; "if expr 05")]
    #[test_case(b"if (1 < 2) { 10 } else { 20 }", Object::integer(10) ; "if expr 06")]
    #[test_case(b"return 10;"      , Object::integer(10) ; "return stmt 01")]
    #[test_case(b"return 10; 9;"   , Object::integer(10) ; "return stmt 02")]
    #[test_case(b"return 2 * 5; 9;", Object::integer(10) ; "return stmt 03")]
    #[test_case(b"9; return 10; 9;", Object::integer(10) ; "return stmt 04")]
    #[test_case(
        b"if (10 > 1) {\n\
                if(10 > 1) {\n\
                    return 10;\n\
                }\n\
                \n\
                return 1;\n\
            }",
        Object::integer(10) ;
        "return stmt 05"
    )]
    #[test_case(b"let a = 5; a"                              , Object::integer(5)  ; "let stmt 01")]
    #[test_case(b"let a = 5 * 5; a"                          , Object::integer(25) ; "let stmt 02")]
    #[test_case(b"let a = 5; let b = a; b"                   , Object::integer(5)  ; "let stmt 03")]
    #[test_case(b"let a = 5; let b = a; let c = a + b + 5; c", Object::integer(15) ; "let stmt 04")]
    #[test_case(b"let identity = fn(x) { x }; identity(5)"            , Object::integer(5)  ; "call expr 01")]
    #[test_case(b"let identity = fn(x) { return x; }; identity(5)"    , Object::integer(5)  ; "call expr 02")]
    #[test_case(b"let double = fn(x) { x * 2 }; double(5)"            , Object::integer(10) ; "call expr 03")]
    #[test_case(b"let add = fn(x, y) { x + y }; add(5, 5)"            , Object::integer(10) ; "call expr 04")]
    #[test_case(b"let add = fn(x, y) { x + y }; add(5 + 5, add(5, 5))", Object::integer(20) ; "call expr 05")]
    #[test_case(b"fn(x) { x }(5)"                                     , Object::integer(5)  ; "call expr 06")]
    #[test_case(
        b"let adder = fn(x) {\n\
              fn(y) { x + y}\n\
          };\n\
          \n\
          let add_two = adder(2);\n\
          add_two(2)"
        ,
        Object::integer(4) ;
        "closure"
    )]
    #[test_case(b"len(\"\")"           , Object::integer(0)  ; "builtin len 01")]
    #[test_case(b"len(\"four\")"       , Object::integer(4)  ; "builtin len 02")]
    #[test_case(b"len(\"hello world\")", Object::integer(11) ; "builtin len 03")]
    #[test_case(b"[1, 2, 3][0]"                         , Object::integer(1) ; "index expr 01")]
    #[test_case(b"[1, 2, 3][1]"                         , Object::integer(2) ; "index expr 02")]
    #[test_case(b"[1, 2, 3][2]"                         , Object::integer(3) ; "index expr 03")]
    #[test_case(b"let i = 0; [1][i]"                    , Object::integer(1) ; "index expr 04")]
    #[test_case(b"[1, 2, 3][1 + 1]"                     , Object::integer(3) ; "index expr 05")]
    #[test_case(b"let a = [1, 2, 3]; a[2]"              , Object::integer(3) ; "index expr 06")]
    #[test_case(b"let a = [1, 2, 3]; a[0] + a[1] + a[2]", Object::integer(6) ; "index expr 07")]
    #[test_case(b"let a = [1, 2, 3]; let i = a[0]; a[i]", Object::integer(2) ; "index expr 08")]
    #[test_case(b"{\"foo\": 5}[\"foo\"]"                , Object::integer(5) ; "index expr 09")]
    #[test_case(b"let key = \"foo\"; {\"foo\": 5}[key]" , Object::integer(5) ; "index expr 10")]
    #[test_case(b"{5: 5}[5]"                            , Object::integer(5) ; "index expr 11")]
    #[test_case(b"{true: 5}[true]"                      , Object::integer(5) ; "index expr 12")]
    #[test_case(b"{false: 5}[false]"                    , Object::integer(5) ; "index expr 13")]
    fn test(input: &[u8], expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let object = eval(ast).unwrap();

        assert_eq!(object, expected)
    }

    #[test]
    fn test_map_literal() {
        let input = b"let two = \"two\";\n\
            {\n\
                \"one\": 10 - 9,\n\
                two: 1 + 1,\n\
                \"thr\" + \"ee\": 6 / 2,\n\
                4: 4,\n\
                true: 5,\n\
                false: 6\n\
            }";

        let mut map = HashMap::new();
        map.insert(Primitive::String("one".to_string()), Object::integer(1));
        map.insert(Primitive::String("two".to_string()), Object::integer(2));
        map.insert(Primitive::String("three".to_string()), Object::integer(3));
        map.insert(Primitive::Integer(4), Object::integer(4));
        map.insert(Primitive::Boolean(true), Object::integer(5));
        map.insert(Primitive::Boolean(false), Object::integer(6));

        let expected = Object::Map(map);

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let object = eval(ast).unwrap();

        assert_eq!(object, expected)
    }

    #[test_case(b"5 + true"              , RuntimeError::type_mismatch("5 + true")        ; "error 01")]
    #[test_case(b"5 + true; 5;"          , RuntimeError::type_mismatch("5 + true")        ; "error 02")]
    #[test_case(b"-true"                 , RuntimeError::type_mismatch("-true")           ; "error 03")]
    #[test_case(b"!5"                    , RuntimeError::type_mismatch("!5")              ; "error 04")]
    #[test_case(b"5; true + false; 5"    , RuntimeError::type_mismatch("true + false")    ; "error 05")]
    #[test_case(b"if (1) { 10 }"         , RuntimeError::type_mismatch("if (1) {...}")    ; "error 06")]
    #[test_case(b"foobar"                , RuntimeError::unknown_name("foobar")           ; "error 07")]
    #[test_case(b"len(1)"                , RuntimeError::type_mismatch("len(1)")          ; "error 08")]
    #[test_case(b"len(\"one\", \"two\")" , RuntimeError::wrong_number_of_args(1, 2)       ; "error 09")]
    #[test_case(b"[1, 2, 3][3]"          , RuntimeError::index_out_of_bounds(3, 2)        ; "error 10")]
    #[test_case(b"[1, 2, 3][-1]"         , RuntimeError::index_out_of_bounds(-1, 2)       ; "error 11")]
    #[test_case(b"{\"foo\": 5}[\"bar\"]" , RuntimeError::missing_index("\"bar\"")         ; "error 12")]
    #[test_case(b"{}[\"foo\"]"           , RuntimeError::missing_index("\"foo\"")         ; "error 13")]
    #[test_case(b"{}[fn(x) { x }]"       , RuntimeError::type_mismatch("{}[fn(x) { x }]") ; "error 14")]
    fn test_error(input: &[u8], expected: RuntimeError) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let ast = parser.parse().unwrap();
        let error = eval(ast).unwrap_err();

        assert_eq!(error, expected)
    }
}
