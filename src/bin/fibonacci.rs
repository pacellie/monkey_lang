use monkey_lang::interpreter::eval;
use monkey_lang::lexer::Lexer;
use monkey_lang::parser::Parser;

use std::env::args;

fn main() {
    let n = args().skip(1).nth(0).expect("`fibonacci n`");

    let fibonacci = format!(
        "let fibonacci = fn(x) {{\n\
            if (x < 1) {{\n\
                0\n\
            }} else {{\n\
                if (x < 2) {{\n\
                    1\n\
                }} else {{\n\
                    fibonacci(x - 2) + fibonacci(x - 1)\n\
                }}\n\
            }}\n\
        }};\n\
        \n\
        fibonacci({})",
        n,
    );

    let lexer = Lexer::new(fibonacci.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();
    let obj = eval(ast).unwrap();

    println!("{}", obj);
}
