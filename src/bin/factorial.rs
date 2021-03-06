use monkey_lang::interpreter::eval;
use monkey_lang::lexer::Lexer;
use monkey_lang::parser::Parser;

use std::env::args;

fn main() {
    let n = args().skip(1).nth(0).expect("`factorial n`");

    let factorial = format!(
        "let factorial = fn(x) {{\n\
            if (x < 1) {{\n\
                1\n\
            }} else {{\n\
                x * factorial(x - 1)\n\
            }}\n\
        }};\n\
        \n\
        factorial({})",
        n,
    );

    let lexer = Lexer::new(factorial.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse().unwrap();
    let obj = eval(ast).unwrap();

    println!("{}", obj);
}
