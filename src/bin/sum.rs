use monkey_lang::interpreter::eval;
use monkey_lang::lexer::Lexer;
use monkey_lang::parser::Parser;

use std::env::args;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let xs = args().skip(1).nth(0).expect("`sum [1, 2, 3]`");

    let sum = format!(
        "let reduce = fn(array, init, f) {{\n\
            let go = fn(array, acc) {{\n\
                if (len(array) == 0) {{\n\
                    acc\n\
                }} else {{\n\
                    go(rest(array), f(acc, first(array)))\n\
                }}\n\
            }};\n\
            go(array, init)\n\
        }};\n\
        \n\
        let sum = fn(array) {{\n\
            reduce(array, 0, fn(result, elem) {{ result + elem }})\n\
        }};\n\
        \n\
        sum({})",
        xs
    );

    let lexer = Lexer::new(sum.as_bytes());
    let mut parser = Parser::new(lexer);
    let (ast, _) = parser.parse();
    let obj = eval(ast)?;

    println!("{}", obj);

    Ok(())
}
