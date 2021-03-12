use monkey_lang::interpreter::eval;
use monkey_lang::lexer::Lexer;
use monkey_lang::parser::Parser;

use std::env::args;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let xs = args().skip(1).nth(0).expect("`map [1, 2, 3]`");

    let map = format!(
        "let map = fn(array, f) {{\n\
            let go = fn(array, acc) {{\n\
                if (len(array) == 0) {{\n\
                    acc\n\
                }} else {{\n\
                    go(rest(array), push(acc, f(first(array))))
                }}\n\
            }};\n\
            go(array, [])\n\
        }};\n\
        \n\
        let double = fn(x) {{ x * 2 }};\n\
        \n\
        map({}, double)",
        xs,
    );

    let lexer = Lexer::new(map.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    let obj = eval(ast)?;

    println!("{}", obj);

    Ok(())
}
