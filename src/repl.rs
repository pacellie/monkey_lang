use crate::interpreter::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;

use std::io::{self, Write};

const PROMPT: &str = ">> ";
const QUIT: &str = "quit";

pub fn repl() -> io::Result<()> {
    let mut buffer = String::new();

    loop {
        print!("{}", PROMPT);
        io::stdout().flush()?;

        let _ = io::stdin().read_line(&mut buffer)?;

        match buffer.as_str().trim() {
            QUIT => return Ok(()),
            line => {
                let lexer = Lexer::new(line.as_bytes());
                let mut parser = Parser::new(lexer);
                match parser.parse() {
                    Ok(program) => {
                        println!("{}", program);
                        match eval(&program) {
                            Ok(obj) => println!("{}", obj),
                            Err(err) => println!("Runtime Error: {}", err),
                        }
                    }
                    Err(err) => println!("Parser Error: {}", err),
                }
            }
        }

        buffer.clear();
    }
}
