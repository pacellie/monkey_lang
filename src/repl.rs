use crate::interpreter::{eval, Environment};
use crate::lexer::Lexer;
use crate::parser::Parser;

use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

const PROMPT: &str = ">> ";
const QUIT: &str = "quit";
const CLEAR: &str = "clear";
const ENV: &str = "env";
const CMDS: &str = "\tquit  := quit the repl\n\
                    \tclear := clear the environment\n\
                    \tenv   := print the environment\n";

fn run(env: Rc<RefCell<Environment>>, input: &str) {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    match parser.parse() {
        Ok(ast) => {
            println!("{}", ast);
            match eval(env, ast) {
                Ok(obj) => println!("{}", obj),
                Err(err) => println!("Runtime Error: {}", err),
            }
        }
        Err(err) => println!("Parser Error: {}", err),
    }
}

pub fn repl() -> io::Result<()> {
    let mut buffer = String::new();
    let env = Rc::new(RefCell::new(Environment::empty()));

    println!("{}", CMDS);

    loop {
        print!("{}", PROMPT);
        io::stdout().flush()?;

        let _ = io::stdin().read_line(&mut buffer)?;

        match buffer.as_str().trim() {
            QUIT => return Ok(()),
            CLEAR => env.borrow_mut().clear(),
            ENV => println!("{}", env.borrow()),
            line => run(env.clone(), line),
        }

        buffer.clear();
    }
}
