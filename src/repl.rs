use crate::interpreter::{eval_program, Environment};
use crate::lexer::Lexer;
use crate::parser::Parser;

use std::cell::RefCell;
use std::io::{self, Write};
use std::rc::Rc;

use itertools::Itertools;

const PROMPT: &str = ">> ";
const QUIT: &str = "quit";
const CLEAR: &str = "clear";
const ENV: &str = "env";
const CMDS: &str = "\tquit  := quit the repl\n\
                    \tclear := clear the environment\n\
                    \tenv   := print the environment\n";

fn interpret(env: Rc<RefCell<Environment>>, input: &str) {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);

    let result = parser
        .parse()
        .and_then(|ast| eval_program(env, ast).map_err(|error| vec![error]));

    match result {
        Ok(obj) => println!("{}", obj),
        Err(errors) => println!(
            "{}",
            errors.iter().map(|error| format!("{}", error)).join(", ")
        ),
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

        match buffer.trim() {
            QUIT => return Ok(()),
            CLEAR => env.borrow_mut().clear(),
            ENV => println!("{}", env.borrow()),
            line => interpret(env.clone(), line),
        }

        buffer.clear();
    }
}
