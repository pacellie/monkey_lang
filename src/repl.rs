use crate::compiler::Compiler;
use crate::error::MonkeyError;
use crate::interpreter;
use crate::interpreter::{eval_program, Environment};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::vm;
use crate::vm::VirtualMachine;

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

pub enum Runtime {
    Interpreter,
    VirtualMachine,
}

fn interpret(
    env: Rc<RefCell<Environment>>,
    input: &str,
) -> std::result::Result<interpreter::Object, Vec<MonkeyError>> {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    eval_program(env, ast).map_err(|error| vec![error])
}

fn compile_run(input: &str) -> std::result::Result<vm::Object, Vec<MonkeyError>> {
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;
    let mut compiler = Compiler::new();
    let byte_code = compiler.compile(&ast);
    let mut vm = VirtualMachine::new(byte_code);
    vm.run().map_err(|error| vec![error])?;
    vm.top().map(|obj| obj.clone()).map_err(|error| vec![error])
}

pub fn repl(runtime: Runtime) -> io::Result<()> {
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
            line => {
                let msg = match runtime {
                    Runtime::Interpreter => interpret(env.clone(), line).map_or_else(
                        |errors| errors.iter().map(|error| error.to_string()).join(", "),
                        |obj| obj.to_string(),
                    ),
                    Runtime::VirtualMachine => compile_run(line).map_or_else(
                        |errors| errors.iter().map(|error| error.to_string()).join(", "),
                        |obj| obj.to_string(),
                    ),
                };
                println!("{}", msg);
            }
        }

        buffer.clear();
    }
}
