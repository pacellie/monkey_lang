use crate::compiler::Compiler;
use crate::interpreter::{eval_program, Environment};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::symbol::SymbolTable;
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

pub fn interpret() -> io::Result<()> {
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
                let lexer = Lexer::new(line.as_bytes());
                let mut parser = Parser::new(lexer);
                match parser.parse() {
                    Ok(ast) => match eval_program(env.clone(), ast) {
                        Ok(obj) => println!("{}", obj),
                        Err(err) => println!("{}", err),
                    },
                    Err(err) => println!("{}", err.iter().join("\n")),
                }
            }
        }

        buffer.clear();
    }
}

fn compile_run() -> io::Result<()> {
    let mut buffer = String::new();

    let mut symbol_table = SymbolTable::toplevel();
    let mut globals = vec![0; 128];
    let mut heap = Compiler::static_constants();

    println!("{}", CMDS);

    loop {
        print!("{}", PROMPT);
        io::stdout().flush()?;

        let _ = io::stdin().read_line(&mut buffer)?;

        match buffer.trim() {
            QUIT => return Ok(()),
            CLEAR => {
                symbol_table = SymbolTable::toplevel();
                globals = vec![0; 128];
                heap = Compiler::static_constants();
            }
            ENV => println!(
                "{}\n[{}]\n[{}]",
                symbol_table,
                globals
                    .iter()
                    .take_while(|global| **global != 0)
                    .enumerate()
                    .map(|(i, reference)| format!("{}: {}", i, reference))
                    .join(", "),
                heap.iter()
                    .enumerate()
                    .map(|(i, obj)| format!("{}: {}", i, obj))
                    .join(", ")
            ),
            line => {
                let lexer = Lexer::new(line.as_bytes());
                let mut parser = Parser::new(lexer);
                match parser.parse() {
                    Ok(ast) => {
                        let mut compiler = Compiler::new();
                        compiler.constants = heap.clone();
                        compiler.symbol_table = symbol_table.clone();
                        match compiler.compile(&ast) {
                            Ok(byte_code) => {
                                symbol_table = compiler.symbol_table.clone();
                                let mut vm = VirtualMachine::new(byte_code);
                                vm.globals = globals.clone();
                                match vm.run().and_then(|_| vm.top()) {
                                    Ok(obj) => {
                                        globals = vm.globals.clone();
                                        heap = vm.heap.clone();
                                        println!("{}", obj)
                                    }
                                    Err(err) => println!("{}", err),
                                }
                            }
                            Err(err) => println!("{}", err),
                        }
                    }
                    Err(err) => println!("{}", err.iter().join("\n")),
                }
            }
        }

        buffer.clear();
    }
}

pub fn repl(runtime: Runtime) -> io::Result<()> {
    match runtime {
        Runtime::Interpreter => interpret(),
        Runtime::VirtualMachine => compile_run(),
    }
}
