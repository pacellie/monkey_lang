use crate::compiler::Compiler;
use crate::compiler::Reference;
use crate::error::MonkeyError;
use crate::interpreter;
use crate::interpreter::{eval_program, Environment};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::symbol::SymbolTable;
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

fn compile_run(
    heap: Vec<vm::Object>,
    symbol_table: SymbolTable,
    globals: Vec<Reference>,
    input: &str,
) -> std::result::Result<(vm::Object, Vec<vm::Object>, SymbolTable, Vec<Reference>), Vec<MonkeyError>>
{
    let lexer = Lexer::new(input.as_bytes());
    let mut parser = Parser::new(lexer);
    let ast = parser.parse()?;

    let mut compiler = Compiler::new();
    compiler.constants = heap;
    compiler.symbol_table = symbol_table;

    let byte_code = compiler.compile(&ast).map_err(|error| vec![error])?;
    let mut vm = VirtualMachine::new(byte_code);
    vm.globals = globals;

    vm.run().map_err(|error| vec![error])?;
    vm.top()
        .map(|obj| {
            (
                obj.clone(),
                vm.heap.clone(),
                compiler.symbol_table.clone(),
                vm.globals.clone(),
            )
        })
        .map_err(|error| vec![error])
}

pub fn repl(runtime: Runtime) -> io::Result<()> {
    let mut buffer = String::new();

    let env = Rc::new(RefCell::new(Environment::empty()));

    let mut heap = vec![vm::Object::Unit, vm::Object::False, vm::Object::True];
    let mut symbol_table = SymbolTable::new();
    let mut globals = vec![];

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
                match runtime {
                    Runtime::Interpreter => match interpret(env.clone(), line) {
                        Ok(obj) => {
                            println!("{}", obj);
                        }
                        Err(errors) => {
                            println!("{}", errors.iter().join(", "));
                        }
                    },
                    Runtime::VirtualMachine => {
                        match compile_run(heap.clone(), symbol_table.clone(), globals.clone(), line)
                        {
                            Ok((obj, h, s, g)) => {
                                heap = h;
                                symbol_table = s;
                                globals = g;
                                println!("{}", obj);
                            }
                            Err(errors) => {
                                println!("{}", errors.iter().join(", "));
                            }
                        }
                    }
                };
            }
        }

        buffer.clear();
    }
}
