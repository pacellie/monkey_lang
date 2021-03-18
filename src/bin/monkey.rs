use monkey_lang::repl::*;

use std::env;

fn main() -> Result<(), std::io::Error> {
    println!("This is the Monkey programming language!");
    println!("Feel free to type in commands!");

    let args: Vec<String> = env::args().skip(1).collect();

    let runtime = match args.len() {
        1 => match args[0].as_str() {
            "eval" => Runtime::Interpreter,
            "vm" => Runtime::VirtualMachine,
            _ => panic!(),
        },
        _ => panic!(),
    };

    repl(runtime)?;

    Ok(())
}
