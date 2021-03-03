use monkey_lang::repl::*;

fn main() -> Result<(), std::io::Error> {
    println!("This is the Monkey programming language!");
    println!("Feel free to type in commands!");

    repl()?;

    Ok(())
}
