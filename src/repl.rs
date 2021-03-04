use crate::lexer::Lexer;

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
                lexer.into_iter().for_each(|token| println!("{}", token));
            }
        }

        buffer.clear();
    }
}
