pub mod ast;
pub mod lexer;
pub mod parser;
pub mod token;

use std::error::Error;
use std::io::{self, Write};

use crate::lexer::Lexer;
use crate::token::TokenType;

type MyResult<T> = Result<T, Box<dyn Error>>;

pub fn start_repl() -> MyResult<()> {
    loop {
        // Print a prompt for the user
        print!(">> ");
        io::stdout().flush()?; // Flush the output buffer

        // Read input from the user
        let mut input = String::new();
        io::stdin().read_line(&mut input)?;

        // Initialize the lexer and tokenize the input
        let mut lexer = Lexer::new(input);

        loop {
            let token = lexer.next_token();
            if token.token_type == TokenType::EOF {
                break;
            }

            // Print the token to the console
            println!("{:?}", token);
        }
    }
}
