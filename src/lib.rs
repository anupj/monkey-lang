pub mod lexer;
pub mod token;

use std::error::Error;

type MyResult<T> = Result<T, Box<dyn Error>>;

pub fn run() -> MyResult<()> {
    println!("Hello from lib");
    Ok(())
}
