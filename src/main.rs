fn main() {
    // Check if `run` from the library
    // returns an error..
    if let Err(e) = monkey_lang::start_repl() {
        // if it does then eprint it and
        // `eprintln` prints to STDERR instead
        // of to STDOUT
        eprintln!("{}", e);
        // exit the program with a nonzero value
        // to indicate an error
        std::process::exit(1);
    }
}
