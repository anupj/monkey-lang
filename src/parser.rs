#![allow(unused)]
use std::error::Error;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenType;

/// The `Parser` has three fields:
/// - `Lexer` is an instance of the lexer
///    on which we repeatedly call `NextToken()`
///    to get the next token in the input.
/// - `current_token` and `peek_token` point to the
///    current and the next token.
/// Both are important: we need to look at the curToken,
/// which is the current token under examination, to decide
/// what to do next, and we also need peekToken for this
/// decision if curToken doesn’t give us enough information.
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: TokenType,
    peek_token: TokenType,
}

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken { expected: String, found: String },
    UnexpectedEOF,
    Unknown,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        // Read two tokens, so `current_token` and `peek_token`
        // are both set
        let current_token = lexer.next_token().token_type;
        let peek_token = lexer.next_token().token_type;

        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    /// Start parsing the program
    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        todo!()
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().token_type;
    }
}
