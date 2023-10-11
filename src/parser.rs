#![allow(unused)]

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::TokenType;

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
    current_token: Token,
    peek_token: Token,
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
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();

        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    /// Start parsing the program
    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while self.current_token.token_type != TokenType::EOF {
            match self.parse_statement() {
                Some(Ok(statement)) => program.statements.push(statement),
                Some(Err(err)) => return Err(err),
                None => {}
            }
            self.next_token();
        }
        Ok(program)
    }

    /// The reason to potentially use Option<Result<_, _>> is to handle
    /// three distinct cases:
    ///  - Parsing was successful, and a statement was generated (Some(Ok(statement))).
    ///  - Parsing encountered an error (Some(Err(err))).
    ///  - The current token didn't lead to a statement, but this isn't an error condition (None).
    ///  In some parsers, you might encounter tokens that don't result in an error but also don't
    ///  produce a statement. Those tokens might be comments or some other sort of
    ///  ignorable input.
    fn parse_statement(&mut self) -> Option<Result<Box<dyn Statement>, ParsingError>> {
        match self.current_token.token_type {
            TokenType::LET => Some(self.parse_let_statement()),
            // Add more here as the language evolves
            _ => None,
        }
    }

    pub fn parse_let_statement(&mut self) -> Result<Box<dyn Statement>, ParsingError> {
        let mut stmt = LetStatement {
            token: self.current_token.clone(),
            // Initial values; these should be properly initialized
            name: Identifier {
                token: Token {
                    token_type: TokenType::ILLEGAL,
                    literal: "".to_string(),
                }, // placeholder
                value: "".to_string(), // placeholder
            },
            value: Box::new(NoneExpression), // placeholder
        };

        if !self.expect_peek(TokenType::IDENT) {
            return Err(ParsingError::UnexpectedToken {
                expected: "IDENT".to_string(),
                found: format!("{:?}", self.peek_token.token_type),
            });
        }

        stmt.name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        if !self.expect_peek(TokenType::ASSIGN) {
            return Err(ParsingError::UnexpectedToken {
                expected: "ASSIGN".to_string(),
                found: format!("{:?}", self.peek_token.token_type),
            });
        }

        // TODO: Skipping the expressions until we encounter a semicolon
        while !self.cur_token_is(TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Box::new(stmt))
    }

    fn cur_token_is(&self, t: TokenType) -> bool {
        self.current_token.token_type == t
    }

    fn peek_token_is(&self, t: TokenType) -> bool {
        self.peek_token.token_type == t
    }

    /// Expect the `TokenType` to be of type `t`
    /// If it is of type `t` then advance to the
    /// next token and return `true`
    /// else just return `false`
    fn expect_peek(&mut self, t: TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            false
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }
}
