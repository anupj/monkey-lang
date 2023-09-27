// Temporarily allowing unused to
// avoid the slightly annoying dead_code lint
// warning messages
#![allow(unused)]

use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: i32,      // current position in input (points to current char)
    read_position: i32, // current reading position in input (after current char)
    ch: char,           // current char under examination
                        /* Lexer only supports ASCII character */
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: '\0',
        };
        // Call `read_char` so that the
        // Lexer is in fully working state before
        // anyone calls `next_token()`, with
        // `position` and `read_position` already
        // initialised
        lexer.read_char();
        lexer
    }

    // The purpose of `read_char` is to give us
    // the next character and advance our position
    // in the input string.
    pub fn read_char(&mut self) {
        // First check if we've reached the end of the
        // input..
        if self.read_position >= self.input.len() as i32 {
            // .. and if we have reached the end
            // then set `ch` to '\0'
            // which signifies *we haven't read anything yet*
            // or *end of file*.
            self.ch = '\0'; // end of file OR haven't read anything yet
        } else {
            // if we haven't reached the end of the input yet then
            // it sets `ch` to the next character
            // remember that `read_position` always points to the next
            // character
            self.ch = self.input.chars().nth(self.read_position as usize).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let token = match self.ch {
            '=' => Token {
                token_type: TokenType::ASSIGN,
                literal: self.ch.to_string(),
            },
            ';' => Token {
                token_type: TokenType::SEMICOLON,
                literal: self.ch.to_string(),
            },
            '(' => Token {
                token_type: TokenType::LPAREN,
                literal: self.ch.to_string(),
            },
            ')' => Token {
                token_type: TokenType::RPAREN,
                literal: self.ch.to_string(),
            },
            ',' => Token {
                token_type: TokenType::COMMA,
                literal: self.ch.to_string(),
            },
            '+' => Token {
                token_type: TokenType::PLUS,
                literal: self.ch.to_string(),
            },
            '{' => Token {
                token_type: TokenType::LBRACE,
                literal: self.ch.to_string(),
            },
            '}' => Token {
                token_type: TokenType::RBRACE,
                literal: self.ch.to_string(),
            },
            '\0' => Token {
                token_type: TokenType::EOF,
                literal: "".to_string(),
            },
            _ => Token {
                token_type: TokenType::ILLEGAL,
                literal: self.ch.to_string(),
            },
        };

        self.read_char();
        token
    }

    // Skip over any whitespace characters
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
}
