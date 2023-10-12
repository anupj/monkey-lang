// Temporarily allowing unused to
// avoid the slightly annoying dead_code lint
// warning messages
#![allow(unused)]

use crate::token::{Token, TokenType};

pub struct Lexer {
    input: String,
    position: i32, // current position in input (points to current char)
    read_position: i32, /* current reading position in input (after current
                    * char) */
    ch: char, /* current char under examination, Lexer only supports ASCII
               * character */
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        // In order to avoid calling read_char twice
        // characters that are numeric or alphabetic
        let mut should_read_char = true;

        let (token_type, literal) = match self.ch {
            '=' => {
                // peek ahead to see if the next char is "="
                if self.peek_char() == '=' {
                    // if it is then the string is "=="
                    // consume the next '='
                    self.read_char();
                    (TokenType::EQ, "==".to_string())
                } else {
                    (TokenType::ASSIGN, self.ch.to_string())
                }
            }
            '!' => {
                // peek ahead to see if the next char is "="
                if self.peek_char() == '=' {
                    // if it is then the string is "!="
                    // consume the next '='
                    self.read_char();
                    (TokenType::NOT_EQ, "!=".to_string())
                } else {
                    (TokenType::BANG, self.ch.to_string())
                }
            }
            '+' => (TokenType::PLUS, self.ch.to_string()),
            '-' => (TokenType::MINUS, self.ch.to_string()),
            '*' => (TokenType::ASTERISK, self.ch.to_string()),
            '/' => (TokenType::SLASH, self.ch.to_string()),
            '<' => (TokenType::LT, self.ch.to_string()),
            '>' => (TokenType::GT, self.ch.to_string()),
            ';' => (TokenType::SEMICOLON, self.ch.to_string()),
            '(' => (TokenType::LPAREN, self.ch.to_string()),
            ')' => (TokenType::RPAREN, self.ch.to_string()),
            ',' => (TokenType::COMMA, self.ch.to_string()),
            '{' => (TokenType::LBRACE, self.ch.to_string()),
            '}' => (TokenType::RBRACE, self.ch.to_string()),
            '\0' => (TokenType::EOF, "".to_string()),
            // If the character is alphabetic, read an
            // identifier
            ch if Self::is_letter(ch) => {
                let ident = self.read_identifier();
                let token_type = Self::lookup_ident(&ident);
                // don't call read_char again because
                // you called it inside `read_identifier()`
                should_read_char = false;
                (token_type, ident)
            }
            // If the character is numeric, read a number
            ch if ch.is_numeric() => {
                let number = self.read_number();
                // don't call read_char again because
                // you called it inside `read_number()`
                should_read_char = false;
                (TokenType::INT, number)
            }
            // if EOF or unknown character, set token to ILLEGAL
            _ => (TokenType::ILLEGAL, self.ch.to_string()),
        };

        // Conditionally advance the lexer to the next character
        if should_read_char {
            self.read_char();
        }
        Token {
            token_type,
            literal,
        }
    }

    // Skip over any whitespace characters
    fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }

    // The purpose of `read_char` is to give us
    // the next character and advance our position
    // in the input string.
    fn read_char(&mut self) {
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
            self.ch =
                self.input.chars().nth(self.read_position as usize).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    // `peek_char` is very similar to `read_char()`
    // except that it doesn't increment `lexer.position`.
    // We only want to "peek" ahead in the input and not
    // move around in it
    fn peek_char(&self) -> char {
        // Check if the next reading position exceeds the length of the input
        if self.read_position >= self.input.len() as i32 {
            // Return '\0' to signify EOF or no next character
            '\0'
        } else {
            // Otherwise, return the next character without changing the lexer
            // state
            self.input.chars().nth(self.read_position as usize).unwrap()
        }
    }

    // Read and return a number
    fn read_number(&mut self) -> String {
        let start_position = self.position;
        while self.ch.is_numeric() {
            self.read_char();
        }
        let end_position = self.position;
        self.input[start_position as usize..end_position as usize].to_string()
    }

    // Read and return the identifier string
    fn read_identifier(&mut self) -> String {
        let start_position = self.position;
        while Self::is_letter(self.ch) {
            self.read_char();
        }
        let end_position = self.position;
        self.input[start_position as usize..end_position as usize].to_string()
    }

    // Helper (associated) to determine if its a letter
    #[allow(clippy::manual_is_ascii_check)]
    fn is_letter(ch: char) -> bool {
        ('a'..='z').contains(&ch) || ('A'..='Z').contains(&ch) || (ch == '_')
    }

    // Helper (associated) function to determine whether an
    // identifier is a keyword
    fn lookup_ident(ident: &str) -> TokenType {
        match ident {
            "fn" => TokenType::FUNCTION,
            "let" => TokenType::LET,
            "true" => TokenType::TRUE,
            "false" => TokenType::FALSE,
            "if" => TokenType::IF,
            "else" => TokenType::ELSE,
            "return" => TokenType::RETURN,
            _ => TokenType::IDENT,
        }
    }
}
