// Temporarily allowing unused to
// avoid the slightly annoying dead_code lint
// warning messages
#![allow(unused)]

// Various token types used by monkey-lang
#[allow(non_camel_case_types)]
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum TokenType {
    // Signifies a token/character we don't know about
    ILLEGAL,
    // "end of file" tells our parser that it can stop
    EOF,

    // Identifiers + literals
    IDENT, // add, foobar, x, y, ...
    INT,   // 134356

    // Operators
    ASSIGN,   // =
    PLUS,     // +
    MINUS,    // -
    BANG,     // !
    ASTERISK, // *
    SLASH,    // /
    LT,       // <
    GT,       // >
    EQ,       // ==
    NOT_EQ,   // !=

    // Delimiters
    COMMA,     // ,
    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    // Keywords
    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

use std::fmt;

// This should allow me to use the
// `to_string()` method as per this SO link
// https://shorturl.at/cvJZ4
impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let printable = match *self {
            TokenType::ILLEGAL => "ILLEGAL",
            TokenType::EOF => "EOF",
            TokenType::IDENT => "IDENT",
            TokenType::INT => "INT",
            TokenType::ASSIGN => "=",
            TokenType::PLUS => "+",
            TokenType::MINUS => "-",
            TokenType::BANG => "!",
            TokenType::ASTERISK => "*",
            TokenType::SLASH => "/",
            TokenType::LT => "<",
            TokenType::GT => ">",
            TokenType::EQ => "==",
            TokenType::NOT_EQ => "!=",
            TokenType::COMMA => ",",
            TokenType::SEMICOLON => ";",
            TokenType::LPAREN => "(",
            TokenType::RPAREN => ")",
            TokenType::LBRACE => "{",
            TokenType::RBRACE => "}",
            TokenType::FUNCTION => "FUNCTION",
            TokenType::LET => "LET",
            TokenType::TRUE => "true",
            TokenType::FALSE => "false",
            TokenType::IF => "if",
            TokenType::ELSE => "else",
            TokenType::RETURN => "return",
        };
        write!(f, "{}", printable)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
