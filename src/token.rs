// Defining this as `String` allows us
// to use many different values as `TokenType`s
type TokenType = String;

// Various token types used by monkey-lang
// -- begin token constants --

// Signifies a token/character we don't know about
pub const ILLEGAL: &str = "ILLEGAL";
// "end of file" tells our parser that it can stop
pub const EOF: &str = "EOF";

// Identifiers + literals
pub const IDENT: &str = "IDENT"; // add, foobar, x, y, ...
pub const INT: &str = "INT"; // 134356

// Operators
pub const ASSIGN: &str = "=";
pub const PLUS: &str = "+";

// Delimiters
pub const COMMA: &str = ",";
pub const SEMICOLON: &str = ";";

pub const LPAREN: &str = "(";
pub const RPAREN: &str = ")";
pub const LBRACE: &str = "{";
pub const RBRACE: &str = "}";

// Keywords
pub const FUNCTION: &str = "FUNCTION";
pub const LET: &str = "LET";
// -- end token constants --

pub struct Token {
    pub token_type: TokenType,
    pub literal: String,
}
