use crate::token::*;

#[derive(Debug)]
struct Lexer {
    input: String,
    // current pos in input (points to current char)
    position: u32,
    // current reading pos in input (after current char)
    read_position: u32,
    // current char under examination
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        let mut lexer = Lexer {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    /// Reads the `char` pointed by
    /// `read_position` i.e. sets `self.ch`
    /// to the character that `read_position` points to;
    /// and advances `read_postion` by 1, and sets
    /// `position` to previous read_position.
    pub fn read_char(&mut self) {
        if self.read_position >= self.input.len() as u32 {
            self.ch = 0;
        } else {
            self.ch = self.input.chars().nth(self.read_position as usize).unwrap() as u8;
        }
        // We read the current `char`, now advance
        // our position in the `input` string.
        // Point `position` to the next character..
        self.position = self.read_position;
        // ..Point `read_position` to the character after that
        self.read_position += 1;
    }

    /// Look at the current character under
    /// examination `self.ch` and return a token
    /// depending on which character it is.
    /// Before returning the token advance
    /// our pointer by calling `read_char()`
    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();
        let next_t = match self.ch {
            0 => Token {
                token_type: EOF.to_string(),
                literal: "".to_string(),
            },
            b'=' => Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            b';' => Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            b'+' => Token {
                token_type: PLUS.to_string(),
                literal: "+".to_string(),
            },
            b'(' => Token {
                token_type: LPAREN.to_string(),
                literal: "(".to_string(),
            },
            b')' => Token {
                token_type: RPAREN.to_string(),
                literal: ")".to_string(),
            },
            b'{' => Token {
                token_type: LBRACE.to_string(),
                literal: "{".to_string(),
            },
            b'}' => Token {
                token_type: RBRACE.to_string(),
                literal: "}".to_string(),
            },
            b',' => Token {
                token_type: COMMA.to_string(),
                literal: ",".to_string(),
            },
            b'a'..=b'z' | b'A'..=b'Z' | b'_' => {
                let identifier = self.read_identifier();
                Token {
                    literal: identifier.to_string(),
                    token_type: Token::lookup_token_type(identifier).to_string(),
                }
            }
            b'0'..=b'9' => {
                let literal: String = self.read_number();
                return Token {
                    literal,
                    token_type: INT.to_string(),
                };
            }
            _ => Token {
                token_type: ILLEGAL.to_string(),
                literal: (self.ch as char).to_string(),
            },
        };

        // Now advance our pointer
        // so when we call `next_token` again
        // the `self.ch` field is already updated
        self.read_char();
        next_t
    }

    /// skip/eat/ignore whitespace
    fn skip_whitespace(&mut self) {
        while self.ch == b' ' || self.ch == b'\t' || self.ch == b'\n' || self.ch == b'\r' {
            self.read_char();
        }
    }

    /// Reads in an identifier and advances
    /// our lexer's positions until it encounters
    /// a non-letter character.
    fn read_identifier(&mut self) -> &str {
        let position = self.position as usize;
        while self.is_letter() {
            self.read_char();
        }
        let end_position = self.position as usize;

        // I have to compensate for an extra
        // call to `read_char` here
        self.read_position = self.position;
        self.position = self.position - 1;

        &self.input[position..end_position]
    }

    /// Check if the character under consideration
    /// is a letter
    fn is_letter(&self) -> bool {
        let ch = self.ch;
        (ch >= b'a' && ch <= b'z') || (ch >= b'A' && ch <= b'Z') || ch == b'_'
    }

    /// Check if the character under consideration
    /// is a number
    fn read_number(&mut self) -> String {
        let position = self.position as usize;
        while self.is_digit() {
            self.read_char();
        }
        self.input[position..self.position as usize].to_string()
    }

    /// Check if the character under consideration
    /// is a digit
    fn is_digit(&self) -> bool {
        let ch = self.ch;
        ch >= b'0' && ch <= b'9'
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token_symbols() {
        let input = "=+(){},;";

        let mut lex = Lexer::new(input.to_string());

        let test_tokens = vec![
            Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            Token {
                token_type: PLUS.to_string(),
                literal: "+".to_string(),
            },
            Token {
                token_type: LPAREN.to_string(),
                literal: "(".to_string(),
            },
            Token {
                token_type: RPAREN.to_string(),
                literal: ")".to_string(),
            },
            Token {
                token_type: LBRACE.to_string(),
                literal: "{".to_string(),
            },
            Token {
                token_type: RBRACE.to_string(),
                literal: "}".to_string(),
            },
            Token {
                token_type: COMMA.to_string(),
                literal: ",".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: EOF.to_string(),
                literal: "".to_string(),
            },
        ];

        for tt in test_tokens.iter() {
            let tok = lex.next_token();
            assert_eq!(tok.token_type, tt.token_type);
            assert_eq!(tok.literal, tt.literal);
        }
    }

    #[test]
    fn test_next_token_complete() {
        let input = r#"let five = 5;
    let ten = 10;

    let add = fn(x,y) {
          x + y;
    };

  let result = add(five, ten);"#;

        let mut lex = Lexer::new(input.to_string());

        let test_tokens = vec![
            Token {
                token_type: LET.to_string(),
                literal: "let".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "five".to_string(),
            },
            Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            Token {
                token_type: INT.to_string(),
                literal: "5".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: LET.to_string(),
                literal: "let".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "ten".to_string(),
            },
            Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            Token {
                token_type: INT.to_string(),
                literal: "10".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: LET.to_string(),
                literal: "let".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "add".to_string(),
            },
            Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            Token {
                token_type: FUNCTION.to_string(),
                literal: "fn".to_string(),
            },
            Token {
                token_type: LPAREN.to_string(),
                literal: "(".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "x".to_string(),
            },
            Token {
                token_type: COMMA.to_string(),
                literal: ",".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "y".to_string(),
            },
            Token {
                token_type: RPAREN.to_string(),
                literal: ")".to_string(),
            },
            Token {
                token_type: LBRACE.to_string(),
                literal: "{".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "x".to_string(),
            },
            Token {
                token_type: PLUS.to_string(),
                literal: "+".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "y".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: RBRACE.to_string(),
                literal: "}".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: LET.to_string(),
                literal: "let".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "result".to_string(),
            },
            Token {
                token_type: ASSIGN.to_string(),
                literal: "=".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "add".to_string(),
            },
            Token {
                token_type: LPAREN.to_string(),
                literal: "(".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "five".to_string(),
            },
            Token {
                token_type: COMMA.to_string(),
                literal: ",".to_string(),
            },
            Token {
                token_type: IDENT.to_string(),
                literal: "ten".to_string(),
            },
            Token {
                token_type: RPAREN.to_string(),
                literal: ")".to_string(),
            },
            Token {
                token_type: SEMICOLON.to_string(),
                literal: ";".to_string(),
            },
            Token {
                token_type: EOF.to_string(),
                literal: "".to_string(),
            },
        ];

        for tt in test_tokens.iter() {
            let tok = lex.next_token();
            assert_eq!(tok.token_type, tt.token_type);
            assert_eq!(tok.literal, tt.literal);
        }
    }
}
