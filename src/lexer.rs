use crate::token::*;

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
        let next_t = match self.ch {
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
            _ => Token {
                token_type: EOF.to_string(),
                literal: "".to_string(),
            },
        };

        // Now advance our pointer
        // so when we call `next_token` again
        // the `self.ch` field is already updated
        self.read_char();
        next_t
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
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
}
