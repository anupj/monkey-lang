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
        Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        }
    }

    pub fn next_token(&mut self) -> Token {
        todo!()
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
                Type: ASSIGN.to_string(),
                Literal: "=".to_string(),
            },
            Token {
                Type: ASSIGN.to_string(),
                Literal: "+".to_string(),
            },
            Token {
                Type: LPAREN.to_string(),
                Literal: "(".to_string(),
            },
            Token {
                Type: RPAREN.to_string(),
                Literal: ")".to_string(),
            },
            Token {
                Type: LBRACE.to_string(),
                Literal: "{".to_string(),
            },
            Token {
                Type: RBRACE.to_string(),
                Literal: "}".to_string(),
            },
            Token {
                Type: COMMA.to_string(),
                Literal: ",".to_string(),
            },
            Token {
                Type: SEMICOLON.to_string(),
                Literal: ";".to_string(),
            },
            Token {
                Type: EOF.to_string(),
                Literal: "".to_string(),
            },
        ];

        for tt in test_tokens.iter() {
            let tok = lex.next_token();
            assert_eq!(tok.Type, tt.Type);
            assert_eq!(tok.Literal, tt.Literal);
        }
    }
}
