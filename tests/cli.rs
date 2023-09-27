use monkey_lang::lexer::*;
use monkey_lang::token::*;

#[test]
fn test_next_token_single_chars() {
    let input = "=+(){},;".to_string();
    let mut lexer = Lexer::new(input);

    let tests = vec![
        Token {
            token_type: TokenType::ASSIGN,
            literal: "=".to_string(),
        },
        Token {
            token_type: TokenType::PLUS,
            literal: "+".to_string(),
        },
        Token {
            token_type: TokenType::LPAREN,
            literal: "(".to_string(),
        },
        Token {
            token_type: TokenType::RPAREN,
            literal: ")".to_string(),
        },
        Token {
            token_type: TokenType::LBRACE,
            literal: "{".to_string(),
        },
        Token {
            token_type: TokenType::RBRACE,
            literal: "}".to_string(),
        },
        Token {
            token_type: TokenType::COMMA,
            literal: ",".to_string(),
        },
        Token {
            token_type: TokenType::SEMICOLON,
            literal: ";".to_string(),
        },
    ];

    for (i, expected_token) in tests.iter().enumerate() {
        let token = lexer.next_token();
        assert_eq!(&token, expected_token, "tests[{}] - token_type wrong", i);
    }
}
