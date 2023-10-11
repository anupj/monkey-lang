use monkey_lang::ast::*;
use monkey_lang::lexer::*;
use monkey_lang::parser::*;
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

#[test]
fn test_next_token_complex_input() {
    let input = r#"let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"#;

    let mut lexer = Lexer::new(input.to_string());

    let expected_tokens = vec![
        (TokenType::LET, "let"),
        (TokenType::IDENT, "five"),
        (TokenType::ASSIGN, "="),
        (TokenType::INT, "5"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::LET, "let"),
        (TokenType::IDENT, "ten"),
        (TokenType::ASSIGN, "="),
        (TokenType::INT, "10"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::LET, "let"),
        (TokenType::IDENT, "add"),
        (TokenType::ASSIGN, "="),
        (TokenType::FUNCTION, "fn"),
        (TokenType::LPAREN, "("),
        (TokenType::IDENT, "x"),
        (TokenType::COMMA, ","),
        (TokenType::IDENT, "y"),
        (TokenType::RPAREN, ")"),
        (TokenType::LBRACE, "{"),
        (TokenType::IDENT, "x"),
        (TokenType::PLUS, "+"),
        (TokenType::IDENT, "y"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::RBRACE, "}"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::LET, "let"),
        (TokenType::IDENT, "result"),
        (TokenType::ASSIGN, "="),
        (TokenType::IDENT, "add"),
        (TokenType::LPAREN, "("),
        (TokenType::IDENT, "five"),
        (TokenType::COMMA, ","),
        (TokenType::IDENT, "ten"),
        (TokenType::RPAREN, ")"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::BANG, "!"),
        (TokenType::MINUS, "-"),
        (TokenType::SLASH, "/"),
        (TokenType::ASTERISK, "*"),
        (TokenType::INT, "5"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::INT, "5"),
        (TokenType::LT, "<"),
        (TokenType::INT, "10"),
        (TokenType::GT, ">"),
        (TokenType::INT, "5"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::IF, "if"),
        (TokenType::LPAREN, "("),
        (TokenType::INT, "5"),
        (TokenType::LT, "<"),
        (TokenType::INT, "10"),
        (TokenType::RPAREN, ")"),
        (TokenType::LBRACE, "{"),
        (TokenType::RETURN, "return"),
        (TokenType::TRUE, "true"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::RBRACE, "}"),
        (TokenType::ELSE, "else"),
        (TokenType::LBRACE, "{"),
        (TokenType::RETURN, "return"),
        (TokenType::FALSE, "false"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::RBRACE, "}"),
        (TokenType::INT, "10"),
        (TokenType::EQ, "=="),
        (TokenType::INT, "10"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::INT, "10"),
        (TokenType::NOT_EQ, "!="),
        (TokenType::INT, "9"),
        (TokenType::SEMICOLON, ";"),
        (TokenType::EOF, ""),
    ];

    for (expected_type, expected_literal) in expected_tokens {
        let token = lexer.next_token();
        assert_eq!(token.token_type, expected_type);
        assert_eq!(token.literal, expected_literal);
    }
}

#[test]
fn test_let_statements() {
    let input = String::from(
        "let x = 5;
            let y = 10;
            let foobar = 838383;",
    );

    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);

    let program = parser
        .parse_program()
        .expect("Something went wrong with the parser");

    // This might be unnecessary but doing it anyway because
    // its in the book
    check_parser_errors(&parser);

    assert!(
        !program.statements.is_empty(),
        "Program should not be empty"
    );

    // Check that you have exactly 3 "statements"
    assert_eq!(
        program.statements.len(),
        3,
        "The program should have 3 `let` statements."
    );

    let tests = ["x", "y", "foobar"];

    for (i, test_case) in tests.iter().enumerate() {
        let stmt = &program.statements[i];
        assert!(
            test_let_statement(stmt.as_ref(), test_case),
            "Failed `test_let_statement`"
        );
    }
}

#[test]
fn test_return_statement() {
    let input = String::from(
        "return 5;
return 10;
return 993322;",
    );

    let mut lexer = Lexer::new(input);
    let mut parser = Parser::new(&mut lexer);

    match parser.parse_program() {
        Ok(program) => {
            check_parser_errors(&parser);
            assert!(
                !program.statements.is_empty(),
                "Program should not be empty"
            );

            assert_eq!(
                program.statements.len(),
                3,
                "program.Statements does not contain 3 statements. got={}",
                program.statements.len()
            );

            for stmt in &program.statements {
                match stmt.as_any().downcast_ref::<ReturnStatement>() {
                    Some(return_stmt) => {
                        assert_eq!(return_stmt.token_literal(), "return");
                    }
                    None => panic!("stmt not ReturnStament. got={:?}", stmt.as_any().type_id()),
                }
            }
        }
        Err(e) => panic!("Failed to parse program: {:?}", e),
    }
}

// TODO: delete this at a later date if deemed
// unnecessary as we already have a `Result` unwrap
// check in our test method
fn check_parser_errors(p: &Parser) {
    let errors = p.errors();
    if errors.is_empty() {
        return;
    }
    panic!("Parser has {} errors: {:?}", errors.len(), errors);
}

fn test_let_statement(stmt: &dyn Statement, expected: &str) -> bool {
    // I am not using `assert_eq` here because I want to return
    // false
    if stmt.token_literal() != "let" {
        panic!("s.TokenLiteral not 'let'. got={}", stmt.token_literal());
    }

    if let Some(let_stmt) = stmt.as_any().downcast_ref::<LetStatement>() {
        if let_stmt.name.value != expected {
            panic!(
                "letStmt.Name.Value not '{}'. got={}",
                expected, let_stmt.name.value
            );
        }

        if let_stmt.name.token_literal() != expected {
            panic!(
                "letStmt.Name.TokenLiteral() not '{}'. got={}",
                expected,
                let_stmt.name.token_literal()
            );
        }
    } else {
        panic!("s is not LetStatement.");
    }

    true
}
