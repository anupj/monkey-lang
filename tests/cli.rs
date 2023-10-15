use std::any::Any;

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
                    None => panic!(
                        "stmt not ReturnStament. got={:?}",
                        stmt.as_any().type_id()
                    ),
                }
            }
        }
        Err(e) => panic!("Failed to parse program: {:?}", e),
    }
}

#[test]
fn test_to_string() {
    let program = Program {
        statements: vec![Box::new(LetStatement {
            token: Token {
                token_type: TokenType::LET,
                literal: "let".to_string(),
            },
            name: Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: "myVar".to_string(),
                },
                value: "myVar".to_string(),
            },
            value: Box::new(Identifier {
                token: Token {
                    token_type: TokenType::IDENT,
                    literal: "anotherVar".to_string(),
                },
                value: "anotherVar".to_string(),
            }),
        })],
    };
    assert_eq!(program.to_string(), "let myVar = anotherVar;");
}

#[test]
fn test_identifier_expression() {
    let input = "foobar";

    let mut lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = parser
        .parse_program()
        .expect("There was a problem parsing the program");

    check_parser_errors(&parser);

    assert_eq!(
        program.statements.len(),
        1,
        "program does not contain 1 statement. got={}",
        program.statements.len()
    );

    let stmt = &program.statements[0];
    if let Some(exp_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>()
    {
        if let Some(ident) =
            exp_stmt.expression.as_any().downcast_ref::<Identifier>()
        {
            assert_eq!(
                ident.value, "foobar",
                "ident.value not {}. got={}",
                "foobar", ident.value
            );
            assert_eq!(
                ident.token_literal(),
                "foobar",
                "ident.token_literal() not {}. got={}",
                "foobar",
                ident.token_literal()
            );
        } else {
            panic!("exp_stmt.expression is not Identifier.");
        }
    } else {
        panic!("program.statements[0] is not ExpressionStatement.");
    }
}

#[test]
fn test_integer_literal_expression() {
    let input = "5;";

    let mut lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(&mut lexer);
    let program = parser.parse_program().expect("Failed to parse the program");

    assert_eq!(
        program.statements.len(),
        1,
        "program does not have enough statements. got={}",
        program.statements.len()
    );

    let stmt = &program.statements[0];
    if let Some(exp_stmt) = stmt.as_any().downcast_ref::<ExpressionStatement>()
    {
        if let Some(int_literal) = exp_stmt
            .expression
            .as_any()
            .downcast_ref::<IntegerLiteral>()
        {
            assert_eq!(
                int_literal.value, 5,
                "literal.value not {}. got={}",
                5, int_literal.value
            );
            assert_eq!(
                int_literal.token_literal(),
                "5",
                "int_literal.token_literal() not {}. got={}",
                "5",
                int_literal.token_literal()
            );
        } else {
            panic!("exp not IntegerLiteral. got={}", exp_stmt.expression);
        }
    } else {
        panic!(
            "program.Statements[0] is not ExpressionStatement. got={}",
            stmt
        );
    }
}

#[test]
fn test_parsing_prefix_expressions() {
    let prefix_tests = vec![("!5;", "!", 5), ("-15;", "-", 15)];
    for (input, operator, integer_value) in prefix_tests {
        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer);
        let program =
            parser.parse_program().expect("Failed to parse the program");

        assert_eq!(
            program.statements.len(),
            1,
            "program.Statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        let stmt = &program.statements[0];

        if let Some(exp_stmt) =
            stmt.as_any().downcast_ref::<ExpressionStatement>()
        {
            if let Some(prefix_exp) = exp_stmt
                .expression
                .as_any()
                .downcast_ref::<PrefixExpression>()
            {
                assert_eq!(
                    prefix_exp.operator, operator,
                    "exp.Operator is not '{}'. got={}",
                    operator, prefix_exp.operator
                );

                test_integer_literal(prefix_exp.right.as_ref(), integer_value);
            } else {
                panic!(
                    "stmt.Expression is not PrefixExpression. got={}",
                    exp_stmt.expression
                );
            }
        } else {
            panic!(
                "program.Statements[0] is not ExpressionStatement. got={}",
                stmt
            );
        }
    }
}

#[test]
fn test_parsing_infix_expressions() {
    let infix_tests = vec![
        ("5 + 5;", 5, "+", 5),
        ("5 - 5;", 5, "-", 5),
        ("5 * 5;", 5, "*", 5),
        ("5 / 5;", 5, "/", 5),
        ("5 > 5;", 5, ">", 5),
        ("5 < 5;", 5, "<", 5),
        ("5 == 5;", 5, "==", 5),
        ("5 != 5;", 5, "!=", 5),
    ];

    for (input, left_value, operator, right_value) in infix_tests {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program().expect("Failed to parse program");

        assert_eq!(
            program.statements.len(),
            1,
            "program.statements does not contain 1 statement. got={}",
            program.statements.len()
        );

        match program.statements[0]
            .as_any()
            .downcast_ref::<ExpressionStatement>()
        {
            Some(expression_statement) => {
                match expression_statement
                    .expression
                    .as_any()
                    .downcast_ref::<InfixExpression>()
                {
                    Some(infix_expression) => {
                        test_integer_literal(
                            infix_expression
                                .left
                                .as_any()
                                .downcast_ref::<IntegerLiteral>()
                                .unwrap(),
                            left_value,
                        );
                        assert_eq!(
                            &infix_expression.operator, operator,
                            "exp.Operator is not {}. got={}",
                            operator, infix_expression.operator
                        );
                        test_integer_literal(
                            infix_expression
                                .right
                                .as_any()
                                .downcast_ref::<IntegerLiteral>()
                                .unwrap(),
                            right_value,
                        );
                    }
                    None => panic!(
                        "exp is not InfixExpression. got={}",
                        expression_statement.expression
                    ),
                }
            }
            None => panic!(
                "program.Statements[0] is not ExpressionStatement. got={}",
                program.statements[0]
            ),
        }
    }
}

#[test]
fn test_operator_precedence_parsing() {
    let tests = vec![
        ("-a * b", "((-a) * b)"),
        ("!-a", "(!(-a))"),
        ("a + b + c", "((a + b) + c)"),
        ("a + b - c", "((a + b) - c)"),
        ("a * b * c", "((a * b) * c)"),
        ("a * b / c", "((a * b) / c)"),
        ("a + b / c", "(a + (b / c))"),
        ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
        ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
        ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
        ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
        ),
    ];

    for (input, expected) in tests.iter() {
        let mut l = Lexer::new(input.to_string());
        let mut p = Parser::new(&mut l);
        let program = p.parse_program().expect("Failed to parse program");
        // You can add a function here to check for parsing errors if needed.

        let actual = program.to_string();
        assert_eq!(actual, *expected, "expected={}, got={}", expected, actual);
    }
}

// ---- Helper methods ----- //

// TODO: delete this method at a later date if deemed
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

fn test_integer_literal(exp: &dyn Expression, value: i64) {
    if let Some(int_literal) = exp.as_any().downcast_ref::<IntegerLiteral>() {
        assert_eq!(
            int_literal.value, value,
            "int_literal.value not {}. got={}",
            value, int_literal.value
        );

        let expected_token_literal = value.to_string();
        assert_eq!(
            int_literal.token_literal(),
            expected_token_literal,
            "int_literal.token_literal not {}. got={}",
            expected_token_literal,
            int_literal.token_literal()
        );
    } else {
        panic!("Expression not IntegerLiteral. got={}", exp);
    }
}

fn test_identifier(exp: &dyn Expression, value: &str) -> bool {
    match exp.as_any().downcast_ref::<Identifier>() {
        Some(ident) => {
            if ident.value != value {
                eprintln!("ident.Value not {}. got={}", value, ident.value);
                return false;
            }

            if ident.token_literal() != value {
                eprintln!(
                    "ident.TokenLiteral not {}. got={}",
                    value,
                    ident.token_literal()
                );
                return false;
            }

            true
        }
        None => {
            eprintln!("exp not Identifier. got={}", exp);
            false
        }
    }
}

fn test_literal_expression(exp: &dyn Expression, expected: &dyn Any) -> bool {
    match expected.downcast_ref::<i64>() {
        Some(val) => {
            test_integer_literal(exp, *val);
            return true;
        }
        None => {}
    }

    match expected.downcast_ref::<&str>() {
        Some(val) => return test_identifier(exp, val),
        None => {}
    }

    eprintln!("type of exp not handled. got={}", exp);
    false
}

fn test_infix_expression(
    exp: &dyn Expression,
    left: &dyn Any,
    operator: &str,
    right: &dyn Any,
) -> bool {
    match exp.as_any().downcast_ref::<InfixExpression>() {
        Some(op_exp) => {
            if !test_literal_expression(&*op_exp.left, left) {
                return false;
            }

            if op_exp.operator != operator {
                eprintln!(
                    "exp.Operator is not '{}'. got={}",
                    operator, op_exp.operator
                );
                return false;
            }

            if !test_literal_expression(&*op_exp.right, right) {
                return false;
            }
            true
        }
        None => {
            eprintln!("exp is not InfixExpression. got={}", exp);
            false
        }
    }
}
