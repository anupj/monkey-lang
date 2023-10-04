#![allow(unused)]
use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::TokenType;

/// Ok, so now we are going to write a parser for Monkey lang. Specifically we are
/// going to write a top down operator precedence parser called recursive descent
/// parser.
/// We’re going to start by parsing statements: `let` and `return` statements. When we
/// can parse statements and the basic structure of our parser stands, we will look
/// at expressions and how to parse these. Afterwards we extend the parser to make
/// it capable of parsing a large subset of the Monkey programming language. As we
/// go along we build up the necessary structures for our AST.
///
/// In Monkey, variable bindings are statements of the following form:
/// ```monkey
/// let x = 5;
/// let y = 10;
/// let foobar = add(5, 5);
/// let barfoo = 5 * 5 / 10 + 18 - add(5, 5) + multiply(124);
/// let anotherName = barfoo;
/// ```
/// These statements are called “let statements” and bind a value to the given
/// name. let x = 5; binds the value 5 to the name x. Our job is
/// to parse let statements correctly. For now we’re going to skip parsing the
/// expressions that produce the value of a given variable binding and come back
/// to this later - as soon as we know how to parse expressions on their own.
///
///  In this example we can see let statements - of the following form:
///    `let <identifier> = <expression>;`
///
/// A let statement in Monkey consists of two changing parts: an identifier and an
/// expression. In the example above x, y and add are identifiers. 10, 15 and the
/// function literal are expressions.

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: TokenType,
    peek_token: TokenType,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        // Read two tokens, so `current_token` and `peek_token`
        // are both set
        let current_token = lexer.next_token().token_type;
        let peek_token = lexer.next_token().token_type;

        Self {
            lexer,
            current_token,
            peek_token,
        }
    }

    /// Start parsing the program
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program {
            statements: Vec::new(),
        };

        // loop till we hit `end of file` token type
        while self.current_token != TokenType::EOF {
            let statement = match self.current_token {
                TokenType::LET => self.parse_let_statement(),
                TokenType::RETURN => self.parse_return_statement(),
                TokenType::IF => self.parse_if_statement(),
                _ => {
                    // handle error or add more cases
                    None
                }
            };

            if let Some(st) = statement {
                program.statements.push(st);
            }
            self.next_token();
        }

        program
    }

    /// Parse let statement
    /// TODO: revisit this to see if this should return a `Result` instead
    fn parse_let_statement(&mut self) -> Option<Box<dyn Statement>> {
        // advance token
        self.next_token();
        let identifier = self.parse_identifier();
        self.next_token();

        if self.current_token != TokenType::ASSIGN {
            // TODO: handle error "no equal sign!"
            return None;
        }

        self.next_token();
        let value = self.parse_expression();

        let let_statement = LetStatement {
            token_type: TokenType::LET,
            name: identifier,
            value,
        };

        Some(Box::new(let_statement))
    }

    fn parse_identifier(&mut self) -> Identifier {
        Identifier {
            token_type: self.current_token.clone(),
            value: "".to_string(), // TODO: fill this based on lexer output?
        }
    }

    fn parse_expression(&mut self) -> Box<dyn Expression> {
        if self.current_token == TokenType::INT {
            if self.peek_token == TokenType::PLUS {
                return self.parse_operator_expression();
            } else {
                return self.parse_integer_literal();
            }
        } else if self.current_token == TokenType::LPAREN {
            return self.parse_grouped_expression();
        }
        // handle other cases or errors
        todo!()
    }

    fn parse_operator_expression(&mut self) -> Box<dyn Expression> {
        let left = self.parse_integer_literal();
        self.next_token();
        let operator = self.current_token.clone();
        self.next_token();
        let right = self.parse_expression();

        let operator_expression = OperatorExpression {
            left,
            operator,
            right,
        };

        Box::new(operator_expression)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token().token_type;
    }
}
