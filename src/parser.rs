#![allow(unused)]

use std::collections::HashMap;

use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;
use crate::TokenType;

/// We are going to implement the Pratt parser.
/// A Pratt parser’s main idea is the association
/// of parsing functions (which Pratt calls “semantic code”)
/// with token types. Whenever this token type is encountered,
/// the parsing functions are called to parse the appropriate
/// expression and return an AST node that represents it.
/// Each token type can have up to two parsing functions
/// associated with it, depending on whether the token is
/// found in a prefix or an infix position.
/// The first thing we need to do is to setup these
/// associations. We define two types of functions:
/// a **prefix** parsing function and an **infix** parsing function.
// `PrefixParseFn` is responsible for parsing unary expressions and
// other expressions that don't require a left-hand side, like `-5` or `!true`
type PrefixParseFn =
    fn(&mut Parser) -> Result<Box<dyn Expression>, ParsingError>;
// `InfixParseFn` is responsible for parsing expressions that do
// require a left-hand side, like binary operations: `5 + 5` or `5 * 7`.
type InfixParseFn = fn(
    &mut Parser,
    Box<dyn Expression>,
) -> Result<Box<dyn Expression>, ParsingError>;

/// The `Parser` has the following fields:
/// - `Lexer` is an instance of the lexer on which we repeatedly call
///   `NextToken()` to get the next token in the input.
/// - `current_token` and `peek_token` point to the current and the next token.
///   Both are important: we need to look at the current_token, which is the
///   current token under examination, to decide what to do next, and we also
///   need peek_token for this decision if curToken doesn’t give us enough
///   information.
/// - `errors`: stores parsing errors (if any)
/// - `prefix_parse_fns`: Maps TokenType to `PrefixParseFn`
/// - `infix_parse_fns`: Maps TokenType to `InfixParseFn`
pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    current_token: Token,
    peek_token: Token,
    errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenType, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenType, InfixParseFn>,
}

#[derive(Debug)]
pub enum ParsingError {
    UnexpectedToken { expected: String, found: String },
    UnexpectedEOF,
    Unknown,
}

#[derive(PartialOrd, PartialEq, Debug, Clone, Copy)]
enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Self {
        // Read two tokens, so `current_token` and `peek_token`
        // are both set
        let current_token = lexer.next_token();
        let peek_token = lexer.next_token();
        let errors = Vec::new();
        let mut prefix_parse_fns = HashMap::new();
        let mut infix_parse_fns = HashMap::new();

        prefix_parse_fns.insert(
            TokenType::IDENT,
            Parser::parse_identifier as PrefixParseFn,
        );
        prefix_parse_fns.insert(
            TokenType::INT,
            Parser::parse_integer_literal as PrefixParseFn,
        );

        Self {
            lexer,
            current_token,
            peek_token,
            errors,
            prefix_parse_fns,
            infix_parse_fns,
        }
    }

    /// Start parsing the program
    pub fn parse_program(&mut self) -> Result<Program, ParsingError> {
        let mut program = Program {
            statements: Vec::new(),
        };

        while !self.cur_token_is(&TokenType::EOF) {
            match self.parse_statement() {
                Some(Ok(statement)) => program.statements.push(statement),
                Some(Err(err)) => return Err(err),
                None => {}
            }
            self.next_token();
        }
        Ok(program)
    }

    /// The reason to potentially use Option<Result<_, _>> is to handle
    /// three distinct cases:
    ///  - Parsing was successful, and a statement was generated
    ///    (Some(Ok(statement))).
    ///  - Parsing encountered an error (Some(Err(err))).
    ///  - The current token didn't lead to a statement, but this isn't an error
    ///    condition (None).
    ///  In some parsers, you might encounter tokens that don't result in an
    /// error but also don't  produce a statement. Those tokens might be
    /// comments or some other sort of  ignorable input.
    fn parse_statement(
        &mut self,
    ) -> Option<Result<Box<dyn Statement>, ParsingError>> {
        match self.current_token.token_type {
            TokenType::LET => Some(self.parse_let_statement()),
            TokenType::RETURN => Some(self.parse_return_statement()),
            // Since the only two real statement types in Monkey are
            // let and return statements, we try to parse expression
            // statements if we don't encounter one of the other two.
            _ => Some(Ok(self.parse_expression_statement())),
        }
    }

    fn parse_expression_statement(&mut self) -> Box<dyn Statement> {
        let stmt = ExpressionStatement {
            token: self.current_token.clone(),
            expression: self.parse_expression(Precedence::Lowest),
        };

        // Check for an optional semicolon,
        // If the `peek`d token is a `;`, then
        // advance so that its the `current_token`
        // This is because we want expression statements to
        // have optional semicolons
        if self.peek_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Box::new(stmt)
    }

    fn parse_expression(
        &mut self,
        _precedence: Precedence,
    ) -> Box<dyn Expression> {
        // Look up if there's a prefix parsing function associated with the
        // current `Token`
        if let Some(prefix) =
            self.prefix_parse_fns.get(&self.current_token.token_type)
        {
            if let Ok(expr) = prefix(self) {
                return expr;
            }
        }

        // If no matching function, return a placeholder or an error
        Box::new(NoneExpression)
    }

    /// Important function to parse the identifier
    /// Note that this is an associated `fn`
    fn parse_identifier(
        parser: &mut Parser,
    ) -> Result<Box<dyn Expression>, ParsingError> {
        Ok(Box::new(Identifier {
            token: parser.current_token.clone(),
            value: parser.current_token.literal.clone(),
        }))
    }

    /// Parse integer literal (associated) function
    fn parse_integer_literal(
        parser: &mut Parser,
    ) -> Result<Box<dyn Expression>, ParsingError> {
        let token = parser.current_token.clone();

        match token.literal.parse::<i64>() {
            Ok(value) => Ok(Box::new(IntegerLiteral { token, value })),
            Err(_) => {
                let msg =
                    format!("could not parse {} as integer", token.literal);
                parser.errors.push(msg);
                Err(ParsingError::Unknown)
            }
        }
    }

    /// Parses `let` statement
    pub fn parse_let_statement(
        &mut self,
    ) -> Result<Box<dyn Statement>, ParsingError> {
        // Initialise the `LetStatement` to be sent
        // in the response
        let mut stmt = LetStatement {
            // sets it to `LET` token
            token: self.current_token.clone(),
            // Initial values for Identifier; these should be properly
            // initialized later
            name: Identifier {
                token: Token {
                    token_type: TokenType::ILLEGAL,
                    literal: "".to_string(),
                },
                value: "".to_string(), // placeholder
            },
            value: Box::new(NoneExpression), // placeholder
        };

        println!(
            "current token is {}, and peek token is {}",
            self.current_token.literal, self.peek_token.literal
        );

        // Lets peek ahead and see its an IDENT
        if !self.expect_peek(&TokenType::IDENT) {
            return Err(ParsingError::UnexpectedToken {
                expected: "IDENT".to_string(),
                found: format!("{:?}", self.peek_token.token_type),
            });
        }
        // In `expect_peek()`, you advance to the identifier (e.g. `x`)

        stmt.name = Identifier {
            token: self.current_token.clone(),
            value: self.current_token.literal.clone(),
        };

        // after the identifier comes "=" or `ASSIGN`
        if !self.expect_peek(&TokenType::ASSIGN) {
            return Err(ParsingError::UnexpectedToken {
                expected: "ASSIGN".to_string(),
                found: format!("{:?}", self.peek_token.token_type),
            });
        }

        // TODO: Skipping the expressions until we encounter a semicolon
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }

        Ok(Box::new(stmt))
    }

    /// Parses `return` statement
    fn parse_return_statement(
        &mut self,
    ) -> Result<Box<dyn Statement>, ParsingError> {
        // Initialise the `ReturnStatement` to be sent
        // in the response
        let mut stmt = ReturnStatement {
            // sets it to `RETURN` token
            token: self.current_token.clone(),
            return_value: Box::new(NoneExpression), // placeholder
        };
        self.next_token();

        // TODO: We're skipping the expressions until we
        // encounter a seimcolon
        while !self.cur_token_is(&TokenType::SEMICOLON) {
            self.next_token();
        }
        Ok(Box::new(stmt))
    }

    fn cur_token_is(&self, t: &TokenType) -> bool {
        self.current_token.token_type == *t
    }

    fn peek_token_is(&self, t: &TokenType) -> bool {
        self.peek_token.token_type == *t
    }

    /// Peek ahead and see if the `TokenType` to be of type `t`
    /// If it is of type `t` then advance to that `t` token
    /// and return `true`;
    /// else just return `false`
    fn expect_peek(&mut self, t: &TokenType) -> bool {
        if self.peek_token_is(t) {
            self.next_token();
            true
        } else {
            self.peek_error(t);
            false
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn errors(&self) -> &Vec<String> {
        &self.errors
    }

    pub fn peek_error(&mut self, t: &TokenType) {
        let msg = format!(
            "expected next token to be {:?}, got {:?} instead",
            t, self.peek_token.token_type
        );
        self.errors.push(msg);
    }

    // Register the prefix functions
    // For example:
    // parser.register_prefix(TokenType::IDENT, parse_identifier);
    // parser.register_prefix(TokenType::INT, parse_integer_literal);
    fn register_prefix(&mut self, t: TokenType, prefix_func: PrefixParseFn) {
        self.prefix_parse_fns.insert(t, prefix_func);
    }

    // Register the infix functions
    // For example:
    // parser.register_infix(TokenType::PLUS, parse_infix_expression);
    fn register_infix(&mut self, t: TokenType, infix_func: InfixParseFn) {
        self.infix_parse_fns.insert(t, infix_func);
    }
}
