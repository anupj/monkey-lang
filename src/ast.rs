use std::any::Any;

use crate::token::Token;

/// An AST is a tree of Nodes
pub trait Node {
    fn token_literal(&self) -> String;
}

/// A Statement is a type of Node that doesn't
/// return a value
/// Statement is a sub-type of Node
pub trait Statement: Node {
    fn statement_node(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if let Some(first_statement) = self.statements.first() {
            first_statement.token_literal()
        } else {
            "".to_string()
        }
    }
}

/// Represents the `let` statement
/// e.g. `let x = 5;`, `let y = 4 * 4;`
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for LetStatement {
    fn statement_node(&self) -> String {
        todo!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// Represents a `return` statement
/// e.g. `return 5;`, `return 10`, `return add(15);`
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl Node for ReturnStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ReturnStatement {
    fn statement_node(&self) -> String {
        todo!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Identifier {
    pub token: Token,
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

/// A Expression is a type of Node that
/// returns a value
/// `Expression` is a sub-type of `Node`
pub trait Expression: Node {
    fn expression_node(&self) -> String;
}

impl Expression for Identifier {
    fn expression_node(&self) -> String {
        todo!()
    }
}

pub struct NoneExpression;

impl Node for NoneExpression {
    fn token_literal(&self) -> String {
        "".to_string()
    }
}

impl Expression for NoneExpression {
    fn expression_node(&self) -> String {
        "".to_string()
    }
}
