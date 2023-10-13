use std::any::Any;
use std::fmt;

use crate::token::Token;

/// An AST is a tree of Nodes
pub trait Node: fmt::Display {
    fn token_literal(&self) -> String;
}

/// A Statement is a type of Node that doesn't
/// return a value
/// Statement is a sub-type of Node
pub trait Statement: Node {
    fn statement_node(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

/// A Expression is a type of Node that
/// returns a value
/// `Expression` is a sub-type of `Node`
pub trait Expression: Node {
    fn expression_node(&self) -> String;
    fn as_any(&self) -> &dyn Any;
}

/// Represents the `let` statement
/// e.g. `let x = 5;`, `let y = 4 * 4;`
pub struct LetStatement {
    pub token: Token,
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl fmt::Display for LetStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {} = {};",
            self.token.literal,
            self.name.value,
            self.value // removing to_string() as per clippy
        )
    }
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
/// e.g. `return x;`, `return add(4, y);`
pub struct ReturnStatement {
    pub token: Token,
    pub return_value: Box<dyn Expression>,
}

impl fmt::Display for ReturnStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {};",
            self.token.literal,
            self.return_value // removing to_string() as per clippy
        )
    }
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

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for Identifier {
    fn expression_node(&self) -> String {
        todo!()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// An `ExpressionStatement` is not really a
/// distinct statement; it's a statement that
/// consists solely of one expression.
/// It's a wrapper. We need it because it's
/// totally legal in Monkey to write the following code:
/// ```Monkey
/// let x = 5;
/// x + 10;
/// ```
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Box<dyn Expression>,
}

impl fmt::Display for ExpressionStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.expression) // removing to_string() as per clippy
    }
}

impl Node for ExpressionStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Statement for ExpressionStatement {
    fn statement_node(&self) -> String {
        // Expand on this later depending on the
        // needs of the Monkey lang interpreter
        self.token_literal()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// An Integer Literal
/// e.g. `5;`
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

impl fmt::Display for IntegerLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for IntegerLiteral {
    fn expression_node(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

/// There are two prefix operators in the Monkey
/// programming language: ! and -. Their usage is pretty
/// much what you’d expect from other languages:
pub struct PrefixExpression {
    pub token: Token, // The prefix token, e.g. !
    pub operator: String,
    pub right: Box<dyn Expression>,
}

impl fmt::Display for PrefixExpression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

impl Node for PrefixExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Expression for PrefixExpression {
    fn expression_node(&self) -> String {
        self.token.literal.clone()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}
/// TODO: Possibly remove `NoneExpression` once
/// we have Expression parsing working
pub struct NoneExpression;

impl fmt::Display for NoneExpression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "")
    }
}

impl Node for NoneExpression {
    fn token_literal(&self) -> String {
        "".to_string()
    }
}

impl Expression for NoneExpression {
    fn expression_node(&self) -> String {
        "".to_string()
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub struct Program {
    pub statements: Vec<Box<dyn Statement>>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let output: String =
            self.statements.iter().map(|s| s.to_string()).collect();
        write!(f, "{}", output)
    }
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
