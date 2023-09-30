/// Ok, so now we are going to write a parser for Monkey lang. Specifically we are
/// going to write a top down operator precedence parser called recursive descent
/// parser.
/// We’re going to start by parsing statements: let and return statements. When we
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

/// An AST is a tree of Nodes
#[derive(Debug)]
pub trait Node {
    fn token_literal(&self) -> String;
}

/// A Statement is a type of Node that doesn't
/// return a value
#[derive(Debug)]
pub trait Statement {
    fn statement_node(&self) -> String;
}

/// A Expression is a type of Node that
/// returns a value
#[derive(Debug)]
pub trait Expression {
    fn expression_node(&self) -> String;
}

/// The program Node is going to be the
/// root node of every AST. Every valid
/// program is a series of statements.
pub struct Program {
    // This is a collection of trait objects
    // because we could have different types of
    // statements in this collection.
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

/// A `LetStatement` represents a let variable binding
/// `let x = 5;` or `let y = add(2, 2) * 5 / 10;`
/// So it will hold the identifier (`x` or `y`), the
/// expression (`5 * 5` or `add(2, 2) * 5`), and the
/// token type (`let`)
pub struct LetStatement {
    pub token_type: TokenType, // the TokenType::LET
    pub name: Identifier,
    pub value: Box<dyn Expression>,
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token_type.ch.to_string()
    }
}

pub struct Identifier {
    pub token_type: TokenType, // the TokenType::IDENT
    pub value: String,
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token_type.ch.to_string()
    }
}
