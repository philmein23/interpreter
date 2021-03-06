use crate::token::Token;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let { name: String, value: Expression },
    Return(Option<Expression>),
    Expression(Expression),
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Statement::Let { name, value } => write!(f, "let {} = {};", name, value),
            Statement::Return(None) => write!(f, "return;"),
            Statement::Return(Some(exp)) => write!(f, "return {};", exp),
            Statement::Expression(exp) => write!(f, "{};", exp),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    Array(Vec<Expression>),
    IntegerLiteral(i64),
    StringLiteral(String),
    Identifier(String),
    Boolean(bool),
    Prefix(Prefix, Box<Expression>),
    Infix(Infix, Box<Expression>, Box<Expression>),
    If(Box<Expression>, BlockStatement, Option<BlockStatement>),
    FunctionLiteral(Vec<String>, BlockStatement),
    Call(Box<Expression>, Box<Vec<Expression>>),
    Index(Box<Expression>, Box<Expression>),
    Hash(Vec<(Expression, Expression)>),
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expression::Array(exp) => write!(f, "[{}]", comma_separated(exp)),
            Expression::StringLiteral(literal) => write!(f, "\"{}\"", literal),
            Expression::Identifier(ident) => write!(f, "{}", ident),
            Expression::IntegerLiteral(int) => write!(f, "{}", int),
            Expression::Boolean(bool_value) => write!(f, "{}", bool_value),
            Expression::Prefix(prefix, exp) => write!(f, "({}{})", prefix, exp),
            Expression::Infix(infix, exp1, exp2) => write!(f, "({} {} {})", exp1, infix, exp2),
            Expression::If(condition, consequence, alternative) => {
                write!(f, "if {} {}", condition, consequence)?;
                if let Some(alt) = alternative {
                    write!(f, " else {}", alt)?;
                }
                Ok(())
            }
            Expression::FunctionLiteral(params, body) => {
                write!(f, "fn({}) {}", params.join(", "), body)
            }
            Expression::Call(exp, args) => write!(f, "{}({})", exp, comma_separated(args)),
            Expression::Index(left, index) => write!(f, "({}[{}])", left, index),
            Expression::Hash(pairs) => {
                let parsed_pairs = pairs
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>()
                    .join(", ");

                write!(f, "{{{}}}", parsed_pairs)
            }
        }
    }
}

fn comma_separated(expressions: &Vec<Expression>) -> String {
    expressions
        .iter()
        .map(|e| e.to_string())
        .collect::<Vec<String>>()
        .join(", ")
}

#[derive(Clone, Debug, PartialEq)]
pub enum Prefix {
    BANG,
    MINUS,
}

impl fmt::Display for Prefix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Prefix::BANG => write!(f, "!"),
            Prefix::MINUS => write!(f, "-"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Infix {
    EQ,
    NOT_EQ,
    LT,
    GT,
    PLUS,
    MINUS,
    SLASH,
    ASTERISK,
}

impl fmt::Display for Infix {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Infix::EQ => write!(f, "=="),
            Infix::NOT_EQ => write!(f, "!="),
            Infix::LT => write!(f, "<"),
            Infix::GT => write!(f, ">"),
            Infix::PLUS => write!(f, "+"),
            Infix::MINUS => write!(f, "-"),
            Infix::SLASH => write!(f, "/"),
            Infix::ASTERISK => write!(f, "*"),
        }
    }
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{}", statement)?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct BlockStatement {
    pub statements: Vec<Statement>,
}

impl fmt::Display for BlockStatement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for statement in &self.statements {
            write!(f, "{{ {} }}", statement)?;
        }

        Ok(())
    }
}
