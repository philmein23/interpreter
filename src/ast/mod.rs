use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let { name: String },
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i32),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

impl Program {
    // fn token_literal(&self) -> String {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {}
}
