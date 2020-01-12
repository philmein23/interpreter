use crate::token::Token;

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Let { name: String },
    Return(Option<Expression>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expression {
    IntegerLiteral(i32),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ast() {}
}
