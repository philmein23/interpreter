use crate::token::Token;

pub struct Identifier {
    pub token: Token,
    pub value: String,
}
pub enum Statement {
    Let { name: String, value: Expression },
}

pub enum Expression {}

pub struct Program {
    statements: Vec<Statement>,
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
