use crate::ast::{self, Expression, Infix, Prefix, Statement};
use std::fmt;

pub mod environment;
pub type EvalResult = Result<Object, EvalError>;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Return(Box<Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
        }
    }
}

#[derive(Debug)]
pub enum EvalError {
    TypeMismatch(Infix, Object, Object),
    UnknownPrefixOperator(Prefix, Object),
    UnknownInfixOperator(Infix, Object, Object),
    IdentifierNotFound(String),
    NotCallable(Object),
    WrongArgumentCount { expected: usize, given: usize },
    UnsupportedArguments(String, Vec<Object>),
    UnknownIndexOperator(Object, Object),
    UnsupportedHashKey(Object),
    General(String),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::TypeMismatch(infix, left, right) => write!(
                f,
                "type mismatch: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::UnknownPrefixOperator(prefix, right) => {
                write!(f, "unknown operator: {}{}", prefix, right.type_name())
            }
            EvalError::UnknownInfixOperator(infix, left, right) => write!(
                f,
                "unknown operator: {} {} {}",
                left.type_name(),
                infix,
                right.type_name()
            ),
            EvalError::IdentifierNotFound(name) => write!(f, "identifier not found: {}", name),
            EvalError::NotCallable(obj) => write!(
                f,
                "not a closure or a builtin function: {}",
                obj.type_name()
            ),
            EvalError::WrongArgumentCount { expected, given } => write!(
                f,
                "wrong number of arguments: expected {}, given {}",
                expected, given
            ),
            EvalError::UnsupportedArguments(name, arguments) => write!(
                f,
                "unsupported arguments to `{}`: {}",
                name,
                arguments
                    .iter()
                    .map(|a| a.type_name())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ),
            EvalError::UnknownIndexOperator(left, index) => write!(
                f,
                "unknown operator: {}[{}]",
                left.type_name(),
                index.type_name()
            ),
            EvalError::UnsupportedHashKey(key) => {
                write!(f, "unusable as hash key: {}", key.type_name())
            }
            EvalError::General(val) => write!(f, "General error: {}", val),
        }
    }
}
