use crate::ast::{self, BlockStatement, Expression, Infix, Prefix, Statement};
use crate::object::environment::Environment;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

pub mod builtins;
pub mod environment;
pub type EvalResult = Result<Object, EvalError>;
pub type BuiltInFn = fn(Vec<Object>) -> EvalResult;

#[derive(Clone, Debug, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    StringLiteral(String),
    Null,
    Return(Box<Object>),
    Function(Vec<String>, BlockStatement, Rc<RefCell<Environment>>),
    BuiltIn(BuiltInFn),
    Array(Vec<Object>),
    Hash(HashMap<HashKey, Object>),
}

impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::StringLiteral(value) => write!(f, "\"{}\"", value),
            Object::Integer(value) => write!(f, "{}", value),
            Object::Boolean(value) => write!(f, "{}", value),
            Object::Null => write!(f, "null"),
            Object::Return(value) => write!(f, "{}", value),
            Object::Function(params, body, env) => {
                write!(f, "fn({}) {{\n{}\n}}", params.join(", "), body)
            }
            Object::BuiltIn(built_in_fn) => write!(f, "{:?}", built_in_fn),
            Object::Array(elements) => write!(
                f,
                "[{}]",
                elements
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Object::Hash(hash_map) => {
                let mut pairs = hash_map
                    .iter()
                    .map(|(key, value)| format!("{}: {}", key, value))
                    .collect::<Vec<String>>();
                pairs.sort();

                write!(f, "{{{}}}", pairs.join(", "))
            }
        }
    }
}

impl Object {
    pub fn type_name(&self) -> &str {
        match self {
            Object::StringLiteral(_) => "STRING",
            Object::Integer(_) => "INTEGER",
            Object::Boolean(_) => "BOOLEAN",
            Object::Null => "NULL",
            Object::Return(_) => "RETURN",
            Object::Function(_, _, _) => "FUNCTION",
            Object::BuiltIn(_) => "BUILTIN",
            Object::Array(_) => "ARRAY",
            Object::Hash(_) => "HASH",
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Debug)]
pub enum HashKey {
    Boolean(bool),
    StringLiteral(String),
    Integer(i64),
}

impl fmt::Display for HashKey {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HashKey::Boolean(val) => write!(f, "{}", val),
            HashKey::StringLiteral(val) => write!(f, "\"{}\"", val),
            HashKey::Integer(val) => write!(f, "{}", val),
        }
    }
}

impl HashKey {
    pub fn from_object(obj: Object) -> Result<HashKey, EvalError> {
        match obj {
            Object::Boolean(val) => Ok(HashKey::Boolean(val)),
            Object::StringLiteral(val) => Ok(HashKey::StringLiteral(val)),
            Object::Integer(val) => Ok(HashKey::Integer(val)),
            _ => Err(EvalError::UnsupportedHashKey(obj.clone())),
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
