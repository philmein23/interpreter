use crate::object::{EvalError, EvalResult, Object};

pub struct BuiltIn;

impl BuiltIn {
    pub fn lookup(name: &str) -> EvalResult {
        match name {
            "len" => Ok(Object::BuiltIn(len)),
            "first" => Ok(Object::BuiltIn(first)),
            "last" => Ok(Object::BuiltIn(last)),
            "rest" => Ok(Object::BuiltIn(rest)),
            "push" => Ok(Object::BuiltIn(push)),
            _ => Err(EvalError::IdentifierNotFound(
                "Identifier not found".to_string(),
            )),
        }
    }
}

fn len(args: Vec<Object>) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::General(format!(
            "wrong number of arguments, got {}, want: 1",
            args.len()
        )));
    }

    let arg = args.get(0);

    match arg.cloned() {
        Some(Object::StringLiteral(val)) => Ok(Object::Integer(String::from(val).len() as i64)),
        Some(Object::Array(elements)) => Ok(Object::Integer(elements.len() as i64)),
        _ => Err(EvalError::General(format!(
            "Unsupport arguments: {:?}",
            args
        ))),
    }
}

fn first(args: Vec<Object>) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::General(format!(
            "wrong number of arguments, got {}, want: 1",
            args.len()
        )));
    }

    let arg = args.get(0);

    match arg.cloned() {
        Some(Object::Array(elements)) => {
            Ok(elements.first().cloned().unwrap_or_else(|| Object::Null))
        }
        _ => Ok(Object::Null),
    }
}

fn last(args: Vec<Object>) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::General(format!(
            "wrong number of arguments, got {}, want: 1",
            args.len()
        )));
    }

    let arg = args.get(0);

    match arg.cloned() {
        Some(Object::Array(elements)) => {
            Ok(elements.last().cloned().unwrap_or_else(|| Object::Null))
        }
        _ => Ok(Object::Null),
    }
}

fn rest(args: Vec<Object>) -> EvalResult {
    if args.len() != 1 {
        return Err(EvalError::General(format!(
            "wrong number of arguments, got {}, want: 1",
            args.len()
        )));
    }

    let arg = args.get(0);

    match arg.cloned() {
        Some(Object::Array(elements)) => {
            if elements.is_empty() {
                return Ok(Object::Null);
            } else {
                Ok(Object::Array(elements[1..].to_vec()))
            }
        }
        _ => Ok(Object::Null),
    }
}

fn push(args: Vec<Object>) -> EvalResult {
    if args.len() != 2 {
        return Err(EvalError::General(format!(
            "wrong number of arguments, got {}, want: 2",
            args.len()
        )));
    }

    let arg = args.get(0);
    let value = args.get(1).cloned().unwrap();

    match arg.cloned() {
        Some(Object::Array(elements)) => {
            let mut new_list = elements[..].to_vec();
            new_list.push(value);
            Ok(Object::Array(new_list))
        }
        _ => Ok(Object::Null),
    }
}
