use crate::object::{EvalError, EvalResult, Object};

pub struct BuiltIn;

impl BuiltIn {
    pub fn lookup(name: &str) -> EvalResult {
        match name {
            "len" => Ok(Object::BuiltIn(len)),
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
        _ => Err(EvalError::General(format!(
            "Unsupport arguments: {:?}",
            args
        ))),
    }
}
