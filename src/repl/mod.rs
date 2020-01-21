use crate::eval::eval;
use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

pub fn start() {
    loop {
        print!(">> ");
        let mut input = String::new();
        let _ = io::stdout().flush();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");
        let input: String = input.trim().parse().unwrap();

        let mut lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);

        let program = parser.parse_program();

        if parser.errors.len() > 0 {
            for error in parser.errors {
                println!("ERROR: {:?}", error);
            }
        }

        if let Ok(evaluated) = eval(&program) {
            println!("{:?}", evaluated);
        }
    }
}
