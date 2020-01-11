use crate::lexer::Lexer;
use crate::token::Token;
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

        loop {
            let token = lexer.next_token();

            if token == Token::EOF {
                break;
            }
            println!("{:?}", token);
        }
    }
}
