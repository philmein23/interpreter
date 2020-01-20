mod ast;
mod eval;
mod lexer;
mod object;
mod parser;
mod repl;
mod token;

use repl::start;

fn main() {
    start();
}
