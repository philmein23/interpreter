mod lexer;
mod token;

fn main() {
    let chars = String::from("hello there").chars().collect::<Vec<char>>();

    println!("{:?}", chars);
}
