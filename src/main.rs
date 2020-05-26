use llisp::compile_to_string;
use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Need argument!");
        return;
    }

    let contents = fs::read(&args[1]).expect("Could not read file!");
    let s = std::str::from_utf8(&contents).unwrap();
    match compile_to_string(s) {
        Ok(res) => println!("{}", res),
        Err(e) => eprintln!("{}", e),
    }
}
