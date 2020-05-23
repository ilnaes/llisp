use std::env;
use std::fs;

// mod expr;
mod sexp;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Need argument!");
        return;
    }

    let contents = fs::read(&args[1]).expect("Could not read file!");
    let res = sexp::parse(contents.as_slice());
    println!("{:?}", res);
}
