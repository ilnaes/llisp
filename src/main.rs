use std::env;
use std::fs;

mod backend;
mod expr;
mod sexp;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() == 1 {
        println!("Need argument!");
        return;
    }

    let contents = fs::read(&args[1]).expect("Could not read file!");
    let sexps = sexp::parse_sexps(contents.as_slice());
    let ast = expr::parse_ast(sexps.as_slice());
    println!("{:?}", ast);

    let mut scope = backend::llvm::scope::Scope::new(contents.as_slice());
    scope.register("what");

    // let inst = backend::asm::compile::compile(&ast);
    // println!("{}", backend::asm::to_asm(inst));
}
