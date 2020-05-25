use crate::backend::llvm::*;
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

    let (mut insts, var) = ast.iter().fold(
        (Vec::new(), backend::llvm::Arg::Const(0)),
        |(mut acc, _), x| {
            let (mut res, v, _) = compile::compile_expr(x, scope::Scope::new());
            acc.append(&mut res);
            (acc, v)
        },
    );

    // insts.push(backend::llvm::Inst::ICall(backend::llvm::Arg::AVar(
    //     backend::llvm::Var::Global(String::from("foo")),
    // )));
    insts.push(backend::llvm::Inst::IRet(var));

    println!(
        // "declare external void @foo()\n\n{}",
        "{}",
        fundef_to_ll(FunDef {
            name: "our_main".to_string(),
            args: vec![],
            inst: insts,
        })
    );
}
