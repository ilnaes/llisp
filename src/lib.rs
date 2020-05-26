mod backend;
mod expr;
mod sexp;

use crate::backend::llvm::*;

pub fn compile_to_string(s: &str) -> Result<String, String> {
    let sexps = sexp::parse_sexps(s)?;
    let ast = expr::parse_ast(sexps.as_slice());

    let (mut insts, var) = ast.iter().fold(
        (Vec::new(), backend::llvm::Arg::Const(0)),
        |(mut acc, _), x| {
            let (mut res, v, _) = compile::compile_expr(x, scope::Scope::new(s));
            acc.append(&mut res);
            (acc, v)
        },
    );

    // insts.push(backend::llvm::Inst::ICall(backend::llvm::Arg::AVar(
    //     backend::llvm::Var::Global(String::from("foo")),
    // )));
    insts.push(backend::llvm::Inst::IRet(var));

    Ok(format!(
        // "declare external void @foo()\n\n{}",
        "{}",
        fundef_to_ll(FunDef {
            name: "our_main".to_string(),
            args: vec![],
            inst: insts,
        })
    ))
}
