mod backend;
mod expr;
mod sexp;

use crate::backend::llvm::*;

pub fn compile_to_string(s: &str) -> Result<String, String> {
    let sexps = sexp::parse_sexps(s)?;
    let ast = expr::parse_ast(sexps.as_slice())?;
    let mut gen = scope::Generator::new();

    let (mut insts, var) = ast.iter().fold(
        (Vec::new(), backend::llvm::Arg::Const(0)),
        |(mut acc, _), x| {
            let (mut res, v) = compile::compile_expr(x, scope::Scope::new(s), &mut gen);
            acc.append(&mut res);
            (acc, v)
        },
    );

    insts.push(backend::llvm::Inst::IRet(var));

    Ok(format!(
        "{}",
        fundef_to_ll(FunDef {
            name: "our_main".to_string(),
            args: vec![],
            inst: insts,
        })
    ))
}
