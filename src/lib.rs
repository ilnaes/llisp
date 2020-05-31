mod backend;
mod expr;
mod sexp;
mod types;
mod well_def;

use crate::backend::llvm::*;
use im::HashSet;

pub fn compile_to_string(s: &str) -> Result<String, String> {
    let sexps = sexp::parse_sexps(s)?;
    let ast = expr::parse_ast(sexps.as_slice())?;
    // for expr in ast.iter() {
    //     well_def::check(expr, HashSet::new())?;
    // }
    let _typenv = types::TypeEnv::new(ast.as_slice())?;

    // let mut gen = scope::Generator::new();

    // let (mut insts, var, mut alloc) = ast.iter().fold(
    //     (Vec::new(), backend::llvm::Arg::Const(0), vec![]),
    //     |(mut acc, _, mut all), x| {
    //         let (mut res, v, mut a) = compile::compile_expr(x, scope::Scope::new(), &mut gen, None);
    //         acc.append(&mut res);
    //         all.append(&mut a);
    //         (acc, v, all)
    //     },
    // );

    // alloc.append(&mut insts);
    // alloc.push(backend::llvm::Inst::IRet(var));

    let insts = compile::compile_defs(&ast);
    let prog = insts.into_iter().fold(String::new(), |mut acc, x| {
        acc.push_str(&fundef_to_ll(x));
        acc
    });

    let prelude = "declare void @print(i64)\n\n";

    Ok(format!("{}{}", prelude, prog))

    // Ok(format!(
    //     "{}{}",
    //     prelude,
    //     fundef_to_ll(FunDef {
    //         name: "our_main".to_string(),
    //         args: vec![],
    //         inst: alloc,
    //     })
    // ))
}
