mod backend;
mod expr;
mod lift;
mod sexp;
mod types;
mod well_def;

use crate::backend::llvm::*;

pub fn compile_to_string(s: &str) -> Result<String, String> {
    let sexps = sexp::parse_sexps(s)?;
    let (ast, g) = expr::parse_ast(sexps.as_slice())?;
    let ast = lift::lift(ast);
    // well_def::check_prog(&ast)?;
    let typenv = types::TypeEnv::new(ast.as_slice())?;

    let insts = compile::compile_prog(&ast, &typenv, g);
    let prog = insts.into_iter().fold(String::new(), |mut acc, x| {
        acc.push_str(&fundef_to_ll(x));
        acc
    });

    let prelude = "declare void @print(i64)\ndeclare i64* @new(i64)\n";

    Ok(format!("{}{}", prelude, prog))
}
