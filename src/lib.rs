mod backend;
mod expr;
mod sexp;
mod types;

use crate::backend::llvm::*;

pub fn compile_to_string(s: &str) -> Result<String, String> {
    let sexps = sexp::parse_sexps(s)?;
    let ast = expr::parse_ast(sexps.as_slice())?;
    let typenv = types::new_typenv(ast.as_slice())?;
    let mut gen = scope::Generator::new();

    let (mut insts, var, mut alloc) = ast.iter().fold(
        (Vec::new(), backend::llvm::Arg::Const(0), vec![]),
        |(mut acc, _, mut all), x| {
            let (mut res, v, mut a) =
                compile::compile_expr(x, scope::Scope::new(s), &mut gen, None, &typenv);
            acc.append(&mut res);
            all.append(&mut a);
            (acc, v, all)
        },
    );

    alloc.append(&mut insts);
    alloc.push(backend::llvm::Inst::IRet(var));

    Ok(format!(
        "{}",
        fundef_to_ll(FunDef {
            name: "our_main".to_string(),
            args: vec![],
            inst: alloc,
        })
    ))
}
