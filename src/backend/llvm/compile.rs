use super::scope::Scope;
use super::*;
use crate::expr::{expr::*, prim2::*};

pub fn compile_expr<'a>(expr: &Expr, mut scope: Scope) -> (Vec<Inst>, Arg, usize) {
    match expr {
        Expr::ENum(n) => {
            let var = scope.sym();
            (
                vec![Inst::IAdd(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(2 * (*n) + 1),
                )],
                var,
                1,
            )
        }
        Expr::EPrim2(op, e1, e2) => {
            let (mut is1, v1, n1) = compile_expr(e1, scope.clone());
            scope.incr(n1);
            let (mut is2, v2, n2) = compile_expr(e2, scope.clone());
            scope.incr(n2);
            let var1 = scope.sym();
            let var2 = scope.sym();

            let mut op_is = match op {
                Prim2::Add => vec![
                    Inst::IAdd(var1.clone(), v1, v2),
                    Inst::ISub(var2.clone(), var1, Arg::Const(1)),
                ],
                _ => panic!("Not implemented"),
            };

            is1.append(&mut is2);
            is1.append(&mut op_is);

            (is1, var2, n1 + n2 + 2)
        }
        _ => panic!("Not implemented"),
    }
}
