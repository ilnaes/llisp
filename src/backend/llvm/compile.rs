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

            let (mut op_is, res, n) = match op {
                Prim2::Add => (
                    vec![
                        Inst::IAdd(var1.clone(), v1, v2),
                        Inst::ISub(var2.clone(), var1, Arg::Const(1)),
                    ],
                    var2,
                    2,
                ),
                Prim2::Minus => (
                    vec![
                        Inst::ISub(var1.clone(), v1, v2),
                        Inst::IAdd(var2.clone(), var1, Arg::Const(1)),
                    ],
                    var2,
                    2,
                ),
                Prim2::Times => {
                    let var3 = scope.sym();
                    let var4 = scope.sym();
                    (
                        vec![
                            Inst::IAshr(var1.clone(), v1, Arg::Const(1)),
                            Inst::IMul(var2.clone(), var1.clone(), v2),
                            Inst::ISub(var3.clone(), var2, var1.clone()),
                            Inst::IAdd(var4.clone(), var3, Arg::Const(1)),
                        ],
                        var4,
                        4,
                    )
                }
            };

            is1.append(&mut is2);
            is1.append(&mut op_is);

            (is1, res, n1 + n2 + n)
        }
        _ => panic!("Not implemented"),
    }
}
