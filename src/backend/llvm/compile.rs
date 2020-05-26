use super::scope::Scope;
use super::*;
use crate::expr::{expr::*, prim2::*};

pub fn compile_expr<'a, 'b>(expr: &'b Expr<'a>, mut scope: Scope<'a>) -> (Vec<Inst>, Arg, usize) {
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
        Expr::EId(x) => (vec![], scope.get(x).unwrap(), 0), // static checkers will/should catch this
        Expr::EPrim2(op, e1, e2) => parse_prim2(op, e1, e2, scope.clone()),
        Expr::ELet(bind, body) => {
            let (mut is1, n1, scope) = bind.iter().fold(
                (vec![], 0, scope.clone()),
                |(mut res, n, mut sc), Binding(x, e)| {
                    // compile exprs and update a new
                    // scope with the bindings but increment
                    // count on both scopes
                    let (mut is, v, m) = compile_expr(e, scope.clone());

                    scope.incr(m);
                    sc.incr(m);
                    sc.register(x, v);

                    res.append(&mut is);

                    (res, n + m, sc)
                },
            );

            let (mut is2, v, n2) = compile_expr(body, scope.clone());
            is1.append(&mut is2);
            (is1, v, n1 + n2)
        }
    }
}

fn parse_prim2<'a, 'b>(
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    mut scope: Scope<'a>,
) -> (Vec<Inst>, Arg, usize) {
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
