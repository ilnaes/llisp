use super::scope::*;
use super::*;
use crate::expr::{expr::*, prim2::*};

const TRUE_CONST: i64 = 0x2;
const FALSE_CONST: i64 = 0x6;

pub fn compile_expr<'a, 'b>(
    expr: &'b Expr<'a>,
    scope: Scope<'a>,
    gen: &mut Generator,
) -> (Vec<Inst>, Arg) {
    match expr {
        Expr::ENum(n) => {
            let var = gen.sym();
            (
                vec![Inst::IAdd(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(2 * (*n) + 1),
                )],
                var,
            )
        }
        Expr::EBool(b) => {
            let var = gen.sym();
            (
                vec![Inst::IAdd(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(if *b { TRUE_CONST } else { FALSE_CONST }),
                )],
                var,
            )
        }
        Expr::EId(x) => {
            // static checkers will/should catch this
            (vec![], scope.get(x).unwrap())
        }
        Expr::EPrim2(op, e1, e2) => parse_prim2(op, e1, e2, scope.clone(), gen),
        Expr::ELet(bind, body) => {
            let (mut is1, scope) = bind.iter().fold(
                (vec![], scope.clone()),
                |(mut res, mut sc), Binding(x, e)| {
                    // compile exprs and update a new
                    // scope with the bindings but increment
                    // count on both scopes
                    let (mut is, v) = compile_expr(e, scope.clone(), gen);

                    sc.register(x, v);

                    res.append(&mut is);
                    (res, sc)
                },
            );

            let (mut is2, v) = compile_expr(body, scope.clone(), gen);
            is1.append(&mut is2);
            (is1, v)
        }
        Expr::EIf(cond, e1, e2) => {
            let (mut _ops, _res) = compile_expr(cond, scope.clone(), gen);
            let (mut _ops1, _res1) = compile_expr(e1, scope.clone(), gen);
            let (mut _ops2, _res2) = compile_expr(e2, scope.clone(), gen);
            panic!("Not implemented");
        }
    }
}

fn parse_prim2<'a, 'b>(
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    scope: Scope<'a>,
    gen: &mut Generator,
) -> (Vec<Inst>, Arg) {
    let (mut is1, v1) = compile_expr(e1, scope.clone(), gen);
    let (mut is2, v2) = compile_expr(e2, scope.clone(), gen);
    let var1 = gen.sym();
    let var2 = gen.sym();

    let (mut op_is, res) = match op {
        Prim2::Add => (
            vec![
                Inst::IAdd(var1.clone(), v1, v2),
                Inst::ISub(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
        ),
        Prim2::Minus => (
            vec![
                Inst::ISub(var1.clone(), v1, v2),
                Inst::IAdd(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
        ),
        Prim2::Times => {
            let var3 = gen.sym();
            let var4 = gen.sym();
            (
                vec![
                    Inst::IAshr(var1.clone(), v1, Arg::Const(1)),
                    Inst::IMul(var2.clone(), var1.clone(), v2),
                    Inst::ISub(var3.clone(), var2, var1.clone()),
                    Inst::IAdd(var4.clone(), var3, Arg::Const(1)),
                ],
                var4,
            )
        }
        Prim2::Equal => (vec![Inst::IEq(var1.clone(), v1, v2)], var1),
        Prim2::Less => (vec![Inst::ILt(var1.clone(), v1, v2)], var1),
        Prim2::Greater => (vec![Inst::IGt(var1.clone(), v1, v2)], var1),
    };

    is1.append(&mut is2);
    is1.append(&mut op_is);

    (is1, res)
}
