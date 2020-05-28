use super::scope::*;
use super::*;
use crate::expr::expr::*;
use crate::types::*;

// const TRUE_CONST: i64 = 0x2;
// const FALSE_CONST: i64 = 0x6;

pub fn compile_expr<'a, 'b>(
    expr: &'b Expr<'a>,
    scope: Scope<'a>,
    gen: &mut Generator,
    env: Option<&'b Expr<'a>>,
    typenv: &TypeEnv<'a, 'b>,
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    match expr {
        Expr::ENum(n) => {
            let var = gen.sym();
            (
                vec![Inst::IAdd64(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(2 * (*n) + 1),
                )],
                var,
                vec![],
            )
        }
        Expr::EBool(b) => {
            let var = gen.sym();
            (
                vec![Inst::IAdd1(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(if *b { 1 } else { 0 }),
                )],
                var,
                vec![],
            )
        }
        Expr::EId(x) => {
            // static checkers will/should catch this
            (vec![], scope.get(x).unwrap(), vec![])
        }
        Expr::EPrim2(op, e1, e2) => parse_prim2(op, e1, e2, scope.clone(), gen, env, typenv),
        Expr::ELet(bind, body) => {
            let (mut is1, scope, mut alloc) = bind.iter().fold(
                (vec![], scope.clone(), vec![]),
                |(mut res, mut sc, mut all), Binding(x, e)| {
                    // be sure to use old scope
                    let (mut is, v, mut a) = compile_expr(e, scope.clone(), gen, env, typenv);
                    sc.register(x, v);

                    all.append(&mut a);
                    res.append(&mut is);
                    (res, sc, all)
                },
            );

            let (mut is2, v, mut alloc1) =
                compile_expr(body, scope.clone(), gen, Some(expr), typenv);
            is1.append(&mut is2);
            alloc.append(&mut alloc1);
            (is1, v, alloc)
        }
        Expr::EIf(cond, e1, e2) => {
            let (mut ins1, v1, mut alloc1) = compile_expr(cond, scope.clone(), gen, env, typenv);
            let (mut ins2, v2, mut alloc2) = compile_expr(e1, scope.clone(), gen, env, typenv);
            let (mut ins3, v3, mut alloc3) = compile_expr(e2, scope.clone(), gen, env, typenv);

            let store = gen.sym();
            let true_branch = gen.sym();
            let true_label = true_branch.to_string();
            let false_branch = gen.sym();
            let false_label = false_branch.to_string();
            let after = gen.sym();
            let after_label = after.to_string();

            let typ = typenv.get_vtype(&expr, env).unwrap();

            ins1.append(&mut vec![
                Inst::IBrk(v1, true_branch, false_branch),
                Inst::ILabel(true_label),
            ]);

            ins1.append(&mut ins2);
            ins1.append(&mut vec![
                Inst::IStore(typ.clone(), store.clone(), v2),
                Inst::IJmp(after.clone()),
                Inst::ILabel(false_label),
            ]);
            alloc1.append(&mut alloc2);

            ins1.append(&mut ins3);
            ins1.append(&mut vec![
                Inst::IStore(typ.clone(), store.clone(), v3),
                Inst::IJmp(after.clone()),
                Inst::ILabel(after_label),
            ]);
            alloc1.append(&mut alloc3);

            let res = gen.sym();
            ins1.push(Inst::ILoad(typ.clone(), res.clone(), store.clone()));
            alloc1.push(Inst::IAlloc(typ.clone(), store));

            return (ins1, res, alloc1);
        }
    }
}

fn parse_prim2<'a, 'b>(
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    scope: Scope<'a>,
    gen: &mut Generator,
    env: Option<&'b Expr<'a>>,
    typenv: &TypeEnv<'a, 'b>,
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    let (mut is1, v1, mut a1) = compile_expr(e1, scope.clone(), gen, env, typenv);
    let (mut is2, v2, mut a2) = compile_expr(e2, scope.clone(), gen, env, typenv);
    let var1 = gen.sym();
    let var2 = gen.sym();

    let (mut op_is, res, mut a3) = match op {
        Prim2::Add => (
            vec![
                Inst::IAdd64(var1.clone(), v1, v2),
                Inst::ISub(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
            vec![],
        ),
        Prim2::Minus => (
            vec![
                Inst::ISub(var1.clone(), v1, v2),
                Inst::IAdd64(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
            vec![],
        ),
        Prim2::Times => {
            let var3 = gen.sym();
            let var4 = gen.sym();
            (
                vec![
                    Inst::IAshr(var1.clone(), v1, Arg::Const(1)),
                    Inst::IMul(var2.clone(), var1.clone(), v2),
                    Inst::ISub(var3.clone(), var2, var1.clone()),
                    Inst::IAdd64(var4.clone(), var3, Arg::Const(1)),
                ],
                var4,
                vec![],
            )
        }
        Prim2::Equal => {
            // let (mut insts, res, alloc) = bool_tail(var1.clone(), gen);
            // let mut ins = vec![Inst::IEq(var1, v1, v2)];
            // ins.append(&mut insts);
            // (ins, res, alloc)

            let typ = typenv.get_vtype(&e1, env).unwrap();
            (vec![Inst::IEq(typ, var1.clone(), v1, v2)], var1, vec![])
        }
        Prim2::Less => {
            // let (mut insts, res, alloc) = bool_tail(var1.clone(), gen);
            // let mut ins = vec![Inst::ILt(var1, v1, v2)];
            // ins.append(&mut insts);
            // (ins, res, alloc)
            (vec![Inst::ILt(var1.clone(), v1, v2)], var1, vec![])
        }
        Prim2::Greater => {
            // let (mut insts, res, alloc) = bool_tail(var1.clone(), gen);
            // let mut ins = vec![Inst::IGt(var1, v1, v2)];
            // ins.append(&mut insts);
            // (ins, res, alloc)
            (vec![Inst::IGt(var1.clone(), v1, v2)], var1, vec![])
        }
    };

    is1.append(&mut is2);
    is1.append(&mut op_is);
    a1.append(&mut a2);
    a1.append(&mut a3);

    (is1, res, a1)
}

// fn bool_tail(cond: Arg, gen: &mut scope::Generator) -> (Vec<Inst>, Arg, Vec<Inst>) {
//     let store = gen.sym();
//     let true_branch = gen.sym();
//     let false_branch = gen.sym();
//     let after_branch = gen.sym();
//     let alloc = vec![Inst::IAlloc(store.clone())];
//     let res = gen.sym();

//     let inst = vec![
//         Inst::IBrk(cond, true_branch.clone(), false_branch.clone()),
//         Inst::ILabel(true_branch.to_string()),
//         Inst::IStore(store.clone(), Arg::Const(TRUE_CONST)),
//         Inst::IJmp(after_branch.clone()),
//         Inst::ILabel(false_branch.to_string()),
//         Inst::IStore(store.clone(), Arg::Const(FALSE_CONST)),
//         Inst::IJmp(after_branch.clone()),
//         Inst::ILabel(after_branch.to_string()),
//         Inst::ILoad(res.clone(), store),
//     ];
//     (inst, res, alloc)
// }
