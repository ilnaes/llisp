use super::scope::*;
use super::*;
use crate::backend::llvm::FunDef;
use crate::expr::expr::*;
// use im;

const TRUE_CONST: i64 = 0xA;
const FALSE_CONST: i64 = 0x2;

pub fn compile_prog<'a, 'b>(prog: &'b [Def<'a>]) -> Vec<FunDef> {
    let mut scope = Scope::new();
    let mut res = Vec::new();

    // register all top-level functions
    for def in prog {
        let Def::FuncDef(f, _, _) = def;
        scope.register(
            f.get_str().unwrap(),
            Arg::AVar(Var::Global(f.get_str().unwrap().to_string())),
        );
    }

    for def in prog {
        let Def::FuncDef(f, args, body) = def;
        let mut sc = scope.clone();

        let a = args
            .iter()
            .map(|x| {
                sc.register(
                    x.get_str().unwrap(),
                    Arg::AVar(Var::Local(x.get_str().unwrap().to_string())),
                );
                x.get_str().unwrap().to_string()
            })
            .collect();

        let (mut insts, v, mut alloc) =
            compile_expr(body, sc.clone(), &mut Generator::new(), Some(f));

        alloc.append(&mut insts);
        alloc.push(Inst::IRet(v));

        res.push(FunDef {
            name: f.get_str().unwrap().to_string(),
            args: a,
            inst: alloc,
        })
    }

    res
}

pub fn compile_expr<'a, 'b>(
    expr: &'b Expr<'a>,
    scope: Scope<'a>,
    gen: &mut Generator,
    env: Option<&'b Expr<'a>>,
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    match expr {
        Expr::ENum(n) => {
            let var = gen.sym(true);
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
            let var = gen.sym(true);
            (
                // vec![Inst::IAdd1(
                //     var.clone(),
                //     Arg::Const(0),
                //     Arg::Const(if *b { 1 } else { 0 }),
                // )],
                vec![Inst::IAdd64(
                    var.clone(),
                    Arg::Const(0),
                    Arg::Const(if *b { TRUE_CONST } else { FALSE_CONST }),
                )],
                var,
                vec![],
            )
        }
        Expr::EId(x) => {
            // static checkers will/should catch this
            match scope.get(x).unwrap() {
                Arg::AVar(_) => (vec![], scope.get(x).unwrap(), vec![]),
                _ => panic!(format!("Improper scoped variable {:?}", x)),
            }
        }
        Expr::EPrim2(op, e1, e2) => parse_prim2(op, e1, e2, scope.clone(), gen, env),
        Expr::ELet(bind, body) => {
            let (mut is1, scope, mut alloc) = bind.iter().fold(
                (vec![], scope.clone(), vec![]),
                |(mut res, mut sc, mut all), Binding(x, e)| {
                    // be sure to use old scope
                    let (mut is, v, mut a) = compile_expr(e, scope.clone(), gen, env);
                    sc.register(x, v);

                    all.append(&mut a);
                    res.append(&mut is);
                    (res, sc, all)
                },
            );

            let (mut is2, v, mut alloc1) = compile_expr(body, scope.clone(), gen, Some(expr));
            is1.append(&mut is2);
            alloc.append(&mut alloc1);
            (is1, v, alloc)
        }
        Expr::EPrint(e) => {
            let (mut is1, v1, alloc1) = compile_expr(e, scope.clone(), gen, env);

            // if let Ok(VType::I1) = typenv.get_vtype(e, env) {
            //     let (mut is2, v2, mut alloc2) = bool_tail(v1.clone(), gen);

            //     alloc1.append(&mut alloc2);
            //     is1.append(&mut is2);
            //     is1.push(Inst::ICall(
            //         VType::Void,
            //         Arg::AVar(Var::Global("print".to_string())),
            //         vec![v2],
            //     ));
            // } else {
            is1.push(Inst::ICall(
                VType::Void,
                None,
                Arg::AVar(Var::Global("print".to_string())),
                vec![v1.clone()],
            ));
            // }
            (is1, v1, alloc1)
        }
        Expr::EIf(cond, e1, e2) => {
            let (mut ins1, v1, mut alloc1) = compile_expr(cond, scope.clone(), gen, env);
            let (mut ins2, v2, mut alloc2) = compile_expr(e1, scope.clone(), gen, env);
            let (mut ins3, v3, mut alloc3) = compile_expr(e2, scope.clone(), gen, env);

            let cmp = gen.sym(true);

            let store = gen.sym(true);
            let true_branch = gen.sym(true);
            let true_label = true_branch.to_string();
            let false_branch = gen.sym(true);
            let false_label = false_branch.to_string();
            let after = gen.sym(true);
            let after_label = after.to_string();

            // let typ = typenv.get_vtype(&expr, env).unwrap();
            let typ = VType::I64;

            ins1.append(&mut vec![
                Inst::IEq(typ.clone(), cmp.clone(), v1.clone(), Arg::Const(TRUE_CONST)),
                Inst::IBrk(cmp, true_branch, false_branch),
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

            let res = gen.sym(true);
            ins1.push(Inst::ILoad(typ.clone(), res.clone(), store.clone()));
            alloc1.push(Inst::IAlloc(typ.clone(), store));

            return (ins1, res, alloc1);
        }
        Expr::EApp(func, args) => {
            let (mut is1, v1, mut all1) = compile_expr(func, scope.clone(), gen, env);
            if let Arg::AVar(_) = v1.clone() {
                let res = gen.sym(true);
                let mut arg_vec = Vec::new();

                for a in args {
                    let (mut is, v, mut all) = compile_expr(a, scope.clone(), gen, env);
                    arg_vec.push(v);
                    is1.append(&mut is);
                    all1.append(&mut all);
                }

                is1.push(Inst::ICall(VType::I64, Some(res.clone()), v1, arg_vec));
                (is1, res, all1)
            } else {
                panic!(format!("Compile error: {:?}", v1))
            }
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
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    let (mut is1, v1, mut a1) = compile_expr(e1, scope.clone(), gen, env);
    let (mut is2, v2, mut a2) = compile_expr(e2, scope.clone(), gen, env);
    let var1 = gen.sym(true);
    let var2 = gen.sym(true);

    let (mut op_is, res) = match op {
        Prim2::Add => (
            vec![
                Inst::IAdd64(var1.clone(), v1, v2),
                Inst::ISub(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
        ),
        Prim2::Minus => (
            vec![
                Inst::ISub(var1.clone(), v1, v2),
                Inst::IAdd64(var2.clone(), var1, Arg::Const(1)),
            ],
            var2,
        ),
        Prim2::Times => {
            let var3 = gen.sym(true);
            let var4 = gen.sym(true);
            (
                vec![
                    Inst::IAshr(var1.clone(), v1, Arg::Const(1)),
                    Inst::IMul(var2.clone(), var1.clone(), v2),
                    Inst::ISub(var3.clone(), var2, var1.clone()),
                    Inst::IAdd64(var4.clone(), var3, Arg::Const(1)),
                ],
                var4,
            )
        }
        Prim2::Equal => {
            let (mut insts, res) = bool_tail(var1.clone(), gen);
            let mut ins = vec![Inst::IEq(VType::I64, var1, v1, v2)];
            ins.append(&mut insts);
            (ins, res)
            // let typ = typenv.get_vtype(&e1, env).unwrap();
            // (vec![Inst::IEq(typ, var1.clone(), v1, v2)], var1, vec![])
        }
        Prim2::Less => {
            let (mut insts, res) = bool_tail(var1.clone(), gen);
            let mut ins = vec![Inst::ILt(var1, v1, v2)];
            ins.append(&mut insts);
            (ins, res)
            // (vec![Inst::ILt(var1.clone(), v1, v2)], var1, vec![])
        }
        Prim2::Greater => {
            let (mut insts, res) = bool_tail(var1.clone(), gen);
            let mut ins = vec![Inst::IGt(var1, v1, v2)];
            ins.append(&mut insts);
            (ins, res)
            // (vec![Inst::IGt(var1.clone(), v1, v2)], var1, vec![])
        }
    };

    is1.append(&mut is2);
    is1.append(&mut op_is);
    a1.append(&mut a2);

    (is1, res, a1)
}

fn bool_tail(cond: Arg, gen: &mut scope::Generator) -> (Vec<Inst>, Arg) {
    let v1 = gen.sym(true);
    let v2 = gen.sym(true);
    let v3 = gen.sym(true);

    (
        vec![
            Inst::IZExt(VType::I64, VType::I1, v1.clone(), cond),
            Inst::IShl(v2.clone(), v1, Arg::Const(3)),
            Inst::IAdd64(v3.clone(), v2, Arg::Const(2)),
        ],
        v3,
    )
}
