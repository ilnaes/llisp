use super::scope::*;
use super::*;
use crate::backend::llvm::{nary_func, FunDef};
use crate::expr::expr::*;
use crate::types::{get_free, TypeEnv};
use im;
use std::collections::HashMap;

// tags:
// XXX1 - int
// 0010 - false
// 1010 - true
// X000 - heap alloc

const TRUE_CONST: i64 = 0xA;
const FALSE_CONST: i64 = 0x2;

pub fn compile_prog<'a, 'b>(
    prog: &'b [Def<'a>],
    typenv: &TypeEnv<'a, 'b>,
    mut gen: Generator,
) -> Vec<FunDef> {
    let mut arg_scope = Scope::new();
    let mut res: Vec<FunDef> = Vec::new();
    let mut all_globals: HashMap<&'b Expr<'a>, bool> = HashMap::new();

    // register all top-level functions
    for def in prog {
        let Def::FuncDef(f, _, _) = def;
        if let Expr::EId(name) = f {
            arg_scope.register(
                name.to_string(),
                Arg::AVar(Var::Global(f.get_str().unwrap())),
            );
            all_globals.insert(f, false);
        }
    }

    for def in prog {
        let Def::FuncDef(f, args, body) = def;
        let mut globals = all_globals.clone();

        // get all top level functions called in body
        hoist_globals(body, &arg_scope, &mut globals);

        let mut a_sc = arg_scope.clone();

        let mut arg_vec: Vec<String> = args
            .iter()
            .map(|x| {
                a_sc.register(
                    x.get_str().unwrap(),
                    Arg::AVar(Var::Local(x.get_str().unwrap())),
                );
                x.get_str().unwrap()
            })
            .collect();

        let mut insts = Vec::new();

        if let Expr::ELambda(_, _, _) = f {
            arg_vec.push("self".to_string());

            let mut free: Vec<&'b Expr<'a>> = Vec::new();
            get_free(f, im::HashSet::new(), &mut free);

            let free: Vec<&'b Expr<'a>> = free.into_iter().collect();

            let selfptr = gen.sym_arg(true);
            insts.push(Inst::IInttoptr(
                VType::Ptr(Box::new(VType::I64)),
                VType::I64,
                selfptr.clone(),
                Arg::AVar(Var::Local("self".to_string())),
            ));

            // bring closed variables into scope
            for (i, var) in free.iter().enumerate() {
                let slot = gen.sym_arg(true);
                let v = gen.sym_arg(true);

                insts.append(&mut vec![
                    Inst::IGEP(
                        VType::I64,
                        slot.clone(),
                        selfptr.clone(),
                        Arg::Const((i + 1) as i64),
                    ),
                    Inst::ILoad(VType::I64, v.clone(), slot),
                ]);

                a_sc.register(var.clone().get_str().unwrap(), v);
            }
        } else if f.get_str().unwrap() != "our_main" {
            arg_vec.push("self".to_string());
        }

        for (f, present) in globals {
            // package all called functions
            if !present {
                continue;
            }

            let malloc = gen.sym_arg(true);
            let ptr = gen.sym_arg(true);
            let val = gen.sym_arg(true);
            let typ = typenv.get_vtype(f).unwrap();

            insts.append(&mut vec![
                Inst::ICall(
                    VType::Ptr(Box::new(VType::I64)),
                    Some(malloc.clone()),
                    Arg::AVar(Var::Global("new".to_string())),
                    vec![Arg::Const(2)],
                ),
                Inst::IPtrtoint(
                    VType::I64,
                    typ,
                    ptr.clone(),
                    Arg::AVar(Var::Global(f.get_str().unwrap())),
                ),
                Inst::IStore(VType::I64, malloc.clone(), ptr),
                Inst::IPtrtoint(
                    VType::I64,
                    VType::Ptr(Box::new(VType::I64)),
                    val.clone(),
                    malloc,
                ),
            ]);

            a_sc.register(f.get_str().unwrap(), val);
        }

        let (mut insts1, v, mut alloc) = compile_expr(body, a_sc.clone(), &mut gen, typenv);
        insts.append(&mut insts1);

        alloc.append(&mut insts);
        alloc.push(Inst::IRet(v));

        let s = match f {
            Expr::EId(s) => s.to_string(),
            Expr::ELambda(s, _, _) => s.clone(),
            _ => panic!(format!("Compile error: Invalid function name {:?}", f)),
        };

        res.push(FunDef {
            name: s,
            args: arg_vec,
            inst: alloc,
        })
    }

    res
}

fn compile_expr<'a, 'b>(
    expr: &'b Expr<'a>,
    arg_scope: Scope,         // EId -> LLVM Variable
    gen: &mut Generator,      // symbol generator
    typenv: &TypeEnv<'a, 'b>, // Expr -> TypeExpr
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    match expr {
        Expr::ENum(n) => {
            let var = gen.sym_arg(true);
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
            let var = gen.sym_arg(true);
            (
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
            // static checkers should catch this
            match arg_scope.get(x).unwrap() {
                Arg::AVar(_) => (vec![], arg_scope.get(x).unwrap(), vec![]),
                _ => panic!(format!("Improper scoped variable {:?}", x)),
            }
        }
        Expr::ETup(vars) => {
            let mut insts = vec![];
            let mut all1 = vec![];

            let malloc = gen.sym_arg(true);
            let num_vars = gen.sym_arg(true);
            let val = gen.sym_arg(true);

            // malloc enough space
            insts.append(&mut vec![
                Inst::ICall(
                    VType::Ptr(Box::new(VType::I64)),
                    Some(malloc.clone()),
                    Arg::AVar(Var::Global("new".to_string())),
                    vec![Arg::Const(1 + vars.len() as i64)],
                ),
                Inst::IAdd64(
                    num_vars.clone(),
                    Arg::Const(0),
                    Arg::Const(vars.len() as i64),
                ),
                Inst::IStore(VType::I64, malloc.clone(), num_vars),
                Inst::IPtrtoint(
                    VType::I64,
                    VType::Ptr(Box::new(VType::I64)),
                    val.clone(),
                    malloc.clone(),
                ),
            ]);

            // store items
            for (i, v) in vars.iter().enumerate() {
                let (mut is, res, mut all) = compile_expr(v, arg_scope.clone(), gen, typenv);
                all1.append(&mut all);
                insts.append(&mut is);

                let slot = gen.sym_arg(true);
                insts.append(&mut vec![
                    Inst::IGEP(
                        VType::I64,
                        slot.clone(),
                        malloc.clone(),
                        Arg::Const((i + 1) as i64),
                    ),
                    Inst::IStore(VType::I64, slot, res),
                ]);
            }
            (insts, val, all1)
        }
        Expr::EPrim2(op, e1, e2) => parse_prim2(op, e1, e2, arg_scope.clone(), gen, typenv),
        Expr::ELet(bind, body) => {
            let (mut is1, arg_scope, mut alloc) = bind.iter().fold(
                (vec![], arg_scope.clone(), vec![]),
                |(mut res, mut a_sc, mut all), Binding(x, e)| {
                    // be sure to use old arg_scope
                    let (mut is, v, mut a) = compile_expr(e, arg_scope.clone(), gen, typenv);
                    a_sc.register(x.get_str().unwrap(), v);

                    all.append(&mut a);
                    res.append(&mut is);
                    (res, a_sc, all)
                },
            );

            let (mut is2, v, mut alloc1) = compile_expr(body, arg_scope.clone(), gen, typenv);
            is1.append(&mut is2);
            alloc.append(&mut alloc1);
            (is1, v, alloc)
        }
        Expr::EPrint(e) => {
            let (mut is1, v1, alloc1) = compile_expr(e, arg_scope.clone(), gen, typenv);

            is1.push(Inst::ICall(
                VType::Void,
                None,
                Arg::AVar(Var::Global("print".to_string())),
                vec![v1.clone()],
            ));
            (is1, v1, alloc1)
        }
        Expr::EIf(cond, e1, e2) => {
            let (mut ins1, v1, mut alloc1) = compile_expr(cond, arg_scope.clone(), gen, typenv);
            let (mut ins2, v2, mut alloc2) = compile_expr(e1, arg_scope.clone(), gen, typenv);
            let (mut ins3, v3, mut alloc3) = compile_expr(e2, arg_scope.clone(), gen, typenv);

            let cmp = gen.sym_arg(true);

            let store = gen.sym_arg(true);
            let true_branch = gen.sym_arg(true);
            let true_label = true_branch.to_string();
            let false_branch = gen.sym_arg(true);
            let false_label = false_branch.to_string();
            let after = gen.sym_arg(true);
            let after_label = after.to_string();

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

            let res = gen.sym_arg(true);
            ins1.push(Inst::ILoad(typ.clone(), res.clone(), store.clone()));
            alloc1.push(Inst::IAlloc(typ.clone(), store));

            return (ins1, res, alloc1);
        }
        Expr::EApp(func, args) => {
            let (mut is1, v1, mut all1) = compile_expr(func, arg_scope.clone(), gen, typenv);
            let ptr = gen.sym_arg(true);
            let load = gen.sym_arg(true);
            let fptr = gen.sym_arg(true);
            let res = gen.sym_arg(true);

            let mut arg_vec = Vec::new();
            for a in args {
                let (mut is, v, mut all) = compile_expr(a, arg_scope.clone(), gen, typenv);
                arg_vec.push(v);
                is1.append(&mut is);
                all1.append(&mut all);
            }

            arg_vec.push(v1.clone());

            let typ = nary_func(args.len() + 1);

            is1.append(&mut vec![
                Inst::IInttoptr(
                    VType::Ptr(Box::new(VType::I64)),
                    VType::I64,
                    ptr.clone(),
                    v1,
                ),
                Inst::ILoad(VType::I64, load.clone(), ptr),
                Inst::IInttoptr(typ, VType::I64, fptr.clone(), load),
                Inst::ICall(VType::I64, Some(res.clone()), fptr, arg_vec),
            ]);
            (is1, res, all1)
        }
        Expr::ELambda(f, args, _) => {
            let mut insts = Vec::new();
            let mut free = Vec::new();
            get_free(expr, im::HashSet::new(), &mut free);

            let free = free
                .into_iter()
                .map(|x| x.get_str().unwrap())
                .collect::<Vec<String>>();

            let malloc = gen.sym_arg(true);
            let ptr = gen.sym_arg(true);
            let val = gen.sym_arg(true);

            let typ = nary_func(args.len() + 1);

            insts.append(&mut vec![
                Inst::ICall(
                    VType::Ptr(Box::new(VType::I64)),
                    Some(malloc.clone()),
                    Arg::AVar(Var::Global("new".to_string())),
                    vec![Arg::Const(2 + free.len() as i64)],
                ),
                Inst::IPtrtoint(
                    VType::I64,
                    typ,
                    ptr.clone(),
                    Arg::AVar(Var::Global(f.clone())),
                ),
                Inst::IStore(VType::I64, malloc.clone(), ptr),
                Inst::IPtrtoint(
                    VType::I64,
                    VType::Ptr(Box::new(VType::I64)),
                    val.clone(),
                    malloc.clone(),
                ),
            ]);

            for (i, v) in free.iter().enumerate() {
                let slot = gen.sym_arg(true);
                insts.append(&mut vec![
                    Inst::IGEP(
                        VType::I64,
                        slot.clone(),
                        malloc.clone(),
                        Arg::Const((i + 1) as i64),
                    ),
                    Inst::IStore(VType::I64, slot, arg_scope.get(v).unwrap()),
                ]);
            }
            (insts, val, vec![])
        }
    }
}

fn parse_prim2<'a, 'b>(
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    arg_scope: Scope,
    gen: &mut Generator,
    typenv: &TypeEnv<'a, 'b>,
) -> (Vec<Inst>, Arg, Vec<Inst>) {
    let (mut is1, v1, mut a1) = compile_expr(e1, arg_scope.clone(), gen, typenv);
    let (mut is2, v2, mut a2) = compile_expr(e2, arg_scope.clone(), gen, typenv);
    let var1 = gen.sym_arg(true);
    let var2 = gen.sym_arg(true);

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
            let var3 = gen.sym_arg(true);
            let var4 = gen.sym_arg(true);
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
        }
        Prim2::Less => {
            let (mut insts, res) = bool_tail(var1.clone(), gen);
            let mut ins = vec![Inst::ILt(var1, v1, v2)];
            ins.append(&mut insts);
            (ins, res)
        }
        Prim2::Greater => {
            let (mut insts, res) = bool_tail(var1.clone(), gen);
            let mut ins = vec![Inst::IGt(var1, v1, v2)];
            ins.append(&mut insts);
            (ins, res)
        }
    };

    is1.append(&mut is2);
    is1.append(&mut op_is);
    a1.append(&mut a2);

    (is1, res, a1)
}

fn hoist_globals<'a, 'b>(
    expr: &'b Expr<'a>,
    arg_scope: &Scope,
    set: &mut HashMap<&'b Expr<'a>, bool>,
) {
    match expr {
        Expr::EId(x) => match arg_scope.get(x) {
            // this may over estimate as globals do not get shadowed
            Ok(Arg::AVar(Var::Global(_))) => {
                set.insert(expr, true);
            }
            _ => {}
        },
        Expr::ETup(vars) => {
            for v in vars {
                hoist_globals(v, arg_scope, set);
            }
        }
        Expr::EPrim2(_, e1, e2) => {
            hoist_globals(e1, arg_scope, set);
            hoist_globals(e2, arg_scope, set);
        }
        Expr::EPrint(e) => hoist_globals(e, arg_scope, set),
        Expr::EIf(c, e1, e2) => {
            hoist_globals(e1, arg_scope, set);
            hoist_globals(e2, arg_scope, set);
            hoist_globals(c, arg_scope, set);
        }
        Expr::EApp(f, args) => {
            hoist_globals(f, arg_scope, set);
            for a in args {
                hoist_globals(a, arg_scope, set);
            }
        }
        Expr::ELet(bind, body) => {
            for b in bind {
                hoist_globals(&b.1, arg_scope, set);
            }
            hoist_globals(body, arg_scope, set);
        }
        Expr::EBool(_) | Expr::ENum(_) | Expr::ELambda(_, _, _) => {}
    }
}

fn bool_tail(cond: Arg, gen: &mut Generator) -> (Vec<Inst>, Arg) {
    let v1 = gen.sym_arg(true);
    let v2 = gen.sym_arg(true);
    let v3 = gen.sym_arg(true);

    (
        vec![
            Inst::IZExt(VType::I64, VType::I1, v1.clone(), cond),
            Inst::IShl(v2.clone(), v1, Arg::Const(3)),
            Inst::IAdd64(v3.clone(), v2, Arg::Const(2)),
        ],
        v3,
    )
}
