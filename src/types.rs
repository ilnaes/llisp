use crate::backend::llvm::VType;
use crate::expr::expr::*;
use im;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

#[derive(Debug, Clone, Hash)]
pub enum TypeExpr<'a, 'b> {
    TNum,
    TBool,
    TFun(Vec<TypeExpr<'a, 'b>>, Box<TypeExpr<'a, 'b>>),

    // an expression and a pointer to when it's defined
    TVar(&'b Expr<'a>, &'b Expr<'a>),
}

use TypeExpr::*;

impl<'a, 'b> PartialEq for TypeExpr<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TNum, TNum) | (TBool, TBool) => true,
            (TVar(x1, p1), TVar(x2, p2)) => {
                if x1 != x2 {
                    false
                } else {
                    // we test strict equality of the parent
                    ptr::eq(*p1, *p2)
                }
            }
            (TFun(a1, r1), TFun(a2, r2)) => {
                if a1.len() != a2.len() {
                    false
                } else {
                    for i in 0..a1.len() {
                        if !a1[i].eq(&a2[i]) {
                            return false;
                        }
                    }
                    r1.eq(r2)
                }
            }
            _ => false,
        }
    }
}

impl<'a, 'b> Eq for TypeExpr<'a, 'b> {}

pub struct TypeEnv<'a, 'b>(pub HashMap<TypeExpr<'a, 'b>, TypeExpr<'a, 'b>>);

fn type_to_vtype(typ: &TypeExpr) -> Result<VType, String> {
    match typ {
        TypeExpr::TNum | TypeExpr::TBool => Ok(VType::I64),
        TypeExpr::TFun(args, ret) => Ok(VType::Func(
            args.iter().map(|x| type_to_vtype(x).unwrap()).collect(),
            Box::new(type_to_vtype(ret)?),
        )),
        TypeExpr::TVar(_, _) => Err(format!("Compile error: Unbound type")),
    }
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn get_vtype(&self, e: &'b Expr<'a>, env: &'b Expr<'a>) -> Result<VType, String> {
        match self.0.get(&TypeExpr::TVar(e, env)) {
            Some(x) => type_to_vtype(x),
            // Some(TypeExpr::TBool) => Ok(VType::I1),
            _ => Err(format!("Compile error: Unbound type {:?} {:?}", e, env)),
        }
    }

    pub fn new(exprs: &'b [Def<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut eqns = HashSet::new();

        extract_prog_eqns(exprs, &mut eqns);

        let eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> = eqns.into_iter().collect();
        unify(eqns)
    }
}

fn extract_prog_eqns<'a, 'b>(
    prog: &'b [Def<'a>],
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
) {
    let mut scope = im::HashMap::new();

    // gather all declared top level functions
    for i in 0..prog.len() {
        let Def::FuncDef(name, _, _) = &prog[i];
        scope.insert(name.get_str().unwrap(), name);

        // TODO: rule out argument shadows
        for j in (i + 1)..prog.len() {
            let Def::FuncDef(name2, _, _) = &prog[j];
            set.insert((TVar(name, name2), TVar(name, name)));
        }
    }

    for f in prog {
        let Def::FuncDef(name, args, body) = f;
        let mut sc = scope.clone();

        // put args in scope
        for x in args {
            sc.insert(x.get_str().unwrap(), name);
        }

        set.insert((
            TVar(name, name),
            TFun(
                args.iter().map(|x| TVar(x, name)).collect(),
                Box::new(get_type(body, name)),
            ),
        ));

        extract_expr_eqns(body, name, set, sc.clone());
    }
}

// returns a list of type equations
fn extract_expr_eqns<'a, 'b>(
    e: &'b Expr<'a>,
    env: &'b Expr<'a>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    scope: im::HashMap<&'a str, &'b Expr<'a>>,
) {
    match e {
        Expr::EId(_) | Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EPrim2(op, e1, e2) => extract_prim2(e, op, e1, e2, env, set, scope),
        Expr::EIf(cond, e1, e2) => {
            extract_expr_eqns(cond, env, set, scope.clone());
            extract_expr_eqns(e1, env, set, scope.clone());
            extract_expr_eqns(e2, env, set, scope.clone());
            set.extend(vec![
                (get_type(cond, env), TBool),
                (get_type(e1, env), get_type(e2, env)),
                (get_type(e, env), get_type(e1, env)),
            ]);
        }
        Expr::ELet(bind, body) => {
            let mut new_scope = scope.clone();
            for Binding(x, exp) in bind {
                extract_expr_eqns(exp, env, set, scope.clone());

                if let Some(y) = extract_eid(body, x, e) {
                    set.insert((y, get_type(exp, env)));
                }
                new_scope.insert(x, e);
            }

            set.insert((get_type(e, env), get_type(body, e)));
            extract_expr_eqns(body, e, set, new_scope.clone());
        }
        Expr::EPrint(expr) => {
            set.insert((get_type(expr, env), get_type(e, env)));
            extract_expr_eqns(expr, env, set, scope.clone());
        }
        Expr::EApp(f, args) => {
            set.insert((
                get_type(f, env),
                TFun(
                    args.iter().map(|x| get_type(x, env)).collect(),
                    Box::new(get_type(e, env)),
                ),
            ));

            extract_expr_eqns(f, env, set, scope.clone());
            for a in args {
                extract_expr_eqns(a, env, set, scope.clone());
            }
        }
    }
}

fn extract_prim2<'a, 'b>(
    e: &'b Expr<'a>,
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    env: &'b Expr<'a>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    scope: im::HashMap<&'a str, &'b Expr<'a>>,
) {
    extract_expr_eqns(e1, env, set, scope.clone());
    extract_expr_eqns(e2, env, set, scope.clone());

    match op {
        Prim2::Add | Prim2::Minus | Prim2::Times => {
            set.extend(vec![
                (get_type(e1, env), TNum),
                (get_type(e2, env), TNum),
                (get_type(e, env), TNum),
            ]);
        }
        Prim2::Less | Prim2::Greater => {
            set.extend(vec![
                (get_type(e1, env), TNum),
                (get_type(e2, env), TNum),
                (get_type(e, env), TBool),
            ]);
        }
        Prim2::Equal => {
            set.extend(vec![
                (get_type(e1, env), get_type(e2, env)),
                (get_type(e, env), TBool),
            ]);
        }
    }
}

fn subst<'a, 'b>(
    e: TypeExpr<'a, 'b>,
    from: &TypeExpr<'a, 'b>,
    to: TypeExpr<'a, 'b>,
) -> TypeExpr<'a, 'b> {
    match e {
        TNum => e,
        TBool => e,
        TVar(_, _) => {
            if from == &e {
                to
            } else {
                e
            }
        }
        TFun(args, ret) => TFun(
            args.into_iter()
                .map(|x| subst(x, from, to.clone()))
                .collect(),
            Box::new(subst(*ret, from, to.clone())),
        ),
    }
}

fn unify<'a, 'b>(
    mut eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
) -> Result<TypeEnv<'a, 'b>, String> {
    let mut subs = Vec::new();

    loop {
        match eqns.pop() {
            None => break,
            Some((TypeExpr::TVar(exp, p), other)) | Some((other, TypeExpr::TVar(exp, p))) => {
                let texpr = TypeExpr::TVar(exp, p);

                // TODO: occurence check

                subs = subs
                    .into_iter()
                    .map(|x: (TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)| {
                        (x.0, subst(x.1, &texpr, other.clone()))
                    })
                    .collect();
                eqns = eqns
                    .into_iter()
                    .map(|x: (TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)| {
                        (
                            subst(x.0, &texpr, other.clone()),
                            subst(x.1, &texpr, other.clone()),
                        )
                    })
                    .collect();
                subs.push((texpr, other))
            }
            Some((TypeExpr::TNum, TypeExpr::TNum)) | Some((TypeExpr::TBool, TypeExpr::TBool)) => {}
            Some((TypeExpr::TFun(mut arg1, ret1), TypeExpr::TFun(mut arg2, ret2))) => {
                if arg1.len() != arg2.len() {
                    return Err("Type inference conflict".to_string());
                }
                for _ in 0..arg1.len() {
                    eqns.push((arg1.pop().unwrap(), arg2.pop().unwrap()));
                }
                eqns.push((*ret1, *ret2));
            }
            _ => return Err("Type inference conflict".to_string()),
        }
    }

    // for e in subs.iter() {
    //     eprintln!("{:?}\n  == {:?}", e.0, e.1);
    // }

    let res: HashMap<TypeExpr<'a, 'b>, TypeExpr<'a, 'b>> = subs.into_iter().collect();
    Ok(TypeEnv(res))
}

// extracts a TVar corresponding to a str from a binding
fn extract_eid<'a, 'b>(
    e: &'b Expr<'a>,
    id: &'a str,
    env: &'b Expr<'a>,
) -> Option<TypeExpr<'a, 'b>> {
    match e {
        Expr::EId(s) => {
            if *s == id {
                Some(TVar(e, env))
            } else {
                None
            }
        }
        Expr::ENum(_) | Expr::EBool(_) => None,
        Expr::EPrim2(_, e1, e2) => {
            let res = extract_eid(e1, id, env);
            if res.is_some() {
                return res;
            }
            extract_eid(e2, id, env)
        }
        Expr::EPrint(e) => extract_eid(e, id, env),
        Expr::EIf(cond, e1, e2) => {
            let res = extract_eid(cond, id, env);
            if res.is_some() {
                return res;
            }
            let res = extract_eid(e1, id, env);
            if res.is_some() {
                return res;
            }
            extract_eid(e2, id, env)
        }
        Expr::ELet(bind, body) => {
            let mut rebound = false;
            for Binding(x, e) in bind {
                let res = extract_eid(e, id, env);
                if res.is_some() {
                    return res;
                }

                if *x == id {
                    rebound = true;
                }
            }

            if rebound {
                None
            } else {
                extract_eid(body, id, env)
            }
        }
        Expr::EApp(f, args) => {
            let res = extract_eid(f, id, env);
            if res.is_some() {
                return res;
            }

            for a in args {
                let res = extract_eid(a, id, env);
                if res.is_some() {
                    return res;
                }
            }
            None
        }
    }
}

fn get_type<'a, 'b>(e: &'b Expr<'a>, env: &'b Expr<'a>) -> TypeExpr<'a, 'b> {
    match e {
        Expr::ENum(_) => TNum,
        Expr::EBool(_) => TBool,
        Expr::EId(_) => TVar(e, env),
        e => TVar(e, env),
    }
}
