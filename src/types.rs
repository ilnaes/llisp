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

impl<'a, 'b> TypeEnv<'a, 'b> {
    // pub fn get_vtype(&self, e: &'b Expr<'a>, env: Option<&'b Expr<'a>>) -> Result<VType, String> {
    //     match self.0.get(&TypeExpr::TVar(&e, env)) {
    //         Some(TypeExpr::TNum) => Ok(VType::I64),
    //         // Some(TypeExpr::TBool) => Ok(VType::I1),
    //         _ => Err(format!("Compile error: Unbound type {:?} {:?}", e, env)),
    //     }
    // }

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
    for f in prog {
        let Def::FuncDef(name, _, _) = f;
        scope.insert(name.get_str().unwrap(), name);
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
                Box::new(get_type(body, name, sc.clone())),
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
                (get_type(cond, env, scope.clone()), TBool),
                (
                    get_type(e1, env, scope.clone()),
                    get_type(e2, env, scope.clone()),
                ),
                (
                    get_type(e, env, scope.clone()),
                    get_type(e1, env, scope.clone()),
                ),
            ]);
        }
        Expr::ELet(bind, body) => {
            let mut new_scope = scope.clone();
            for Binding(x, exp) in bind {
                extract_expr_eqns(exp, env, set, scope.clone());

                if let Some(y) = extract_eid(body, x, e) {
                    set.insert((y, get_type(exp, env, scope.clone())));
                }
                new_scope.insert(x, e);
            }

            set.insert((
                get_type(e, env, scope.clone()),
                get_type(body, e, new_scope.clone()),
            ));
            extract_expr_eqns(body, e, set, new_scope.clone());
        }
        Expr::EPrint(expr) => {
            set.insert((
                get_type(expr, env, scope.clone()),
                get_type(e, env, scope.clone()),
            ));
            extract_expr_eqns(expr, env, set, scope.clone());
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
                (get_type(e1, env, scope.clone()), TNum),
                (get_type(e2, env, scope.clone()), TNum),
                (get_type(e, env, scope.clone()), TNum),
            ]);
        }
        Prim2::Less | Prim2::Greater => {
            set.extend(vec![
                (get_type(e1, env, scope.clone()), TNum),
                (get_type(e2, env, scope.clone()), TNum),
                (get_type(e, env, scope.clone()), TBool),
            ]);
        }
        Prim2::Equal => {
            set.extend(vec![
                (
                    get_type(e1, env, scope.clone()),
                    get_type(e2, env, scope.clone()),
                ),
                (get_type(e, env, scope.clone()), TBool),
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

    //     for e in subs.iter() {
    //         eprintln!("{:?}\n  == {:?}", e.0, e.1);
    //     }

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
    }
}

fn get_type<'a, 'b>(
    e: &'b Expr<'a>,
    env: &'b Expr<'a>,
    scope: im::HashMap<&'a str, &'b Expr<'a>>,
) -> TypeExpr<'a, 'b> {
    match e {
        Expr::ENum(_) => TNum,
        Expr::EBool(_) => TBool,
        Expr::EId(s) => TVar(e, scope.get(s).expect(&format!("BAD {}", s))),
        e => TVar(e, env),
    }
}
