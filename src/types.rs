use crate::backend::llvm::VType;
use crate::expr::expr::*;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

#[derive(Debug, Clone, Hash)]
pub enum TypeExpr<'a, 'b> {
    TVar(&'b Expr<'a>, Option<&'b Expr<'a>>), // an expression and its defining let (parent)
    TNum,
    TBool,
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
                    match (p1, p2) {
                        (None, None) => true,
                        (Some(loc1), Some(loc2)) => ptr::eq(*loc1, *loc2),
                        _ => false,
                    }
                }
            }
            _ => false,
        }
    }
}

impl<'a, 'b> Eq for TypeExpr<'a, 'b> {}

pub struct TypeEnv<'a, 'b>(pub HashMap<TypeExpr<'a, 'b>, TypeExpr<'a, 'b>>);

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn get_vtype(&self, e: &'b Expr<'a>, env: Option<&'b Expr<'a>>) -> Result<VType, String> {
        match self.0.get(&TypeExpr::TVar(&e, env)) {
            Some(TypeExpr::TNum) => Ok(VType::I64),
            Some(TypeExpr::TBool) => Ok(VType::I1),
            _ => Err(format!("Compile error: Unbound type {:?} {:?}", e, env)),
        }
    }

    pub fn new(exprs: &'b [Expr<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut eqns = HashSet::new();

        for e in exprs.iter() {
            extract_eqns(e, None, &mut eqns);
        }

        let eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> = eqns.into_iter().collect();
        unify(eqns)
    }
}

fn subst<'a, 'b>(
    e: TypeExpr<'a, 'b>,
    from: TypeExpr<'a, 'b>,
    to: TypeExpr<'a, 'b>,
) -> TypeExpr<'a, 'b> {
    match e {
        TNum => e,
        TBool => e,
        TVar(_, _) => {
            if from == e {
                to
            } else {
                e
            }
        }
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
                subs = subs
                    .into_iter()
                    .map(|x: (TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)| {
                        (x.0, subst(x.1, texpr.clone(), other.clone()))
                    })
                    .collect();
                eqns = eqns
                    .into_iter()
                    .map(|x: (TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)| {
                        (
                            subst(x.0, texpr.clone(), other.clone()),
                            subst(x.1, texpr.clone(), other.clone()),
                        )
                    })
                    .collect();
                subs.push((texpr, other))
            }
            Some((TypeExpr::TNum, TypeExpr::TNum)) | Some((TypeExpr::TBool, TypeExpr::TBool)) => {}
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
    env: Option<&'b Expr<'a>>,
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

// returns a list of type equations
fn extract_eqns<'a, 'b>(
    e: &'b Expr<'a>,
    env: Option<&'b Expr<'a>>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
) {
    match e {
        Expr::EId(_) => {}
        Expr::ENum(_) => {
            set.insert((TVar(e, env), TNum));
        }
        Expr::EBool(_) => {
            set.insert((TVar(e, env), TBool));
        }
        Expr::EPrim2(op, e1, e2) => extract_prim2(e, op, e1, e2, env, set),
        Expr::EIf(cond, e1, e2) => {
            extract_eqns(cond, env, set);
            extract_eqns(e1, env, set);
            extract_eqns(e2, env, set);
            set.extend(vec![
                (TVar(cond, env), TBool),
                (TVar(e1, env), TVar(e2, env)),
                (TVar(e, env), TVar(e1, env)),
            ]);
        }
        Expr::ELet(bind, body) => {
            set.insert((TVar(e, env), TVar(body, Some(e))));

            for Binding(x, exp) in bind {
                extract_eqns(exp, env, set);

                if let Some(y) = extract_eid(body, x, Some(e)) {
                    set.insert((y, TVar(exp, env)));
                }
            }
            extract_eqns(body, Some(e), set);
        }
        Expr::EPrint(expr) => {
            set.insert((TVar(expr, env), TVar(e, env)));
            extract_eqns(expr, env, set);
        }
    }
}

fn extract_prim2<'a, 'b>(
    e: &'b Expr<'a>,
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    env: Option<&'b Expr<'a>>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
) {
    extract_eqns(e1, env, set);
    extract_eqns(e2, env, set);

    match op {
        Prim2::Add | Prim2::Minus | Prim2::Times => {
            set.extend(vec![
                (TVar(e1, env), TNum),
                (TVar(e2, env), TNum),
                (TVar(e, env), TNum),
            ]);
        }
        Prim2::Less | Prim2::Greater => {
            set.extend(vec![
                (TVar(e1, env), TNum),
                (TVar(e2, env), TNum),
                (TVar(e, env), TBool),
            ]);
        }
        Prim2::Equal => {
            set.extend(vec![(TVar(e1, env), TVar(e2, env)), (TVar(e, env), TBool)]);
        }
    }
}
