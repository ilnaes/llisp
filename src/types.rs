use crate::expr::expr::*;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

#[derive(Debug, Clone, Hash)]
enum TypeExpr<'a, 'b> {
    TEVar(&'b Expr<'a>, Option<&'b Expr<'a>>), // an expression and its defining let (parent)
    TENum,
    TEBool,
}

impl<'a, 'b> PartialEq for TypeExpr<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TENum, TENum) | (TEBool, TEBool) => true,
            (TEVar(x1, p1), TEVar(x2, p2)) => {
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

use TypeExpr::*;

// pub struct EnvGen(usize); // enverates a new environment

pub struct TypeEnv<'a, 'b>(HashMap<&'b Expr<'a>, TypeExpr<'a, 'b>>);

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(exprs: &'b [Expr<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut eqns = HashSet::new();

        for e in exprs.iter() {
            Self::extract_eqns(e, None, &mut eqns);
        }

        let mut _eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> = eqns.into_iter().collect();

        Ok(TypeEnv(HashMap::new()))
    }

    // extracts a TEVar corresponding to a str from a binding
    fn extract_eid(
        e: &'b Expr<'a>,
        id: &'a str,
        env: Option<&'b Expr<'a>>,
    ) -> Option<TypeExpr<'a, 'b>> {
        match e {
            Expr::EId(s) => {
                if *s == id {
                    Some(TEVar(e, env))
                } else {
                    None
                }
            }
            Expr::ENum(_) | Expr::EBool(_) => None,
            Expr::EPrim2(_, e1, e2) => {
                let res = Self::extract_eid(e1, id, env);
                if res.is_some() {
                    return res;
                }
                Self::extract_eid(e2, id, env)
            }
            Expr::EIf(cond, e1, e2) => {
                let res = Self::extract_eid(cond, id, env);
                if res.is_some() {
                    return res;
                }
                let res = Self::extract_eid(e1, id, env);
                if res.is_some() {
                    return res;
                }
                Self::extract_eid(e2, id, env)
            }
            Expr::ELet(bind, body) => {
                let mut rebound = false;
                for Binding(x, e) in bind {
                    let res = Self::extract_eid(e, id, env);
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
                    Self::extract_eid(body, id, env)
                }
            }
        }
    }

    // returns a list of type equations
    fn extract_eqns(
        e: &'b Expr<'a>,
        env: Option<&'b Expr<'a>>,
        set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    ) {
        match e {
            Expr::EId(_) => {}
            Expr::ENum(_) => {
                set.insert((TEVar(e, env), TENum));
            }
            Expr::EBool(_) => {
                set.insert((TEVar(e, env), TEBool));
            }
            Expr::EPrim2(op, e1, e2) => Self::extract_prim2(e, op, e1, e2, env, set),
            Expr::EIf(cond, e1, e2) => {
                Self::extract_eqns(cond, env, set);
                Self::extract_eqns(e1, env, set);
                Self::extract_eqns(e2, env, set);
                set.extend(vec![
                    (TEVar(cond, env), TEBool),
                    (TEVar(e1, env), TEVar(e2, env)),
                    (TEVar(e, env), TEVar(e1, env)),
                ]);
            }
            Expr::ELet(bind, body) => {
                set.insert((TEVar(e, env), TEVar(body, Some(e))));

                for Binding(x, exp) in bind {
                    Self::extract_eqns(exp, env, set);

                    if let Some(y) = Self::extract_eid(body, x, Some(e)) {
                        set.insert((y, TEVar(exp, env)));
                    }
                }
                Self::extract_eqns(body, Some(e), set);
            }
        }
    }

    fn extract_prim2(
        e: &'b Expr<'a>,
        op: &'b Prim2,
        e1: &'b Expr<'a>,
        e2: &'b Expr<'a>,
        env: Option<&'b Expr<'a>>,
        set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    ) {
        Self::extract_eqns(e1, env, set);
        Self::extract_eqns(e2, env, set);

        match op {
            Prim2::Add | Prim2::Minus | Prim2::Times => {
                set.extend(vec![
                    (TEVar(e1, env), TENum),
                    (TEVar(e2, env), TENum),
                    (TEVar(e, env), TENum),
                ]);
            }
            Prim2::Less | Prim2::Greater => {
                set.extend(vec![
                    (TEVar(e1, env), TENum),
                    (TEVar(e2, env), TENum),
                    (TEVar(e, env), TEBool),
                ]);
            }
            Prim2::Equal => {
                set.extend(vec![
                    (TEVar(e1, env), TEVar(e2, env)),
                    (TEVar(e, env), TEBool),
                ]);
            }
        }
    }
}
