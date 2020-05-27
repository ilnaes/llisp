use crate::expr::expr::*;
use std::collections::HashMap;

#[derive(Debug, Clone, PartialEq)]
enum TypeExpr<'a, 'b> {
    TEVar(&'b Expr<'a>, usize),
    TENum,
    TEBool,
}

use TypeExpr::*;

pub struct EnvGen(usize);

pub struct TypeEnv<'a, 'b>(HashMap<&'b Expr<'a>, TypeExpr<'a, 'b>>);

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn new(expr: &'b [Expr<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut gen = EnvGen(0);
        let _eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> = expr
            .iter()
            .map(|x| Self::extract_eqns(x, &mut gen))
            .flatten()
            .collect();

        // for eq in _eqns {
        //     println!("{:?}\n  == {:?}", eq.0, eq.1);
        // }

        Ok(TypeEnv(HashMap::new()))
    }

    // extracts a TEVar corresponding to a str from a binding
    fn extract_eid(e: &'b Expr<'a>, id: &'a str, env: usize) -> Option<TypeExpr<'a, 'b>> {
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
        gen: &mut EnvGen,
    ) -> Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> {
        let env = gen.0;
        let mut res = Vec::new();
        match e {
            Expr::EId(_) => {}
            Expr::ENum(_) => res.push((TEVar(e, env), TENum)),
            Expr::EBool(_) => res.push((TEVar(e, env), TEBool)),
            Expr::EPrim2(op, e1, e2) => res.append(&mut Self::extract_prim2(e, op, e1, e2, gen)),
            Expr::EIf(cond, e1, e2) => {
                res.append(&mut Self::extract_eqns(cond, gen));
                res.append(&mut Self::extract_eqns(e1, gen));
                res.append(&mut Self::extract_eqns(e2, gen));
                res.append(&mut vec![
                    (TEVar(cond, env), TEBool),
                    (TEVar(e1, env), TEVar(e2, env)),
                    (TEVar(e, env), TEVar(e1, env)),
                ]);
            }
            Expr::ELet(bind, body) => {
                res.push((TEVar(e, env), TEVar(body, env + 1)));

                for Binding(x, e) in bind {
                    res.append(&mut Self::extract_eqns(e, gen));

                    if let Some(y) = Self::extract_eid(body, x, env + 1) {
                        res.push((y, TEVar(e, env)));
                    }
                }
                gen.0 += 1;
                res.append(&mut Self::extract_eqns(body, gen));
            }
        }
        res
    }

    fn extract_prim2(
        e: &'b Expr<'a>,
        op: &'b Prim2,
        e1: &'b Expr<'a>,
        e2: &'b Expr<'a>,
        gen: &mut EnvGen,
    ) -> Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)> {
        let env = gen.0;
        let mut res = Vec::new();
        res.append(&mut Self::extract_eqns(e1, gen));
        res.append(&mut Self::extract_eqns(e2, gen));

        match op {
            Prim2::Add | Prim2::Minus | Prim2::Times => {
                res.append(&mut vec![
                    (TEVar(e1, env), TENum),
                    (TEVar(e2, env), TENum),
                    (TEVar(e, env), TENum),
                ]);
            }
            Prim2::Less | Prim2::Greater => {
                res.append(&mut vec![
                    (TEVar(e1, env), TENum),
                    (TEVar(e2, env), TENum),
                    (TEVar(e, env), TEBool),
                ]);
            }
            Prim2::Equal => {
                res.append(&mut vec![
                    (TEVar(e1, env), TEVar(e2, env)),
                    (TEVar(e, env), TEBool),
                ]);
            }
        }

        res
    }
}
