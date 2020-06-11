use crate::backend::llvm::VType;
use crate::expr::expr::*;
use im;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

const PRINT: bool = false;

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
        TypeExpr::TFun(args, _) => {
            let mut a: Vec<VType> = args.iter().map(|_| VType::I64).collect();
            a.push(VType::I64);
            Ok(VType::Func(a, Box::new(VType::I64)))
        }
        TypeExpr::TVar(_, _) => Err(format!("Compile error: Unbound type")),
    }
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn get_vtype(
        &self,
        e: &'b Expr<'a>,
        env: &'b Expr<'a>,
        scope: im::HashMap<String, &'b Expr<'a>>,
    ) -> Result<VType, String> {
        let tv = match e {
            Expr::EId(s) => TypeExpr::TVar(e, scope.get(&s.to_string()).unwrap()),
            Expr::ELambda(s, _, _) => {
                let expr = scope.get(s).unwrap();
                TypeExpr::TVar(expr, expr)
            }
            _ => TypeExpr::TVar(e, env),
        };

        match self.0.get(&tv) {
            Some(x) => type_to_vtype(x),
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
    // keep track of when identifiers are introduced
    let mut scope: im::HashMap<String, &'b Expr<'a>> = im::HashMap::new();

    // gather all declared top level functions
    for i in 0..prog.len() {
        let Def::FuncDef(name, _, _) = &prog[i];

        if let Expr::EId(s) = name {
            // if non-lambda func, then populate into namespace
            // of any other function

            scope.insert(s.to_string(), name);

            // TODO: rule out argument shadows
            for j in (i + 1)..prog.len() {
                let Def::FuncDef(name2, _, _) = &prog[j];
                set.insert((TVar(name, name2), TVar(name, name)));
            }
        } else if let Expr::ELambda(s, _, _) = name {
            // put the right reference into scope
            scope.insert(s.clone(), name);
        }
    }

    for f in prog {
        let Def::FuncDef(name, args, body) = f;

        let mut sc = scope.clone();

        for a in args.iter() {
            sc.insert(a.get_str().unwrap(), name);
        }

        set.insert((
            TVar(name, name),
            TFun(
                args.iter().map(|x| TVar(x, name)).collect(),
                Box::new(get_type(body, name, &sc)),
            ),
        ));

        // add free variables into scope
        if let Expr::ELambda(_, _, _) = name {
            let mut free: im::HashSet<String> = im::HashSet::new();
            get_free(name, im::HashSet::new(), &mut free);

            for var in free {
                sc.insert(var, name);
            }
        }

        extract_expr_eqns(body, name, set, sc.clone());
    }
}

// returns a list of type equations
fn extract_expr_eqns<'a, 'b>(
    e: &'b Expr<'a>,
    env: &'b Expr<'a>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    scope: im::HashMap<String, &'b Expr<'a>>,
) {
    match e {
        Expr::EId(_) | Expr::ENum(_) | Expr::EBool(_) | Expr::ELambda(_, _, _) => {}
        Expr::EPrim2(op, e1, e2) => extract_prim2(e, op, e1, e2, env, set, scope.clone()),
        Expr::EIf(cond, e1, e2) => {
            extract_expr_eqns(cond, env, set, scope.clone());
            extract_expr_eqns(e1, env, set, scope.clone());
            extract_expr_eqns(e2, env, set, scope.clone());
            set.extend(vec![
                (get_type(cond, env, &scope), TBool),
                (get_type(e1, env, &scope), get_type(e2, env, &scope)),
                (get_type(e, env, &scope), get_type(e1, env, &scope)),
            ]);
        }
        Expr::ELet(bind, body) => {
            let mut sc = scope.clone();
            for Binding(x, exp) in bind {
                extract_expr_eqns(exp, env, set, scope.clone());

                set.insert((TVar(x, e), get_type(exp, env, &scope)));
                sc.insert(x.get_str().unwrap(), e);
            }

            set.insert((get_type(e, env, &scope), get_type(body, e, &sc)));
            extract_expr_eqns(body, e, set, sc);
        }
        Expr::EPrint(expr) => {
            set.insert((get_type(expr, env, &scope), get_type(e, env, &scope)));
            extract_expr_eqns(expr, env, set, scope.clone());
        }
        Expr::EApp(f, args) => {
            set.insert((
                get_type(f, env, &scope),
                TFun(
                    args.iter().map(|x| get_type(x, env, &scope)).collect(),
                    Box::new(get_type(e, env, &scope)),
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
    scope: im::HashMap<String, &'b Expr<'a>>,
) {
    extract_expr_eqns(e1, env, set, scope.clone());
    extract_expr_eqns(e2, env, set, scope.clone());

    match op {
        Prim2::Add | Prim2::Minus | Prim2::Times => {
            set.extend(vec![
                (get_type(e1, env, &scope), TNum),
                (get_type(e2, env, &scope), TNum),
                (get_type(e, env, &scope), TNum),
            ]);
        }
        Prim2::Less | Prim2::Greater => {
            set.extend(vec![
                (get_type(e1, env, &scope), TNum),
                (get_type(e2, env, &scope), TNum),
                (get_type(e, env, &scope), TBool),
            ]);
        }
        Prim2::Equal => {
            set.extend(vec![
                (get_type(e1, env, &scope), get_type(e2, env, &scope)),
                (get_type(e, env, &scope), TBool),
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

    if PRINT {
        for e in subs.iter() {
            eprintln!("{:?}\n  == \x1b[32m{:?}\x1b[0m\n", e.0, e.1);
        }
    }

    let res: HashMap<TypeExpr<'a, 'b>, TypeExpr<'a, 'b>> = subs.into_iter().collect();
    Ok(TypeEnv(res))
}

fn get_type<'a, 'b>(
    e: &'b Expr<'a>,
    env: &'b Expr<'a>,
    scope: &im::HashMap<String, &'b Expr<'a>>,
) -> TypeExpr<'a, 'b> {
    match e {
        Expr::ENum(_) => TNum,
        Expr::EBool(_) => TBool,
        Expr::EId(s) => TVar(e, scope.get(&s.to_string()).expect(&format!("Null: {}", s))),
        Expr::ELambda(s, _, _) => {
            // lambdas are unique
            let lam = scope.get(s).unwrap();
            TVar(lam, lam)
        }
        e => TVar(e, env),
    }
}

// get free variables
pub fn get_free<'a, 'b>(
    expr: &'b Expr<'a>,
    mut scope: im::HashSet<String>,
    res: &mut im::HashSet<String>,
) {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(s) => {
            if !scope.contains(&s.to_string()) {
                res.insert(s.to_string());
            }
        }
        Expr::EPrint(e) => get_free(e, scope, res),
        Expr::EPrim2(_, e1, e2) => {
            get_free(e1, scope.clone(), res);
            get_free(e2, scope.clone(), res);
        }
        Expr::EIf(c, e1, e2) => {
            get_free(c, scope.clone(), res);
            get_free(e1, scope.clone(), res);
            get_free(e2, scope.clone(), res);
        }
        Expr::EApp(f, args) => {
            get_free(f, scope.clone(), res);
            for a in args.iter() {
                get_free(a, scope.clone(), res);
            }
        }
        Expr::ELet(binds, body) => {
            let mut sc = scope.clone();
            for Binding(x, e) in binds.iter() {
                get_free(e, scope.clone(), res);
                sc.insert(x.get_str().unwrap());
            }

            get_free(body, sc, res);
        }
        Expr::ELambda(_, args, body) => {
            for a in args.iter() {
                scope.insert(a.get_str().unwrap());
            }

            get_free(body, scope, res);
        }
    }
}
