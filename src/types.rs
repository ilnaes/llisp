use crate::backend::llvm::VType;
use crate::expr::expr::*;
use im;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

mod scc;

const PRINT: bool = false;

#[derive(Debug, Clone, Hash)]
pub enum TypeCons {
    TNum,
    TBool,
    TArrow,
    TTup,
}

use TypeCons::*;

#[derive(Debug, Clone, Hash)]
pub enum TypeExpr<'a, 'b> {
    App(TypeCons, Vec<TypeExpr<'a, 'b>>),

    // type variable identified by a pointer to the node in the ast
    TVar(&'b Expr<'a>),
}

use TypeExpr::*;

impl<'a, 'b> PartialEq for TypeExpr<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (App(TNum, _), App(TNum, _)) | (App(TBool, _), App(TBool, _)) => true,
            (TVar(p1), TVar(p2)) => ptr::eq(*p1, *p2),
            (App(TArrow, a1), App(TArrow, a2)) => {
                if a1.len() != a2.len() {
                    false
                } else {
                    for i in 0..a1.len() {
                        if !a1[i].eq(&a2[i]) {
                            return false;
                        }
                    }
                    true
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
        App(s, args) => match s {
            TNum | TBool | TTup => Ok(VType::I64),
            TArrow => {
                // the following is correct as self is included as an argument
                Ok(VType::Func(
                    args.iter().map(|_| VType::I64).collect(),
                    Box::new(VType::I64),
                ))
            }
        },
        TypeExpr::TVar(_) => Err(format!("Compile error: Unbound type")),
    }
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn get_vtype(
        &self,
        e: &'b Expr<'a>,
        scope: im::HashMap<String, &'b Expr<'a>>,
    ) -> Result<VType, String> {
        let tv = match e {
            Expr::EId(s) => TypeExpr::TVar(
                scope
                    .get(&s.to_string())
                    .expect(&format!("{} unbound\n\nscope: {:?}", s, scope)),
            ),
            Expr::ELambda(s, _, _) => TypeExpr::TVar(
                scope
                    .get(s)
                    .expect(&format!("{} unbound\n\nscope: {:?}", s, scope)),
            ),
            _ => TypeExpr::TVar(e),
        };

        match self.0.get(&tv) {
            Some(x) => type_to_vtype(x),
            _ => Err(format!("Compile error: Unbound type {:?}", e)),
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
    // environment: keeps track of when identifiers are introduced
    // also used to have one canonical reference for a lambda
    let mut scope: im::HashMap<String, &'b Expr<'a>> = im::HashMap::new();

    // decompose into SCC of mutually recursive functions
    let groups = scc::scc(prog);

    for group in groups.iter() {
        // gather top level functions
        for def in group.iter() {
            let Def::FuncDef(f, _, _) = def;

            if let Expr::EId(s) = f {
                // if non-lambda func, then populate into namespace
                // of any other non-lambda function
                scope.insert(s.to_string(), f);
            } else if let Expr::ELambda(s, _, _) = f {
                // put the right reference into scope
                scope.insert(s.clone(), f);
            }
        }

        for f in group.iter() {
            let Def::FuncDef(name, args, body) = f;

            let mut sc = scope.clone();

            // args shadow global funcs
            for a in args.iter() {
                sc.insert(a.get_str().unwrap(), a);
            }

            let mut funtype: Vec<TypeExpr<'a, 'b>> = args.iter().map(|x| TVar(x)).collect();
            funtype.push(get_type(body, &sc));

            set.insert((TVar(name), App(TArrow, funtype)));

            // bring free variables into scope
            if let Expr::ELambda(_, _, _) = name {
                let mut free: im::HashSet<&'b Expr<'a>> = im::HashSet::new();
                get_free(name, im::HashSet::new(), &mut free);

                for var in free {
                    sc.insert(var.get_str().unwrap(), var);
                }
            }

            extract_expr_eqns(body, set, sc.clone());
        }
    }
}

// appends type equations to set
fn extract_expr_eqns<'a, 'b>(
    e: &'b Expr<'a>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    scope: im::HashMap<String, &'b Expr<'a>>, // canonical Expr for an identifier
) {
    match e {
        Expr::EId(_) | Expr::ENum(_) | Expr::EBool(_) | Expr::ELambda(_, _, _) | Expr::ETup(_) => {}
        Expr::EPrim2(op, e1, e2) => extract_prim2(e, op, e1, e2, set, scope.clone()),
        Expr::EIf(cond, e1, e2) => {
            extract_expr_eqns(cond, set, scope.clone());
            extract_expr_eqns(e1, set, scope.clone());
            extract_expr_eqns(e2, set, scope.clone());
            set.extend(vec![
                (get_type(cond, &scope), App(TBool, vec![])),
                (get_type(e1, &scope), get_type(e2, &scope)),
                (get_type(e, &scope), get_type(e1, &scope)),
            ]);
        }
        Expr::ELet(bind, body) => {
            let mut sc = scope.clone();
            for Binding(x, exp) in bind {
                extract_expr_eqns(exp, set, scope.clone());

                set.insert((TVar(x), get_type(exp, &scope)));
                sc.insert(x.get_str().unwrap(), x);
            }

            set.insert((get_type(e, &scope), get_type(body, &sc)));
            extract_expr_eqns(body, set, sc);
        }
        Expr::EPrint(expr) => {
            set.insert((get_type(expr, &scope), get_type(e, &scope)));
            extract_expr_eqns(expr, set, scope.clone());
        }
        Expr::EApp(f, args) => {
            let mut funtype: Vec<TypeExpr<'a, 'b>> =
                args.iter().map(|x| get_type(x, &scope)).collect();
            funtype.push(get_type(e, &scope));
            set.insert((get_type(f, &scope), App(TArrow, funtype)));

            extract_expr_eqns(f, set, scope.clone());
            for a in args {
                extract_expr_eqns(a, set, scope.clone());
            }
        }
    }
}

fn extract_prim2<'a, 'b>(
    e: &'b Expr<'a>,
    op: &'b Prim2,
    e1: &'b Expr<'a>,
    e2: &'b Expr<'a>,
    set: &mut HashSet<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
    scope: im::HashMap<String, &'b Expr<'a>>,
) {
    extract_expr_eqns(e1, set, scope.clone());
    extract_expr_eqns(e2, set, scope.clone());

    match op {
        Prim2::Add | Prim2::Minus | Prim2::Times => {
            set.extend(vec![
                (get_type(e1, &scope), App(TNum, vec![])),
                (get_type(e2, &scope), App(TNum, vec![])),
                (get_type(e, &scope), App(TNum, vec![])),
            ]);
        }
        Prim2::Less | Prim2::Greater => {
            set.extend(vec![
                (get_type(e1, &scope), App(TNum, vec![])),
                (get_type(e2, &scope), App(TNum, vec![])),
                (get_type(e, &scope), App(TBool, vec![])),
            ]);
        }
        Prim2::Equal => {
            set.extend(vec![
                (get_type(e1, &scope), get_type(e2, &scope)),
                (get_type(e, &scope), App(TBool, vec![])),
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
        TVar(_) => {
            if from == &e {
                to
            } else {
                e
            }
        }
        App(con, args) => match con {
            TNum => App(TNum, vec![]),
            TBool => App(TBool, vec![]),
            TTup => App(
                TTup,
                args.into_iter()
                    .map(|x| subst(x, from, to.clone()))
                    .collect(),
            ),
            TArrow => App(
                TArrow,
                args.into_iter()
                    .map(|x| subst(x, from, to.clone()))
                    .collect(),
            ),
        },
    }
}

fn unify<'a, 'b>(
    mut eqns: Vec<(TypeExpr<'a, 'b>, TypeExpr<'a, 'b>)>,
) -> Result<TypeEnv<'a, 'b>, String> {
    let mut subs = Vec::new();

    loop {
        match eqns.pop() {
            None => break,
            Some((TypeExpr::TVar(exp), other)) | Some((other, TypeExpr::TVar(exp))) => {
                let texpr = TypeExpr::TVar(exp);

                // TODO: occurs check

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
            Some((App(c1, mut arg1), App(c2, mut arg2))) => match (c1, c2) {
                (TNum, TNum) | (TBool, TBool) => {}
                (TArrow, TArrow) => {
                    if arg1.len() != arg2.len() {
                        return Err("Type inference conflict".to_string());
                    }
                    for _ in 0..arg1.len() {
                        eqns.push((arg1.pop().unwrap(), arg2.pop().unwrap()));
                    }
                }
                (TTup, TTup) => {
                    if arg1.len() != arg2.len() {
                        return Err("Type inference conflict".to_string());
                    }
                    for _ in 0..arg1.len() {
                        eqns.push((arg1.pop().unwrap(), arg2.pop().unwrap()));
                    }
                }
                _ => return Err("Type inference conflict".to_string()),
            },
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
    scope: &im::HashMap<String, &'b Expr<'a>>,
) -> TypeExpr<'a, 'b> {
    match e {
        Expr::ENum(_) => App(TNum, vec![]),
        Expr::EBool(_) => App(TBool, vec![]),
        Expr::EId(s) => TVar(scope.get(&s.to_string()).expect(&format!("Null: {}", s))),
        Expr::ELambda(s, _, _) => {
            // lambdas are unique
            let lam = scope.get(s).unwrap();
            TVar(lam)
        }
        Expr::ETup(vars) => App(TTup, vars.into_iter().map(|x| get_type(x, scope)).collect()),
        e => TVar(e),
    }
}

// get free variables, added to res
pub fn get_free<'a, 'b>(
    expr: &'b Expr<'a>,
    mut scope: im::HashSet<String>,
    res: &mut im::HashSet<&'b Expr<'a>>,
) {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(s) => {
            if !scope.contains(&s.to_string()) {
                res.insert(expr);
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
        Expr::ETup(vars) => {
            for v in vars.iter() {
                get_free(v, scope.clone(), res);
            }
        }
    }
}
