use crate::backend::llvm::VType;
use crate::expr::expr::*;
use im;
use std::collections::{HashMap, HashSet};
use std::iter::Extend;
use std::ptr;

mod scc;

const PRINT: bool = false;

// a pointer equality version of &'b Expr<'a>
#[derive(Debug, Clone, Hash)]
struct ExprPtr<'a, 'b>(&'b Expr<'a>);

impl<'a, 'b> PartialEq for ExprPtr<'a, 'b> {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.0, other.0)
    }
}
impl<'a, 'b> Eq for ExprPtr<'a, 'b> {}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum TypeCons {
    TNum,
    TBool,
    TArrow,
    TTup,
}

#[derive(Clone, Hash, PartialEq, Eq)]
pub enum Type {
    TApp(TypeCons, Vec<Type>),
    TMeta(usize),
    TVar(usize),
    TPoly(Vec<Type>, Box<Type>),
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

impl Type {
    fn to_string(&self) -> String {
        match self {
            TMeta(n) => format!("M({})", n),
            TApp(cons, args) => match cons {
                TNum => "Num".to_string(),
                TBool => "Bool".to_string(),
                TArrow => {
                    let mut args = args.clone();
                    let ret = args.pop().unwrap();
                    format!("{{{:?} -> {:?}}}", args, ret)
                }
                TTup => format!("{:?}", args),
            },
            _ => panic!(),
        }
    }

    pub fn get_free_t(&self) -> Vec<&Type> {
        match self {
            TMeta(_) => vec![self],
            TApp(cons, args) => match cons {
                TNum | TBool => Vec::new(),
                TArrow | TTup => args.iter().map(|x| x.get_free_t()).flatten().collect(),
            },
            _ => panic!("Getting free of polytype"),
        }
    }
}

use Type::*;
use TypeCons::*;

#[derive(Clone)]
pub struct TypeEnv<'a, 'b> {
    var_env: HashMap<ExprPtr<'a, 'b>, usize>,
    metas: Vec<Type>,
    n: usize,
    m: usize,
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    pub fn get_vtype(&self, e: &'b Expr<'a>) -> Result<VType, String> {
        match self.var_env.get(&ExprPtr(e)) {
            Some(n) => type_to_vtype(&self.metas[*n]),
            None => Err(format!("Unbound type during compile: {:?}\n", e)),
        }
    }

    // retrieves a type assigned to expression e;
    // if not seen before, creates a new metavariable
    fn get(&mut self, e: &'b Expr<'a>) -> Type {
        match e {
            Expr::ENum(_) => TApp(TNum, vec![]),
            Expr::EBool(_) => TApp(TBool, vec![]),
            Expr::ETup(vars) => TApp(TTup, vars.into_iter().map(|x| self.get(x)).collect()),
            _ => match self.var_env.get(&ExprPtr(e)) {
                Some(ty) => self.metas[*ty].clone(),
                None => {
                    let meta = TMeta(self.n);
                    self.metas.push(meta.clone());
                    self.var_env.insert(ExprPtr(e), self.n);
                    self.n += 1;
                    meta
                }
            },
        }
    }

    pub fn new(prog: &'b [Def<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut tenv = TypeEnv {
            var_env: HashMap::new(),
            metas: Vec::new(),
            n: 0,
            m: 0,
        };
        let mut scope: im::HashMap<String, &'b Expr<'a>> = im::HashMap::new();

        // decompose into SCC of mutually recursive functions
        let groups = scc::scc(prog);

        if PRINT {
            for g in groups.iter() {
                eprintln!("{:?}\n\n", g);
            }
        }

        for group in groups.iter() {
            let start = tenv.n;
            let mut free_vars: im::HashMap<String, Vec<&'b Expr<'a>>> = im::HashMap::new();

            // preprocess functions
            for def in group.iter() {
                let Def::FuncDef(f, args, body) = def;

                // bring functions into scope
                if let Expr::EId(s) = f {
                    scope.insert(s.to_string(), f);
                } else if let Expr::ELambda(s, _, _) = f {
                    scope.insert(s.clone(), f);

                    // collect references to free variables in bodies of lambdas
                    let mut free: im::HashSet<&'b Expr<'a>> = im::HashSet::new();
                    let mut sc: im::HashSet<String> = scope.keys().collect();
                    for a in args.iter() {
                        sc.insert(a.get_str().unwrap());
                    }

                    get_free(body, sc, &mut free);

                    free_vars.insert(s.clone(), free.into_iter().collect());
                }
            }

            // let mut eqns: HashSet<(Type, Type)> = HashSet::new();

            for def in group.iter() {
                let Def::FuncDef(f, args, body) = def;
                let mut sc = scope.clone();

                // args shadow global funcs
                for a in args.iter() {
                    sc.insert(a.get_str().unwrap(), a);
                }

                let f_typ = tenv.get(f);
                let mut funtype: Vec<Type> = args.iter().map(|x| tenv.get(x)).collect();
                funtype.push(tenv.get(body));

                // instantiate as monomorphic function
                tenv.unify(f_typ, TApp(TArrow, funtype), start)?;

                // bring free variables into scope
                if let Expr::ELambda(_, _, _) = f {
                    let mut free: im::HashSet<&'b Expr<'a>> = im::HashSet::new();
                    get_free(f, im::HashSet::new(), &mut free);

                    for var in free {
                        sc.insert(var.get_str().unwrap(), var);
                    }
                }

                tenv.extract_type(body, sc.clone(), &free_vars, start)?;
            }

            // let eqns: Vec<(Type, Type)> = eqns.into_iter().collect();
            // tenv.unify_eqns(eqns)?;

            for Def::FuncDef(f, _, _) in group.iter() {
                // generalize
                let ty = tenv.get(f);
                let ty = tenv.generalize(ty);
                let n = tenv.var_env.get(&ExprPtr(f)).unwrap();
                tenv.metas[*n] = ty;
            }
        }

        if PRINT {
            for (e, i) in tenv.var_env.iter() {
                eprintln!("{:?}\n  == \x1b[32m{:?}\x1b[0m\n", e.0, tenv.metas[*i]);
            }
        }

        Ok(tenv)
    }

    fn unify(&mut self, t1: Type, t2: Type, start: usize) -> Result<(), String> {
        if t1 == t2 {
            return Ok(());
        }

        match (t1, t2) {
            (TMeta(mid), other) | (other, TMeta(mid)) => {
                let texpr = TMeta(mid);

                if occurs(&texpr, &other, true) {
                    return Err("Type inference: occurs check".to_string());
                }

                for i in start..self.metas.len() {
                    self.metas[i] = subst(self.metas[i].clone(), &texpr, other.clone())
                }
                self.metas[mid] = other
            }
            (TApp(c1, arg1), TApp(c2, arg2)) => match (c1, c2) {
                (TNum, TNum) | (TBool, TBool) => {}
                (TArrow, TArrow) => {
                    if arg1.len() != arg2.len() {
                        return Err("Type inference conflict".to_string());
                    }
                    for i in 0..arg1.len() {
                        self.unify(arg1[i].clone(), arg2[i].clone(), start)?
                    }
                }
                (TTup, TTup) => {
                    if arg1.len() != arg2.len() {
                        return Err("Type inference conflict".to_string());
                    }
                    for i in 0..arg1.len() {
                        self.unify(arg1[i].clone(), arg2[i].clone(), start)?
                    }
                }
                _ => return Err("Type inference conflict".to_string()),
            },
            _ => return Err("Type inference conflict".to_string()),
        }

        Ok(())
    }

    fn extract_type(
        &mut self,
        e: &'b Expr<'a>,
        scope: im::HashMap<String, &'b Expr<'a>>, // pointer to where ident was bound
        free: &im::HashMap<String, Vec<&'b Expr<'a>>>, // free vars of lambdas to be unified
        start: usize,
    ) -> Result<Type, String> {
        let res = match e {
            Expr::ENum(_) => TApp(TNum, vec![]),
            Expr::EBool(_) => TApp(TBool, vec![]),
            Expr::ETup(arg) => {
                let mut tup = vec![];
                for a in arg.iter() {
                    tup.push(self.extract_type(a, scope.clone(), free, start)?);
                }

                TApp(TTup, tup)
            }
            Expr::EId(s) => {
                let typ = self.get(*scope.get(&s.to_string()).unwrap());
                self.instantiate(typ)
            }
            Expr::EPrint(expr) => self.extract_type(expr, scope.clone(), free, start)?,
            Expr::EIf(cond, e1, e2) => {
                let cond_t = self.extract_type(cond, scope.clone(), free, start)?;
                self.unify(cond_t, TApp(TBool, vec![]), start)?;

                let e1_t = self.extract_type(e1, scope.clone(), free, start)?;
                let e2_t = self.extract_type(e2, scope.clone(), free, start)?;
                self.unify(e1_t.clone(), e2_t, start)?;

                e1_t
            }
            Expr::EPrim2(op, e1, e2) => self.extract_prim2(e, op, e1, e2, scope, free, start)?,
            Expr::EApp(f, arg) => {
                let mut arg_vec = vec![];
                for a in arg.iter() {
                    let a_t = self.extract_type(a, scope.clone(), free, start)?;
                    arg_vec.push(a_t);
                }

                let typ = self.get(e);
                arg_vec.push(typ.clone());

                let f_t = self.extract_type(f, scope, free, start)?;
                self.unify(f_t, TApp(TArrow, arg_vec), start)?;
                self.get(e)
            }
            Expr::ELet(bind, body) => {
                let mut sc = scope.clone();
                for Binding(x, exp) in bind {
                    let x_t = self.get(x);
                    let exp_t = self.extract_type(exp, scope.clone(), free, start)?;
                    let gen_t = self.generalize(exp_t);

                    // let polymorphism
                    self.unify(x_t, gen_t, start)?;
                    sc.insert(x.get_str().unwrap(), x);
                }

                self.extract_type(body, sc, free, start)?
            }
            Expr::ELambda(s, _, _) => {
                match free.get(s) {
                    Some(vars) => {
                        for v in vars.iter() {
                            let free_t = self.get(*v);
                            let local_t = self.get(scope.get(&v.get_str().unwrap()).unwrap());
                            self.unify(free_t, local_t, start)?;
                        }
                    }
                    None => {}
                }
                self.get(scope.get(s).unwrap())
            }
        };

        let self_t = self.get(e);
        self.unify(self_t, res.clone(), start)?;

        Ok(res)
    }

    fn extract_prim2(
        &mut self,
        e: &'b Expr<'a>,
        op: &'b Prim2,
        e1: &'b Expr<'a>,
        e2: &'b Expr<'a>,
        scope: im::HashMap<String, &'b Expr<'a>>,
        free: &im::HashMap<String, Vec<&'b Expr<'a>>>,
        start: usize,
    ) -> Result<Type, String> {
        let e1_t = self.extract_type(e1, scope.clone(), free, start)?;
        let e2_t = self.extract_type(e2, scope.clone(), free, start)?;

        match op {
            Prim2::Add | Prim2::Minus | Prim2::Times => {
                self.unify(e1_t, TApp(TNum, vec![]), start)?;
                self.unify(e2_t, TApp(TNum, vec![]), start)?;

                let self_t = self.get(e);
                self.unify(self_t, TApp(TNum, vec![]), start)?;
            }
            Prim2::Less | Prim2::Greater => {
                self.unify(e1_t, TApp(TNum, vec![]), start)?;
                self.unify(e2_t, TApp(TNum, vec![]), start)?;

                let self_t = self.get(e);
                self.unify(self_t, TApp(TBool, vec![]), start)?;
            }
            Prim2::Equal => {
                self.unify(e1_t, e2_t, start)?;

                let self_t = self.get(e);
                self.unify(self_t, TApp(TBool, vec![]), start)?;
            }
        }

        Ok(self.get(e))
    }

    fn generalize(&mut self, t: Type) -> Type {
        t
    }

    fn instantiate(&mut self, t: Type) -> Type {
        match t {
            TPoly(_, _) => panic!(),
            _ => t,
        }
    }
}

fn subst<'a, 'b>(e: Type, from: &Type, to: Type) -> Type {
    match e {
        TMeta(_) => {
            if from == &e {
                to
            } else {
                e
            }
        }
        TApp(con, args) => match con {
            TNum => TApp(TNum, vec![]),
            TBool => TApp(TBool, vec![]),
            TTup => TApp(
                TTup,
                args.into_iter()
                    .map(|x| subst(x, from, to.clone()))
                    .collect(),
            ),
            TArrow => TApp(
                TArrow,
                args.into_iter()
                    .map(|x| subst(x, from, to.clone()))
                    .collect(),
            ),
        },
        TPoly(_, t) => subst(*t, from, to),
        TVar(_) => e,
    }
}

// ty1 will be a metavariable
fn occurs<'a, 'b>(ty1: &Type, ty2: &Type, top: bool) -> bool {
    match ty2 {
        TMeta(_) => {
            if top {
                false
            } else {
                ty1 == ty2
            }
        }
        TApp(_, args) => {
            for a in args.iter() {
                if occurs(ty1, a, false) {
                    return true;
                }
            }
            false
        }
        TPoly(_, ty) => occurs(ty1, ty, false),
        TVar(_) => false,
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

fn type_to_vtype(typ: &Type) -> Result<VType, String> {
    match typ {
        TApp(s, args) => match s {
            TNum | TBool | TTup => Ok(VType::I64),
            TArrow => {
                // the following is correct as self is included as an argument
                Ok(VType::Func(
                    args.iter().map(|_| VType::I64).collect(),
                    Box::new(VType::I64),
                ))
            }
        },
        TPoly(_, t) => type_to_vtype(t),
        TMeta(_) | TVar(_) => Err(format!("Compile error: Unbound type")),
    }
}
