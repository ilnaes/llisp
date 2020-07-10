use crate::backend::llvm::VType;
use crate::expr::expr::*;
use im;
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
            TMeta(n) => format!("t{}", n),
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
            TPoly(vars, t) => format!("∀{:?}.{:?}", vars, t),
        }
    }

    pub fn get_free(&self) -> im::HashSet<Type> {
        match self {
            TMeta(_) => im::hashset![self.clone()],
            TApp(cons, args) => match cons {
                TNum | TBool => im::HashSet::new(),
                TArrow | TTup => args.iter().map(|x| x.get_free()).flatten().collect(),
            },
            TPoly(args, t) => t
                .get_free()
                .relative_complement(args.clone().into_iter().collect()),
        }
    }
}

use Type::*;
use TypeCons::*;

#[derive(Debug, Clone)]
pub struct TypeEnv<'a, 'b> {
    var_env: im::HashMap<ExprPtr<'a, 'b>, usize>,
    metas: im::HashMap<usize, Type>,
    n: usize,
}

impl<'a, 'b> TypeEnv<'a, 'b> {
    fn get_free(&self) -> im::HashSet<Type> {
        self.var_env
            .iter()
            .map(|(_, x)| self.metas.get(x).unwrap().clone())
            .collect()
    }

    pub fn get_vtype(&self, e: &'b Expr<'a>) -> Result<VType, String> {
        match self.var_env.get(&ExprPtr(e)) {
            Some(n) => type_to_vtype(self.metas.get(n).unwrap()),
            None => Err(format!("Unbound type during compile: {:?}\n", e)),
        }
    }

    // retrieves a type assigned to expression e;
    // if not seen before, creates a new metavariable
    fn get(&mut self, e: &'b Expr<'a>) -> Type {
        match e {
            Expr::ENum(_) => TApp(TNum, Vec::new()),
            Expr::EBool(_) => TApp(TBool, Vec::new()),
            Expr::ETup(vars) => TApp(TTup, vars.into_iter().map(|x| self.get(x)).collect()),
            _ => match self.var_env.get(&ExprPtr(e)) {
                Some(ty) => self.metas.get(ty).unwrap().clone(),
                None => {
                    let meta = TMeta(self.n);
                    self.metas.insert(self.n, meta.clone());
                    self.var_env.insert(ExprPtr(e), self.n);
                    self.n += 1;
                    meta
                }
            },
        }
    }

    pub fn new(prog: &'b [Def<'a>]) -> Result<TypeEnv<'a, 'b>, String> {
        let mut res = TypeEnv {
            var_env: im::HashMap::new(),
            metas: im::HashMap::new(),
            n: 0,
        };
        let mut scope: im::HashMap<String, &'b Expr<'a>> = im::HashMap::new();

        // decompose into SCC of mutually recursive functions
        let groups = scc::scc(prog);

        for group in groups.iter() {
            let start = res.n;
            let mut free_vars: im::HashMap<String, Vec<&'b Expr<'a>>> = im::HashMap::new();

            // preprocess functions and register functions
            for def in group.iter() {
                let Def::FuncDef(f, args, body) = def;
                res.get(f);

                // bring functions into scope
                if let Expr::EId(s) = f {
                    scope.insert(s.to_string(), f);
                } else if let Expr::ELambda(s, _, _) = f {
                    scope.insert(s.clone(), f);

                    // collect references to free variables in bodies of lambdas
                    let mut free: Vec<&'b Expr<'a>> = Vec::new();
                    let mut sc: im::HashSet<String> = scope.keys().collect();
                    for a in args.iter() {
                        sc.insert(a.get_str().unwrap());
                    }

                    get_free(body, sc, &mut free);

                    free_vars.insert(s.clone(), free.into_iter().collect());
                }
            }

            let mut tenv = res.clone();

            for def in group.iter() {
                let Def::FuncDef(f, args, body) = def;
                let mut sc = scope.clone();

                // args shadow global funcs
                for a in args.iter() {
                    sc.insert(a.get_str().unwrap(), a);
                }

                // bring free variables into scope
                if let Expr::ELambda(_, _, _) = f {
                    let mut free: Vec<&'b Expr<'a>> = Vec::new();
                    get_free(f, im::HashSet::new(), &mut free);

                    for var in free {
                        sc.insert(var.get_str().unwrap(), var);
                    }
                }

                let res_t = tenv.infer_type(body, sc.clone(), &free_vars, start)?;

                let f_typ = tenv.get(f);

                let mut funtype: Vec<Type> = args.iter().map(|x| tenv.get(x)).collect();
                funtype.push(res_t);

                tenv.unify(f_typ, TApp(TArrow, funtype), start)?;

                // if PRINT {
                //     for (e, i) in tenv.var_env.iter() {
                //         eprintln!(
                //             "{:?}, {:?}\n  == \x1b[32m{:?}\x1b[0m\n",
                //             e.0,
                //             i,
                //             tenv.metas.get(i).unwrap()
                //         );
                //     }
                // }
            }

            for def in group.iter() {
                let Def::FuncDef(f, _, _) = def;
                let f_t = res.get(f);

                if let Expr::EId(_) = f {
                    // generalize functions
                    let gen_t = res.generalize(tenv.get(f));
                    res.unify(f_t, gen_t, start)?;
                } else {
                    res.unify(f_t, tenv.get(f), start)?;
                }
            }

            res.n = tenv.n;
        }

        if PRINT {
            for (e, i) in res.var_env.iter() {
                eprintln!(
                    "{:?}\n  == \x1b[32m{:?}\x1b[0m\n",
                    e.0,
                    res.metas.get(i).unwrap()
                );
            }
        }

        Ok(res)
    }

    fn unify(
        &mut self,
        t1: Type,
        t2: Type,
        start: usize,
    ) -> Result<im::HashMap<Type, Type>, String> {
        // eprintln!("UNIFY: {:?}, {:?}", t1, t2);
        if t1 == t2 {
            return Ok(im::hashmap! {});
        }

        match (t1, t2) {
            (TMeta(mid), other) | (other, TMeta(mid)) => {
                let texpr = TMeta(mid);

                if occurs(&texpr, &other, true) {
                    return Err("Type inference: occurs check".to_string());
                }

                let mut res = im::HashMap::new();
                res.insert(texpr, other.clone());

                for i in start..self.n {
                    if self.metas.contains_key(&i) {
                        self.metas
                            .insert(i, subst(self.metas.get(&i).unwrap().clone(), res.clone()));
                    }
                }
                self.metas.insert(mid, other.clone());

                Ok(res)
            }
            (TApp(c1, arg1), TApp(c2, arg2)) => match (c1, c2) {
                (TNum, TNum) | (TBool, TBool) => Ok(im::HashMap::new()),
                (TArrow, TArrow) | (TTup, TTup) => {
                    if arg1.len() != arg2.len() {
                        return Err("Type inference conflict".to_string());
                    }

                    let mut res = im::HashMap::new();
                    for i in 0..arg1.len() {
                        res = self
                            .unify(
                                subst(arg1[i].clone(), res.clone()),
                                subst(arg2[i].clone(), res.clone()),
                                start,
                            )?
                            .union(res);
                    }

                    Ok(res)
                }
                _ => return Err("Type inference conflict".to_string()),
            },
            _ => return Err("Type inference conflict".to_string()),
        }
    }

    fn infer_type(
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
                    tup.push(self.infer_type(a, scope.clone(), free, start)?);
                }

                TApp(TTup, tup)
            }
            Expr::EId(s) => {
                let typ = self.get(*scope.get(&s.to_string()).unwrap());
                self.instantiate(typ)
            }
            Expr::EPrint(expr) => self.infer_type(expr, scope.clone(), free, start)?,
            Expr::EIf(cond, e1, e2) => {
                let cond_t = self.infer_type(cond, scope.clone(), free, start)?;
                self.unify(cond_t, TApp(TBool, vec![]), start)?;

                let e1_t = self.infer_type(e1, scope.clone(), free, start)?;
                let e2_t = self.infer_type(e2, scope.clone(), free, start)?;
                self.unify(e1_t.clone(), e2_t, start)?;

                e1_t
            }
            Expr::EPrim2(op, e1, e2) => self.infer_prim2(e, op, e1, e2, scope, free, start)?,
            Expr::EApp(f, arg) => {
                let f_t = self.infer_type(f, scope.clone(), free, start)?;

                let mut arg_vec = vec![];
                for a in arg.iter() {
                    let a_t = self.infer_type(a, scope.clone(), free, start)?;
                    arg_vec.push(a_t);
                }

                let typ = self.get(e);
                arg_vec.push(typ.clone());

                self.unify(f_t, TApp(TArrow, arg_vec), start)?;
                self.get(e)
            }
            Expr::ELet(bind, body) => {
                let mut sc = scope.clone();
                for Binding(x, exp) in bind {
                    let x_t = self.get(x);
                    let exp_t = self.infer_type(exp, scope.clone(), free, start)?;

                    // let polymorphism
                    let gen_t = self.generalize(exp_t);
                    self.unify(x_t, gen_t, start)?;
                    sc.insert(x.get_str().unwrap(), x);
                }

                self.infer_type(body, sc, free, start)?
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

    fn infer_prim2(
        &mut self,
        e: &'b Expr<'a>,
        op: &'b Prim2,
        e1: &'b Expr<'a>,
        e2: &'b Expr<'a>,
        scope: im::HashMap<String, &'b Expr<'a>>,
        free: &im::HashMap<String, Vec<&'b Expr<'a>>>,
        start: usize,
    ) -> Result<Type, String> {
        let e1_t = self.infer_type(e1, scope.clone(), free, start)?;
        let e2_t = self.infer_type(e2, scope.clone(), free, start)?;

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
        let vars = t
            .get_free()
            .relative_complement(self.get_free())
            .into_iter()
            .collect();

        TPoly(vars, Box::new(t))
    }

    fn instantiate(&mut self, t: Type) -> Type {
        match t {
            TPoly(args, t) => {
                let mut dict = im::HashMap::new();
                for a in args.iter() {
                    let meta = TMeta(self.n);
                    self.metas.insert(self.n, meta.clone());
                    self.n += 1;

                    dict.insert(a.clone(), meta);
                }

                subst(*t, dict)
            }
            _ => t,
        }
    }
}

fn subst(e: Type, dict: im::HashMap<Type, Type>) -> Type {
    match e {
        TMeta(_) => match dict.get(&e) {
            Some(res) => res.clone(),
            None => e,
        },
        TApp(con, args) => match con {
            TNum => TApp(TNum, vec![]),
            TBool => TApp(TBool, vec![]),
            TTup => TApp(
                TTup,
                args.into_iter().map(|x| subst(x, dict.clone())).collect(),
            ),
            TArrow => TApp(
                TArrow,
                args.into_iter().map(|x| subst(x, dict.clone())).collect(),
            ),
        },
        TPoly(vars, t) => {
            let mut ndict = dict.clone();
            for v in vars.iter() {
                ndict.remove(v);
            }
            TPoly(vars, Box::new(subst(*t, ndict)))
        }
    }
}

// ty1 will be a metavariable
fn occurs(ty1: &Type, ty2: &Type, top: bool) -> bool {
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
    }
}

// get free variables, added to res
pub fn get_free<'a, 'b>(
    expr: &'b Expr<'a>,
    mut scope: im::HashSet<String>,
    res: &mut Vec<&'b Expr<'a>>,
) {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(s) => {
            if !scope.contains(&s.to_string()) {
                for v in res.iter() {
                    if *v == expr {
                        return;
                    }
                }
                res.push(expr);
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
        TMeta(_) => Err(format!("Compile error: Unbound type")),
    }
}
