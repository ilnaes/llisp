use crate::expr::expr::*;
use im::HashSet;

pub fn check_prog<'a, 'b>(prog: &'b [Def<'a>]) -> Result<(), String> {
    let mut scope = HashSet::new();
    for def in prog {
        match def {
            Def::FuncDef(Expr::EId(s), arg, _) => {
                if let Some(_) = scope.insert(s.to_string()) {
                    return Err(format!("Welldef error: Duplicate defs {:?}", s));
                }

                if s == &"our_main" && arg.len() > 0 {
                    return Err("Welldef error: our_main cannot have arguments".to_string());
                }
            }
            _ => {}
        }
    }

    if !scope.contains("our_main") {
        return Err("Welldef error: No our_main".to_string());
    }

    for def in prog {
        let Def::FuncDef(_, args, body) = def;
        let mut sc = scope.clone();
        for a in args {
            if let Some(_) = sc.insert(a.get_str().unwrap()) {
                return Err(format!("Welldef error: Duplicate arg {:?}", a));
            }
        }

        check(body, sc)?;
    }
    Ok(())
}

// checks for unbounded identifiers and double bindings
pub fn check<'a, 'b>(expr: &'b Expr<'a>, scope: HashSet<String>) -> Result<(), String> {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(x) => {
            if !scope.contains(&x.to_string()) {
                return Err(format!("Welldef error: Unbound identifier {}", x));
            }
        }
        Expr::EPrint(e) => {
            check(e, scope.clone())?;
        }
        Expr::EPrim2(_, e1, e2) => {
            check(e1, scope.clone())?;
            check(e2, scope.clone())?;
        }
        Expr::ELet(bind, body) => {
            let mut names = HashSet::new();
            let mut sc = scope.clone();
            for Binding(x, e) in bind {
                if names.contains(x) {
                    return Err(format!(
                        "Welldef error: Duplicate binding {}",
                        x.get_str().unwrap()
                    ));
                }
                names.insert(x);
                // be sure to use old scope
                check(e, scope.clone())?;

                sc.insert(x.get_str().unwrap());
            }

            check(body, sc.clone())?;
        }
        Expr::EIf(cond, e1, e2) => {
            check(cond, scope.clone())?;
            check(e1, scope.clone())?;
            check(e2, scope.clone())?;
        }
        Expr::EApp(f, args) => {
            check(f, scope.clone())?;
            for a in args {
                check(a, scope.clone())?;
            }
        }
        Expr::ELambda(_, args, body) => {
            let mut sc = scope.clone();
            for x in args.iter() {
                sc.insert(x.get_str().unwrap());
            }

            check(body, sc)?;
        }
    }
    Ok(())
}
