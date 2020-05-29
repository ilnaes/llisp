use crate::expr::expr::*;
use im::HashSet;

// checks for unbounded identifiers and double bindings
pub fn check<'a, 'b>(expr: &'b Expr<'a>, scope: HashSet<&'a str>) -> Result<(), String> {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) => {}
        Expr::EId(x) => {
            if !scope.contains(x) {
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
                    return Err(format!("Welldef error: Duplicate binding {}", x));
                }
                names.insert(x);
                // be sure to use old scope
                check(e, scope.clone())?;

                sc.insert(x);
            }

            check(body, sc.clone())?;
        }
        Expr::EIf(cond, e1, e2) => {
            check(cond, scope.clone())?;
            check(e1, scope.clone())?;
            check(e2, scope.clone())?;
        }
    }
    Ok(())
}
