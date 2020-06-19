use crate::expr::expr::*;

pub fn lift<'a>(mut prog: Vec<Def<'a>>) -> Vec<Def<'a>> {
    let mut lams = Vec::new();

    for d in prog.iter() {
        let Def::FuncDef(_, _, body) = d;
        lift_lambdas(&body, &mut lams);
    }

    lams.append(&mut prog);
    lams
}

fn lift_lambdas<'a, 'b>(expr: &'b Expr<'a>, lams: &mut Vec<Def<'a>>) {
    match expr {
        Expr::ENum(_) | Expr::EBool(_) | Expr::EId(_) => {}
        Expr::ETup(vars) => {
            for v in vars.iter() {
                lift_lambdas(v, lams);
            }
        }
        Expr::EPrint(e) => lift_lambdas(e, lams),
        Expr::EPrim2(_, e1, e2) => {
            lift_lambdas(e1, lams);
            lift_lambdas(e2, lams);
        }
        Expr::EIf(c, e1, e2) => {
            lift_lambdas(c, lams);
            lift_lambdas(e1, lams);
            lift_lambdas(e2, lams);
        }
        Expr::ELet(binds, body) => {
            for Binding(_, e) in binds.iter() {
                lift_lambdas(e, lams);
            }
            lift_lambdas(body, lams);
        }
        Expr::EApp(f, args) => {
            lift_lambdas(f, lams);
            for a in args.iter() {
                lift_lambdas(a, lams);
            }
        }
        Expr::ELambda(_, args, body) => {
            lams.push(Def::FuncDef(expr.clone(), args.clone(), *body.clone()));
            lift_lambdas(body, lams);
        }
    }
}
