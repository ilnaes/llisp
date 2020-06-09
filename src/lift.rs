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
        Expr::EIf(c, e1, e2) => {
            lift_lambdas(c, lams);
            lift_lambdas(e1, lams);
            lift_lambdas(e2, lams);
        }
        _ => {}
    }
}
