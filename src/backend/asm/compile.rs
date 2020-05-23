use super::{Arg, Arg::*, IReg::*, Inst, Inst::*};
use crate::expr::{expr::Expr, expr::Expr::*, prim2::Prim2, prim2::Prim2::*};

fn stackloc(si: i64) -> Arg {
    RegOffset(RSP, -(si * 8))
}

fn compile_expr(exp: Expr, si: i64) -> Vec<Inst> {
    match exp {
        ENum(n) => vec![IMov(Reg(RAX), Const(2 * n + 1))],
        EPrim2(op, e1, e2) => compile_prim2(op, *e1, *e2, si),
    }
}

fn compile_prim2(op: Prim2, e1: Expr, e2: Expr, si: i64) -> Vec<Inst> {
    let mut is1 = compile_expr(e1, si + 1);
    let mut is2 = compile_expr(e2, si);
    let mut res = Vec::new();

    let op_is = match op {
        Add => IAdd(Reg(RAX), stackloc(si)),
        Minus => ISub(Reg(RAX), stackloc(si)),
    };

    res.append(&mut is2);
    res.push(IMov(stackloc(si), Reg(RAX)));
    res.append(&mut is1);
    res.push(op_is);

    return res;
}

pub fn compile(prog: Vec<Expr>) -> Vec<Inst> {
    let mut it: Vec<Vec<Inst>> = prog.into_iter().map(|x| compile_expr(x, 0)).collect();
    let mut res = Vec::new();

    for mut inst in it.drain(..) {
        res.append(&mut inst);
    }

    res
}
