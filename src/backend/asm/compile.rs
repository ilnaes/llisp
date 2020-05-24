use super::{Arg, Arg::*, IReg::*, Inst, Inst::*};
use crate::expr::{expr::Expr, expr::Expr::*, prim2::Prim2, prim2::Prim2::*};

fn stackloc(si: i64) -> Arg {
    RegOffset(RSP, -(si * 8))
}

fn compile_expr(exp: &Expr, si: i64) -> Vec<Inst> {
    match exp {
        ENum(n) => vec![IMov(Reg(RAX), Const(2 * n + 1))],
        EPrim2(op, e1, e2) => compile_prim2(op, e1, e2, si),
        EId(_s) => panic!("Not implemented"),
    }
}

fn compile_prim2(op: &Prim2, e1: &Expr, e2: &Expr, si: i64) -> Vec<Inst> {
    let mut is1 = compile_expr(e1, si + 1);
    let mut is2 = compile_expr(e2, si);
    let mut res = Vec::new();

    let mut op_is = match op {
        Add => vec![IAdd(Reg(RAX), stackloc(si)), ISub(Reg(RAX), Const(1))],
        Minus => vec![ISub(Reg(RAX), stackloc(si)), IAdd(Reg(RAX), Const(1))],
    };

    res.append(&mut is2);
    res.push(IMov(stackloc(si), Reg(RAX)));
    res.append(&mut is1);
    res.append(&mut op_is);

    return res;
}

pub fn compile(prog: &[Expr]) -> Vec<Inst> {
    let mut it: Vec<Vec<Inst>> = prog.iter().map(|x| compile_expr(x, 2)).collect();
    let mut res = Vec::new();

    for mut inst in it.drain(..) {
        res.append(&mut inst);
    }

    res
}
