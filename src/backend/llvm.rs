pub mod compile;
pub mod scope;

// #[derive(Debug, Clone)]
// pub enum Type {
//     I32,
//     I64,
// }

#[derive(Debug, Clone)]
pub enum Var {
    Local(String),
    Global(String),
}

#[derive(Debug, Clone)]
pub enum Arg {
    AVar(Var),
    Const(i64),
}

#[derive(Debug, Clone)]
pub enum Inst {
    IAdd(Arg, Arg, Arg),
    ISub(Arg, Arg, Arg),
    IRet(Arg),
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub name: String,
    pub args: Vec<String>,
    pub inst: Vec<Inst>,
}

pub fn var_to_ll(v: &Var) -> String {
    match v {
        Var::Local(s) => format!("%{}", s),
        Var::Global(s) => format!("@{}", s),
    }
}

pub fn arg_to_ll(a: &Arg) -> String {
    match a {
        Arg::AVar(v) => var_to_ll(v),
        Arg::Const(n) => format!("{}", n),
    }
}

pub fn inst_to_ll(is: &Inst) -> String {
    match is {
        Inst::IAdd(dst, arg1, arg2) => format!(
            "  {} = add i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::ISub(dst, arg1, arg2) => format!(
            "  {} = sub i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IRet(arg) => format!("  ret i64 {}", arg_to_ll(arg)),
    }
}

pub fn fundef_to_ll(fun: FunDef) -> String {
    let args = fun.args.iter().fold(String::new(), |acc, x| {
        if acc.len() > 0 {
            format!("{}, i64 %{}", acc, x)
        } else {
            String::from(x)
        }
    });
    let insts = fun.inst.iter().fold(String::new(), |acc, x| {
        format!("{}{}\n", acc, inst_to_ll(x))
    });

    format!("define i64 @{}({}) {{\n{}}}", fun.name, args, insts)
}
