pub mod compile;
pub mod scope;

// #[derive(Debug, Clone)]
// pub enum VType {
//     I1,
//     I64,
// }

#[derive(Debug, Clone)]
pub enum Var {
    Local(String),
    // Global(String),
}

#[derive(Debug, Clone)]
pub enum Arg {
    AVar(Var),
    Const(i64),
}

impl Arg {
    pub fn to_string(&self) -> String {
        match self {
            Arg::Const(n) => format!("{}", n),
            Arg::AVar(Var::Local(s)) => s.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Inst {
    IAdd64(Arg, Arg, Arg),
    IAdd1(Arg, Arg, Arg),
    ISub(Arg, Arg, Arg),
    IAshr(Arg, Arg, Arg),
    IMul(Arg, Arg, Arg),
    IGt(Arg, Arg, Arg),
    ILt(Arg, Arg, Arg),
    IEq(Arg, Arg, Arg),
    IRet(Arg),
    IAlloc(Arg),
    IStore(Arg, Arg),
    ILoad(Arg, Arg),
    ILabel(String),
    IBrk(Arg, Arg, Arg), // conditional break
    IJmp(Arg),           // unconditional break
                         // ICall(Arg),
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub name: String,
    pub args: Vec<String>,
    pub inst: Vec<Inst>,
}

// pub fn typ_to_ll(t: &VType) -> String {
//     match t {
//         VType::I1 => String::from("i1"),
//         VType::I64 => String::from("i64"),
//     }
// }

pub fn var_to_ll(v: &Var) -> String {
    match v {
        Var::Local(s) => format!("%{}", s.replace("-", "_")),
        // Var::Global(s) => format!("@{}", s),
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
        Inst::IAdd64(dst, arg1, arg2) => format!(
            "  {} = add i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IAdd1(dst, arg1, arg2) => format!(
            "  {} = add i1 {}, {}",
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
        Inst::IMul(dst, arg1, arg2) => format!(
            "  {} = mul i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IAshr(dst, arg1, arg2) => format!(
            "  {} = ashr i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IEq(dst, arg1, arg2) => format!(
            "  {} = icmp eq i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IGt(dst, arg1, arg2) => format!(
            "  {} = icmp sgt i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::ILt(dst, arg1, arg2) => format!(
            "  {} = icmp slt i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IAlloc(dst) => format!("  {} = alloca i64, align 8", arg_to_ll(dst)),
        Inst::IStore(dst, src) => format!(
            "  store i64 {}, i64* {}, align 8",
            arg_to_ll(src),
            arg_to_ll(dst)
        ),
        Inst::ILoad(dst, src) => format!(
            "  {} = load i64, i64* {}, align 8",
            arg_to_ll(dst),
            arg_to_ll(src)
        ),
        Inst::ILabel(l) => format!("{}:", l),
        Inst::IBrk(cond, thn, els) => format!(
            "  br i1 {}, label {}, label {}",
            arg_to_ll(cond),
            arg_to_ll(thn),
            arg_to_ll(els)
        ),
        Inst::IJmp(dst) => format!("  br label {}", arg_to_ll(dst)),
        // Inst::ICall(arg) => format!("  call void {}()", arg_to_ll(arg)),
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
