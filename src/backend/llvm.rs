pub mod compile;
pub mod scope;

#[derive(Debug, Clone)]
pub enum VType {
    I1,
    I64,
    Void,

    Ptr(Box<VType>),
    Func(Vec<VType>, Box<VType>),
}

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

impl Arg {
    pub fn to_string(&self) -> String {
        match self {
            Arg::Const(n) => format!("{}", n),
            Arg::AVar(Var::Global(s)) | Arg::AVar(Var::Local(s)) => s.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Inst {
    IAdd64(Arg, Arg, Arg),
    ISub(Arg, Arg, Arg),
    IAshr(Arg, Arg, Arg),
    IShl(Arg, Arg, Arg),
    IMul(Arg, Arg, Arg),
    IGt(Arg, Arg, Arg),
    ILt(Arg, Arg, Arg),
    IEq(VType, Arg, Arg, Arg),
    IRet(Arg),
    IAlloc(VType, Arg),
    IStore(VType, Arg, Arg),
    ILoad(VType, Arg, Arg),
    ILabel(String),
    IBrk(Arg, Arg, Arg), // conditional break
    IJmp(Arg),           // unconditional break
    ICall(VType, Option<Arg>, Arg, Vec<Arg>),
    IZExt(VType, VType, Arg, Arg),
    IGEP(VType, Arg, Arg, Arg),
    IPtrtoint(VType, VType, Arg, Arg),
    IInttoptr(VType, VType, Arg, Arg),
}

#[derive(Debug, Clone)]
pub struct FunDef {
    pub name: String,
    pub args: Vec<String>,
    pub inst: Vec<Inst>,
}

pub fn nary_func(n: usize) -> VType {
    let mut args = Vec::new();
    for _ in 0..n {
        args.push(VType::I64);
    }
    VType::Func(args, Box::new(VType::I64))
}

pub fn typ_to_ll(t: &VType) -> String {
    match t {
        VType::I1 => String::from("i1"),
        VType::I64 => String::from("i64"),
        VType::Void => String::from("void"),
        VType::Ptr(typ) => format!("{}*", typ_to_ll(typ)),
        VType::Func(args, ret) => {
            let mut res = typ_to_ll(ret);
            res.push('(');

            let mut inside = String::new();
            for _ in args {
                // inside.push_str(&format!("{}, ", typ_to_ll(a)));
                // all data types are i64
                inside.push_str("i64, ");
            }

            if inside.len() > 0 {
                inside.pop();
                inside.pop();
                res.push_str(&inside);
            }
            res.push_str(")*");
            res
        }
    }
}

pub fn var_to_ll(v: &Var) -> String {
    match v {
        Var::Local(s) => format!("%{}", s.replace("-", "_")),
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
        Inst::IGEP(t, dst, src, len) => {
            let t = typ_to_ll(t);
            format!(
                "  {} = getelementptr inbounds {}, {}* {}, i64 {}",
                arg_to_ll(dst),
                t,
                t,
                arg_to_ll(src),
                arg_to_ll(len),
            )
        }
        Inst::IInttoptr(dtyp, styp, dst, src) => format!(
            "  {} = inttoptr {} {} to {}",
            arg_to_ll(dst),
            typ_to_ll(styp),
            arg_to_ll(src),
            typ_to_ll(dtyp),
        ),
        Inst::IPtrtoint(dtyp, styp, dst, src) => format!(
            "  {} = ptrtoint {} {} to {}",
            arg_to_ll(dst),
            typ_to_ll(styp),
            arg_to_ll(src),
            typ_to_ll(dtyp),
        ),
        Inst::IZExt(dtyp, styp, dst, src) => format!(
            "  {} = zext {} {} to {}",
            arg_to_ll(dst),
            typ_to_ll(styp),
            arg_to_ll(src),
            typ_to_ll(dtyp),
        ),
        Inst::IAdd64(dst, arg1, arg2) => format!(
            "  {} = add i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        // Inst::IAdd1(dst, arg1, arg2) => format!(
        //     "  {} = add i1 {}, {}",
        //     arg_to_ll(dst),
        //     arg_to_ll(arg1),
        //     arg_to_ll(arg2)
        // ),
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
        Inst::IShl(dst, arg1, arg2) => format!(
            "  {} = shl i64 {}, {}",
            arg_to_ll(dst),
            arg_to_ll(arg1),
            arg_to_ll(arg2)
        ),
        Inst::IEq(typ, dst, arg1, arg2) => format!(
            "  {} = icmp eq {} {}, {}",
            arg_to_ll(dst),
            typ_to_ll(typ),
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
        Inst::IAlloc(typ, dst) => {
            format!("  {} = alloca {}, align 8", arg_to_ll(dst), typ_to_ll(typ))
        }
        Inst::IStore(typ, dst, src) => format!(
            "  store {} {}, {}* {}, align 8",
            typ_to_ll(typ),
            arg_to_ll(src),
            typ_to_ll(typ),
            arg_to_ll(dst)
        ),
        Inst::ILoad(typ, dst, src) => format!(
            "  {} = load {}, {}* {}, align 8",
            arg_to_ll(dst),
            typ_to_ll(typ),
            typ_to_ll(typ),
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
        Inst::ICall(typ, ret, func, args) => {
            let mut inj = String::new();
            for (i, a) in args.into_iter().enumerate() {
                inj.push_str("i64 ");
                inj.push_str(&arg_to_ll(a));
                if i < args.len() - 1 {
                    inj.push_str(&", ");
                }
            }

            if let Some(r) = ret {
                format!(
                    "  {} = call {} {}({})",
                    arg_to_ll(r),
                    typ_to_ll(typ),
                    arg_to_ll(func),
                    inj
                )
            } else {
                format!("  call {} {}({})", typ_to_ll(typ), arg_to_ll(func), inj)
            }
        }
        Inst::IRet(arg) => format!("  ret i64 {}", arg_to_ll(arg)),
    }
}

pub fn fundef_to_ll(fun: FunDef) -> String {
    let args = fun.args.iter().fold(String::new(), |acc, x| {
        if acc.len() > 0 {
            format!("{}, i64 %{}", acc, x)
        } else {
            format!("i64 %{}", x)
        }
    });
    let insts = fun.inst.iter().fold(String::new(), |acc, x| {
        format!("{}{}\n", acc, inst_to_ll(x))
    });

    format!("define i64 @{}({}) {{\n{}}}\n", fun.name, args, insts)
}
