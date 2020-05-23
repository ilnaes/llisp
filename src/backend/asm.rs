pub mod compile;

#[derive(Debug, Clone)]
pub enum IReg {
    RAX,
    RSP,
}

#[derive(Debug, Clone)]
pub enum Arg {
    Const(i64),
    Reg(IReg),
    RegOffset(IReg, i64),
}

#[derive(Debug, Clone)]
pub enum Inst {
    IMov(Arg, Arg),
    IAdd(Arg, Arg),
    ISub(Arg, Arg),
    IRet,
}

fn reg_to_asm(r: IReg) -> String {
    match r {
        IReg::RAX => String::from("rax"),
        IReg::RSP => String::from("rsp"),
    }
}

fn arg_to_asm(a: Arg) -> String {
    match a {
        Arg::Const(n) => format!("{}", n),
        Arg::Reg(r) => reg_to_asm(r),
        Arg::RegOffset(r, n) => {
            if n < 0 {
                format!("[{}{}]", reg_to_asm(r), n)
            } else {
                format!("[{}+{}]", reg_to_asm(r), n)
            }
        }
    }
}

fn inst_to_asm(is: Inst) -> String {
    match is {
        Inst::IMov(a1, a2) => format!("  mov {}, {}", arg_to_asm(a1), arg_to_asm(a2)),
        Inst::IAdd(a1, a2) => format!("  add {}, {}", arg_to_asm(a1), arg_to_asm(a2)),
        Inst::ISub(a1, a2) => format!("  sub {}, {}", arg_to_asm(a1), arg_to_asm(a2)),
        Inst::IRet => format!("  ret"),
    }
}

pub fn to_asm(insts: Vec<Inst>) -> String {
    insts.into_iter().fold(String::new(), |acc, is| {
        format!("{}\n{}", acc, inst_to_asm(is))
    })
}
