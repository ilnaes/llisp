extern "C" {
    fn our_code_starts_here() -> i64;
}

#[no_mangle]
pub extern "C" fn print_out(n: i64) {
    if (n & 0x1) == 0x1 {
        print!("{}", n >> 1);
    } else if n == 0x2 {
        print!("false");
    } else if n == 0x6 {
        print!("true");
    } else {
        print!("BAD: {}", n);
    }
}

pub fn main() {
    let i;
    println!("");
    unsafe {
        i = our_code_starts_here();
    }
    print_out(i);
    println!("");
}
