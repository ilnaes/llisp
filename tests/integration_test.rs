use llisp::compile_to_string;
use std::fs::File;
use std::io::prelude::*;
use std::process::*;

enum TestType<'a> {
    Runs(&'a str), // program should run and produce a desired output
    ErrC(&'a str), // compile should fail with error containing a desired output
}
use TestType::*;

fn wrap(s: &str) -> String {
    format!("(defn our_main () {})", s)
}

fn compile(name: &str, prog: &str, val: TestType) {
    match (compile_to_string(prog), val) {
        (Err(e), ErrC(s)) => {
            assert!(e.contains(s));
        }
        (Ok(s), Runs(_)) => {
            assert!(std::fs::create_dir_all("output/").is_ok());

            let mut file =
                File::create(format!("output/{}.ll", name)).expect("Failed to create file");
            file.write_all(s.as_bytes())
                .expect("Couldn't write to file");

            let boutput = Command::new("make")
                .arg(format!("output/{}.run", name))
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .expect("Failed to build program");

            let ecode = boutput.wait_with_output().expect("failed to wait on child");
            assert!(ecode.status.success());
        }
        _ => assert!(false),
    };
}

fn run(name: &str, prog: &str, val: TestType) {
    match val {
        ErrC(_) => {
            compile(name, prog, val);
        }
        Runs(res) => {
            compile(name, prog, val);

            let prog = Command::new(format!("output/{}.run", name))
                .stdout(Stdio::piped())
                .stderr(Stdio::piped())
                .spawn()
                .expect("Failed to run program");

            let mut output = prog.wait_with_output().expect("failed to wait on child");
            assert!(output.status.success());
            assert_eq!(output.stderr.len(), 0);

            // pop newline
            output.stdout.pop();
            assert_eq!(std::str::from_utf8(&output.stdout).unwrap(), res);
        }
    }
}

macro_rules! run_tests {
    ($($name:ident: ($t:expr, $val:expr),)*) => {
    $(
        #[test]
        fn $name() {
            run(stringify!($name), $t, $val);
        }
    )*
    }
}

run_tests! {
    func_err1: ("(defn f () 2)", ErrC("No our_main")),
    func_err2: ("(defn our_main () 1) (defn our_main () 2)", ErrC("Duplicate def")),
    func_err3: ("(defn f (x x) 1) (defn our_main () 2)", ErrC("Duplicate arg")),
    func_err4: ("(defn f (x) y) (defn our_main () 2)", ErrC("Unbound")),

    func1: ("(defn f (x) x) (defn our_main () 2)", Runs("2")),

    fib: ("(defn f (n) (if (< n 2) 1 (+ (f (- n 1)) (f (- n 2))))) (defn our_main () (f 6))", Runs("13")),
}

macro_rules! run_wrap_tests {
    ($($name:ident: ($t:expr, $val:expr),)*) => {
    $(
        #[test]
        fn $name() {
            run(stringify!($name), &wrap($t), $val);
        }
    )*
    }
}

run_wrap_tests! {
    test1: ("1", Runs("1")),
    test2: ("(+ 2 3)", Runs("5")),
    def_x1: ("(let ((x-y 5)) (- 1 x-y))", Runs("-4")),
    def_x2: ("(let ((x (let ((x 5)) (- x 1)))) (- x 1))", Runs("3")),
    let_nested: ("(let ((x_y (+ 5 (+ 10 20)))) (* x_y x_y))", Runs("1225")),
    two_depth1: ("(let ((x 1)) (+ x (let ((y 2)) (+ x y))))", Runs("4")),
    two_depth2: ("(let ((x 1)) (let ((y 2)) (+ x y)))", Runs("3")),

    fail: ("((1)", ErrC("Lex error")),
    fail2: ("(1))", ErrC("Lex error")),
    fail3: ("(1 2)", ErrC("Type inference conflict")),
    fail4: ("(let ((let 1)) let)", ErrC("Parse error")),
    fail5: ("(let ((a* 1)) a*)", ErrC("Parse error")),
    fail6: ("(let (()) 1)", ErrC("Parse error: binding")),

    cond1: ("(if (> 1 0) 1 0)", Runs("1")),
    cond2: ("(if (< 1 0) 1 0)", Runs("0")),
    cond3: ("(let ((x true)) (if x 0 1))", Runs("0")),
    cond4: ("(if (== 1 0) 1 0)", Runs("0")),
    cond5: ("(if (== false false) 1 0)", Runs("1")),
    nest_cond1: ("(if (let ((x false)) x) 2 1)", Runs("1")),
    nest_cond2: ("(if (if (> 2 1) true false) 2 1)", Runs("2")),

    cond_fail: ("(if (> 1 0) 1)", ErrC("Parse error")),

    type_err1: ("(if true 1 false)", ErrC("Type inference conflict")),
    type_err2: ("(if 1 1 2)", ErrC("Type inference conflict")),
    type_err3: ("(if (== 1 false) 1 0)", ErrC("Type inference conflict")),

    print1: ("(if (print (== 1 1)) 0 1)", Runs("true\n0")),
    print2: ("(if (== 1 1) 0 (print 1))", Runs("0")),

    welldef_error1: ("(if (< 1 0) 0 x)", ErrC("Welldef error: Unbound")),
    welldef_error2: ("(let ((x 1) (y 2) (x 1)) x)", ErrC("Welldef error: Duplicate")),
}
