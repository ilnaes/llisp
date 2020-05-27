use llisp::compile_to_string;
use std::fs::File;
use std::io::prelude::*;
use std::process::*;

enum TestType<'a> {
    Runs(&'a str), // program should run and produce a desired output
    ErrC(&'a str), // compile should fail with error containing a desired output
}
use TestType::*;

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
    test1: ("1", Runs("1")),
    test2: ("(+ 2 3)", Runs("5")),
    def_x1: ("(let ((x-y 5)) (- 1 x-y))", Runs("-4")),
    def_x2: ("(let ((x (let ((x 5)) (- x 1)))) (- x 1))", Runs("3")),
    let_nested: ("(let ((x_y (+ 5 (+ 10 20)))) (* x_y x_y))", Runs("1225")),

    fail: ("((1)", ErrC("Lex error")),
    fail2: ("(1))", ErrC("Lex error")),
    fail3: ("(1 2)", ErrC("Parse error")),
    fail4: ("(let ((let 1)) let)", ErrC("Parse error")),
    fail5: ("(let ((a* 1)) a*)", ErrC("Parse error")),

    cond1: ("(if (> 1 0) 1 0)", Runs("1")),
    cond2: ("(if (< 1 0) 1 0)", Runs("0")),
    cond3: ("(let ((x true)) (if x 0 1))", Runs("0")),

    cond_fail: ("(if (> 1 0) 1)", ErrC("Parse error")),
}
