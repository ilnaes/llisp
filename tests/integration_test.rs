use llisp::compile_to_string;
use std::fs::File;
use std::io::prelude::*;
use std::process::*;

enum TestType<'a> {
    Runs(&'a str), // program should run and produce a desired output
    ErrC(&'a str),
}
use TestType::*;

fn compile(name: &str, prog: &str, val: TestType) {
    assert!(std::fs::create_dir_all("output/").is_ok());
    let mut file = File::create(format!("output/{}.ll", name)).expect("Failed to create file");
    match (compile_to_string(prog), val) {
        (Ok(s), Runs(_)) => file
            .write_all(s.as_bytes())
            .expect("Couldn't write to file"),
        (Err(e), ErrC(s)) => {
            assert!(e.contains(s));
            return;
        }
        _ => assert!(false),
    }

    let boutput = Command::new("make")
        .arg(format!("output/{}.run", name))
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to build program");

    let ecode = boutput.wait_with_output().expect("failed to wait on child");
    assert!(ecode.status.success());
}

fn run(name: &str, prog: &str, val: TestType) {
    std::fs::create_dir_all("output/").unwrap();
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
    ($($name:ident: $t:expr, $val:expr,)*) => {
    $(
        #[test]
        fn $name() {
            run(stringify!($name), $t, $val);
        }
    )*
    }
}

run_tests! {
    test1: "1", Runs("1"),
    test2: "(+ 2 3)", Runs("5"),
    def_x1: "(let ((x 5)) (- 1 x))", Runs("-4"),
    def_x2: "(let ((x (let ((x 5)) (- x 1)))) (- x 1))", Runs("3"),
    let_nested: "(let ((x (+ 5 (+ 10 20)))) (* x x))", Runs("1225"),

    fail: "((1)", ErrC("Lex error"),
    fail2: "(1))", ErrC("Lex error"),
    fail3: "(1 2)", ErrC("Parse error"),
}
