use llisp::compile_to_string;
use std::fs::File;
use std::io::prelude::*;
use std::process::*;

enum TestType<'a> {
    Runs(&'a str), // program should run and produce a desired output
    ErrC,
}
use TestType::*;

struct T<'a>(&'a str, TestType<'a>); // a program as string and an outcome

impl<'a> T<'a> {
    pub fn compile(&self, name: &str) {
        assert!(std::fs::create_dir_all("output/").is_ok());
        let mut file = File::create(format!("output/{}.ll", name)).expect("Failed to create file");
        match compile_to_string(&self.0) {
            Ok(s) => file
                .write_all(s.as_bytes())
                .expect("Couldn't write to file"),
            Err(_) => assert!(false),
        }

        let boutput = Command::new("make")
            .arg(format!("output/{}.run", name))
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .expect("Failed to build program");

        let ecode = boutput.wait_with_output().expect("failed to wait on child");
        match self.1 {
            Runs(_) => {
                assert!(ecode.status.success());
            }
            ErrC => {
                assert!(!ecode.status.success());
            }
        }
    }

    pub fn run(&self, name: &str) {
        std::fs::create_dir_all("output/").unwrap();
        match self.1 {
            ErrC => {
                self.compile(name);
            }
            Runs(res) => {
                self.compile(name);

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
}

macro_rules! run_tests {
    ($($name:ident: $t:expr,)*) => {
    $(
        #[test]
        fn $name() {
            $t.run(stringify!($name));
        }
    )*
    }
}

run_tests! {
    test1: T("1", Runs("1")),
    test2: T("(+ 2 3)", Runs("5")),
    def_x: T("(let ((x 5)) x)", Runs("5")),
    def_x2: T("(let ((x 5)) (- 1 x))", Runs("-4")),
    def_x3: T("(let ((x 5)) (let ((x 67)) (- x 1)))", Runs("66")),
    def_x4: T("(let ((x (let ((x 5)) (- x 1)))) (- x 1))", Runs("3")),
    let_nested: T("(let ((x (+ 5 (+ 10 20)))) (* x x))", Runs("1225")),
}
