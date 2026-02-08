use crate::*;
use ruruby_parse::Parser;
use std::{io::Write, path::PathBuf};
use tempfile::NamedTempFile;

pub fn run_test(code: &str) {
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..25 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, code);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_test_once(code: &str) {
    let wrapped = format!(
        r##"
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, code);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_tests(codes: &[String]) {
    let mut code = "__a = [];".to_string();
    for c in codes {
        code += &format!("__a << ({c});");
    }
    code += "__a";
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..25 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped).as_array();
    let ruby_res = run_ruby(&mut globals, &code).as_array();

    for i in 0..codes.len() {
        let interp_elem = interp_val.get(i).unwrap();
        let ruby_elem = ruby_res.get(i).unwrap();
        eprintln!("{}", codes[i]);
        Value::assert_eq(&globals, *interp_elem, *ruby_elem);
    }
    //Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_binop_tests(lhs: &[&str], op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for lhs in lhs {
        for rhs in rhs {
            for op in op {
                test.extend_from_slice(&[
                    format!("({lhs}) {op} ({rhs})"),
                    format!("({lhs}).{op}({rhs})"),
                    format!("({lhs}) {op} (-({rhs}))"),
                    format!("-({lhs}) {op} ({rhs})"),
                    format!("-({lhs}) {op} -({rhs})"),
                ]);
            }
        }
    }
    run_tests(&test);
}

pub fn run_binop_tests2(lhs: &[&str], op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for lhs in lhs {
        for rhs in rhs {
            for op in op {
                test.extend_from_slice(&[format!("({lhs}) {op} ({rhs})")]);
                test.extend_from_slice(&[format!("({lhs}).{op}({rhs})")]);
            }
        }
    }
    run_tests(&test);
}

pub fn run_unop_tests(op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for rhs in rhs {
        for op in op {
            test.extend_from_slice(&[format!("{op} ({rhs})"), format!("{op} (-{rhs})")]);
        }
    }
    run_tests(&test);
}

pub fn run_test_with_prelude(code: &str, prelude: &str) {
    let wrapped = format!(
        r##"
      {prelude}
      res = ({code})
      for __i in 0..100 do
          res2 = ({code})
          __assert(res, res2)
      end
      ({code})
  "##
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, &wrapped);
    let ruby_res = run_ruby(&mut globals, &format!("{prelude}\n{code}"));

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_test2(code: &str) {
    let mut globals = Globals::new_test();
    let interp_val = run_test_main(&mut globals, code);
    let ruby_res = run_ruby(&mut globals, code);

    Value::assert_eq(&globals, interp_val, ruby_res);
}

pub fn run_test_no_result_check(code: &str) -> Value {
    eprintln!("{code}");
    let mut globals = Globals::new_test();
    run_test_main(&mut globals, code)
}

pub fn run_test_error(code: &str) {
    eprintln!("{code}");
    let mut globals = Globals::new_test();
    match globals.run(code, std::path::Path::new(".")) {
        Ok(v) => {
            eprintln!("{}", v.inspect(&globals.store));
            panic!()
        }
        Err(err) => err.show_error_message_and_all_loc(&globals.store),
    }
}

fn run_test_main(globals: &mut Globals, code: &str) -> Value {
    let res = match globals.run(code, std::path::Path::new(".")) {
        Ok(res) => res,
        Err(err) => {
            err.show_error_message_and_all_loc(&globals.store);
            panic!();
        }
    };

    let jit_str = res.inspect(&globals.store);
    eprintln!("monoruby:  {jit_str}");

    res
}

fn run_ruby(globals: &mut Globals, code: &str) -> Value {
    let code = format!(
        r#"
            ____a = ({});
            puts;
            p(____a)
        "#,
        code
    );
    let mut tmpfile = NamedTempFile::new().unwrap();
    tmpfile.write_all(code.as_bytes()).unwrap();

    let res = match std::process::Command::new("sh")
        .args(["-c", &format!("ruby {}", tmpfile.path().to_str().unwrap())])
        .output()
    {
        Ok(output) => String::from_utf8(output.stdout).unwrap(),
        Err(err) => {
            panic!("failed to invoke ruby. {}", err);
        }
    };

    let res = res.trim_end().split('\n').last().unwrap();
    let nodes = Parser::parse_program(res.to_string(), PathBuf::new())
        .unwrap()
        .node;

    let res = Value::from_ast(&nodes, globals);

    eprintln!("ruby: {}", res.inspect(&globals.store));
    res
}
