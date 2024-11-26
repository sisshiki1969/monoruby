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
    let mut globals = Globals::new(1, false, true);
    let interp_val = run_test_main(&mut globals, &wrapped, false);
    let ruby_res = run_ruby(&mut globals, code);

    assert!(Value::eq(interp_val, ruby_res));
}

/*pub fn run_test_nogc(code: &str) {
    let wrapped = format!(
        r##"
      __res = ({0})
      for __i in 0..7 do
          __res2 = ({0})
          __assert(__res, __res2)
      end
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let (interp_val, mut globals) = run_test_main(&wrapped, true);
    let ruby_res = run_ruby(&mut globals, code);

    assert!(Value::eq(interp_val, ruby_res));
}*/

pub fn run_test_once(code: &str) {
    let wrapped = format!(
        r##"
      ({0})
  "##,
        code
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new(1, false, true);
    let interp_val = run_test_main(&mut globals, &wrapped, false);
    let ruby_res = run_ruby(&mut globals, code);

    assert!(Value::eq(interp_val, ruby_res));
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
    let mut globals = Globals::new(1, false, true);
    let interp_val = run_test_main(&mut globals, &wrapped, false);
    let ruby_res = run_ruby(&mut globals, &code);

    assert!(Value::eq(interp_val, ruby_res));
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
      for __i in 0..25 do
          res2 = ({code})
          __assert(res, res2)
      end
      ({code})
  "##
    );
    eprintln!("{}", wrapped);
    let mut globals = Globals::new(1, false, true);
    let interp_val = run_test_main(&mut globals, &wrapped, false);
    let ruby_res = run_ruby(&mut globals, &format!("{prelude}\n{code}"));

    assert!(Value::eq(interp_val, ruby_res));
}

/*pub fn run_test2_with_prelude(code: &str, prelude: &str) {
    let wrapped = format!(
        r##"
      {prelude}
      {code}
  "##
    );
    eprintln!("{}", wrapped);
    let (interp_val, mut globals) = run_test_main(&wrapped);
    let ruby_res = run_ruby(&(prelude.to_string() + code), &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}*/

pub fn run_test2(code: &str) {
    let mut globals = Globals::new(1, false, true);
    let interp_val = run_test_main(&mut globals, code, false);
    let ruby_res = run_ruby(&mut globals, code);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_test_no_result_check(code: &str) -> Value {
    #[cfg(debug_assertions)]
    eprintln!("{code}");
    let mut globals = Globals::new(1, false, true);
    run_test_main(&mut globals, code, false)
}

pub fn run_test_error(code: &str) {
    #[cfg(debug_assertions)]
    eprintln!("{code}");
    let mut globals = Globals::new(1, false, true);
    match globals.run(code, std::path::Path::new(".")) {
        Ok(v) => {
            eprintln!("{}", v.inspect(&globals));
            panic!()
        }
        Err(err) => err.show_error_message_and_all_loc(&globals),
    }
}

fn run_test_main(globals: &mut Globals, code: &str, no_gc: bool) -> Value {
    #[cfg(not(debug_assertions))]
    let now = std::time::Instant::now();
    Globals::gc_enable(!no_gc);
    let res = match globals.run(code, std::path::Path::new(".")) {
        Ok(res) => res,
        Err(err) => {
            err.show_error_message_and_all_loc(&globals);
            panic!();
        }
    };

    let jit_str = res.inspect(globals);
    #[cfg(not(debug_assertions))]
    eprintln!("monoruby:  {jit_str} elapsed:{:?}", now.elapsed());
    #[cfg(debug_assertions)]
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

    let res = match std::process::Command::new("bash")
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

    #[cfg(debug_assertions)]
    eprintln!("ruby: {}", res.inspect(&globals));
    res
}
