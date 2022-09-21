#![feature(box_patterns)]
#![feature(int_roundings)]
pub use alloc::*;
pub use fxhash::FxHashMap as HashMap;
pub use monoasm::CodePtr;
pub use ruruby_parse::*;
use std::io::Write;
use std::path::PathBuf;
use tempfile::NamedTempFile;
//use std::collections::HashMap;
use std::fs::File;
use std::io::prelude::*;
#[cfg(not(debug_assertions))]
use std::time::*;

use rustyline::error::ReadlineError;
use rustyline::Editor;

mod alloc;
mod executor;
mod rvalue;
mod value;
use executor::*;
use rvalue::*;
use value::*;

#[derive(clap::Parser, Debug)]
#[clap(author, version, about, long_about = None, trailing_var_arg = true)]
struct CommandLineArgs {
    /// one liner. several -e's allowed. Omit [programfile]
    #[clap(short, multiple_occurrences = true)]
    exec: Vec<String>,
    /// print the version number, then turn on verbose mode
    #[clap(short)]
    verbose: bool,
    /// switch just-in-time compilation.
    #[clap(short, long)]
    no_jit: bool,
    #[clap(short = 'W', default_value = "1")]
    warning: u8,
    /// File name.
    file: Option<String>,
}

fn main() {
    use clap::Parser;
    let args = CommandLineArgs::parse();

    if !args.exec.is_empty() {
        for code in args.exec {
            exec(
                &code,
                args.no_jit,
                args.warning,
                std::path::Path::new("REPL"),
            );
        }
        return;
    }

    match args.file {
        Some(file_name) => {
            let mut file = File::open(file_name.clone()).unwrap();
            let mut code = String::new();
            file.read_to_string(&mut code).unwrap();
            exec(
                &code,
                args.no_jit,
                args.warning,
                &std::path::Path::new(&file_name),
            );
        }
        None => {
            let mut rl = Editor::<()>::new().unwrap();
            let mut globals = Globals::new(args.warning, args.no_jit);
            let mut cont_mode = false;
            let mut buf = String::new();
            let mut script_line = 0;
            let mut context = None;
            let mut interp = Interp::new(args.no_jit);
            loop {
                let prompt = format!(
                    "monoruby:{:03}{} ",
                    script_line,
                    if cont_mode { "*" } else { ">" }
                );
                let readline = rl.readline(&prompt);
                match readline {
                    Ok(code) => {
                        buf = if cont_mode {
                            format!("{}\n{}", buf, code)
                        } else {
                            code.clone()
                        };
                        let main_fid = match globals.compile_script_with_binding(
                            buf.clone(),
                            std::path::Path::new(&format!("REPL:{:03}", script_line)),
                            context.clone(),
                        ) {
                            Ok((fid, collector)) => {
                                context = Some(collector);
                                fid
                            }
                            Err(err) => {
                                if err.kind == MonorubyErrKind::Syntax(ParseErrKind::UnexpectedEOF)
                                {
                                    rl.add_history_entry(code.as_str());
                                    cont_mode = true;
                                } else {
                                    eprintln!("{}", err.get_error_message(&globals));
                                    err.show_all_loc();
                                    cont_mode = false;
                                };
                                continue;
                            }
                        };
                        rl.add_history_entry(code.as_str());
                        cont_mode = false;
                        match interp.eval(&mut globals, main_fid) {
                            Ok(val) => eprintln!("=> {}", val.inspect(&globals)),
                            Err(err) => {
                                eprintln!("{}", err.get_error_message(&globals));
                                err.show_all_loc();
                            }
                        };
                        script_line += 1;
                    }
                    Err(ReadlineError::Interrupted) => {
                        // Ctrl-C
                        cont_mode = false;
                    }
                    Err(ReadlineError::Eof) => {
                        // Ctrl-D
                        break;
                    }
                    Err(err) => {
                        println!("Error: {:?}", err);
                        break;
                    }
                }
            }
        }
    }
}

fn exec(code: &str, no_jit_flag: bool, warning: u8, path: &std::path::Path) {
    let mut globals = Globals::new(warning, no_jit_flag);
    let main_fid = match globals.compile_script(code.to_string(), path) {
        Ok(func_id) => func_id,
        Err(err) => {
            eprintln!("{:?}", err.get_error_message(&globals));
            err.show_loc();
            return;
        }
    };

    match Interp::eval_toplevel(&mut globals, main_fid) {
        Ok(_val) => {
            #[cfg(debug_assertions)]
            eprintln!("=> {:?}", _val)
        }
        Err(err) => {
            eprintln!("{}", err.get_error_message(&globals));
            err.show_loc();
        }
    };
}

pub fn run_test(code: &str) {
    let wrapped = format!(
        r##"
        res = ({0})
        for __i in 0..15 do
            res2 = ({0})
            __assert(res, res2)
        end
        res
    "##,
        code
    );
    eprintln!("{}", wrapped);
    let (interp_val, mut globals) = run_test_main(&wrapped);
    let ruby_res = run_ruby(code, &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_test2(code: &str) {
    let (interp_val, mut globals) = run_test_main(code);
    let ruby_res = run_ruby(code, &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_test_no_result_check(code: &str) -> Value {
    #[cfg(debug_assertions)]
    dbg!(code);
    run_test_main(code).0
}

pub fn run_test_main(code: &str) -> (Value, Globals) {
    #[cfg(not(debug_assertions))]
    let now = Instant::now();
    let (mut globals, fid) = new_globals(code);
    let jit_val = Interp::eval_toplevel(&mut globals, fid).unwrap();
    let jit_str = jit_val.to_s(&globals);
    #[cfg(not(debug_assertions))]
    eprintln!("jit: {jit_str} elapsed:{:?}", now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("jit: {jit_str}");

    (jit_val, globals)
}

pub fn run_test_error(code: &str) {
    #[cfg(debug_assertions)]
    dbg!(code);
    let (mut globals, fid) = new_globals(code);
    let jit_val = Interp::eval_toplevel(&mut globals, fid);
    eprintln!("result: {:?}", jit_val);
    eprintln!("error:  {:?}", jit_val.unwrap_err());
}

fn new_globals(code: &str) -> (Globals, FuncId) {
    let mut globals = Globals::new(1, false);
    let fid = match globals.compile_script(code.to_string(), std::path::Path::new("")) {
        Ok(fid) => fid,
        Err(err) => {
            err.show_all_loc();
            panic!("Error in compiling AST. {:?}", err);
        }
    };
    (globals, fid)
}

fn run_ruby(code: &str, globals: &mut Globals) -> Value {
    use std::process::Command;
    let mut tmp_file = NamedTempFile::new().unwrap();
    tmp_file
        .write_all(
            format!(
                r#"a = ({});
                puts;
                p(a)"#,
                code
            )
            .as_bytes(),
        )
        .unwrap();

    let output = Command::new("ruby")
        .args(&[tmp_file.path().to_string_lossy().to_string()])
        .output();

    let res = match &output {
        Ok(output) => {
            let res = std::str::from_utf8(&output.stdout)
                .unwrap()
                .trim_end()
                .split('\n')
                .last()
                .unwrap();
            let nodes = Parser::parse_program(res.to_string(), PathBuf::new())
                .unwrap()
                .node;

            Value::from_ast(&nodes, globals)
        }
        Err(err) => {
            panic!("Error occured in executing Ruby. {:?}", err);
        }
    };
    #[cfg(debug_assertions)]
    eprintln!("ruby: {}", res.inspect(&globals));
    res
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test0() {
        run_test("");
        run_test("4 * (2.9 + 7 / (1.15 - 6))");
        run_test("-4 * (2.9 + 7 / (-1.15 - 6))");
        run_test("1.5 + (2.0 + 3) + 1.1");
        run_test("-100/5");

        run_test("a = 55; a = a /5; a");

        run_test("true != true");
        run_test("true != false");
        run_test("false != false");
        run_test("false != true");

        run_test("a = 42; if a == 42 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a == 42.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a != 42.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a < 52.0 then 1.1 else 2.2 end");
        run_test("a = 42.0; if a > 52.0 then 1.1 else 2.2 end");
        run_test("a = 42.0 > 52.0; if a then 1.1 else 2.2 end");
    }

    #[test]
    fn test0_err() {
        for lhs in [
            "4.77",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ] {
            for rhs in ["0", "0.0"] {
                run_test_error(&format!("{} / {}", lhs, rhs));
            }
        }
        run_test_error(
            r#"
            a = [1,2,3,4,5]
            a[-6] = "Rust"
            a
        "#,
        );
    }

    #[test]
    fn test_multi_assign() {
        run_test("a, B = 7, 9.5; a + B");
        run_test("@a, @b = 1, 2; [@a, @b]");
        run_test("@a, @b = 1, 2; @a, @b = @b, @a; [@a, @b]");
        run_test("@a = []; @a[0], @a[1] = 1, 2; @a[0], @a[1] = @a[1], @a[0]; @a");
        run_test(
            r##"
        @a = []

        def foo
          @a << :foo
          []
        end

        def bar
          @a << :bar
          end

        foo[0] = bar
        a = foo[0] = bar
        b = (x, foo[0] = bar, 0)
        [@a, b]
        "##,
        );
    }

    #[test]
    fn test_numbers() {
        let lhs_integer = [
            "0",
            "53785",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ];
        let rhs_integer = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
        ];
        for lhs in lhs_integer {
            for rhs in rhs_integer {
                for op in ["&", "|", "^"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
        for lhs in lhs_integer.iter().chain(&["234.2345"]) {
            for rhs in rhs_integer.iter().chain(&["169.5333"]) {
                for op in ["+", "-", "*", "/", "==", "!=", "<", "<=", ">", ">="] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    //#[ignore]
    fn test_call() {
        run_test("print 1"); // max number of 63bit signed int.
    }

    #[test]
    fn test_int_bigint() {
        run_test("4611686018427387903"); // max number of 63bit signed int.
        run_test("4611686018427387903 + 1");
        run_test("4611686018400000000 + 27387904");
        run_test("-4611686018427387904"); // min number of 63bit signed int.
        run_test("-4611686018427387904 - 1");
        run_test("-4611686018400000001 - 27387904");
    }

    #[test]
    fn test_shift() {
        for lhs in ["157"] {
            for rhs in ["1", "54", "64"] {
                for op in ["<<", ">>"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    fn test_assign_op() {
        run_test("a=3; a+=7; a");
        run_test("a=3; a-=7; a");
        run_test("a=3; a*=7; a");
        run_test("a=300; a/=7; a");
        run_test("a=30; a<<=7; a");
        run_test("a=3000; a>>=7; a");
        run_test("a=36; a|=77; a");
        run_test("a=36; a&=77; a");
        run_test("a=36; a^=77; a");
        run_test("@a=36; @a+=7; @a");
        run_test("@a=[1,2,3]; @a[0]+=@a[1]; @a");
    }

    #[test]
    fn test1() {
        run_test("a=42; b=35.0; c=7; def f(x) a=4; end; if a-b==c then 0 else 1 end");
        run_test("def fn(x) x*2 end; a=42; c=b=a+7; d=b-a; e=b*d; d=f=fn(e); f=d/a");
        run_test("a=42; b=-a");
        run_test("a=42; a; b=-a");
    }

    #[test]
    fn test_assign() {
        run_test("a=8; b=2; c = (a,b=b,a); [a,b,c]");
        run_test("e = (a,b,c=1,2,3); [e, a, b, c]");
        run_test("a=b=c=7; [a,b,c]");
    }

    #[test]
    fn test_instance_var() {
        run_test("@a=42; @a");
        run_test("@a=42; @a = @a * 2; @a");
        run_test("@a=42; b = @a * 2; b");
        run_test("@a=42; c = b = @a * 2; c");
        run_test(r#"a=Object.new; a.instance_variable_set("@i", 42)"#);
        run_test(r#"a=Object.new; a.instance_variable_get(:@i)"#);
        run_test(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_defined?(:@i)"#,
        );
        run_test(
            r#"a=Object.new; a.instance_variable_set("@i", 42); a.instance_variable_get(:@i)"#,
        );
    }

    #[test]
    fn test_class_def() {
        run_test(
            r#"
        class C
          self
        end
        "#,
        );
        run_test(
            r#"
        class C
          self
        end
        42
        "#,
        );
        run_test(
            r#"
        a = class C
          self
        end
        a
        "#,
        );
    }

    #[test]
    fn test_fn() {
        run_test("def f; end; f");
    }

    #[test]
    fn test_stacktrace() {
        run_test_no_result_check(
            r##"
        class A 
          __dump
          def f(x)
            if x < 2
              __dump
              1
            else
              x*f(x-1)
            end
          end
        end
        f(7)
        "##,
        );
    }

    #[test]
    fn test_fibpoly() {
        run_test2(
            r#"
            def fib(x)
                if x<3 then
                    1.0
                else
                    fib(x-1)+fib(x-2)
                end
            end;
            fib(32.0)
            "#,
        );
    }

    #[test]
    fn bench_factorialpoly() {
        run_test2(
            r#"
            def fact(x)
                if x <= 1.0 then
                    1.0
                else
                    x * fact(x-1.0)
                end
            end;
            fact 130.0
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_fibo() {
        run_test2(
            r#"
            def fib(x)
                if x<3 then
                    1
                else
                    fib(x-1) + fib(x-2)
                end
            end;
            fib 40
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_factorial() {
        run_test2(
            r#"
            def fact(x)
                if x <= 1 then
                    1
                else
                    x * fact(x-1)
                end
            end;
            fact 4000
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_while2() {
        run_test2(
            r#"
            i = 0
            while i < 1000000000
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_for() {
        run_test2(
            r#"
            j = 0
            for i in 0..1000000000
              j = j + 1
            end
            j
            "#,
        );
    }

    #[test]
    #[ignore]
    fn bench_redefine() {
        run_test2(
            r#"
            def f; 1; end
            a = 0; i = 0
            while i < 200000000
              a = a + f
              if i == 500
                def f; 0; end
              end
              i = i + 1
            end
            a
            "#,
        );
    }

    #[test]
    fn test_while1() {
        run_test(
            r#"
            a=1;
            b=while a<2500 do
                a=a+1
            end
            a
            "#,
        );
    }

    #[test]
    fn test_while2() {
        run_test(
            r#"
            a=1
            b=while a<2500 do
                a=a+1
                if a == 100 then break a end
            end
            b
            "#,
        );
    }

    #[test]
    fn test_while_float() {
        run_test(
            r#"
            a = 7.9
            i = 0.0
            while i < 15
              a = 1 * a + 2 + a * 2
              i = i + 1.0
            end
            a
            "#,
        );
    }

    /*#[test]
    fn test_for1() {
        run_test(
            r#"
            a=1
            b = for a in 0..300 do
            end
            b # => 0..300
            "#,
        );
    }*/

    #[test]
    fn test_for2() {
        run_test(
            r#"
            b = for a in 0..300 do
                if a == 77 then break a/7 end
            end
            b
            "#,
        );
    }

    #[test]
    fn test_for_float() {
        run_test(
            r#"
            a = 7.9
            for i in 0..15
              a = -(1.1 * a + 2 + a * 2)
            end
            a
            "#,
        );
    }

    #[test]
    fn test3() {
        run_test(
            r#"
        a=3;
        if a==1;
          3
        else
          4
        end"#,
        );
    }

    #[test]
    fn test4() {
        run_test(
            r#"
        def f(a,b)
          a + b
        end
        f(5,7)
        f(4,9)
        "#,
        );
    }

    #[test]
    fn test5a() {
        run_test(
            r#"
        def f(a)
          a
        end
        f(7)
        "#,
        );
    }

    #[test]
    fn test5b() {
        run_test(
            r#"
        def f(a); a; end
        f(7)
        "#,
        );
    }

    #[test]
    fn test5() {
        run_test(
            r#"
        def f(a,b)
          a + b
        end
        f(5.1, 7)
        "#,
        );
    }

    #[test]
    fn test6() {
        run_test("def f; return 5; end; f");
        run_test("def f; return 5; end; f()");
        run_test("def f; return 5; end; self.f");
        run_test("def f; return 5; end; self.f()");
        run_test("def f; a=5; return a; end; f");
        run_test("def f; a=5; b=6; return a+b; end; f");
        run_test("def foo; end");
    }

    #[test]
    fn test7() {
        run_test(
            r#"
        def f
          1
        end
        a = 0
        i = 0
        while i < 10000
          a = a + f()
          if i == 500
            def f
              0
            end
          end
          i = i + 1
        end
        a 
        "#,
        );
    }

    #[test]
    fn test8() {
        run_test(
            r#"
        def f(x)
          x * 2
        end
        def g(x)
          x + 2
        end
        def h(x)
          x * x
        end
        h g f 7
        "#,
        );
    }

    #[test]
    fn test9() {
        run_test2(
            r#"
            puts 100
        "#,
        );
    }

    #[test]
    fn test9a() {
        run_test(
            r#"
            64.chr
            a = 64.chr
        "#,
        );
    }

    #[test]
    fn test10() {
        run_test(
            r#"
            if nil then 2*5/3 else 5 end
        "#,
        );
    }

    #[test]
    fn test_const() {
        run_test(
            r#"
            Const=4
            Const+=100
            a = Const
            Const
        "#,
        );
    }

    #[test]
    fn test_const2() {
        run_test2(
            r#"
            CONST = 5.7
            sum = 0
            for i in 0..19 do
                sum += CONST
                CONST = 1000 if i == 12
            end
            sum
        "#,
        );
    }

    #[test]
    fn test_string() {
        run_test(
            r##"
            def f(x); end
            x = " #{f 3} "
            f("windows")
            a = "linux"
        "##,
        );
    }

    #[test]
    fn test_symbol() {
        run_test(
            r#"
            def f(x); end
            f(:windows)
            a = :linux
        "#,
        );
    }

    #[test]
    fn test_array() {
        run_test(r#"[1,"2", true, nil]"#);
        run_test(r#"[1,"2", true, nil][-5]"#);
        run_test(r#"[1,"2", true, nil][-1]"#);
        run_test(r#"[1,"2", true, nil][0]"#);
        run_test(r#"[1,"2", true, nil][1]"#);
        run_test(r#"[1,"2", true, nil][2]"#);
        run_test(r#"[1,"2", true, nil][3]"#);
        run_test(r#"[1,"2", true, nil][4]"#);
        run_test(
            r#"
            a = [1,2,3,4,5]
            a[0] = 42
            a[3] = 77
            a[9] = "God"
            a[-4] = "Ruby"
            a
        "#,
        );
    }
}
