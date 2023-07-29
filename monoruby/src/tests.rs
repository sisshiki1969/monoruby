use super::*;
use ruruby_parse::Parser;
use std::io::Write;
use std::path::PathBuf;
use tempfile::NamedTempFile;

pub fn run_test(code: &str) {
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
    let (interp_val, mut globals) = run_test_main(&wrapped, false);
    let ruby_res = run_ruby(code, &mut globals);

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
    let ruby_res = run_ruby(code, &mut globals);

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
    let (interp_val, mut globals) = run_test_main(&wrapped, false);
    let ruby_res = run_ruby(code, &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_tests(code: &[String]) {
    let code = format!("[{}]", code.join(", "));
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
    let (interp_val, mut globals) = run_test_main(&wrapped, false);
    let ruby_res = run_ruby(&code, &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_binop_tests(lhs: &[&str], op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for lhs in lhs {
        for rhs in rhs {
            for op in op {
                test.extend_from_slice(&[
                    format!("{lhs} {op} {rhs}"),
                    format!("{lhs} {op} (-{rhs})"),
                    format!("-{lhs} {op} {rhs}"),
                    format!("-{lhs} {op} (-{rhs})"),
                ]);
            }
        }
    }
    run_tests(&test);
}

pub fn run_unop_tests(op: &[&str], rhs: &[&str]) {
    let mut test = vec![];
    for rhs in rhs {
        for op in op {
            test.extend_from_slice(&[format!("{op} {rhs}"), format!("{op} (-{rhs})")]);
        }
    }
    run_tests(&test);
}

pub fn run_test_with_prelude(code: &str, prelude: &str) {
    let wrapped = format!(
        r##"
      {prelude}
      res = ({code})
      for __i in 0..7 do
          res2 = ({code})
          __assert(res, res2)
      end
      ({code})
  "##
    );
    eprintln!("{}", wrapped);
    let (interp_val, mut globals) = run_test_main(&wrapped, false);
    let ruby_res = run_ruby(&format!("{prelude}\n{code}"), &mut globals);

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
    let (interp_val, mut globals) = run_test_main(code, false);
    let ruby_res = run_ruby(code, &mut globals);

    assert!(Value::eq(interp_val, ruby_res));
}

pub fn run_test_no_result_check(code: &str) -> Value {
    #[cfg(debug_assertions)]
    dbg!(code);
    run_test_main(code, false).0
}

pub fn run_test_error(code: &str) {
    #[cfg(debug_assertions)]
    dbg!(code);
    let mut globals = Globals::new(1, false);
    match globals.compile_and_run(code, std::path::Path::new("")) {
        Ok(_) => panic!(),
        Err(err) => err.show_error_message_and_all_loc(),
    }
}

fn run_test_main(code: &str, no_gc: bool) -> (Value, Globals) {
    #[cfg(not(debug_assertions))]
    let now = std::time::Instant::now();
    let mut globals = Globals::new(1, false);
    Globals::gc_enable(!no_gc);
    let res = match globals.compile_and_run(code, std::path::Path::new("")) {
        Ok(res) => res,
        Err(err) => {
            err.show_error_message_and_all_loc();
            panic!();
        }
    };

    let jit_str = globals.inspect(res);
    #[cfg(not(debug_assertions))]
    eprintln!("monoruby:  {jit_str} elapsed:{:?}", now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("monoruby:  {jit_str}");

    (res, globals)
}

fn run_ruby(code: &str, globals: &mut Globals) -> Value {
    use std::process::Command;
    let mut tmp_file = NamedTempFile::new().unwrap();
    tmp_file
        .write_all(
            format!(
                r#"____a = ({});
              puts;
              p(____a)"#,
                code
            )
            .as_bytes(),
        )
        .unwrap();

    let output = Command::new("ruby")
        .args(&[tmp_file.path().to_string_lossy().to_string()])
        .output();

    let res = match &output {
        Err(err) => {
            panic!("Error occured in executing Ruby. {:?}", err);
        }
        Ok(output) if !output.status.success() => {
            panic!(
                "Error occured in executing Ruby. {}",
                std::str::from_utf8(&output.stderr).unwrap()
            );
        }
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
    };
    #[cfg(debug_assertions)]
    eprintln!("ruby: {}", globals.inspect(res));
    res
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
        run_test("4..173");
        run_test("4...173");
        run_test("!true");
        run_test("!false");
        run_test("!!false");
        run_test("!nil");
        run_test("a = !nil; a");
        run_test("!!nil");
        run_test("!100");
        run_test("!!100");
        run_test("!7.55");
        run_test("!:a");
        run_test(r#"!"g""#);
    }

    #[test]
    fn test0_err() {
        for lhs in [
            "0",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ] {
            run_test_error(&format!("{lhs} / 0"));
        }
        run_test_error(
            r#"
            a = [1,2,3,4,5]
            a[-6] = "Rust"
            a
        "#,
        );
        run_test_error(
            r#"
            class Integer
              attr_accessor :a
            end
            4.a = 100
        "#,
        );
        run_test_error(
            r#"
            class Float
              def f
                @a = 42
              end
            end
            4.0.f
        "#,
        );
        run_test_error("break");
        run_test_error("joke");
        run_test_error("Joke");
        run_test_error("91552338.chr");
        run_test_error(r#"9155.."s""#);
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
        run_test(
            r##"
        d = (a,b,c = [1,2])
        [a,b,c,d]
        "##,
        );
        run_test(
            r##"
        d = (a,b,c = [1,2,3])
        [a,b,c,d]
        "##,
        );
        run_test(
            r##"
        d = (a,b,c = [1,2,3,4])
        [a,b,c,d]
        "##,
        );
        run_test(
            r##"
        d = (a,b,c = 100)
        [a,b,c,d]
        "##,
        );
    }

    #[test]
    fn test_exp() {
        run_test("3.78**432");
        run_test("3.78**-432");
        run_test("-3.78**432");
        run_test("-3.78**-432");

        run_test("39**3");
        run_test("39**3431");
        run_test("392909**3431");
        run_test("392909**4");
        run_test("-392909**3431");
        run_test("-392909**3432");

        run_test("378**4.32");
        run_test("378**-4.32");
        run_test("-378**4.32");
        run_test("-378**-4.32");
        run_test("378258461125841513588485555**-4.32");

        run_test("90.78**43.2");
        run_test("90.78**-43.2");
        run_test("-90.78**43.2");
        run_test("-90.78**-43.2");
    }

    #[test]
    fn binop_numeric() {
        let lhs_integer = [
            "0",
            "5375",
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
        let mut test = vec![];
        for lhs in lhs_integer {
            for rhs in rhs_integer {
                for op in ["%"] {
                    test.extend_from_slice(&[
                        format!("{lhs} {op} {rhs}"),
                        format!("-{lhs} {op} (-{rhs})"),
                    ]);
                }
            }
        }
        run_tests(&test);
        run_binop_tests(
            &lhs_integer,
            &["+", "-", "*", "/", "&", "|", "^"],
            &rhs_integer,
        );
    }

    #[test]
    fn cmp_numeric() {
        let lhs = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
            "234.2345",
            "234234645.0",
        ];
        let rhs = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
            "169.5333",
            "690426.0",
        ];
        run_binop_tests(
            &lhs,
            &["==", "!=", "<", "<=", ">", ">=", "===", "<=>"],
            &rhs,
        );
    }

    #[test]
    fn unop_numeric() {
        let rhs = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
        ];
        run_unop_tests(&["-", "~", "+", "!"], &rhs);
        let rhs = [
            "0.0",
            "53.75",
            "248.29482958347598570210950349530597028472983429873",
        ];
        run_unop_tests(&["-", "+"], &rhs);
    }

    #[test]
    fn test_call() {
        run_test("print 1");
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
        run_test("@a = 10; @a += 15; @a");
        run_test_with_prelude(
            r###"
        x = C.new
        x.ivar
        "###,
            r###"
        class C
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
          def ivar
            [@a, @b, @c, @d, @e, @f, @g, @h]
          end
        end
        "###,
        );
        run_test_with_prelude(
            r###"
        x = C.new
        x.ivar
        "###,
            r###"
        class C < Array
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
          def ivar
            [@a, @b, @c, @d, @e, @f, @g, @h]
          end
        end
        "###,
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
    fn test_if() {
        run_test("if false then 100 end")
    }

    #[test]
    fn test_class_def2() {
        run_test(
            r#"
        class A
        end
        class B < A
        end
        class C < B
        end
        [A, B.superclass, C.superclass.superclass]
        "#,
        );
        run_test(
            r#"
        class A < Array
        end
        [A, A.superclass]
        "#,
        );
    }

    #[test]
    fn test_fn() {
        run_test("def f; end; f");
        run_test_with_prelude(
            "
        a = []
        a << f(4,7)
        a << f(4,7) do end
        a
        ",
            "
        def f(a,b)
          a-b
        end
        ",
        );
    }

    #[test]
    fn test_stacktrace() {
        run_test_no_result_check(
            r##"
        def f(x)
          if x < 2
            __dump
            1
          else
            x*f(x-1)
          end
        end
        f(7)
        "##,
        );
    }

    #[test]
    fn test_stacktrace2() {
        run_test_no_result_check(
            r##"
        1.times { 1.times { __dump } }
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
    fn bench_while2() {
        run_test2(
            r#"
            i = 0
            while i < 1000
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_while_postfix() {
        run_test2(
            r#"
            i = 0
            begin
              i = i + 1
            end while i < 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            i += 1 while i < 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            begin
              i += 1
            end while i > 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            i += 1 while i > 10
            i
            "#,
        );
    }

    #[test]
    fn bench_while_and() {
        run_test2(
            r#"
            i = 0
            while i < 1000 && i < 120
              i = i + 1
            end
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            while i < 120 && i < 1000 
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_while_or() {
        run_test2(
            r#"
            i = 0
            while i < 1000 || i < 120
              i = i + 1
            end
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            while i < 120 || i < 1000 
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_until() {
        run_test2(
            r#"
            i = 0
            until i == 1000
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_until_postfix() {
        run_test2(
            r#"
            i = 0
            begin
              i = i + 1
            end until i > 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            i += 1 until i > 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            begin
              i += 1
            end until i < 10
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            i += 1 until i < 10
            i
            "#,
        );
    }

    #[test]
    fn bench_until_and() {
        run_test2(
            r#"
            i = 0
            until i > 1000 && i > 120
              i = i + 1
            end
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            until i > 120 && i > 1000 
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_until_or() {
        run_test2(
            r#"
            i = 0
            until i > 1000 || i > 120
              i = i + 1
            end
            i
            "#,
        );
        run_test2(
            r#"
            i = 0
            until i > 120 || i > 1000 
              i = i + 1
            end
            i
            "#,
        );
    }

    #[test]
    fn bench_for() {
        run_test2(
            r#"
            j = 0
            for i in 0..1000
              j = j + 1
            end
            j
            "#,
        );
        run_test2(
            r#"
            j = 0
            for i in 0..0
              j = j + 1
            end
            j
            "#,
        );
        run_test2(
            r#"
            j = 0
            for i in 0...1000
              j = j + 1
            end
            j
            "#,
        );
        run_test2(
            r#"
            j = 0
            for i in 0...0
              j = j + 1
            end
            j
            "#,
        );
    }

    #[test]
    fn bench_redefine() {
        run_test2(
            r#"
            def f; 1; end
            a = 0; i = 0
            while i < 20000
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
            x=0
            b=while a<2500 do
            if a==100 then break a end
                x=x+a
                a=a+1
            end
            [a,b,x]
            "#,
        );
        run_test_once(
            r#"
            a=1
            x=0
            b=while a<4 do
                a=a+1
                if a==2 then next a end
                x=x+a
            end
            [a,b,x]
            "#,
        );
        run_test(
            r#"
            a=1
            x=0
            b=until a>=2500 do
                if a==100 then break a end
                x=x+a
                a=a+1
            end
            [a,b,x]
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

    #[test]
    fn test_for1() {
        run_test(
            r#"
            a=1
            b = for a in 0..300 do
            end
            b # => 0..300
            "#,
        );
    }

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
    fn test_case0() {
        run_test(
            r#"
        foo = false
        bar = true
        quu = false
        
        case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        "#,
        );
        run_test(
            r#"
        foo = false
        bar = true
        quu = false
        
        case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        100
        "#,
        );
        run_test(
            r#"
        foo = false
        bar = true
        quu = false
        
        x = case
        when foo then 'foo is true'
        when bar then 'bar is true'
        when quu then 'quu is true'
        end
        "#,
        );
        run_test(
            r#"
        c = 5
        case
        when c == 3, c == 6, c == 9
          '10より小さな3の倍数'
        when c % 10 == 0
          '10の倍数'
        when c % 5 == 0
          '5の倍数'
        else
          'それ以外'
        end
        "#,
        )
    }

    #[test]
    fn test_case1() {
        run_test(
            r#"
        case :symbol
        when Integer then 'integer'
        when Float then 'float'
        when Symbol then 'symbol'
        end
        "#,
        );
        run_test(
            r#"
        x = case :symbol
        when Integer then 'integer'
        when Float then 'float'
        when Symbol then 'symbol'
        end
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

    #[test]
    fn test_logical_ops() {
        let input = ["true", "false", "nil", "100", ":ab", "Object"];
        let mut test = vec![];
        for lhs in input {
            for rhs in input {
                test.push(format!(
                    r##"[
                        {lhs} && {rhs},
                        {lhs} || {rhs},
                        if {lhs} && {rhs} then 17 else 42 end,
                        if {lhs} || {rhs} then 17 else 42 end
                        ]"##
                ));
            }
        }
        run_tests(&test);
        run_test("if 4 == 4 and 3 < 1 then 0 else 42 end");
        run_test("if 4 != 4 or 3 < 1 then 0 else 42 end");
    }

    #[test]
    fn test_block_call1() {
        run_test_with_prelude(
            r#"
        f {|a,b|
          e=42
          [a,b,e]
        }
        "#,
            r#"
        def f
          yield 1,2,3,4
        end
        "#,
        );
    }

    #[test]
    fn test_block_call2() {
        run_test_with_prelude(
            r#"
        f {|a,b,c,d|
          e=42
          [a,b,c,d,e]
        }
        "#,
            r#"
        def f
          yield 1,2
        end
        "#,
        );
    }

    #[test]
    fn test_block_nest1() {
        run_test_with_prelude(
            r#"
        f {|a,(b,c,d),e,f|
            [a,b,c,d,e,f]
        }
        "#,
            r#"
        def f
          yield 1,[2,3],4
        end
        "#,
        );
    }

    #[test]
    fn test_block_nest2() {
        run_test_with_prelude(
            r#"
                f {|a,(b)|
                    [a,b]
                }
                "#,
            r#"
                def f
                  yield 1,[2,3],4
                end
                "#,
        );
    }

    #[test]
    fn test_block_array_expand1() {
        run_test_with_prelude(
            r#"
            f { |a,(b,c),d|
                e = 100
                [a,b,c,d,e]
            }
            "#,
            r#"
            def f
                yield [1,[2,3],4]
            end
            "#,
        );
    }

    #[test]
    fn test_block_array_expand2() {
        run_test_with_prelude(
            r#"
            f { |a,b|
                c = 42
                [a,b,c]
            }
            "#,
            r#"
            def f
              yield [1,[2,3],4]
            end
            "#,
        );
    }

    #[test]
    fn test_block_optional() {
        run_test_with_prelude(
            r#"
        f { |a,b,c=42|
          [a,b,c]
        }
        "#,
            r#"
        def f
          yield [1,2]
        end
        "#,
        );
    }

    #[test]
    fn test_method_optional() {
        run_test_with_prelude(
            r#"
        f(1,2)
        "#,
            r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f(1,2,3)
        "#,
            r#"
        def f(x,y,z=42)
            [x,y,z]
        end
        "#,
        );
    }

    #[test]
    fn test_method_rest() {
        run_test_with_prelude(
            r#"
        f(1,2)
        "#,
            r#"
        def f(x,y,a=42,b=55,*z)
            [x,y,a,b,z]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f(1,2,3,4,5,6,7)
        "#,
            r#"
        def f(x,y,a=42,b=55,*z)
            [x,y,a,b,z]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f(1,2,3,4,5,6,7)
        "#,
            r#"
        def f(x,y,*z)
            [x,y,z]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f { |a,b,x=42,y=12,*c|
          [a,b,c,x,y]
        }
        "#,
            r#"
        def f
          yield [1,2,3]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f { |a,b,x=42,y=12,*c|
          [a,b,c,x,y]
        }
        "#,
            r#"
        def f
          yield [1,2,3,4,5,6]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f(0,1,2,3,4,5,6,7,8)
        "#,
            r#"
        def f(*x)
          x
        end
        "#,
        );
    }

    #[test]
    fn test_keyword() {
        run_test_with_prelude(
            r#"
        x = []
        x << f(1,2)
        x << f(1,2,3,c:10,a:20)
        x << f(1,2,3,4,5,6,7,8)
        x << f(1,2,3,4,5,6,7,8,b:50)
        x
        "#,
            r#"
        def f(x,y,z=10,*r,a:1,b:2,c:3)
          [x, y, z, r, a, b, c]
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        x = []
        x << f(1,2) { |x| x * 14 }
        x << f(1,2,3,c:10,a:20) { |x| x * 14 }
        x << f(1,2,3,4,5,6,7,8) { |x| x * 14 }
        x << f(1,2,3,4,5,6,7,8,b:50) { |x| x * 14 }
        x
        "#,
            r#"
        def f(x,y,z=10,*r,a:1,b:2,c:3,&p)
          [x, y, z, r, a, b, c, g(&p)]
        end
        def g
          yield 3
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        [f(c:7), f(b:8, a:10), f]
        "#,
            r#"
        def f(a:1,b:2,c:3)
          [a, b, c]
        end
        "#,
        );
    }

    #[test]
    fn test_splat() {
        run_test_with_prelude(
            r#"
        f(*[0,1,2,3,4,5,6,7,8])
        "#,
            r#"
        def f(*x)
          x
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f(*[0,1,2,3])
        "#,
            r#"
        def f(a,b,c=12,d=23)
          a+b+c+d
        end
        "#,
        );
        run_test_with_prelude(
            r#"
        f([1,2,3,4,5]) {|a,b,c,d,e| a+b+c+d+e}
        "#,
            r#"
        def f(x)
          yield *x
        end
        "#,
        );
    }

    #[test]
    fn test_destruct() {
        run_test_with_prelude(
            r#"
        f(1,[2,3,4],5,[6])
        "#,
            r#"
        def f(a,(b,c),d,(e,f))
          g = 42
          [a,b,c,d,e,f,g]
        end
        "#,
        );
    }

    #[test]
    fn test_method_error() {
        run_test_error(
            r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        f(1)
        "#,
        );
        run_test_error(
            r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        f(1,2,3,4,5)
        "#,
        );
        run_test_error(
            r#"
        def f
        end
        f(1,2,3,4,5)
        "#,
        );
        run_test_error(
            r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        10.times {|x|
            if x == 9
                f(1)
            else
                f(1,2)
            end
        }
        "#,
        );
        run_test_error(
            r#"
        def f(x,y,z=42,w=12)
            [x,y,z,w]
        end
        10.times {|x|
            if x == 9
                f(1,2,3,4,5)
            else
                f(1,2)
            end
        }
        "#,
        );
        run_test_error(
            r#"
        def f
        end
        10.times {|x|
            if x == 9
                f(1)
            else
                f
            end
        }
        "#,
        );
    }

    #[test]
    fn test_block_param() {
        run_test_with_prelude(
            r#"
        f do |a,b|
            a + b + h
        end
        "#,
            r#"
        def g
            yield 1,2
        end

        def h
            yield 1,3,5
        end
          
        def f(&p)
            [g(&p), h(&p)]
        end
        h = 39
        "#,
        );
    }

    #[test]
    fn test_global_var() {
        run_test(
            r#"
        a = []
        a << $std
        $std = 42
        a << $std
        $std = nil
        a
        "#,
        )
    }

    #[test]
    fn test_alias() {
        run_test(
            r#"
        def f
          "f"
        end
        alias g f
        g
        "#,
        )
    }

    #[test]
    fn test_alias_class() {
        run_test(
            r#"
        class S
          def f
            "f"
          end
          def g
            "g"
          end
        end
        class C < S
          alias g f
        end
        c = C.new
        s = S.new
        [c.f, c.g, s.f, s.g]
        "#,
        )
    }

    #[test]
    fn test_module() {
        run_test_with_prelude(
            "[M.class, M.singleton_class.superclass]",
            r#"
            module M
            end
        "#,
        );
        run_test_with_prelude(
            "[C.new.class == C, C.singleton_class.superclass == Object.singleton_class, D.superclass == C]",
            r#"
            C = Class.new
            D = Class.new(C)
            "#,
        );
    }

    #[test]
    fn test_singleton_method() {
        run_test_with_prelude(
            r#"
            def C.f; 5; end
            C.f
            "#,
            r#"
            class C
            end
            def C.f; 5; end
        "#,
        );
        run_test_with_prelude(
            r#"
            def c.f; 5; end
            c.f
            "#,
            r#"
            class C
            end
            c = C.new
            def c.f; 5; end
        "#,
        );
    }

    #[test]
    fn test_singleton_class_def() {
        run_test_with_prelude(
            r#"
            class << c
              def f; end
            end
            [c.x, c.y, c.z]
            "#,
            r#"
            class C
              attr_accessor :x,:y,:z
            end
            c = C.new
            c.x = 10
            c.y = 20
            c.z = 30
        "#,
        );
    }

    #[test]
    fn test_singleton_obj_attr() {
        run_test_with_prelude(
            r#"
            def c.f; 42; end
            [c.x, c.y, c.z]
            "#,
            r#"
            class C
              attr_accessor :x,:y,:z
            end
            c = C.new
            c.x = 10
            c.y = 20
            c.z = 30
        "#,
        );
    }

    #[test]
    fn test_constant_cache_miss() {
        run_test_once(
            r#"
            $a = []
            C = 100
            
            for i in 0..19
              if i == 10
                C = 10
              end
              $a << C
            end

            $a
            "#,
        );
    }

    #[test]
    fn test_constant_in_method() {
        run_test(
            r#"
            class C
              CONST = 1
              def f
                a = 0;
                5.times { a += CONST }
                a
              end
            end
            CONST = 2
            C.new.f
        "#,
        );
        run_test(
            r#"
            class C
                CONST = 1
            end
            CONST = 2
            [CONST, C::CONST]
        "#,
        );
        run_test(
            r#"
            $a = []
            class Foo
                CONST = 'Foo'
            end
          
            class Bar
                CONST = 'Bar'
                class Baz < Foo
                    $a << CONST             # => "Bar"      外側の定数
                    # この場合、親クラスの定数は明示的に指定しなければ見えない
                    $a << Foo::CONST        # => "Foo"
                end
            end
            $a
        "#,
        );
        run_test_with_prelude(
            "$a",
            r#"
            $a = []
            class Foo
                CONST = 'Foo'
            end
          
            class Bar < Foo
                $a << CONST               # => "Foo"
                CONST = 'Bar'         # Bar の定数 CONST を定義
                $a << CONST               # => "Bar"  (Foo::CONST は隠蔽される)
                $a << Foo::CONST          # => "Foo"  (:: 演算子で明示すれば見える)
            end
        "#,
        );
    }

    #[test]
    fn test_yield_in_block() {
        run_test(
            "
        class C
          def f
            x = 0
            7.times { x += yield }
            x
          end
        end

        a = 42
        c = C.new

        c.f { a }
        ",
        );
        run_test(
            "
        class C
          def f
            x = 0
            10.times { x += yield }
            x
          end
        end

        @a = 42
        c = C.new

        c.f { @a }
        ",
        );
    }

    #[test]
    fn test_for_each() {
        run_test(
            r#"
        a = 0
        for i in [0,1,2,3,4]
            1.times do
                for j in [5,6,7,8]
                    for k in [10,11,12]
                        1.times do
                            a += i + j + k
                        end
                    end
                end
            end
        end
        [i,a]
        "#,
        );
    }

    #[test]
    fn test_nested_blockargproxy() {
        run_test_with_prelude(
            r#"
        $x = 0
        g { 42 }
        $x
        "#,
            r#"
        def e
          10.times do
            $x += yield
          end
        end

        def f(&q)
          10.times do
            e(&q)
          end
        end

        def g(&p)
          10.times do
            10.times do
              f(&p)
            end
          end
        end
        "#,
        );
    }

    #[test]
    fn test_block_arg() {
        run_test_with_prelude(
            r##"
        $x = []
        f { 100 }
        p = Proc.new { 200 }
        f(&p)
        $x
    "##,
            r##"
        def f(&p)
            g(&p)
        end
                
        def g(&p)
            $x << yield
        end
    "##,
        );
        run_test_with_prelude(
            r##"
        $x = []
        f { 100 }
        p = Proc.new { 200 }
        f(&p)
        $x
    "##,
            r##"
        def f(&p)
            g(&p)
        end
                
        def g(&p)
            $x << p.call
        end
    "##,
        );
    }

    #[test]
    fn test_nested_call_opt() {
        run_test_with_prelude(
            r#"
            a = [1,2,3,4,5]
            [f(100), f(*a)]
        "#,
            r#"
            def f(*x); x; end
        "#,
        );
    }

    #[test]
    fn test_super() {
        run_test_with_prelude(
            r#"
            D.new.f(42, 100)
        "#,
            r#"
            class C
                def f(x,y,z,a:1000)
                    x+y+z+a
                end
            end

            class D < C
                def f(x,y,z=10,a:77)
                    super x,y,z,a:a
                end
            end
        "#,
        );
        run_test_with_prelude(
            r##"
            D.new.f(1,[2,3],f:70)
                "##,
            r##"
            class C
              def f(a,(b,c),d,e:30,f:40)
                [a,b,c,d,e,f]
              end
            end

            class D < C
              def f(a,(b,c),d=100,e:42,f:10)
                a = 100
                c = 50
                e = 200
                super
              end
            end
            "##,
        );
    }

    #[test]
    fn rescue() {
        run_test(
            r#"
            # Use
            begin
              100
            end
        "#,
        );
        run_test(
            r#"
            # NoUse
            begin
              100
            end
            nil
        "#,
        );
        run_test_once(
            r#"
            # Ret
            begin
              100
            end
        "#,
        );
        run_test(
            r#"
            begin
              100
            end
        "#,
        );
        run_test_once(
            r#"
            begin
              100
            rescue
              200
            end
        "#,
        );
        run_test(
            r#"
            begin
              100
            rescue
            else
              200
            end
        "#,
        );
        run_test(
            r#"
            begin
              100
            rescue
            else
              200
            ensure
              300
            end
        "#,
        );
        run_test(
            r#"
            $x = []
            begin
                begin
                    $x << 50
                    1/0
                    $x << 100
                rescue => c
                    $x << 150
                else
                    $x << 200
                ensure
                    $x << 250
                    1/0
                    $x << 300
                end
            rescue => d
                $x << d.to_s
            end
            $x
        "#,
        );
    }

    #[test]
    fn rescue_write_back() {
        run_test(
            r#"
        res = []
        for i in 0..10
            begin
                x = 50.0 + i
                1/0
                x = 100.0
            rescue => c
                res << x
                x = 150.0
            else
                x = 200.0
            ensure
                x = 300.0
            end
        end
        res << x
        res
        "#,
        );
        run_test(
            r#"
        res = []
        begin
            for i in 0..10
                x = 50.0 + 50/(9-i)
                res << x
            end
        rescue => c
            res << x
            x = 150.0
        else
            x = 200.0
        ensure
            x = 300.0
        end
        res << x
        "#,
        );
    }

    #[test]
    fn rest_discard() {
        run_test_with_prelude(
            r#"
            [f(1,2), f(1,2,3,4,5)]
        "#,
            r#"
            def f(a,b,*)
              [a,b]
            end
        "#,
        );
    }

    #[test]
    fn hash_splat() {
        run_test_with_prelude(
            r#"
            f(1,2,d:4,a:1,**{c:3,b:2})
        "#,
            r#"
            def f(x,y,a:100,b:200,c:300,d:400)
              [a,b,c,d,x,y]
            end
        "#,
        );
        run_test_with_prelude(
            r#"
            f(1,2,**{c:3,b:2})
        "#,
            r#"
            def f(x,y,a:100,b:200,c:300,d:400)
              [a,b,c,d,x,y]
            end
        "#,
        );
        run_test_with_prelude(
            r#"
            f(1,2,**{c:3,b:2})
        "#,
            r#"
            def f(x,y,b:200,c:300)
              [b,c,x,y]
            end
        "#,
        );
    }

    #[test]
    fn defined() {
        run_test_with_prelude(
            r#"
            $x = []
            f{}
            f
            f{}
            $x
        "#,
            r#"
            def f
                5.times { $x << (defined? yield) }
            end
        "#,
        );
        run_test(r#"defined? 10"#);
        run_test(r#"defined? 100000000000000000000000000000000000000000000000000000000"#);
        run_test(r#"defined? 5.5"#);
        run_test(r#"defined? 5i"#);
        run_test(r#"defined? :a"#);
        run_test(r#"defined? "abs""#);
        run_test(r#"defined? /abs/"#);
        run_test(r#"defined? AA..ZZ"#);
        run_test(r#"defined? ({a:AA, b:BB})"#);
        run_test(r#"defined? `ls -a`"#);
        run_test(r#"defined? nil"#);
        run_test(r#"defined? self"#);
        run_test(r#"defined? true"#);
        run_test(r#"defined? false"#);
        run_test(r#"defined? break"#);
        run_test(r#"defined? return"#);
        run_test(r#"defined? next"#);
        run_test(r#"defined? a=z"#);
        run_test(r#"defined? a+=z"#);
        run_test(r#"defined? (def f;end)"#);
        run_test(r#"defined? (def self.f;end)"#);
        run_test(r#"defined? (class F;end)"#);
        run_test(r#"defined? (class << obj F;end)"#);
        run_test(r#"a=10; defined? a"#);
        run_test(r#"defined? a"#);
        run_test(r#"a=""; defined? 1+a"#);
        run_test(r#"defined? puts"#);
        run_test(r#"defined? @a"#);
        run_test(r#"@a=10; defined? @a"#);
        run_test(r#"defined? $a"#);
        run_test(r#"$a=10; defined? $a"#);
        run_test(r#"C=10; defined? C"#);
        run_test(r#"defined? C"#);
        run_test(r#"defined? [1,2].map{}.to_s"#);
        run_test(r#"defined? [1,2].map{}.zxzxz"#);
        run_test(r#"defined? a[1]"#);
        run_test(r#"a = []; defined? a[1]"#);
        run_test(r#"defined? (a[1]=5)"#);
        run_test(r#"a = []; defined? (a[1]=5)"#);
        run_test(r#"defined?(1+(2+3))"#);
        run_test(r#"defined? 1+(2+3)"#);
    }

    #[test]
    fn method_return() {
        run_test_with_prelude(
            r#"
            $res = [] 
            [f, $res]
            "#,
            r#"
            def f
                7.times {
                    9.times { |x|
                        9.times { |y|
                            $res << x+y
                            return x,y if x+y==11
                        }
                        $res << 10
                    }
                    $res << 20
                }
                $res << 30
            end
            "#,
        )
    }

    #[test]
    fn block_break() {
        run_test_with_prelude(
            r#"
            $res = []
            f
            $res
            "#,
            r#"
            def f
                7.times { |x|
                    $res << 7.times { |y|
                        $res << x+y
                        break x,y if x+y>=5
                    }
                    $res << 100
                }
                $res << 200
            end
            "#,
        );
        run_test_with_prelude(
            r#"
            $res = []
            f
            $res
            "#,
            r#"
            def f
                $res << 12.times { |x|
                    $res << x
                    break x if x==9
                }
                $res << 100
            end
            "#,
        );
    }

    #[test]
    fn block_return_ensure() {
        run_test_with_prelude(
            r#"
            $x = []
            [foo, $x]
            "#,
            r#"
            def foo
              2.times do |i|
                2.times  do |j|
                  $x << [i,j]
                  return 3 if i == 1 && j == 0
                ensure
                  $x << ["j",j]
                end
              ensure
                $x << ["i",i]
              end
            ensure
              $x << "foo"
            end
            "#,
        );
    }

    #[test]
    fn polymorphic_call() {
        run_test_with_prelude(
            r#"
            $res = []
            x.each do |e|
              $res << e.g
            end
            $res
            "#,
            r#"
            class S
              def g
                f
              end
            end

            class A < S
              def f
                1
              end
            end

            class B < S
              def f
                2
              end
            end

            x = [A.new, B.new, A.new, B.new, A.new, B.new, B.new, A.new, B.new, A.new, B.new]
            "#,
        );
    }

    #[test]
    fn polymorphic_call2() {
        run_test_with_prelude(
            r#"
            $res = []
            for i in 0...x.size
              $res << x[i].g
            end
            $res
            "#,
            r#"
            class S
              def g
                x = 0
                for i in 0..10
                  x += @x
                end
                x
              end
            end

            class A < S
              def initialize
                @x = 1
                @y = 2
              end
            end

            class B < S
              def initialize
                @y = 10
                @x = 20
              end
            end

            x = [A.new, B.new, A.new, B.new, A.new, B.new, B.new, A.new, B.new, A.new, B.new]
            "#,
        );
    }
}
