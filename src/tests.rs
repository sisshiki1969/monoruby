use super::*;
use std::io::Write;
use std::path::PathBuf;
use tempfile::NamedTempFile;

pub fn run_test(code: &str) {
    let wrapped = format!(
        r##"
      res = ({0})
      for __i in 0..5 do
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

pub fn run_test_error(code: &str) {
    #[cfg(debug_assertions)]
    dbg!(code);
    let mut globals = Globals::new(1, false);
    compile_and_run(&mut globals, code, std::path::Path::new("")).unwrap_err();
}

fn run_test_main(code: &str) -> (Value, Globals) {
    #[cfg(not(debug_assertions))]
    let now = std::time::Instant::now();
    let mut globals = Globals::new(1, false);
    let res = compile_and_run(&mut globals, code, std::path::Path::new("")).unwrap();
    let jit_str = res.inspect(&globals);
    #[cfg(not(debug_assertions))]
    eprintln!("jit:  {jit_str} elapsed:{:?}", now.elapsed());
    #[cfg(debug_assertions)]
    eprintln!("jit:  {jit_str}");

    (res, globals)
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

            from_ast(&nodes, globals)
        }
        Err(err) => {
            panic!("Error occured in executing Ruby. {:?}", err);
        }
    };
    #[cfg(debug_assertions)]
    eprintln!("ruby: {}", res.inspect(&globals));
    res
}

fn from_ast(node: &Node, globals: &mut Globals) -> Value {
    match &node.kind {
        NodeKind::CompStmt(stmts) => {
            assert_eq!(1, stmts.len());
            from_ast(&stmts[0], globals)
        }
        NodeKind::Integer(num) => Value::new_integer(*num),
        NodeKind::Bignum(num) => Value::new_bigint(num.clone()),
        NodeKind::Float(num) => Value::new_float(*num),
        NodeKind::Bool(b) => Value::bool(*b),
        NodeKind::Nil => Value::nil(),
        NodeKind::Symbol(sym) => Value::new_symbol(IdentId::get_ident_id(sym)),
        NodeKind::String(s) => Value::new_string_from_str(s),
        NodeKind::Array(v, _) => {
            let v = v.iter().map(|node| from_ast(node, globals)).collect();
            Value::new_array(v)
        }
        NodeKind::Const {
            toplevel,
            parent,
            prefix,
            name,
        } => {
            assert_eq!(false, *toplevel);
            assert_eq!(None, *parent);
            assert_eq!(0, prefix.len());
            let constant = IdentId::get_ident_id(name);
            globals.get_constant(OBJECT_CLASS, constant).unwrap()
        }
        _ => unimplemented!(),
    }
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
    fn test_numbers1() {
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
        for lhs in lhs_integer.iter() {
            for rhs in rhs_integer.iter() {
                for op in ["%"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
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
    }

    #[test]
    fn test_numbers2() {
        let lhs_integer = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
            "234.2345",
        ];
        let rhs_integer = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
            "169.5333",
        ];
        for lhs in lhs_integer.iter() {
            for rhs in rhs_integer.iter() {
                for op in ["+", "-", "*", "/"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    fn test_numbers3() {
        let lhs_integer = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
            "234.2345",
        ];
        let rhs_integer = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
            "169.5333",
        ];
        for lhs in lhs_integer.iter() {
            for rhs in rhs_integer.iter() {
                for op in ["==", "!=", "<"] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
    }

    #[test]
    fn test_numbers4() {
        let lhs_integer = [
            "0",
            "5375",
            "690426",
            "24829482958347598570210950349530597028472983429873",
            "234.2345",
        ];
        let rhs_integer = [
            "17",
            "3454",
            "25084",
            "234234645",
            "2352354645657876868978696835652452546462456245646",
            "169.5333",
        ];
        for lhs in lhs_integer.iter() {
            for rhs in rhs_integer.iter() {
                for op in ["<=", ">", ">="] {
                    run_test(&format!("{} {} {}", lhs, op, rhs));
                    run_test(&format!("{} {} (-{})", lhs, op, rhs));
                    run_test(&format!("-{} {} {}", lhs, op, rhs));
                    run_test(&format!("-{} {} (-{})", lhs, op, rhs));
                }
            }
        }
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
        run_test(
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
        x = C.new
        x.ivar
        "###,
        );
        run_test(
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
        x = C.new
        x.ivar
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
    }

    #[test]
    #[ignore]
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
