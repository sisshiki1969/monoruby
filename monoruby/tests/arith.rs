extern crate monoruby;
use monoruby::tests::*;

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

    run_test_error(r##"2.56 < :Ruby"##);
    run_test_error(r##"150[:Ruby]"##);
    run_test_error(
        r##"1500000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000[:Ruby]"##,
    );
}

#[test]
fn test0_err() {
    for lhs in [
        "0",
        "690426",
        "24829482958347598570210950349530597028472983429873",
        "3+50i",
        "3.66+50i",
        "345778234627285787563857682746287423847246827462387+50i",
        "3+50.7i",
        "3.66+50.7i",
        "345778234627285787563857682746287423847246827462387+50.7i",
        "3+893457246287428472876876872342847268746283476287426i",
        "3.66+893457246287428472876876872342847268746283476287426i",
        "345778234627285787563857682746287423847246827462387+893457246287428472876876872342847268746283476287426i",
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
fn binop_integer() {
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
    run_test_error("3 + :aaa");
    run_test_error("3 - :aaa");
    run_test_error("3 * :aaa");
    run_test_error("3 / :aaa");
    run_test_error("3 % :aaa");
    run_test_error("3 & :aaa");
    run_test_error("3 | :aaa");
    run_test_error("3 ^ :aaa");
}

#[test]
fn binop_numeric() {
    let lhs = [
        "0",
        "0.0",
        "1.2e-13",
        "1.5e21",
        "690426",
        "690426.0",
        "24829482958347598570210950349530597028472983429873",
        "12.3+45.6i",
        "123+45.6i",
        "12.3+456i",
        "12.3+347782374687589587409204209428795359385387874687i",
        "345765834750980975394579384583756387493850539458394+45.6i",
        "123+456i",
        "123+347782374687589587409204209428795359385387874687i",
        "345765834750980975394579384583756387493850539458394+456i",
        "345765834750980975394579384583756387493850539458394+347782374687589587409204209428795359385387874687i",
    ];
    let rhs = [
        "25084",
        "234234645",
        "2352354645657876868978696835652452546462456245646",
        "1.2e-10",
        "1.9e24",
        "54.679+678.234i",
        "54679+54336i",
        "54679+678.234i",
        "54679+12988593475873426874687983435698234234882342i",
        "54.679+54336i",
        "54.679+12988593475873426874687983435698234234882342i",
        "84623499894518399348254679127328445692349+54336i",
        "84623499894518399348254679127328445692349+678.234i",
        "84623499894518399348254679127328445692349+12988593475873426874687983435698234234882342i",
    ];
    run_binop_tests(&lhs, &["+", "-", "*"], &rhs);
    run_binop_tests(&lhs[0..7], &["/"], &rhs[0..5]);
    //for i in 0..6 {
    //    run_binop_tests(&[lhs[7]], &["/"], &[rhs[i]]);
    //}
    run_test_error("3.1 + :aaa");
    run_test_error("3.1 - :aaa");
    run_test_error("3.1 * :aaa");
    run_test_error("3.1 / :aaa");
    run_test_error("3.1 % :aaa");
    run_test_error("3.1 & :aaa");
    run_test_error("3.1 | :aaa");
    run_test_error("3.1 ^ :aaa");
}

#[test]
fn cmp_numeric() {
    let lhs = [
        "0",
        "0.0",
        "17",
        "5375",
        "25084",
        "234234645",
        "24829482958347598570210950349530597028472983429873",
        "2352354645657876868978696835652452546462456245646",
        "234.2345",
        "690426.0",
        "234234645.0",
        "0.34e-17",
        "0.34e-18",
        "Float::NAN",
    ];
    let rhs = [":aaa", "nil", "false"];
    run_binop_tests(
        &lhs,
        &["==", "!=", "<", "<=", ">", ">=", "===", "<=>"],
        &lhs,
    );
    run_binop_tests2(&lhs, &["==", "!=", "===", "<=>"], &rhs);
    run_test_error("3 < :aaa");
    run_test_error("3 <= :aaa");
    run_test_error("3 > :aaa");
    run_test_error("3 >= :aaa");
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
    run_test("9223372036854775807"); // max number of 63bit signed int.
    run_test("9223372036854775808"); // max number of 63bit signed int.
    run_test("9223372036854775807 + 1");
    run_test("9223372036854775000 + 27387904");
    run_test("-(-9223372036854775808)");
    run_test("-4611686018427387904"); // min number of 63bit signed int.
    run_test("-4611686018427387904 - 1");
    run_test("-4611686018400000001 - 27387904");
    run_test("1 << 62");
    run_test("1 << 63");
    run_test("1 << 64");
    run_test("-1 << 62");
    run_test("-1 << 63");
    run_test("-1 << 64");
}

#[test]
fn test_shift() {
    for lhs in ["157"] {
        for rhs in ["0", "1", "54", "70"] {
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
            fib(20.0)
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
fn logical_assign_ops() {
    run_test("a = nil; a||=100; a");
    run_test("a = nil; a&&=100; a");
    run_test("a = 200; a&&=100; a");
    run_test("a = nil; b = a||=100; [a, b]");
    run_test("a ||= 100; a");
    run_test("b = a ||= 100; [a, b]");
    run_test("a &&= 100; a");
    run_test("a = 200; b = a &&= 100; [a, b]");
}

#[test]
fn test_undef() {
    run_test_once(
        r#"
        res = []
        class S
          def foo
            "foo"
          end
        end
        class C < S
          def self.undefine
            undef foo
          end
        end

        res << C.instance_methods(false)
        res << S.instance_methods(false)

        C.undefine

        res << C.instance_methods(false)
        res << S.instance_methods(false)
        res
        "#,
    );
    run_test_error(
        r##"
        def foo
          "foo"
        end
        undef foo
        foo
    "##,
    );
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
    run_test(
        r#"
        $res = []
        class S
          class C
            def f
              42
            end
          end
        end

        class C
          def f
            99
          end
        end

        obj = S::C.new
        $res << obj.f

        class S::C
          def f
            100
          end
        end

        $res << obj.f
        $res
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
fn test_yield_in_loop() {
    run_test_with_prelude(
        r#"
        m{}
        "#,
        r#"
        def m
            i = 0
            while i<30
              i += 1
              yield
            end
        end
        "#,
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
    run_test(r#"defined? redo"#);
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
    //run_test(r#"defined? [1,2].map{}.to_s"#);
    //run_test(r#"defined? [1,2].map{}.zxzxz"#);
    run_test(r#"defined? a[1]"#);
    run_test(r#"a = []; defined? a[1]"#);
    run_test(r#"defined? (a[1]=5)"#);
    run_test(r#"a = []; defined? (a[1]=5)"#);
    run_test(r#"defined?(1+(2+3))"#);
    run_test(r#"defined? 1+(2+3)"#);
    run_test_with_prelude(
        r#"
            [ C.new.f, S.new.f ]
        "#,
        r#"
            class S
              def f
              end
            end

            class C < S
              def f
                defined?(super)
              end
            end
    "#,
    );
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
fn redo() {
    run_test(
        r#"
        res = []
        flag = false
        for x in 0..100
          res << x
          if x % 7 == 6 && !flag
            flag = true
            redo
          end
        end
        res
        "#,
    );
    run_test(
        r#"
        res = []
        flag = false
        x = 0
        while x <= 100
          res << x
          if x % 7 == 6 && !flag
            flag = true
            redo
          end
          x += 1
        end
        res << x
        res
        "#,
    );
    run_test(
        r#"
        res = []
        flag = false
        100.times do |x|
          res << x
          if x % 7 == 6 && !flag
            flag = true
            redo
          end
          flag = false
        end
        res
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
