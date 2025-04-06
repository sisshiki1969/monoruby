extern crate monoruby;
use monoruby::tests::*;

#[test]
fn multi_assign() {
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
    run_test(
        r##"
        c = (a,b, = [1,2,3,4,5])
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        c = (a,b, = 100)
        [a,b,c]
        "##,
    );
}

#[test]
fn multi_assign_with_splat() {
    run_test(
        r##"
        a,*b,c = 100
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        a,*b,c = [11]
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        a,*b,c = [11,22]
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        a,*b,c = [11,22,33]
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        a,*b,c = [11,22,33,44]
        [a,b,c]
        "##,
    );
    run_test(
        r##"
        a,*b,c = [11,22,33,44,55]
        [a,b,c]
        "##,
    );
}

#[test]
fn multi_assign_with_splat2() {
    run_test(
        r##"
        x = (a,*b,c = 100)
        [a,b,c,x]
        "##,
    );
    run_test(
        r##"
        x = (a,*b,c = [11])
        [a,b,c,x]
        "##,
    );
    run_test(
        r##"
        x = (a,*b,c = [11,22])
        [a,b,c,x]
        "##,
    );
    run_test(
        r##"
        x = (a,*b,c = [11,22,33])
        [a,b,c,x]
        "##,
    );
    run_test(
        r##"
        x = (a,*b,c = [11,22,33,44])
        [a,b,c,x]
        "##,
    );
    run_test(
        r##"
        x = (a,*b,c = [11,22,33,44,55])
        [a,b,c,x]
        "##,
    );
}
