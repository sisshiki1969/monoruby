extern crate monoruby;
use monoruby::tests::*;

#[test]
fn multi_assign() {
    run_tests(&[
        "a, B = 7, 9.5; a + B",
        "@a, @b = 1, 2; [@a, @b]",
        "@a, @b = 1, 2; @a, @b = @b, @a; [@a, @b]",
        "@a = []; @a[0], @a[1] = 1, 2; @a[0], @a[1] = @a[1], @a[0]; @a",
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
        r##"
        d = (a,b,c = [1,2])
        [a,b,c,d]
        "##,
        r##"
        d = (a,b,c = [1,2,3])
        [a,b,c,d]
        "##,
        r##"
        d = (a,b,c = [1,2,3,4])
        [a,b,c,d]
        "##,
        r##"
        d = (a,b,c = 100)
        [a,b,c,d]
        "##,
        r##"
        c = (a,b, = [1,2,3,4,5])
        [a,b,c]
        "##,
        r##"
        c = (a,b, = 100)
        [a,b,c]
        "##,
        r##"
        a,*b,c = 100
        [a,b,c]
        "##,
        r##"
        a,*b,c = [11]
        [a,b,c]
        "##,
        r##"
        a,*b,c = [11,22]
        [a,b,c]
        "##,
        r##"
        a,*b,c = [11,22,33]
        [a,b,c]
        "##,
        r##"
        a,*b,c = [11,22,33,44]
        [a,b,c]
        "##,
        r##"
        a,*b,c = [11,22,33,44,55]
        [a,b,c]
        "##,
        r##"
        a,*b,c,d,e,f,g = [11,22,33,44,55]
        [a,b,c,d,e,f,g]
        "##,
        r##"
        a,b,c,d,e,*f,g = [11,22,33,44]
        [a,b,c,d,e,f,g]
        "##,
        r##"
        *a = 100
        *b,c,d = 200
        [a,b,c,d]
        "##,
        r##"
        x = (a,*b,c = 100)
        [a,b,c,x]
        "##,
        r##"
        x = (a,*b,c = [11])
        [a,b,c,x]
        "##,
        r##"
        x = (a,*b,c = [11,22])
        [a,b,c,x]
        "##,
        r##"
        x = (a,*b,c = [11,22,33])
        [a,b,c,x]
        "##,
        r##"
        x = (a,*b,c = [11,22,33,44])
        [a,b,c,x]
        "##,
        r##"
        x = (a,*b,c = [11,22,33,44,55])
        [a,b,c,x]
        "##,
    ]);
}
