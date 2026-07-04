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

#[test]
fn nested_assign() {
    run_tests(&[
        // Basic nested destructure.
        "(a, (b, c)) = [1, [2, 3]]; [a, b, c]",
        "((a, b), c) = [[1, 2], 3]; [a, b, c]",
        // Nested with a splat inside the group.
        "(x, (y, *z)) = [1, [2, 3, 4]]; [x, y, z]",
        "(a, (*b)) = [1, [2, 3]]; [a, b]",
        // Splat at the outer level combined with a nested group.
        "(a, *b, (c, d)) = [1, 2, 3, [4, 5]]; [a, b, c, d]",
        "(a, (b, c), *d) = [1, [2, 3], 4, 5]; [a, b, c, d]",
        // Deep nesting, multiple splats.
        "(a, (b, (c, d))) = [1, [2, [3, 4]]]; [a, b, c, d]",
        "(a, (b, *c, (d, e)), *f) = [1, [2, 3, 4, [5, 6]], 7, 8]; [a, b, c, d, e, f]",
        // Short / nil-fill RHS.
        "(a, (b, c)) = [1, 2]; [a, b, c]",
        "(a, (b, c)) = [1, nil]; [a, b, c]",
        // Return value of a nested multiple assignment is the RHS.
        "r = ((a, (b, c)) = [1, [2, 3]]); [r, a, b, c]",
        // Ivar / gvar targets inside a nested group.
        "$g = nil; (@x, (@y, $g)) = [1, [2, 3]]; [@x, @y, $g]",
        // Index / attr targets inside a nested group.
        "arr = [0, 0]; h = {}; (arr[0], (arr[1], h[:k])) = [1, [2, 3]]; [arr, h]",
        // Nested destructure in a block-parameter scope.
        "[1, [2, 3]].then { |(m, (n, o))| [m, n, o] }",
        // Nested destructure *assignment* inside a block body (exercises
        // `level_down`'s local binding + depth adjustment).
        "proc { (a, (b, c)) = [1, [2, 3]]; [a, b, c] }.call",
        "proc { (a, (b, *c)) = [1, [2, 3, 4]]; [a, b, c] }.call",
        // ...and captured by a nested block referencing an outer local.
        "x = 100; [0].each { proc { (a, (b, c)) = [1, [2, 3]]; x = a + b + c }.call }; x",
        // ...and inside a `for`-loop body.
        "r = nil; for i in [1] do (a, (b, c)) = [1, [2, 3]]; r = [a, b, c] end; r",
        // Splat-assign as a value in a push context (array element).
        "y = [(*a = 1)]; [y, a]",
        // `*a = rhs` used as a value: the expression is `rhs`, `a` is the
        // splatted form (Array distributes, scalar/nil wraps). Both the
        // store context (`x = ...`) and the method-return context.
        "x = (*a = [1, 2]); [x, a]",
        "x = (*a = 1); [x, a]",
        "x = (*a = nil); [x, a]",
        "def f; (*a = 1); end; f",
        "def g; (*a = [3, 4]); end; g",
    ]);
}
