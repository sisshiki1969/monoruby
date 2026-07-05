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

#[test]
fn multi_assign_arity_mismatch() {
    // A multiple assignment whose lhs has a rest target, or whose arities
    // differ, collects the rhs into an array and destructures it:
    // extras truncate, shortfalls nil-fill, and a rest slot soaks up the
    // middle. The expression evaluates to the whole rhs array.
    run_tests(&[
        // Too many rhs values: extras are dropped.
        "a, b = 1, 2, 3; [a, b]",
        "a, b, c = 1, 2, 3, 4, 5; [a, b, c]",
        // Too few: missing targets become nil.
        "a, b, c = 1, 2; [a, b, c]",
        "a, b, c = 1; [a, b, c]",
        // Rest target with equal / longer / shorter rhs.
        "a, *b = 1, 2; [a, b]",
        "a, *b = 1, 2, 3, 4; [a, b]",
        "a, *b = 1; [a, b]",
        "a, *b = 5; [a, b]",
        // Rest in the head / middle / with a trailing target.
        "*a, z = 1, 2, 3; [a, z]",
        "a, *b, c = 1, 2, 3, 4, 5; [a, b, c]",
        "a, *b, c = 1, 2; [a, b, c]",
        // Lone splat lhs collects everything.
        "*a = 1, 2, 3; a",
        // Anonymous rest / trailing splat discard the collected part.
        "*, z = 1, 2, 3; z",
        "a, * = 1, 2, 3; a",
        // Splats inside the rhs are flattened before distribution.
        "a, b, c = 1, *[2, 3]; [a, b, c]",
        "a, *b = 0, *[1, 2, 3]; [a, b]",
        "x, y, z = *[1, 2], 3; [x, y, z]",
        // Index / attr targets, mixed with a rest.
        "arr = [0, 0, 0]; arr[0], arr[1] = 1, 2, 3; arr",
        "h = {}; h[:a], *h[:b] = 1, 2, 3; [h[:a], h[:b]]",
        // Value of the assignment is the whole rhs array.
        "x = (a, b, c = 1, 2); [x, a, b, c]",
        "(a, b = 1, 2, 3)",
        "(a, *b = 1, 2)",
        "def f; a, *b = 1, 2, 3; end; f",
        // Equal arity, no rest: still a plain parallel assignment (swaps).
        "x, y = 5, 6; x, y = y, x; [x, y]",
    ]);
}

#[test]
fn index_assign_with_splat() {
    // `a[*idx] = v` and friends: the index list is expanded at runtime and
    // the whole thing lowers to a `[]=` call whose trailing arg is `v`.
    run_tests(&[
        // Single splat index.
        "a = []; a[*[0]] = 10; a",
        "a = []; x = [2]; a[*x] = 99; a",
        // Splat mixed with fixed indices (front / middle / back).
        r##"
        class Obj
          attr_reader :result
          def []=(*args); @result = args; end
        end
        b = Obj.new
        b[1, *[2, 3], 4] = 5
        b.result
        "##,
        // Multiple assignment, both targets are splat-indexed.
        "a = []; a[*[1]], a[*[2]] = 1, 2; a",
        // Nested / mixed with a plain-index target.
        "c = []; d = []; c[*[0]], d[0] = :a, :b; [c, d]",
        // The splat expression is a method call.
        "def idx; [2]; end; e = []; e[*idx] = :hit; e",
        // Value of an index-splat assignment is the RHS, not the `[]=` result.
        "d = []; (d[*[0]] = 7)",
        // Op-assign through a splat index.
        "a = [10, 20, 30]; a[*[1]] += 5; a",
        "h = Hash.new(0); k = [:x]; h[*k] += 1; h[*k] += 1; h[:x]",
    ]);
}

#[test]
fn multi_assign_evaluation_order() {
    // CRuby evaluates every target's receiver / index subexpression
    // left-to-right BEFORE the rhs — including inside nested destructure
    // groups. `$s` records the order so a wrong nested order (rhs before a
    // nested receiver) is caught.
    run_tests(&[
        // Flat accessor targets: receivers, then rhs.
        r#"$s = []; o = Object.new; def o.a=(v); end
           (($s << :a); o).a, (($s << :b); o).a = (($s << :c); 1), (($s << :d); 2); $s"#,
        // Nested accessor: the nested receiver is evaluated before the rhs.
        r#"$s = []; o = Object.new; def o.a=(v); end
           ((($s << :a); o).a, foo), bar = [(($s << :b); 1)]; $s"#,
        // Deeply nested accessors: full left-to-right, then the rhs value.
        r#"$s = []; o = Object.new
           def o.a=(v); end; def o.b=(v); end; def o.c=(v); end
           def o.d=(v); end; def o.e=(v); end; def o.f=(v); end
           (($s << :a); o).a,
             ((($s << :b); o).b,
             ((($s << :c); o).c, (($s << :d); o).d),
             (($s << :e); o).e),
           (($s << :f); o).f = (($s << :v); :v)
           $s"#,
        // Nested #[]=: receiver, index, then rhs.
        r#"$s = []; o = Object.new; def o.[]=(_, _); end
           ((($s << :a); o)[(($s << :b); 0)], foo), bar = [(($s << :c); 1)]; $s"#,
        // Deeply nested #[]=.
        r#"$s = []; o = Object.new; def o.[]=(_, _); end
           (($s << :ra); o)[(($s << :aa); 0)],
             ((($s << :rb); o)[(($s << :ab); 1)],
             ((($s << :rc); o)[(($s << :ac); 2)], (($s << :rd); o)[(($s << :ad); 3)]),
             (($s << :re); o)[(($s << :ae); 4)]),
           (($s << :rf); o)[(($s << :af); 5)] = (($s << :v); :v)
           $s"#,
    ]);
}

#[test]
fn multi_assign_nested_values() {
    // Correctness of nested destructuring (values, not just order).
    run_tests(&[
        "((a, b), c), d = [[1, 2], 3], 4; [a, b, c, d]",
        "(x, (y, z)), *w = [10, [20, 30]], 40, 50; [x, y, z, w]",
        "(a, *b), (c, d) = [1, 2, 3], [4, 5]; [a, b, c, d]",
        "arr = []; (arr[0], (arr[1], arr[2])), arr[3] = [1, [2, 3]], 4; arr",
    ]);
}

#[test]
fn for_loop_destructuring_index() {
    // `for` with a splat / post / nested / non-local index target
    // destructures each element, and the targets still leak to the
    // enclosing scope.
    run_tests(&[
        // Trailing comma / anonymous splat.
        "r = []; for i, in [[1, 2]]; r << i; end; r",
        "r = []; for i, * in [[1, 2, 3]]; r << i; end; r",
        // Rest in head / middle / tail.
        "r = []; for a, *b in [[1, 2, 3], [4, 5]]; r << [a, b]; end; r",
        "r = []; for *a, z in [[1, 2, 3]]; r << [a, z]; end; r",
        "r = []; for a, *b, c in [[1, 2, 3, 4]]; r << [a, b, c]; end; r",
        // Scalar elements (single-value multiple assignment).
        "r = []; for a, *b in [1, 2]; r << [a, b]; end; r",
        // Nested destructure target.
        "r = []; for a, (b, c) in [[1, [2, 3]]]; r << [a, b, c]; end; r",
        // Targets leak to the enclosing scope.
        "for a, *b in [[1, 2, 3]]; end; [a, b]",
        // Single non-local targets.
        "@x = nil; for @x in [1, 2, 3]; end; @x",
        "arr = [0, 0]; for arr[1] in [7, 8, 9]; end; arr",
        "r = []; for @y in [[1, 2]]; r << @y; end; r",
        // Nested for-loops with destructuring reuse the hidden variable.
        "r = []; for i, * in [[10, 20]]; for j, *k in [[1, 2, 3]]; r << [i, j, k]; end; end; r",
        // Plain (flat) destructure still works unchanged.
        "r = []; for a, b in [[1, 2], [3, 4]]; r << [a, b]; end; r",
    ]);
}
