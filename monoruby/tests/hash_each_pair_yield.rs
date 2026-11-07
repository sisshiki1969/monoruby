extern crate monoruby;
use monoruby::tests::*;

// Hash#each yields a [k, v] pair; a literal block that auto-splats it gets
// key and value without the Array being built (the JIT yields through the
// expanded twin of `yield [k, v]`, `CallSiteInfo::yield_expanded`). Every
// block shape must bind exactly as it binds the pair.

#[test]
fn hash_each_block_shapes() {
    run_test_with_prelude(
        r##"
        t({a: 1, b: 2})
        "##,
        r##"
        def m(h, &b) = h.each(&b)
        def t(h)
          r = []
          h.each { |k, v| r << [k, v] }
          h.each { |kv| r << kv }
          h.each { |k, v = 9| r << [k, v] }
          h.each { |k, *v| r << [k, v] }
          h.each { |*a| r << a }
          h.each { |k, v, w| r << [k, v, w] }
          h.each { |(a, b), c| r << [a, b, c] }
          h.each_pair { |k, v| r << k }
          pr = proc { |k, v| r << [v, k] }; h.each(&pr)
          pr2 = proc { |k, *v| r << [v, k] }; h.each(&pr2)
          l1 = ->(kv) { r << kv }; h.each(&l1)
          begin; h.each(&->(k, v) { r << [k, v] }); rescue => e; r << e.class; end
          m(h) { |k, v| r << [:m, k, v] }
          r << h.each { |k, v| break [k, v] }
          h2 = h.dup
          h2.each { |k, v| h2.delete(:b); r << [k, v] }
          r << h.map { |k, v| "#{k}=#{v}" }
          r
        end
        "##,
    );
}

#[test]
fn hash_each_non_block_procs() {
    // Symbol and Method procs are not blocks: they keep the pair Array
    // (a Method proc is a lambda, so a two-parameter one raises).
    run_test_with_prelude(
        r##"
        t({a: 1, b: 2})
        "##,
        r##"
        def m1(kv) = kv
        def m2(k, v) = [k, v]
        def t(h)
          r = [h.each(&:to_s), h.map(&method(:m1))]
          begin; h.each(&method(:m2)); r << :ok; rescue ArgumentError => e; r << e.class; end
          r << h.each_with_object([]) { |(k, v), a| a << k }
        end
        "##,
    );
}

#[test]
fn recursion_guard_out_of_order_exit() {
    // A Fiber suspended inside an Array#== keeps its guard entry while
    // the main fiber starts another comparison and resumes it from
    // there: the inner walk's entry is then removed below the top of
    // the guard stack.
    run_test_once(
        r##"
        class E; def ==(o) = (Fiber.yield; true); end
        class R; def ==(o) = ($f.resume; true); end
        $f = Fiber.new { [E.new] == [1] }
        $f.resume
        r = [[R.new] == [2], [[1, 2], [3]] == [[1, 2], [3]]]
        a = [1]; a << a
        r << (a == a) << (a.hash == a.hash) << a.inspect
        r
        "##,
    );
}

#[test]
fn yield_array_literal_block_shapes() {
    // `yield [e1, .., en]` into each block shape, with the yield's value
    // used and discarded: a block that auto-splats the Array binds the
    // elements just as it binds the Array, whichever way the JIT yields.
    run_test_with_prelude(
        r##"
        t(3, 4)
        "##,
        r##"
        def y2(a, b) = yield [a, b]
        def y3(a, b) = yield [a, b, a + b]
        def y1(a) = yield [a]
        def ysp(a, b) = yield [a, *b]
        def y2x(a, b)
          yield [a, b]
          :done
        end
        def y2b(a, b, &blk) = yield [a, b]
        def t(a, b)
          r = []
          r << y2(a, b) { |x, y| x + y }
          r << y2(a, b) { |x| x }
          r << y2(a, b) { |x,| x }
          r << y2(a, b) { |*x| x }
          r << y2(a, b) { |x, *y| [x, y] }
          r << y2(a, b) { |x, y, z = 9| [x, y, z] }
          r << y2(a, b) { |x, y, z| [x, y, z] }
          r << y2(a, b) { |(p, q), y| [p, q, y] }
          r << y2(a, b) { |x, (p, q)| [x, p, q] }
          r << y2(a, b) { |x, y, k: 1| [x, y, k] }
          r << y2(a, b) { |x, y, &blk| [x, y, blk] }
          r << y2(a, b) { [_1, _2] }
          r << y2(a, b) { it }
          r << y3(a, b) { |x, y| [x, y] }
          r << y3(a, b) { |x, *y, z| [x, y, z] }
          r << y1(a) { |x, y| [x, y] }
          r << ysp(a, [b, 5]) { |x, y, z| [x, y, z] }
          r << y2x(a, b) { |x, y| r << x * y }
          r << y2b(a, b) { |x, y| [y, x] }
          r << y2(a, b, &->(x) { x })
          r << (y2(a, b, &->(x, y) { x }) rescue $!.class)
          r << y2(a, b, &proc { |x, y| y })
          r << y2(a, b) { |x, y| break x - y }
          r << y2(a, b) { |x, y| next x * y }
          r << y2(a, b) { |x, y| binding.local_variable_get(:y) }
          r << y2(a, b) { |x, y| Array.new(100) { x }.size + y }
          r
        end
        "##,
    );
}

#[test]
fn yield_array_literal_type_change() {
    // The block's specialization guards fail on a later element type and
    // deopt inside the yielded block; an exception raised there unwinds
    // through the yield.
    run_test_with_prelude(
        r##"
        r = []
        30.times { |i| r << t(i) }
        r << t(1.5) << t("s") << (t(nil) rescue $!.class)
        r
        "##,
        r##"
        def y2(a, b) = yield [a, b]
        def t(v) = y2(v, v) { |x, y| x + y }
        "##,
    );
}
