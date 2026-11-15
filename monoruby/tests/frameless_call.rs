extern crate monoruby;
use monoruby::tests::*;

// Frameless specialized calls (`jitgen/compile/frameless_call.rs`): a small
// loop-free callee runs without a control frame, and every side exit in it
// hands the whole call back to the interpreter.

#[test]
fn frameless_branchy_bodies() {
    run_test(
        r#"
        def max(a, b) = a > b ? a : b
        def clamp(x, lo, hi)
          if x < lo
            lo
          elsif x > hi
            hi
          else
            x
          end
        end
        def sign(x) = x == 0 ? 0 : (x < 0 ? -1 : 1)
        res = []
        30.times do |i|
          res << max(i, 10) << clamp(i, 5, 20) << sign(i - 15)
        end
        res
        "#,
    );
}

#[test]
fn frameless_float_bodies() {
    run_test(
        r#"
        def sq(x) = x * x
        def dot(ax, ay, bx, by) = ax * bx + ay * by
        def lerp(a, b, t) = t < 0.5 ? a + (b - a) * t : b - (b - a) * (1.0 - t)
        s = 0.0
        30.times do |i|
          f = i * 0.25
          s += sq(f) + dot(f, 1.5, 2.0, f) + lerp(1.0, 3.0, f / 8.0)
        end
        s
        "#,
    );
}

#[test]
fn frameless_redo_on_type_change() {
    // The guards were compiled for Integer: a Float, a String and an Array
    // each take a redo, and the interpreter performs the call.
    run_test(
        r#"
        def add(a, b) = a + b
        def pick(a, b) = a < b ? b : a
        res = []
        40.times do |i|
          x = case i % 4
              when 0 then i
              when 1 then i.to_f
              when 2 then i.to_s
              else [i]
              end
          y = x.is_a?(Integer) ? 1 : x.is_a?(Float) ? 0.5 : x
          res << add(x, y)
          res << pick(i, 20) if i.even?
        end
        res
        "#,
    );
}

#[test]
fn frameless_nested() {
    run_test(
        r#"
        def inner(x, y) = x > y ? x - y : y - x
        def outer(x) = inner(x, 10) * 2 + inner(x, 3)
        def top(x) = outer(x) + outer(x + 1)
        res = []
        30.times { |i| res << top(i) }
        30.times { |i| res << top(i.to_f) } # redo through two levels
        res
        "#,
    );
}

#[test]
fn frameless_raise_reports_the_callee() {
    // The division guard hands the call back, so ZeroDivisionError is
    // raised from a real frame for `div`.
    run_test(
        r#"
        def div(a, b) = a / b
        res = []
        30.times do |i|
          begin
            res << div(100, 5 - i % 6)
          rescue ZeroDivisionError => e
            res << e.message << e.backtrace_locations.map(&:label).include?("Object#div")
          end
        end
        res
        "#,
    );
}

#[test]
fn frameless_ivar_bodies() {
    // Stores are side effects: a guard after one cannot redo, so these
    // bodies take the ordinary specialized call, or keep every guard ahead
    // of the store.
    run_test(
        r#"
        class P
          def initialize(x) = (@x = x; @n = 0)
          def x = @x
          def set(v) = (@x = v; @n = v + 1)
          def bump(d) = d > 0 ? @n += d : @n
          def n = @n
        end
        res = []
        p = P.new(0)
        40.times do |i|
          p.set(i % 7 == 0 ? i.to_f : i)
          p.bump(i % 3 - 1)
          res << p.x << p.n
        end
        q = P.new(1).freeze
        30.times do |i|
          begin
            q.bump(1)
          rescue => e
            res << e.class
          end
          res << q.bump(0)
        end
        res
        "#,
    );
}

#[test]
fn frameless_frame_readers() {
    // Bodies that read their own frame header must see a well-formed one.
    run_test(
        r#"
        def bg(x) = block_given? ? x : -x
        def name(x) = x > 0 ? __method__ : :none
        def m(s) = s =~ /b/ ? $~[0] : nil
        res = []
        30.times do |i|
          res << bg(i) << name(i - 10) << m(i.even? ? "abc" : "xyz")
        end
        res
        "#,
    );
}

#[test]
fn frameless_recursion() {
    run_test(
        r#"
        def fib(n) = n < 2 ? n : fib(n - 1) + fib(n - 2)
        def fact(n) = n <= 1 ? 1 : n * fact(n - 1)
        res = []
        25.times { |i| res << fib(i % 15) << fact(i) }
        res
        "#,
    );
}

#[test]
fn frameless_redefinition() {
    // The callee, and a method the callee calls, redefined after the
    // frameless call was compiled.
    run_test(
        r#"
        class V
          def initialize(v) = @v = v
          def v = @v
          def >(o) = @v > o
        end
        def f(x) = x > 5 ? x.v * 2 : x.v
        def g(x) = x * 3
        res = []
        30.times { |i| res << f(V.new(i)) << g(i) }
        def g(x) = x - 1
        30.times { |i| res << f(V.new(i)) << g(i) }
        class V
          def >(o) = @v > o + 10
        end
        30.times { |i| res << f(V.new(i)) << g(i) }
        res
        "#,
    );
}

#[test]
fn frameless_callee_redefined_inside_loop() {
    // The call is compiled in a loop that is already running when the
    // callee is redefined under it.
    run_test_once(
        r#"
        def g(x) = x > 100 ? x - 100 : x + 1
        res = []
        i = 0
        while i < 300
          res << g(i)
          if i == 150
            def g(x) = x * 2
          end
          i += 1
        end
        res
        "#,
    );
}

#[test]
fn frameless_caller_keeps_floats_across_the_window() {
    // The caller holds floats in registers across the inline call, and
    // enough of them that some are spilled: the spill region sits below
    // the callee's window and both survive the call.
    run_test(
        r#"
        def half(x) = x * 0.5
        def pick(a, b) = a > b ? a : b
        def run
          a = 1.5; b = 2.5; c = 3.5; d = 4.5; e = 5.5; f = 6.5; g = 7.5
          s = 0.0
          i = 0
          while i < 40
            s += half(a) + half(b) + half(c) + half(d) + half(e) + half(f) + half(g)
            a += 0.25; b += 0.5; c += 0.75; d += 1.0; e += 1.25; f += 1.5; g += 1.75
            s += pick(a, g) - pick(b, f)
            i += 1
          end
          [s, a, b, c, d, e, f, g]
        end
        res = []
        30.times { res << run }
        res
        "#,
    );
}

#[test]
fn frameless_deep_nesting_and_wide_callees() {
    // Three levels of inline callees, the innermost with many locals and
    // temporaries, so the windows nest and each one is sized by its body.
    run_test(
        r#"
        def wide(a, b, c, d)
          t1 = a + b; t2 = c + d; t3 = t1 * t2; t4 = t3 - a
          t5 = t4 + b; t6 = t5 * 2; t7 = t6 - c; t8 = t7 + d
          t9 = t8 > 100 ? t8 - 100 : t8
          t9 + t1 + t2 + t3
        end
        def mid(x, y) = wide(x, y, x + 1, y + 1) + wide(y, x, 2, 3)
        def top(x) = mid(x, x + 2) * 2 + mid(1, x)
        res = []
        40.times { |i| res << top(i) }
        res
        "#,
    );
}

#[test]
fn frameless_inside_specialized_block() {
    // The caller is a block compiled into the method it is passed to, so
    // its outer-variable accesses walk a frame chain that has to account
    // for the window inside it.
    run_test(
        r#"
        def sq(x) = x * x
        def add(a, b) = a + b
        def run(arr)
          s = 0
          t = 0.0
          arr.each { |x| s = add(s, sq(x)); t += sq(x * 0.5) }
          [s, t]
        end
        res = []
        arr = (1..20).to_a
        30.times { res << run(arr) }
        res
        "#,
    );
}

#[test]
fn frameless_window_in_toplevel_loop() {
    // A loop compiled on its own (loop JIT) reserves the window too.
    run_test_once(
        r#"
        def f(a, b) = a * 3 + b
        def g(x) = x > 50 ? x - 50 : x
        res = []
        i = 0
        x = 0.5
        while i < 400
          res << f(i, g(i)) if i % 7 == 0
          x += 0.25
          i += 1
        end
        res << x
        res
        "#,
    );
}

#[test]
fn frameless_callee_spills_below_the_callers_spills() {
    // The shape of ruby-bench's blurhash: a block that keeps floats of
    // its own and of the enclosing method live across an inline call
    // whose body spills too (under `stress-spill-pool` every third float
    // does). The callee's last spill slot is the bottom word of its
    // window, which once sat on top of the caller's first spill slot and
    // clobbered `basis` — the window must be the callee's whole local
    // area, so the caller's spill region starts below it.
    run_test_once(
        r#"
        def srgb(value)
          v = value.to_f / 255
          if v <= 0.04045
            v / 12.92
          else
            ((v + 0.055) / 1.055) ** 2.4
          end
        end
        def mul(w, h, rgb)
          r = 0.0
          h.times do |y|
            y_coef = Math.cos(Math::PI * y / h)
            w.times do |x|
              basis = Math.cos(Math::PI * x / w) * y_coef
              r += basis * srgb(rgb[x + y * w])
            end
          end
          r
        end
        rgb = (0...(30 * 30)).map { |i| (i * 37) % 256 }
        mul(30, 30, rgb)
        "#,
    );
}

#[test]
fn frameless_entry_extends_the_ivar_table() {
    // A frameless callee enters with `self` in rdi (`note_rdi_holds`), and
    // its `Preparation` may have to grow the receiver's heap ivar table on
    // the way: that cold path calls out, and must put `self` back in rdi
    // before the body's first heap ivar load addresses through it.
    run_test(
        r#"
        class C
          def initialize(full)
            if full
              40.times { |i| instance_variable_set(:"@v#{i}", i) }
              @h = 7
            end
          end
          def h = @h
          def g = @v30
        end
        def f(o) = o.h
        def f2(o) = o.g
        res = []
        o = C.new(true)
        30.times { res << f(o) << f2(o) }
        30.times { res << f(C.new(false)) << f2(C.new(false)) }
        res
        "#,
    );
}

// Materializing exits (`LSideExitKind::Materialize`): a side exit after
// the callee's first side effect cannot redo the call, so it makes the
// window a real frame and resumes the callee in the interpreter.

#[test]
fn frameless_materialize_after_store() {
    // `x + 1` overflows at one iteration: the deopt comes after the `@n`
    // store, the callee finishes in the interpreter, and its result
    // reaches the caller through the continuation stub. `@n` moves once
    // per call.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @k = 0)
          attr_reader :n, :k
          def acc(x) = (@n += 1; @k = x + 1)
          def mul(x, y) = (@n += 1; @k = x * y; @k - 1)
        end
        c = C.new
        res = []
        80.times do |i|
          x = i == 70 ? 2**62 : i
          res << c.acc(x) << c.mul(x, 3)
          GC.start if i == 71
        end
        res << c.n << c.k
        "#,
    );
}

#[test]
fn frameless_materialize_error_after_store() {
    // `Array#[]=` with an out-of-range negative index raises from the slow
    // path: the callee's frame exists by then, so the error names it and
    // the store before it stays done.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @a = [0, 1, 2, 3, 4, 5, 6, 7])
          attr_reader :n, :a
          def put(i, v) = (@n += 1; @a[i] = v; @n)
        end
        c = C.new
        res = []
        80.times do |i|
          begin
            res << c.put(i == 75 ? -100 : i % 8, i)
          rescue IndexError => e
            res << [e.class, e.backtrace_locations.map(&:label).include?("C#put"), c.n]
          end
        end
        res << c.n << c.a
        "#,
    );
}

#[test]
fn frameless_materialize_nested() {
    // The inner callee's overflow deopt materializes both inline levels:
    // the inner frame resumes in the interpreter, returns into the outer
    // frame's continuation, which finishes `* 2` interpreted and returns
    // into the framed caller.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @m = 0)
          attr_reader :n, :m
          def inner(x) = (@m += 1; x + 1)
          def outer(x) = (@n += 1; inner(x) * 2)
        end
        c = C.new
        res = []
        80.times do |i|
          x = i == 70 ? 2**62 : i
          res << c.outer(x)
        end
        res << c.n << c.m
        "#,
    );
}

#[test]
fn frameless_materialize_with_floats() {
    // The callee's float parameters arrive in registers and the caller
    // keeps floats of its own across the window: both are boxed into their
    // slots by the exit, and the caller's come back for the interpreter.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @k = 0.0; @j = 0)
          attr_reader :n, :k, :j
          def fl(a, b, i) = (@n += 1; @k = a * b; @j = i + 1; @k)
        end
        c = C.new
        res = []
        s = 1.5
        t = 0.25
        80.times do |i|
          f = i * 0.5
          s += c.fl(f, 2.0, i == 70 ? 2**62 : i) + t
          t *= 1.01
          res << s
        end
        res << c.n << c.k << c.j
        "#,
    );
}

#[test]
fn frameless_materialize_on_float_guard() {
    // The exit is the Float guard of a float load (`load_fpr`), taken
    // after the counter store, with the locals after it still unwritten:
    // the materialized frame must hold `nil` in them for the interpreter.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @t = 1.5)
          attr_reader :n
          def t=(v); @t = v; end
          def g(x) = (@n += 1; y = x * @t; z = y + 1.0; w = z * 2; w)
        end
        c = C.new
        res = []
        80.times do |i|
          c.t = 3 if i == 70
          res << c.g(i.to_f)
        end
        res << c.n
        "#,
    );
}

#[test]
fn frameless_materialize_in_toplevel_loop() {
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @k = 0)
          attr_reader :n, :k
          def acc(x) = (@n += 1; @k = x + 1)
        end
        c = C.new
        res = []
        i = 0
        while i < 80
          x = i == 70 ? 2**62 : i
          res << c.acc(x)
          i += 1
        end
        res << c.n << c.k
        "#,
    );
}

#[test]
fn frameless_materialize_class_new() {
    // The `initialize` twin of `P.new(x)` (`inline_class_new`) runs
    // frameless right after the allocation: its exit after the first
    // store resumes the constructor in the interpreter, and `new` still
    // answers the object, with both stores done.
    run_test(
        r#"
        class P
          def initialize(x) = (@a = x; @b = x + 1)
          attr_reader :a, :b
        end
        res = []
        80.times do |i|
          p = P.new(i == 70 ? 2**62 : i)
          res << p.a << p.b
        end
        res
        "#,
    );
}

#[test]
fn frameless_materialize_recompile_exit() {
    // A class guard that misses after the store takes the recompile
    // flavour of the exit: the site heals to the polymorphic lowering, and
    // every call in between completes in the interpreter.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @t = 5)
          attr_reader :n
          def t=(v); @t = v; end
          def cmp(x) = (@n += 1; x > @t ? 1 : 0)
        end
        c = C.new
        res = []
        80.times do |i|
          c.t = (i % 3 == 0 ? 5.5 : 5) if i >= 70
          res << c.cmp(i)
        end
        res << c.n
        "#,
    );
}

#[test]
fn frameless_local_objects() {
    // An Array or Range literal assigned to a local of a frameless callee
    // is elided like a framed callee's (`doc/escape_analysis.md` §10): its
    // elements live in the callee's scratch window inside the caller's
    // frame. A type miss after the body's first side effect takes a
    // materializing exit, which builds the object from the window into
    // the frame it makes; an error exit does the same; a returned local
    // is copied out of the window; a Range local answers `include?`
    // from its endpoints.
    run_test(
        r#"
        class C
          attr_reader :log
          def initialize; @log = []; end
          def m1(a, b)
            x = [a, b]
            @log << a
            x.max
          end
          def m2(a, b)
            x = [a, b]
            @log << b
            x.max
          end
          def m3(a, b)
            x = [a, b]
            x
          end
          def m4(lo, hi, v)
            r = (lo..hi)
            @log << v
            r.include?(v)
          end
          def m5(a, b)
            x = [a, b]
            y = x.size
            x.first + y
          end
          def m6(a, b)
            x = [a, b]
            x.size + x.max
          end
        end
        c = C.new
        res = []
        60.times do |i|
          a = i < 50 ? i : (i.even? ? i.to_f : "s#{i}")
          res << (c.m1(a, 3) rescue $!.class)
          res << (c.m2(3, a) rescue $!.class)
          res << c.m3(i, a)
          res << (c.m4(i, i + 2, a) rescue $!.class)
          res << (c.m5(a, 1) rescue $!.class)
          res << (c.m6(a, 2) rescue $!.class)
        end
        res << c.log.size << c.log.last(5)
        res
        "#,
    );
}


// ---- `yield` ----------------------------------------------------------------
//
// A specialized `yield` runs a block of the same shape frameless too
// (`frameless_call::yield_eligible`): the block's `self` and its home's LFP
// go into the window, and a materializing exit keeps the latter as the
// frame's outer.

#[test]
fn frameless_yield_ivar_counter() {
    // `100.times { @a += 1 }`: loads, stores and arithmetic on `self`, in a
    // window of the yielding frame. Once from a method body, once from a
    // loop at the top level.
    run_test(
        r#"
        def run
          @b = 0
          200.times { @b += 2 }
          @b
        end
        @a = 0
        i = 0
        while i < 30
          100.times { @a += 1 }
          i += 1
        end
        r = []
        30.times { r << run }
        r << @a
        "#,
    );
}

#[test]
fn frameless_yield_parameters() {
    // A block drops the arguments it has no parameter for and binds `nil`
    // to the ones it is not given.
    run_test(
        r#"
        def each3; i = 0; while i < 3; yield i, i * 2; i += 1; end; end
        def each0; i = 0; while i < 3; yield; i += 1; end; end
        class C
          def initialize; @s = 0; @log = []; end
          def go
            40.times { |i| @s += i }
            each3 { |x| @s += x }
            each0 { |x| @log << x }
            5.upto(9) { |i| @s = @s * 2 + i }
            [@s, @log]
          end
        end
        r = nil
        30.times { r = C.new.go }
        r
        "#,
    );
}

#[test]
fn frameless_yield_home_two_frames_up() {
    // Through a `(...)` forwarder the block's home is two specialized
    // frames above the yield: `YieldOuterLfp` hops that far.
    run_test(
        r#"
        def each_n(n); i = 0; while i < n; yield i; i += 1; end; end
        def fwd(...) = each_n(...)
        def two
          @t = 0
          fwd(30) { |i| @t += i }
          @t
        end
        r = []
        30.times { r << two }
        r
        "#,
    );
}

#[test]
fn frameless_yield_raise_reports_the_block() {
    // The division guard materializes the window: ZeroDivisionError is
    // raised from a real frame of the block.
    run_test(
        r#"
        def div_all
          @acc = 0
          30.times { |i| @acc += 100 / (5 - i % 6) }
          @acc
        rescue ZeroDivisionError => e
          [e.message, e.backtrace_locations.map(&:label).include?("block in Object#div_all")]
        end
        r = []
        30.times { r << div_all }
        r
        "#,
    );
}

#[test]
fn frameless_yield_redo_on_type_change() {
    // The guards were compiled for Integer: a Float and a String take a
    // redo, and the interpreter performs the yield.
    run_test(
        r#"
        def sum(arr)
          @sum = 0
          arr.each { |x| @sum += x }
          @sum
        end
        r = []
        30.times { r << sum([1, 2, 3]) }
        r << sum([1, 2.5, 3])
        r << (sum(["a", "b"]) rescue $!.class)
        r << sum([2**62, 2**62])
        r
        "#,
    );
}

#[test]
fn frameless_yield_nested_frameless_call() {
    // A frameless method call inside a frameless block: two inline
    // levels, the block's the outer one.
    run_test(
        r#"
        def sq(x) = x * x
        def nested
          @n = 0
          30.times { |i| @n += sq(i) }
          @n
        end
        r = []
        30.times { r << nested }
        r
        "#,
    );
}

#[test]
fn frameless_yield_float_body() {
    run_test(
        r#"
        def flt(k)
          @f = 0.0
          50.times { |i| @f += i * k }
          @f
        end
        r = []
        30.times { |i| r << flt(0.5) << flt(i) }
        r
        "#,
    );
}

#[test]
fn frameless_yield_block_reaching_its_home_stays_framed() {
    // Outer locals, `break`, `next` and a non-local `return` reach the
    // block's home: these take the framed yield.
    run_test(
        r#"
        def ctl
          t = 0
          r = 50.times { |i| next if i.odd?; break i if i > 20; t += i }
          [t, r]
        end
        def early
          @e = 0
          100.times { |i| @e += i; return @e if i == 10 }
        end
        r = []
        30.times { r << ctl << early }
        r
        "#,
    );
}

#[test]
fn frameless_keyword_bodies() {
    // Literal keywords bind the callee's keyword slots in the window: given
    // ones from the call site (constants not even stored), missing optional
    // ones `None`, so the default prologue is a static branch. Required,
    // optional, reordered, mixed with positionals, defaults that read an
    // earlier parameter or build an object.
    run_test(
        r#"
        def add(left:, right:) = left + right
        def opt(x:, y: 10) = x + y
        def dflt(x: 1, y: [x]) = y
        def mixed(p, x:, y: p * 2) = p + x + y
        def len(x:) = x.size
        def fl(x: 1.5, y:) = x * y
        res = []
        40.times do |i|
          res << add(left: 1, right: i) << add(right: i, left: i * 2)
          res << opt(x: i) << opt(x: i, y: 2) << opt(y: 3, x: i)
          res << dflt << dflt(x: i) << dflt(y: i)
          res << mixed(i, x: 1) << mixed(i, y: 5, x: 1)
          res << len(x: [1, 2, i]) << len(x: "ab" * (i % 3))
          res << fl(y: i.to_f) << fl(x: 2, y: 2.5) << fl(x: i * 0.5, y: i)
        end
        res
        "#,
    );
}

#[test]
fn frameless_keyword_redo_and_errors() {
    // A keyword argument whose type changes takes the redo; a raise from
    // the callee reports its frame; a missing or unknown keyword is never
    // compiled frameless (the generic path raises).
    run_test(
        r#"
        def add(left:, right:) = left + right
        def div(a:, b: 3) = a / b
        res = []
        60.times do |i|
          x = case i % 4
              when 0 then i
              when 1 then i * 0.5
              when 2 then "s#{i}"
              else [i]
              end
          res << add(left: x, right: x)
          begin
            res << div(a: 100, b: i % 5) << div(a: i)
          rescue ZeroDivisionError => e
            res << e.backtrace_locations.map(&:label).include?("Object#div")
          end
        end
        begin; add(left: 1); rescue ArgumentError => e; res << e.message; end
        begin; add(left: 1, right: 2, up: 3); rescue ArgumentError => e; res << e.message; end
        res
        "#,
    );
}

#[test]
fn frameless_keyword_materialize() {
    // The deopt comes after the ivar store (the `+` overflows once), so the
    // window becomes a real frame — keyword slots included, a missing `y`
    // already holding its default — and the callee finishes in the
    // interpreter. Frame readers see a well-formed header.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @k = 0)
          attr_reader :n, :k
          def acc(x:) = (@n += 1; @k = x + 1)
          def acc2(x:, y: 1) = (@n += 1; @k = x + y)
          def name(x:) = x > 0 ? __method__ : block_given?
        end
        c = C.new
        res = []
        80.times do |i|
          x = i == 70 ? 2**62 : i
          res << c.acc(x: x) << c.acc2(x: x) << c.acc2(y: 2, x: x)
          res << c.name(x: i - 40)
          GC.start if i == 71
        end
        res << c.n << c.k
        "#,
    );
}

#[test]
fn frameless_chain_past_the_framed_depth_limit() {
    // A five-deep chain of frameless callees, entered from a block nested
    // in another block: the chain is longer than the framed specialization
    // budget, and the enclosing blocks spend levels of it too. The
    // innermost callee sees a Float (redo), an overflowing `+` after an
    // ivar store (materialize) and a raise (error exit) at full depth.
    run_test(
        r#"
        class C
          def initialize = (@k = 3; @n = 0)
          attr_reader :n
          def a(x) = b(x) + 1
          def b(x) = c(x) * 2
          def c(x) = d(x) - @k
          def d(x) = e(x) + 1
          def e(x)
            raise ArgumentError, "neg" if x < 0
            @n += 1
            x + 1
          end
          def run(arr)
            s = []
            arr.each do |v|
              2.times do |j|
                begin
                  s << a(v + j)
                rescue ArgumentError => err
                  s << [err.message, err.backtrace_locations.map(&:label).include?("C#e")]
                end
              end
            end
            s
          end
        end
        c = C.new
        res = []
        arr = (0..12).to_a
        30.times do |i|
          res << c.run(arr)
          res << c.run([1.5, 2**62 - 2, -5, 4]) if i % 10 == 9
        end
        res << c.n
        "#,
    );
}

#[test]
fn frameless_recursion_inside_a_chain() {
    // Mutual recursion: `odd?` is already on the specialization stack when
    // `even?` calls it again, so that call stays a real call and the chain
    // above it stays frameless.
    run_test(
        r#"
        def ev(n) = n == 0 ? true : od(n - 1)
        def od(n) = n == 0 ? false : ev(n - 1)
        def wrap(n) = ev(n) ? 1 : 0
        def top(n) = wrap(n) + wrap(n + 1)
        res = []
        40.times { |i| res << top(i % 9) }
        res
        "#,
    );
}

#[test]
fn frameless_range_include_argument_kinds() {
    // `(0...@h).include?(y)` on an elided Range inside a frameless callee:
    // Integer arguments take the inline compare, the rest (Float, nil,
    // String, Bignum) go through the generic cover check.
    run_test(
        r#"
        class R
          def initialize = (@h = 10)
          def chk(y) = (0...@h).include?(y)
          def chk2(y) = (0..@h).include?(y) ? 1 : 2
          def run(a) = chk(a)
        end
        r = R.new
        res = []
        30.times { |i| res << r.run(i % 15) << r.chk2(i % 13) }
        res << [r.run(3.5), r.run(10.0), r.run(nil), r.run("a"), r.chk2(10), r.chk2(11), r.run(-1), r.run(2**70)]
        30.times { |i| res << r.run(i % 15) }
        res
        "#,
    );
}

#[test]
fn range_include_answers_in_the_branch_flags() {
    // `include?` on an elided Range consumed only by the branch leaves its
    // answer in the flags: both endpoints (a failed start compare must read
    // false for `<` and `<=`), the start alone (`(1..)`), the end alone
    // (`(..h)`), and the `raise unless` guard shape.
    run_test(
        r#"
        def a(x, w) = (0...w).include?(x) ? 1 : 0
        def b(x, h) = (2..h).include?(x) ? 1 : 0
        def c(x) = (1..).include?(x) ? 1 : 0
        def d(x, h) = (..h).include?(x) ? 1 : 0
        def e(x, w)
          raise "out" unless (0...w).include?(x)
          x
        end
        res = []
        40.times do |i|
          x = i % 11 - 3
          res << a(x, 5) << b(x, 6) << c(x) << d(x, 4)
          res << (begin; e(x, 5); rescue => err; err.message; end)
        end
        res
        "#,
    );
}

#[test]
fn frameless_integer_pow_exits() {
    // `a ** b` in a frameless callee: an overflow returns a Bignum, a
    // negative exponent a Rational, and `0 ** -1` raises ZeroDivisionError,
    // after an ivar store that has to stay done.
    run_test(
        r#"
        class P
          def initialize = (@n = 0)
          attr_reader :n
          def pw(a, b) = (@n += 1; a ** b)
          def run(a, b) = pw(a, b) + 0
        end
        p = P.new
        res = []
        40.times do |i|
          b = case i
              when 30 then 70
              when 31 then -2
              else i % 7
              end
          a = i == 32 ? 0 : 2
          b = -1 if i == 32
          begin
            res << p.run(a, b)
          rescue ZeroDivisionError => e
            res << [e.class, p.n]
          end
        end
        res << p.n
        "#,
    );
}

#[test]
fn frameless_yield_outer_locals() {
    // Blocks that read and write their home's locals run frameless: one
    // level out (`each`), two levels out through a framed block in
    // between, with Integer, Float and object values, with block
    // parameters and with a block-local.
    run_test(
        r#"
        def sum(a) = (s = 0; a.each { |x| s += x }; s)
        def fsum(a) = (s = 0.0; c = 0; a.each { |x| s += x * 0.5; c += 1 }; [s, c])
        def last(a) = (l = nil; a.each { |x| l = x }; l)
        def odd(a) = (r = []; a.each { |x| r << x if x.odd? }; r)
        def grid(m)
          s = 0
          m.each { |row| t = 0; row.each { |v| t += v; s += v }; s -= t / 2 }
          s
        end
        def wi(a) = (s = 0; a.each_with_index { |x, i| s += x * i }; s)
        def mp(a) = (k = 1; a.map { |x| k *= 3; x + k }.sum + k)
        def tm(n) = (s = 0; n.times { |i| s += i if i.even? }; s)
        res = []
        a = (1..20).to_a
        m = [[1, 2], [3, 4, 5], [6]]
        30.times do
          res << sum(a) << fsum(a) << last(a) << odd(a) << grid(m) << wi(a) << mp(a.first(5)) << tm(30)
        end
        res
        "#,
    );
}

#[test]
fn frameless_hash_index_pure_helper() {
    // `@h[k]` in a frameless callee calls the pure `Hash#[]` helper. It
    // answers plain lookups, misses with a default value and identity
    // hashes itself, and declines a miss on a default-proc Hash and a key
    // whose `#hash` is user-defined: the body deopts (redo before the ivar
    // store, materialize after it) and the site recompiles to the
    // ordinary helper.
    run_test(
        r##"
        class K
          def initialize(v) = (@v = v)
          attr_reader :v
          def hash = @v.hash
          def eql?(o) = o.is_a?(K) && o.v == @v
          def to_s = "K#{@v}"
          def inspect = to_s
        end
        class C
          def initialize(h) = (@h = h; @n = 0)
          attr_reader :n
          attr_writer :h
          def get(k) = (@n += 1; @h[k])
          def peek(k) = @h[k]
          def run(k) = get(k)
          def run2(k) = peek(k)
        end
        key = "x"
        plain = {a: 1, "s" => 2, 3 => 4, nil => 5}
        dflt = Hash.new(:dv); dflt[:a] = 6
        prc = Hash.new { |h, k| h[k] = "#{k}!" }
        ident = {}.compare_by_identity; ident[key] = 9
        obj = {K.new(1) => :k1, a: 7}
        keys = [:a, "s", 3, nil, :zz, key, K.new(1), 2.5]
        res = []
        [plain, dflt, prc, ident, obj].each do |h|
          c = C.new(h)
          40.times { |i| res << c.run(keys[i % 8]) << c.run2(keys[(i + 3) % 8]) }
          res << c.n
        end
        c = C.new(plain)
        60.times do |i|
          c.h = prc if i == 30
          res << c.run(keys[i % 5]) << c.run2(keys[i % 4])
        end
        res << prc.size << c.n
        res
        "##,
    );
}

#[test]
fn frameless_yield_outer_store_then_exit() {
    // The exits of a block after it has stored to its home: an overflow
    // (materialize; the store and the rest of the iteration stay done), a
    // raise rescued outside the iterator, and a type change of the outer
    // local the home had a guard for.
    run_test(
        r#"
        def sum(a) = (s = 0; a.each { |x| s += x }; s)
        def boom(a)
          s = 0
          begin
            a.each { |x| s += 1; raise "boom #{x}" if x == 7 }
          rescue => e
            return [s, e.message]
          end
          s
        end
        def mix(a) = (s = 0; a.each { |x| s = s + x }; s)
        res = []
        40.times do |i|
          a = (1..20).to_a
          a << 2**62 if i == 30
          b = i == 35 ? a.map(&:to_f) : a
          res << sum(a) << boom(a) << mix(b)
          GC.start if i == 31
        end
        res
        "#,
    );
}

#[test]
fn frameless_yield_outer_captured() {
    // The home's locals are captured (a Proc, a `binding`), so the block
    // reaches them through the header word rather than at a static
    // distance.
    run_test(
        r#"
        def pr(a)
          s = 0
          get = proc { s }
          a.each { |x| s += x }
          [s, get.call]
        end
        def bd(a)
          s = 0
          b = binding
          a.each { |x| s += x }
          [s, b.local_variable_get(:s)]
        end
        res = []
        30.times { res << pr([1, 2, 3]) << bd([4, 5]) }
        res
        "#,
    );
}

#[test]
fn frameless_hash_index_assign_pure_helper() {
    // `@h[k] = v` in a frameless callee: a new key, an update, a String
    // key that is dup-frozen, an identity hash, a user-defined key, and
    // the raising cases (frozen receiver, a new key during iteration),
    // which are reported from the callee's own frame.
    run_test(
        r##"
        class K
          def initialize(v) = (@v = v)
          attr_reader :v
          def hash = @v.hash
          def eql?(o) = o.is_a?(K) && o.v == @v
          def to_s = "K#{@v}"
          def inspect = to_s
        end
        class S
          def initialize(h) = (@h = h)
          attr_accessor :h
          def set(k, v) = (@h[k] = v)
          def run(k, v) = set(k, v)
        end
        res = []
        s = S.new({})
        40.times { |i| res << s.run([:a, "s", i, K.new(i % 3)][i % 4], i) }
        res << s.h.size << s.h.keys.map(&:class)
        str = +"mut"
        s.run(str, 1)
        str << "ated"
        res << s.h.key?("mut") << s.h.keys.last.frozen?
        s.h = {}.compare_by_identity
        30.times { |i| s.run(i.to_s, i) }
        res << s.h.size
        s.h = {a: 1}.freeze
        begin
          s.run(:b, 2)
        rescue FrozenError => e
          res << e.class << e.backtrace_locations.map(&:label).include?("S#set")
        end
        h = {a: 1}
        s.h = h
        begin
          h.each { s.run(:new, 2) }
        rescue RuntimeError => e
          res << e.message
        end
        h.each { s.run(:a, 3) }
        res << h
        res
        "##,
    );
}

#[test]
fn frameless_array_slice_assign_pure_helper() {
    // `a[s, 2] = v` in a frameless callee: the in-place copy, a resizing
    // splice, a right-hand side converted by `#to_ary` (Ruby code: the
    // body hands the call back), and an out-of-range start.
    run_test(
        r##"
        class V
          def to_ary = [:t, :u, :v]
          def inspect = "V"
        end
        class Q
          def initialize = (@a = [0] * 8; @n = 0)
          attr_reader :a, :n
          def put(s, v) = (@n += 1; @a[s, 2] = v)
          def run(s, v) = put(s, v)
        end
        q = Q.new
        res = []
        40.times do |i|
          v = case i % 5
              when 0 then [i, i + 1]
              when 1 then [i]
              when 2 then V.new
              when 3 then :sym
              else [1, 2, 3]
              end
          res << q.run(i % 6, v).inspect << q.a.size
        end
        begin
          q.run(-100, [1, 2])
        rescue IndexError => e
          res << e.class << e.backtrace_locations.map(&:label).include?("Q#put")
        end
        res << q.a << q.n
        res
        "##,
    );
}

#[test]
fn frameless_three_levels_materialize_with_spilled_floats() {
    // x86-64 runs a frameless body without moving rbp / r14: every slot,
    // spill slot and header word of the window is addressed from the
    // enclosing frame's registers plus the window's displacement, three
    // levels deep here. The innermost body spills floats, then a guard
    // after its ivar store materializes all three windows; the exit moves
    // the registers to the innermost window before it writes anything.
    run_test(
        r#"
        class C
          def initialize = (@n = 0; @m = 0; @l = 0)
          attr_reader :n, :m, :l
          def f3(x, a)
            @l += 1
            b = a * 1.5; c = b + 0.25; d = c * b; e = d - a; f = e * c
            g = f + d; h = g * 0.5; k = h + e; q = k * f; r = q - g
            r + h + (x + 1)
          end
          def f2(x, a) = (@m += 1; f3(x, a * 2.0) + 1.0)
          def f1(x, a) = (@n += 1; f2(x + 1, a) * 2.0)
        end
        c = C.new
        res = []
        80.times do |i|
          x = i == 70 ? 2**62 : i
          res << c.f1(x, i * 0.125)
        end
        res << c.n << c.m << c.l
        "#,
    );
}

#[test]
fn frameless_block_outer_locals_with_nested_window() {
    // A frameless block reads and writes its home's locals through the
    // enclosing frame's rbp, and runs a frameless call of its own; the
    // `block_given?` in a frameless body reads the window's header.
    run_test(
        r#"
        def bump(v) = v + 3
        def given? = block_given?
        def each3; yield 1; yield 2; yield 3; end
        def run(k)
          s = 0
          t = 10
          each3 { |i| s += bump(i) * k; t -= i }
          [s, t, given?, given? { }]
        end
        res = []
        40.times { |k| res << run(k) }
        res
        "#,
    );
}

#[test]
fn frameless_callee_past_the_old_size_limit() {
    // A 53-bytecode body: over the per-callee limit this file's other
    // tests stay under, inside `FRAMELESS_MAX_BYTECODES`. A Float takes a
    // redo, an overflow after the ivar store a materializing exit, and
    // `nil` the error exit.
    run_test(
        r#"
        class Mix
          def initialize = (@acc = 0; @hits = 0)
          attr_reader :acc, :hits
          # 41..80 bytecodes: framed under the old 40 limit.
          def step(a, b, c)
            x = a + b
            y = b - c
            z = a * 2 + c
            if x > y
              x -= 1
              @hits += 1
            elsif x < y
              y += 2
            else
              z += 3
            end
            w = x * y - z
            w += 7 if w % 2 == 1
            w -= a if a > c
            w += b if b > c
            w = -w if w < -1000
            @acc += 1
            w + x + y + z
          end
        end
        m = Mix.new
        res = []
        40.times do |i|
          res << m.step(i, i % 7, 3)
        end
        res << m.step(1.5, 2, 3)            # redo / materialize (after the ivar store)
        res << m.step(2**62 - 1, 2**62 - 1, 0)  # overflow after the ivar store
        begin
          m.step(nil, 1, 2)
        rescue NoMethodError => e
          res << e.class
        end
        res << m.acc << m.hits
        res
        "#,
    );
}

#[test]
fn frameless_chain_over_the_budget() {
    // `all` calls four 45-bytecode siblings: 193 bytecodes together, over
    // `FRAMELESS_CHAIN_BUDGET`, so the fourth does not fit, `all` rolls
    // back, and each sibling runs frameless under a framed `all` instead.
    // `two` fits with its two siblings.
    run_test(
        r#"
        class W
          def initialize = (@k = 1)
          def p1(x)
            a = x + 1; b = a * 2; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 2 + a - b + c
          end
          def p2(x)
            a = x + 2; b = a * 3; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 3 + a - b + c
          end
          def p3(x)
            a = x + 3; b = a * 4; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 4 + a - b + c
          end
          def p4(x)
            a = x + 4; b = a * 5; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 5 + a - b + c
          end
          # Four siblings of ~45 bytecodes each: over the chain budget together.
          def all(x) = p1(x) + p2(x) + p3(x) + p4(x)
          def two(x) = p1(x) + p2(x)
        end
        w = W.new
        res = []
        40.times { |i| res << w.all(i) << w.two(i) }
        res << w.all(1.5) << w.two(2**62)
        res
        "#,
    );
}

#[test]
fn frameless_callee_refused_for_the_budget_is_retried_in_a_shorter_chain() {
    // `deep` nests p1 -> p2 -> p3 -> tail -> leaf, and `leaf` does not fit
    // the end of that chain (`FRAMELESS_CHAIN_BUDGET`), which abandons the
    // frameless compile of `tail` there. That says nothing about `tail`
    // called from `direct`, where it starts a chain of its own: it is
    // tried again, not refused for the rest of the unit.
    run_test(
        r#"
        class W2
          def initialize = (@k = 1)
          def leaf(x)
            a = x + 1; b = a * 2; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 2 + a - b + c
          end
          def tail(x) = leaf(x) + @k
          def p3(x)
            a = x + 3; b = a * 4; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 4 + a - b + tail(c)
          end
          def p2(x)
            a = x + 2; b = a * 3; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 3 + a - b + p3(c)
          end
          def p1(x)
            a = x + 1; b = a * 2; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 2 + a - b + p2(c)
          end
          def deep(x) = p1(x)
          def direct(x) = tail(x) + tail(x + 1)
          def both(x) = p1(x) + tail(x)
        end
        w = W2.new
        res = []
        40.times { |i| res << w.deep(i) << w.direct(i) << w.both(i) }
        res << w.deep(1.5) << w.direct(2**62) << w.both(2.5)
        res
        "#,
    );
}

#[test]
fn frameless_callee_refused_for_the_budget_twice() {
    // As above, but `p3` calls `tail` twice at the end of the long chain.
    // The second call finds `tail` already known not to fit from there and
    // does not try it; that refusal is a budget miss like the first, so
    // `p3` around it is not taken for a callee that needs a frame either.
    run_test(
        r#"
        class W3
          def initialize = (@k = 1)
          def leaf(x)
            a = x + 1; b = a * 2; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 2 + a - b + c
          end
          def tail(x) = leaf(x) + @k
          def p3(x)
            a = x + 3; b = a * 4; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 4 + a - b + tail(c) + tail(d)
          end
          def p2(x)
            a = x + 2; b = a * 3; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 3 + a - b + p3(c)
          end
          def p1(x)
            a = x + 1; b = a * 2; c = b - x; d = c + @k
            a += 1 if a % 2 == 1
            a -= b if a > b
            b -= c if b > c
            b += 2 if b > 10
            c -= 3 if c < 5
            d += a + b + c
            d * 2 + a - b + p2(c)
          end
          def deep(x) = p1(x)
          def direct(x) = tail(x) + tail(x + 1)
        end
        w = W3.new
        res = []
        40.times { |i| res << w.deep(i) << w.direct(i) }
        res << w.deep(1.5) << w.direct(2**62)
        res
        "#,
    );
}

#[test]
fn frameless_class_new_recursive_initialize() {
    // `initialize` builds the rest of the list through `Node.new`: the
    // nested construction finds its `initialize` on the specialization
    // stack and stays a call (which keeps the outer one framed too),
    // rather than descending level by level until the chain budget runs
    // out and the whole tower rolls back.
    run_test(
        r#"
        class Node
          attr_reader :v, :nxt
          def initialize(n)
            @v = n
            @nxt = n > 0 ? Node.new(n - 1) : nil
          end
        end
        def len(node)
          c = 0
          while node
            c += node.v
            node = node.nxt
          end
          c
        end
        res = []
        40.times { |i| res << len(Node.new(i % 12)) }
        res
        "#,
    );
}

#[test]
fn frameless_failure_is_remembered_per_call_site() {
    // `Ctx#evaluate` compiles frameless where the argument is known to be
    // a String (the `respond_to?` folds and the call disappears) and needs
    // a frame where it is an `Ev`, whose `evaluate` calls a block. `lit`
    // and `obj` are inlined into `c2`, `c1` and `c3`, three compilation
    // units that reach the same two call sites: the failed attempt at the
    // one in `obj` is remembered from the first unit on, and must not
    // keep the one in `lit` from compiling frameless in the later ones.
    // In `c2`, the first to compile, `obj` comes first, so `lit` fails
    // there too, second-hand (`evaluate` is refused for the rest of that
    // unit): that failure is not `lit`'s own and is not remembered.
    run_test(
        r#"
        class Ev
          def evaluate(c) = [1, 2].sum { |x| x + c.k }
        end
        class Ctx
          attr_reader :k
          def initialize = (@k = 1; @ev = Ev.new)
          def evaluate(o) = o.respond_to?(:evaluate) ? o.evaluate(self) : o
          def lit = evaluate("s")
          def obj = evaluate(@ev)
          def c1 = lit.size + obj
          def c2 = obj + lit.size
          def c3 = [lit, obj]
        end
        c = Ctx.new
        r = []
        30.times { r << c.c2 }
        30.times { r << c.c1 }
        30.times { r << c.c3 }
        30.times { r << c.obj << c.lit }
        r.uniq
        "#,
    );
}

#[test]
fn integer_with_a_float_copy_is_not_passed_in_a_float_register() {
    // #1925. `i * 0.5` leaves `i` with a float copy in a register
    // (`Sf(_, Fixnum)`). Passing `i` to a frameless callee must hand over
    // the Integer, not that copy: the callee would see a Float (`o + 1`
    // a Float, `o.class` Float, `Float#kind` dispatched).
    run_test(
        r#"
        class Integer; def kind = [self, :int]; end
        class Float; def kind = [self, :float]; end
        class FloatCopyArg
          def inc(o) = o + 1
          def kd(o) = o.kind
          def idt(o) = o
          def a1(i) = [i * 0.5, inc(i)]
          def a2(i) = [i * 0.5, kd(i)]
          def a3(i) = (x = i * 0.5; y = i; [x, kd(y), idt(y)])
          # A real Float still travels in the register.
          def a4(f) = [f * 0.5, inc(f), kd(f)]
          # Integer or Float, by the path taken.
          def a5(i, c) = (v = c ? i : i + 0.5; [v * 2.0, kd(v), inc(v)])
        end
        o = FloatCopyArg.new
        res = []
        50.times do |i|
          res << o.a1(i) << o.a2(i) << o.a3(i) << o.a4(i + 0.25) << o.a5(i, i.even?)
        end
        res
        "#,
    );
}

#[test]
fn integer_with_a_float_copy_is_not_returned_as_a_raw_float() {
    // #1925, the return side: a specialized body whose return value is an
    // Integer with a float copy must not use the raw-f64 return
    // convention.
    run_test(
        r#"
        class FloatCopyRet
          def initialize = @g = 0.0
          def keep(i)
            @g = i * 0.5
            i
          end
          def keep2(i, f) = (@g = i + f; i)
          # A Float on every return edge still returns raw.
          def flt(i) = (@g = i * 0.5; @g + 1.0)
          # Integer on one edge, Float on the other.
          def mix(i) = (x = i * 0.5; i.even? ? i : x)
          def c1(i) = keep(i) + 1
          def c2(i) = [keep(i), keep2(i, 0.25).to_s]
          def c3(i) = flt(i) * 2.0
          def c4(i) = mix(i) + 1
        end
        o = FloatCopyRet.new
        res = []
        50.times { |i| res << o.c1(i) << o.c2(i) << o.c3(i) << o.c4(i) }
        res
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_calls() {
    // A callee whose body calls a builtin that runs no Ruby code and
    // reaches no safepoint (`LeafCall`) compiles frameless: the call is
    // made from the window, with no chain exit.
    run_test(
        r#"
        class LeafCalls
          def initialize(a, h, s) = (@a = a; @h = h; @s = s)
          def asz = @a.size
          def afirst(n) = @a.first(n)
          def alast = @a.last
          def aempty = @a.empty?
          def hsz = @h.size + @h.length
          def ssz = @s.size + @s.bytesize
          def sbyte(i) = @s.getbyte(i)
          def sslice(i, n) = @s.byteslice(i, n)
          def sym = @s.to_sym
          def istr(i) = i.to_s
          def istr2(i, r) = i.to_s(r)
          def fstr(f) = f.to_s
          def cls(o) = o.class
          def nilp(o) = o.nil?
          def frz(o) = o.frozen?
          def same(a, b) = a.equal?(b)
          # A float live in a register across the leaf call.
          def mix(x, i) = (x * 1.5) + istr(i).size + x
          def all(i)
            [asz, afirst(2), alast, aempty, hsz, ssz, sbyte(i % 3), sslice(i % 3, 2), sym,
             istr(i), istr2(i, 16), fstr(i * 0.5), cls(i), cls(@s), nilp(nil), nilp(i),
             frz(@s), frz(:a), same(@a, @a), same(@a, @h), mix(i * 0.25, i * 1000)]
          end
        end
        l = LeafCalls.new([1, 2, 3], { a: 1 }, "héllo")
        res = []
        40.times { |i| res << l.all(i) }
        res
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_calls_more() {
    // The rest of the leaf table: singleton methods, methods that mutate
    // the receiver, a Bignum result, and strscan.rb's primitive
    // (`String#__strscan_match`) under `StringScanner#scan` / `#skip`.
    run_test(
        r#"
        require "strscan"
        class LeafMore
          def initialize(a, h) = (@a = a; @h = h)
          def cur = Thread.current
          def fib = Fiber.current
          def symstr(s) = s.to_s
          def iabs(i) = i.abs
          def fabs(f) = f.abs
          def hempty = @h.empty?
          def pop1 = @a.pop
          def pop2(n) = @a.pop(n)
          def shift1 = @a.shift
          def shift2(n) = @a.shift(n)
          def last2(n) = @a.last(n)
          def first1 = @a.first
          def same(a, b) = a == b
          def tok(ss) = ss.scan(/\w+/)
          def skip(ss) = ss.skip(/\s+/)
          def lit(ss) = ss.scan("=")
          def all(i)
            [cur.equal?(Thread.current), fib.equal?(Fiber.current), symstr(:"s#{i % 3}"), iabs(i - 20),
             iabs(-(2**62) - i), fabs(i * -0.5), hempty, pop1, pop2(2), shift1, shift2(1), last2(2), first1,
             same(@a, @a), same(self, @h)]
          end
          def lex(src)
            ss = StringScanner.new(src)
            out = []
            until ss.eos?
              skip(ss)
              t = tok(ss) || lit(ss) || ss.getch
              out << t
            end
            out
          end
        end
        l = LeafMore.new((1..400).to_a, {})
        res = []
        40.times { |i| res << l.all(i) }
        30.times { |i| res << l.lex("ab = c#{i} + d  = 12") }
        res
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_raises() {
    // An error raised by a leaf builtin is taken by a materializing exit:
    // the backtrace has the builtin's own frame at the line of the call
    // in the inlined body (the cont frame's pc is tagged and carries the
    // body's iseq, `LEAF_CALL_PC_TAG`), then the inlined method, then its
    // caller.
    run_test(
        r#"
        class LeafRaise
          def initialize(a) = @a = a
          def radix(i, r)
            i.to_s(r)
          end
          def take
            @a.pop
          end
          def run(i, r)
            radix(i, r).size + 1
          end
          def run2
            take.to_i + 1
          end
          def a = @a
        end
        base = __LINE__
        w = LeafRaise.new((1..100).to_a)
        r = []
        30.times { |i| r << w.run(i * 7, 2 + i % 30) << w.run2 }
        trace = ->(e) {
          [e.class, e.message] +
            e.backtrace.first(3).map { _1 =~ /:(\d+):in (.*)$/; [$1.to_i - base, $2] }
        }
        [[255, 1], [255, 100]].each do |i, radix|
          begin
            w.run(i, radix)
          rescue => e
            r << trace.(e)
          end
        end
        w.a.replace([1, 2]).freeze
        begin
          w.run2
        rescue => e
          r << trace.(e)
        end
        r << w.run(255, 16)
        r
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_argument_forms() {
    // `Array#first(n)` and `String#getbyte(i)` are leaves only where the
    // argument is proven an Integer (`LeafCall::IntArgs`): with anything
    // else they call `to_int`, which is Ruby code and needs the frame.
    run_test(
        r#"
        class LeafToInt; def to_int = 2; end
        class LeafArgs
          def initialize(a) = @a = a
          def head(n) = @a.first(n)
          def run(n) = head(n).size
          def byte(s, i) = s.getbyte(i)
          def run2(s, i) = byte(s, i).to_i + 1
        end
        a = LeafArgs.new([1, 2, 3, 4])
        n = LeafToInt.new
        r = []
        30.times { |i| r << a.run(i % 3) << a.run2("abc", i % 4) }
        r << a.run(n) << a.run2("abc", n)
        30.times { |i| r << a.run(n) << a.run(1) << a.run2("xyz", n) << a.run2("xyz", i % 3) }
        r
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_redefined() {
    // The leaf call is under the class-version guard like any call: a
    // redefinition of the builtin is seen.
    run_test(
        r#"
        class LeafRedef
          def initialize(a) = @a = a
          def lst = @a.last
          def run = lst + 1
        end
        a = LeafRedef.new([1, 2, 3])
        r = []
        30.times { r << a.run }
        class Array
          alias __leaf_last last
          def last = 100
        end
        30.times { r << a.run }
        class Array
          alias last __leaf_last
        end
        30.times { r << a.run }
        r.uniq
        "#,
    );
}

#[test]
fn frameless_generic_eq_pure_helper() {
    // `==` / `!=` on operands of no known class go through the helper
    // that answers the cases decided without running Ruby code (nil,
    // booleans, Symbols, numbers, Strings, `BasicObject#==`) and declines
    // the rest: a user-defined `==` or `!=`, a String subclass, the
    // reverse dispatch of `1 == obj`, Array and Hash. A decline deopts
    // and the interpreter does the comparison in a real frame.
    run_test(
        r#"
        class EqP
          attr_reader :v
          def initialize(v) = @v = v
          def ==(o) = o.is_a?(EqP) && v == o.v
        end
        class EqQ; def initialize(v) = @v = v; end
        class EqS < String; def ==(o) = o == :s2; end
        class EqR; def !=(o) = :custom; end
        class EqCmp
          def eq(a, b) = a == b
          def ne(a, b) = a != b
          def both(a, b) = [eq(a, b), ne(a, b)]
        end
        c = EqCmp.new
        q = EqQ.new(1)
        vals = [nil, true, false, :a, :b, 1, 2, 1.0, 2**70, 2**70, 2**200, 2**200, 2.0**200,
                "a", "b", "a".b, q, EqQ.new(1)]
        r = []
        3.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        pv = [EqP.new(1), EqP.new(1), EqP.new(2), EqS.new("x"), EqR.new, [1], [1], { a: 1 }]
        3.times { pv.each { |x| (vals + pv).each { |y| r << c.both(x, y) << c.both(y, x) } } }
        3.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        r
        "#,
    );
}

#[test]
fn frameless_generic_eq_redefined() {
    // The pure helper checks the basic-op redefinition and a custom `!=`
    // itself, per call.
    run_test_once(
        r#"
        class EqCmp2
          def eq(a, b) = a == b
          def ne(a, b) = a != b
          def both(a, b) = [eq(a, b), ne(a, b)]
        end
        class EqT; end
        c = EqCmp2.new
        t = EqT.new
        vals = [nil, :a, 1, "a", t, EqT.new]
        r = []
        10.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        class EqT
          def ==(o) = o.is_a?(EqT)
        end
        10.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        class NilClass
          def ==(o) = o == :a
        end
        res = vals.map { |y| c.both(nil, y) }
        class NilClass
          remove_method :==
        end
        r << res
        10.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        r
        "#,
    );
}

#[test]
fn frameless_generic_eq_each_decline() {
    // A site whose pure helper has declined once is an ordinary call from
    // then on, so each kind of decline gets a site of its own: a String
    // subclass receiver, a String compared with a `to_str` responder, a
    // receiver with its own `!=`, and a number compared with an object
    // that defines `==` (the reverse dispatch).
    run_test(
        r#"
        class EqDSub < String; def ==(o) = o == :sub; end
        class EqDStr; def to_str = "a"; def ==(o) = o == "a"; end
        class EqDNe; def !=(o) = :custom; end
        class EqDRev; def ==(o) = o == 1; end
        class EqDecl
          def eq1(a, b) = a == b
          def ne1(a, b) = a != b
          def eq2(a, b) = a == b
          def ne2(a, b) = a != b
          def eq3(a, b) = a == b
          def ne3(a, b) = a != b
          def eq4(a, b) = a == b
          def ne4(a, b) = a != b
          def sub(a, b) = [eq1(a, b), ne1(a, b)]
          def str(a, b) = [eq2(a, b), ne2(a, b)]
          def neq(a, b) = [eq3(a, b), ne3(a, b)]
          def rev(a, b) = [eq4(a, b), ne4(a, b)]
        end
        class EqD1; end
        class EqD2; end
        class EqD3; end
        class EqD4; end
        c = EqDecl.new
        # Receivers of enough classes that the sites give up on dispatching
        # by class and compare through the helper.
        plain = [nil, :a, true, false, 1, 2.5, "a", "b", EqD1.new, EqD2.new, EqD3.new, EqD4.new]
        r = []
        warm = -> {
          plain.each do |y|
            plain.each { |x| r << c.sub(x, y) << c.str(x, y) << c.neq(x, y) << c.rev(x, y) }
          end
        }
        3.times { warm.() }
        s, t, n, v = EqDSub.new("a"), EqDStr.new, EqDNe.new, EqDRev.new
        r << c.sub(s, :sub) << c.sub(s, "a")
        r << c.str("a", t) << c.str("b", t)
        r << c.neq(n, 1) << c.neq(n, n)
        r << c.rev(1, v) << c.rev(2.5, v) << c.rev(2**70, v)
        3.times { warm.() }
        r << c.sub(s, :sub) << c.str("a", t) << c.neq(n, 1) << c.rev(1, v)
        r
        "#,
    );
}

#[test]
fn frameless_generic_eq_float_eq_redefined() {
    // `Float#==` is no assumption of the compiled code (only the `==` of
    // Integer, nil, true, false and Symbol is), so its redefinition does
    // not evict the body: the pure helper sees it at the call and
    // declines.
    run_test_once(
        r#"
        class EqFlt
          def eq(a, b) = a == b
          def ne(a, b) = a != b
          def both(a, b) = [eq(a, b), ne(a, b)]
        end
        c = EqFlt.new
        vals = [nil, :a, 1, 1.5, 2.5, "a"]
        r = []
        10.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        class Float
          alias_method :__eq_flt, :==
          def ==(o) = o.nil?
        end
        res = vals.map { |y| c.both(1.5, y) + c.both(y, 1.5) }
        class Float
          alias_method :==, :__eq_flt
          remove_method :__eq_flt
        end
        r << res
        10.times { vals.each { |x| vals.each { |y| r << c.both(x, y) } } }
        r
        "#,
    );
}

#[test]
fn frameless_leaf_builtin_not_simple_call() {
    // A leaf builtin called with a splat, a block argument or a block is
    // no leaf call.
    run_test(
        r#"
        class LeafSplat
          def initialize(a) = @a = a
          def h1(args) = @a.first(*args)
          def h2(blk) = @a.last(&blk)
          def h3 = @a.size { 1 }
          def h4(h) = @a.empty?(**h)
          def run(args) = [h1(args), h2(nil), h3, h4({})]
        end
        a = LeafSplat.new([1, 2, 3, 4])
        r = []
        30.times { |i| r << a.run([i % 3]) << a.run([]) }
        r
        "#,
    );
}

#[test]
fn strscan_match_takes_an_integer_position() {
    // `String#__strscan_match` is monoruby's own (strscan.rb's primitive;
    // CRuby has the C extension instead), so this is not oracle-checked.
    // It is a leaf builtin and so calls no `to_int` on its position.
    let v = run_test_no_result_check(
        r##"
        class StrscanPos; def to_int = 0; end
        r = []
        r << "abc".__strscan_match(/a/, 0, true)
        r << "abc".__strscan_match(/a/, -1, true)
        r << (begin; "abc".__strscan_match(/a/, StrscanPos.new, true); rescue TypeError => e; e.message; end)
        expected = [1, nil, "no implicit conversion of StrscanPos into Integer"]
        raise "got #{r.inspect}" unless r == expected
        r.size
        "##,
    );
    assert_eq!(v.try_fixnum(), Some(3));
}
