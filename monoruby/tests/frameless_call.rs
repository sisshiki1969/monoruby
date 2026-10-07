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
