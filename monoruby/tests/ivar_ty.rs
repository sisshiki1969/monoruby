extern crate monoruby;
use monoruby::tests::*;

// Per-(class, ivar) type tracking (`src/ivar_ty.rs`): every ivar write
// widens the type state of its slot, and JIT code types a load by a state
// that is still monomorphic. These pin the ways a state widens under a
// unit that already assumed it, against CRuby.

#[test]
fn ivar_ty_widened_by_every_writer() {
    run_test_once(
        r#"
        class C
          attr_accessor :b
          def initialize; @a = 1; @f = 1.5; @s = :x; @n = nil; @o = "s"; @b = true; end
          def run(x); @a = @a + x; @f = @f * 1.0; @s = :y; @o = x.to_s; @b = !@b; @n = x if x > 1000; @a; end
        end
        c = C.new
        s = 0
        100.times { |i| s += c.run(i); c.b = (i % 3 == 0) }
        c.instance_variable_set(:@a, 2.5)
        r1 = c.run(1)
        c.b = 1
        r2 = (c.run(2) rescue $!.class)
        [s, r1, r2, c.instance_variables]
        "#,
    );
}

#[test]
fn ivar_ty_widened_by_jit_store() {
    run_test_once(
        r#"
        class D
          def initialize; @x = 0; end
          def set(v); @x = v; end
          def get = @x + @x
        end
        d = D.new
        r = []
        200.times { |i| d.set(i); r << d.get }
        d.set("str")
        r << d.get
        d2 = D.new
        300.times { |i| d2.set(i.to_f) }
        r << d2.get
        d2.set(nil)
        r << (d2.get rescue $!.class)
        r.last(4)
        "#,
    );
}

#[test]
fn ivar_ty_unset_slot() {
    run_test_once(
        r#"
        class Q
          def initialize; @v = 1; end
          def lazy; @w ||= 10; @w + @v; end
          def bump(o); @v = o; end
        end
        q = Q.new
        s = 0
        300.times { s += q.lazy }
        q2 = Q.new
        300.times { s += q2.lazy }
        q.bump("a")
        e = (q.lazy rescue $!.class)
        [s, e]
        "#,
    );
}

#[test]
fn ivar_ty_widened_by_callee_inside_loop() {
    run_test_once(
        r#"
        class R
          def initialize; @n = 0; end
          def run(k)
            i = 0
            while i < 1000
              @n = @n + 1
              poke(k) if i == 500
              i += 1
            end
            @n
          end
          def poke(k); @n = @n.to_f if k; end
        end
        r = R.new
        20.times { r.run(false) }
        [r.run(true), R.new.run(true)]
        "#,
    );
}

#[test]
fn ivar_ty_heap_ivars() {
    run_test_once(
        r#"
        class H
          def initialize
            @p0 = 0; @p1 = 0; @p2 = 0; @p3 = 0; @p4 = 0; @p5 = 0
            @a = 1; @b = 2
          end
          def sum; i = 0; s = 0; while i < 300; s += @a + @b; i += 1; end; s; end
          def b=(v); @b = v; end
        end
        h = H.new
        r = [h.sum]
        h.b = 2r
        r << h.sum
        h2 = H.new
        r << h2.sum
        r
        "#,
    );
}

#[test]
fn ivar_ty_object_changes_class() {
    run_test_once(
        r#"
        class Pt; def v = 1; end
        class Holder
          def initialize(o); @o = o; end
          def get = @o.v
        end
        hs = (1..3).map { Holder.new(Pt.new) }
        t = 0
        300.times { hs.each { t += _1.get } }
        o = hs[1].instance_variable_get(:@o)
        def o.v = 100
        300.times { hs.each { t += _1.get } }
        class Holder2
          def initialize(o); @o = o; end
          def run
            s = 0
            i = 0
            while i < 2000
              s += @o.v
              def (@o).v = 7 if i == 1000
              i += 1
            end
            s
          end
        end
        [t, Holder2.new(Pt.new).run, Holder2.new(Pt.new).run]
        "#,
    );
}

/// A widening by another thread lands while the loop waits at its
/// safepoint poll, the one point in a call-free loop where other Ruby code
/// runs: the poll deopts the frame, so the load after the loop is not
/// typed by the stale state.
#[test]
fn ivar_ty_widened_by_another_thread() {
    run_test_once(
        r#"
        class W
          attr_writer :n
          def initialize; @n = 1; @k = 0; end
          def run
            s = 0
            while true
              s = @k + @n
              break if $done
            end
            @n + 1
          end
        end
        w = W.new
        $done = false
        t = Thread.new { sleep 0.05; w.n = 2.5; $done = true }
        r = w.run
        t.join
        r
        "#,
    );
}

/// A typed ivar lets the compile run past where the interpreter has ever
/// been: a division whose divisor the state cannot name and whose cache
/// is empty deopts to fill the cache (and recompiles) instead of being
/// left an out-of-line call for good.
#[test]
fn ivar_ty_unrun_division_fills_its_cache() {
    run_test_once(
        r#"
        class Tm
          def initialize; @cycles = 0; @tac = 0; @tima = 0; end
          attr_writer :tac
          def step(c)
            before = @cycles
            after = @cycles + c
            @cycles = after & 0xffff
            return if @tac[2] == 0
            divider = case @tac & 0b11
                      when 0b00 then 1024
                      when 0b01 then 16
                      when 0b10 then 64
                      when 0b11 then 256
                      end
            @tima += after / divider - before / divider
          end
          def tima = @tima
        end
        t = Tm.new
        i = 0
        while i < 3000
          t.step(4 + (i & 4))
          t.tac = 0b100 if i == 1000
          t.tac = 0b111 if i == 2000
          i += 1
        end
        t.tima
        "#,
    );
}
