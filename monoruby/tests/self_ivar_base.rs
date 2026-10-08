extern crate monoruby;
use monoruby::tests::*;

// The JIT caches the buffer of `self`'s heap ivar table (the ivars past the
// sixth of an ordinary object) in a register across a run of instructions
// that cannot grow the table (`AsmIr::self_ivar_base`). These pin the cases
// where the buffer moves, or where the run must end, against CRuby.

const PRELUDE: &str = r#"
class C
  def initialize
    @p0 = 0; @p1 = 0; @p2 = 0; @p3 = 0; @p4 = 0; @p5 = 0
    @a = 0; @b = 1; @x = 0.0; @vx = 1.5
  end
  attr_accessor :a
end
"#;

#[test]
fn self_ivar_base_straight_line_and_loop() {
    run_test_with_prelude(
        r#"
        c = C.new
        res = []
        30.times { res << c.step << c.run(5) << c.unset }
        res
        "#,
        &format!(
            "{PRELUDE}{}",
            r#"
            class C
              def step
                @a = @a + @b
                @x = @x + @vx
                @b = (@b * 3) & 0xffff
                [@a, @b, @x]
              end
              def run(n)
                i = 0
                while i < n
                  @a = @a + @b
                  i += 1
                end
                @a
              end
              def unset = [@zz, @a]
            end
            "#
        ),
    );
}

#[test]
fn self_ivar_base_table_grows_in_a_callee() {
    // `grow` adds enough ivars to reallocate `self`'s table between two
    // heap ivar accesses of the caller.
    run_test_with_prelude(
        r#"
        res = []
        30.times do |i|
          c = C.new
          res << c.f(i)
        end
        res
        "#,
        &format!(
            "{PRELUDE}{}",
            r#"
            class C
              def grow(i)
                40.times { |j| instance_variable_set("@g#{i}_#{j}", j) }
              end
              def f(i)
                @a = @a + 1
                grow(i)
                @a = @a + 10
                @b = @a + @b
                [@a, @b, instance_variables.size]
              end
            end
            "#
        ),
    );
}

#[test]
fn self_ivar_base_table_grows_through_another_reference() {
    // The store through `other` (which is `self`) takes the non-self ivar
    // store path, whose slow path grows the table.
    run_test_with_prelude(
        r#"
        res = []
        30.times do |i|
          c = C.new
          res << c.f(c, i)
        end
        res
        "#,
        &format!(
            "{PRELUDE}{}",
            r#"
            class C
              def f(other, i)
                @a = @a + 1
                other.a = @a + 1
                other.instance_variable_set("@n#{i}", i)
                @b = @a + @b
                [@a, @b]
              end
            end
            "#
        ),
    );
}

#[test]
fn self_ivar_base_branches_and_calls() {
    // Runs that end at a branch, a block end, a yield and a send.
    run_test_with_prelude(
        r#"
        c = C.new
        res = []
        30.times { |i| res << c.g(i) { |v| v * 2 } }
        res
        "#,
        &format!(
            "{PRELUDE}{}",
            r#"
            class C
              def g(i)
                if i.even?
                  @a = @a + 1
                else
                  @b = @b + 2
                end
                v = yield(@a)
                @a = @a + v
                w = send(:binding).local_variable_get(:v)
                @b = @b + w
                [@a, @b]
              end
            end
            "#
        ),
    );
}

#[test]
fn self_ivar_base_inlined_callee_with_another_self() {
    // A small callee runs inline in the caller's frame with its own
    // `self`; the caller's heap ivars around it must not see its table.
    run_test_with_prelude(
        r#"
        c = C.new
        d = D.new
        res = []
        30.times { res << c.h(d) }
        res
        "#,
        &format!(
            "{PRELUDE}{}",
            r#"
            class D
              def initialize
                @q0 = 0; @q1 = 0; @q2 = 0; @q3 = 0; @q4 = 0; @q5 = 0
                @k = 100
              end
              def bump = @k = @k + 1
            end
            class C
              def h(d)
                @a = @a + 1
                k = d.bump
                @b = @b + k
                [@a, @b]
              end
            end
            "#
        ),
    );
}

#[test]
fn self_ivar_base_table_grows_in_another_thread() {
    // Another thread grows the table while the loop runs; it can only do
    // so at a safepoint (the loop head), where the run ends.
    run_test_once(&format!(
        "{PRELUDE}{}",
        r#"
        class C
          def run(n)
            i = 0
            while i < n
              @a = @a + 1
              @b = @b + @a
              i += 1
            end
            @a
          end
        end
        c = C.new
        t = Thread.new { 300.times { |j| c.instance_variable_set("@t#{j}", j); Thread.pass } }
        r = c.run(200_000)
        t.join
        [r, c.instance_variables.size]
        "#
    ));
}
