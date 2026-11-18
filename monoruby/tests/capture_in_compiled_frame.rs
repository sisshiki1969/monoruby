use monoruby::tests::*;

// `binding` / `eval` reaching a *compiled* frame in place, through a call
// the JIT did not see as a capture (#2069): the frame's elided objects,
// unboxed Floats and constant-folded locals must be visible to the capture,
// and what `eval` writes must be visible to the frame afterwards. Each
// method runs enough times to be compiled.

const EV: &str = r##"
class Ev
  alias_method :+, :eval
  alias_method :==, :eval
  alias_method :===, :eval
  alias_method :[], :eval
  public :+, :==, :===, :[]
end
O = Ev.new
"##;

/// A registered call site: `send` with a literal name, and a native method
/// calling back (`Array#include?` -> `==`).
#[test]
fn capture_through_a_registered_call_site() {
    run_test_with_prelude(
        r##"
        [by_send(7), by_native(7), float_send(7), const_native(7)]
        "##,
        &format!(
            r##"{EV}
        def by_send(i)
          x = [i, i + 1]
          r = send(:eval, "x")
          [r, x[0]]
        end
        def by_native(i)
          x = [i, i + 1]
          r = [O].include?("x")
          [r, x[0]]
        end
        def float_send(i)
          y = i * 0.5
          y = y + 1.0
          r = send(:eval, "y")
          [r, y]
        end
        def const_native(i)
          k = 3
          y = i * 0.5
          [O].include?("k = 5; y = 1.25")
          [k, y + 1.0]
        end
        "##
        ),
    );
}

/// A call the JIT emits inline: `send` with a dynamic name, and
/// `Method#call`.
#[test]
fn capture_through_an_inline_send() {
    run_test_with_prelude(
        r##"
        [dyn_send(7), method_call(7)]
        "##,
        r##"
        def dyn_send(i)
          k = 3
          y = i * 0.5
          x = [i, i + 1]
          m = :eval
          r = send(m, "k = 5; y = 1.25; x")
          [k, y + 1.0, r, x[0]]
        end
        def method_call(i)
          x = [i, i + 1]
          m = method(:send)
          r = m.call(:eval, "x")
          [r, x[0]]
        end
        "##,
    );
}

/// A frameless runtime helper: a polymorphic operator, `==`, `[]` and
/// `case`/`when` reaching `eval`. The instruction is given back to the
/// interpreter and run again there.
#[test]
fn capture_through_a_frameless_helper() {
    run_test_with_prelude(
        r##"
        r = [O, "s", 1].map do |o|
          [(op(o, 7) rescue nil), eq(o, 7), (idx(o, 7) rescue nil)]
        end
        [r, teq(O, 7), arr_teq(O, 7)]
        "##,
        &format!(
            r##"{EV}
        def op(o, i)
          x = [i, i + 1]
          y = i * 0.5 + 1.0
          r = o + "[x, y]"
          [r, x[0], y]
        end
        def eq(o, i)
          x = [i, i + 1]
          r = (o == "x")
          [r, x[0]]
        end
        def idx(o, i)
          x = [i, i + 1]
          r = o["x"]
          [r, x[0]]
        end
        def teq(o, i)
          x = [i, i + 1]
          r = case "x" when o then :hit else :miss end
          [r, x[0]]
        end
        def arr_teq(o, i)
          x = [i, i + 1]
          r = case "x" when *[o] then :hit else :miss end
          [r, x[0]]
        end
        "##
        ),
    );
}

/// `eval` writes locals of the compiled frame through a frameless helper,
/// and the frame goes on with them; objects stored after the capture
/// survive a GC.
#[test]
fn eval_writes_through_a_frameless_helper() {
    run_test_with_prelude(
        r##"
        write_back(O, 7)
        "##,
        &format!(
            r##"{EV}
        def write_back(o, i)
          x = [i, i + 1]
          y = i * 0.5
          o == "x = [7, 8]; y = 2.5"
          h = {{a: x}}
          GC.start
          [x, y + 1.0, h]
        end
        "##
        ),
    );
}
