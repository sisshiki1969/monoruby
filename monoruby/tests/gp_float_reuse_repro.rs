extern crate monoruby;
use monoruby::tests::*;

// Regression for a JIT mixed Integer/Float bug (mspec SpinnerFormatter ETR
// crash, `NilClass can't be coerced into Float`). A Float operand held only in
// a GP resident register (a builtin call result) was promoted to `Sf` and read
// straight from the register without flushing its boxed value to the stack
// home; under fpr pressure the `Sf` cache could be demoted (`Sf -> S`, no spill
// emitted) and the slot then reloaded a stale `nil`. Reproduces under
// `--features stress-spill-pool` (CI runs that).
#[test]
fn gp_resident_float_reused_after_mixed_op() {
    run_test(
        r##"
        class C
          def initialize; @percent = 7; @start = Time.now; end
          def f
            elapsed = Time.now - @start
            ((100 * elapsed / @percent) - elapsed).is_a?(Float)
          end
        end
        c = C.new
        r = nil
        60.times { r = c.f }
        r
        "##,
    );
}
