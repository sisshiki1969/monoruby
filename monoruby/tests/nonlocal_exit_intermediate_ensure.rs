//! Issue #1185, stage 2: the `ensure` a non-local exit has to run lives in
//! an *intermediate* frame of the inlined chain.
//!
//! Stage 1 (#1187) spliced the exit's **own** frame's `ensure`: defer the
//! unwind, branch into the shared body — ordinary compiled code of the
//! frame being compiled — and let its `EnsureEnd` deliver the exit through
//! the specialized teardown. When the region belongs to a frame the unwind
//! merely *crosses*, that branch has nowhere to go: the owner's compile is
//! parked at the call that leads to the exit, several machine frames down.
//!
//! ```ruby
//! def mid(arr)
//!   begin
//!     arr.each { |y| yield y }   # <- parked here while the block compiles
//!   ensure
//!     $n += 1
//!   end
//! end
//! mid(arr) { |x| break x if x == t }
//! ```
//!
//! Stage 2 reaches it through the machine's own return path: the exit
//! builds and defers its error keyed on the *host* frame's LFP, sets rbp
//! to the frame the host called and `leave; ret`s — landing exactly at the
//! host's call site, where a two-instruction landing recognizes the marker
//! in the return register and branches into the host's `ensure` body. That
//! branch is an ordinary side branch of the host's own CFG, so the body's
//! writes are modelled by the abstract interpreter rather than happening
//! behind its back.
//!
//! Everything here must hold identically whether the splice is taken or
//! the generic unwind is (both shapes are exercised — `run_test` runs each
//! snippet 26 times, so the first rounds are interpreted and the rest
//! compiled), which is why every case asserts the `ensure` count as well
//! as the value.
extern crate monoruby;
use monoruby::tests::*;

/// The shape above: a `break` whose only `ensure` is one frame up. The
/// value must arrive and the body must run exactly once per call.
#[test]
fn a_break_crossing_an_intermediate_ensure() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
          :fell_through
        end
        arr = [1, 2, 3, 4, 5]
        r = []
        40.times { |i| r << mid(arr) { |x| break x * 10 if x == 3 } }
        [r.uniq, $n]
        "#,
    );
}

/// The non-local `return` twin. Its home is one frame further out than
/// the `break`'s, so the teardown the host's `EnsureEnd` runs is longer.
#[test]
fn a_return_crossing_an_intermediate_ensure() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
          :fell_through
        end
        def home(arr, t)
          mid(arr) { |x| return x * 10 if x == t }
          :not_reached
        end
        r = []
        40.times { r << home([1, 2, 3, 4, 5], 3) }
        [r.uniq, $n]
        "#,
    );
}

/// The modelling gap #1185 names: after the spliced `ensure` runs, the
/// frame the `break` returns to keeps executing *compiled* — its
/// continuation has to be the merge of the normal-return edge and the
/// break edge, with the `ensure`'s writes on it. Every round accumulates
/// into floats the JIT keeps unboxed, so a state the merge got wrong
/// shows up as a wrong number rather than a crash.
#[test]
fn a_spliced_break_continuation_keeps_accumulating() {
    run_test(
        r#"
        def guarded(arr, log)
          begin
            arr.each { |y| yield y }
          ensure
            log << :ran
          end
          :fell_through
        end
        def run(n)
          c = 2.0
          log = []
          i = 0
          while i < n
            [1].each do |x|
              begin
                v = guarded([4, 5, 6], log) { |y| break y if y == 5 }
                c += v
              ensure
                c += 0.5
              end
            end
            i += 1
          end
          [c, log.size]
        end
        run(120)
        "#,
    );
}

/// The host frame is reached by a `yield`, not a method send — the other
/// call shape that pushes a specialized frame, and so the other place the
/// landing has to be emitted.
#[test]
fn an_intermediate_ensure_hosted_by_a_yield() {
    run_test(
        r#"
        $n = 0
        def mid
          begin
            yield 1
            yield 2
            yield 3
          ensure
            $n += 1
          end
          :fell_through
        end
        def home(t)
          mid { |x| return x * 10 if x == t }
          :not_reached
        end
        r = []
        40.times { r << home(2) }
        [r.uniq, $n]
        "#,
    );
}

/// CRuby semantics the splice must not lose: an exception raised *inside*
/// the `ensure` body replaces the deferred exit.
#[test]
fn an_intermediate_ensure_raising_over_the_exit() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
            raise "from ensure" if $n > 20
          end
          :fell_through
        end
        r = []
        40.times do
          begin
            r << (mid([1, 2, 3]) { |x| break x if x == 2 })
          rescue => e
            r << e.message
          end
        end
        [r.uniq, $n]
        "#,
    );
}

/// Two `ensure`s on the way out — the exit's own frame and an
/// intermediate one. Both must run, innermost first. (Stage 2 splices one
/// region; this shape keeps the generic unwind, and has to stay correct.)
#[test]
fn two_ensures_on_the_way_out_still_run_in_order() {
    run_test(
        r#"
        $log = []
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << :outer
          end
          :fell_through
        end
        r = []
        40.times do
          $log.clear
          r << mid([1, 2, 3]) { |x|
            begin
              break x if x == 2
            ensure
              $log << :inner
            end
          }
        end
        [r.uniq, $log]
        "#,
    );
}

/// Two *intermediate* `ensure`s, one inside the other's frame chain. The
/// plan refuses to splice more than one region, so this is the generic
/// unwind — and both bodies must still run, innermost first.
#[test]
fn two_intermediate_ensures_chain() {
    run_test(
        r#"
        $log = []
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << :inner
          end
        end
        def outer(arr)
          begin
            inner(arr) { |y| yield y }
          ensure
            $log << :outer
          end
        end
        r = []
        40.times do
          $log.clear
          r << outer([1, 2, 3]) { |x| break x if x == 2 }
        end
        [r.uniq, $log]
        "#,
    );
}

/// A `rescue`-only region in the same place is not an `ensure`: nothing of
/// it runs on a non-local exit, and the exit keeps the plain specialized
/// teardown (#1185's stage-1 narrowing). Asserted by behaviour.
#[test]
fn an_intermediate_rescue_only_region_runs_nothing() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          begin
            arr.each { |y| yield y }
          rescue
            $n += 1
          end
          :fell_through
        end
        r = []
        40.times { r << (mid([1, 2, 3]) { |x| break x if x == 2 }) }
        [r.uniq, $n]
        "#,
    );
}

/// The region is an *expression*: the begin body's value is live across
/// the `ensure` and returned after it. On the spliced path that value was
/// never produced, so the landing edge has to claim the slot without
/// taking the normal path's value with it.
#[test]
fn an_intermediate_ensure_region_that_is_an_expression() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          v = begin
            arr.each { |y| yield y }
            :completed
          ensure
            $n += 1
          end
          [v, :after]
        end
        r = []
        40.times { |i| r << (mid([1, 2, 3]) { |x| break x if i.even? && x == 2 }) }
        [r.uniq, $n]
        "#,
    );
}

/// The block turns the host frame into a heap frame (`binding` captures
/// it). The splice addresses the host's locals off `rbp`, so the deferral
/// helper checks the host's `Meta` — the same two bits the JIT's capture
/// guard tests — and degenerates to the generic unwind.
#[test]
fn a_captured_host_frame_falls_back_to_the_generic_unwind() {
    run_test(
        r#"
        $n = 0
        def mid(arr)
          d = 7
          begin
            arr.each { |y| yield y, binding }
          ensure
            $n += d
          end
          :fell_through
        end
        r = []
        40.times { r << (mid([1, 2, 3]) { |x, b| break b.local_variable_get(:d) + x if x == 2 }) }
        [r.uniq, $n]
        "#,
    );
}

/// A `break` out of a proc whose defining frame is gone degenerates to
/// `LocalJumpError`. The splice's degenerate path must raise it from the
/// exit's own position, not swallow it.
#[test]
fn a_degenerate_break_still_raises_local_jump_error() {
    run_test(
        r#"
        $n = 0
        def mid(arr, &blk)
          begin
            arr.each { |y| blk.call(y) }
          ensure
            $n += 1
          end
          :fell_through
        end
        def make = proc { |x| break x if x == 2 }
        r = []
        40.times do
          begin
            r << mid([1, 2, 3], &make)
          rescue LocalJumpError => e
            r << :ljerr
          end
        end
        [r.uniq, $n]
        "#,
    );
}
