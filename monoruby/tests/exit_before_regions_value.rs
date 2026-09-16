//! A region that is an *expression* holds the begin body's value across
//! the body, so an exit taken **before** that value is produced leaves the
//! slot void:
//!
//! ```ruby
//! arr.each do |x|
//!   begin
//!     break x if x == t   # the region's value is never produced
//!   ensure
//!     $n += 1
//!   end
//! end
//! ```
//!
//! These four shapes were a hard process abort (`as_return` on a void
//! slot, a JIT-compile panic across an `extern "C"` boundary) until #1368
//! taught the stage-1 splice's branch edge to claim those slots as an
//! unknown boxed `Value`.
//!
//! That splice is gone. Since #1370 the exit replays its own frame's
//! `ensure` bodies inline and leaves by the plain specialized teardown, so
//! it opens no branch edge into the body and the void slot never reaches a
//! merge; the stage-1 path that needed the fix was removed with it.
//!
//! The shapes stay as behaviour tests — they are the ones the whole
//! `ensure`-exit line of work is for, and they would abort again if the
//! inline replay were ever disabled in favour of a branch into the body.
//! The intermediate-frame equivalents, where a splice *is* still used and
//! the landing does claim those slots (`emit_spliced_landing`), live in
//! `tests/nonlocal_exit_intermediate_ensure.rs`.
extern crate monoruby;
use monoruby::tests::*;

/// The original crash, minimized: a `break` inside the block's own
/// `begin`..`ensure`, hot enough to compile.
#[test]
fn a_break_before_the_regions_value_is_produced() {
    run_test(
        r#"
        $n = 0
        def f(arr, t)
          arr.each do |x|
            begin
              break x if x == t
            ensure
              $n += 1
            end
          end
        end
        r = []
        40.times { r << f([1, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}

/// The non-local `return` twin of the same shape.
#[test]
fn a_return_before_the_regions_value_is_produced() {
    run_test(
        r#"
        $n = 0
        def g(arr, t)
          arr.each do |x|
            begin
              return x * 10 if x == t
            ensure
              $n += 1
            end
          end
          :fell_through
        end
        r = []
        40.times { r << g([1, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}

/// The region's value *is* used after it, which is what makes the slot
/// live at the body's entry in the first place. The exiting rounds and the
/// completing rounds have to agree on it.
#[test]
fn the_regions_value_survives_when_the_exit_is_not_taken() {
    run_test(
        r#"
        $n = 0
        def h(arr, t)
          arr.each do |x|
            v = begin
              break :broke if x == t
              x * 2
            ensure
              $n += 1
            end
            $log = v
          end
        end
        r = []
        40.times { |i| r << h([1, 2, 3], i.even? ? 2 : 99) }
        [r.uniq.sort_by(&:to_s), $log, $n]
        "#,
    );
}

/// The same shape with a `rescue` also present on the region, so the
/// exception edge's own copy of the body is in play too.
#[test]
fn a_break_with_a_rescue_on_the_same_region() {
    run_test(
        r#"
        $n = 0
        def k(arr, t)
          arr.each do |x|
            begin
              raise "x" if x == 99
              break x if x == t
            rescue
              :rescued
            ensure
              $n += 1
            end
          end
        end
        r = []
        40.times { r << k([1, 99, 2, 3], 2) }
        [r.uniq, $n]
        "#,
    );
}
