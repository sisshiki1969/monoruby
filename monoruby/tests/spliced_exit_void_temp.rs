//! A `break` / non-local `return` written inside its own frame's
//! `begin`..`ensure` is spliced into the shared `ensure` body (#1187): the
//! exit defers its unwind and branches there, and the body's `EnsureEnd`
//! delivers it through the specialized teardown.
//!
//! That branch enters the body at the temp level the body's join expects,
//! which can be *higher* than the exit's own. A region that is an
//! expression holds the begin body's value across the body, and an exit
//! taken before that value is produced leaves the slot void. Left void it
//! collapsed at the merge and took the normal path's value with it — so
//! the `Ret` that reads it after the region had nothing to return, and the
//! compiler hit `as_return` on a void slot:
//!
//! ```text
//! thread 'main' panicked at codegen/jitgen/state/slot.rs:
//! internal error: entered unreachable code
//!   <AbstractFrame>::as_return
//!   <JitContext>::compile_instruction
//! ```
//!
//! A JIT-compile panic aborts the process, so this was a crash on
//! `arr.each { |x| begin; break x if ..; ensure; ..; end }` — one of the
//! shapes #1185 exists to make fast — as soon as the block got hot enough
//! to compile. The exit edge now claims those slots as an unknown boxed
//! `Value`, which is true (the prologue nil-fills the frame) and never
//! read on that path. The stage-2 landing already did this; the stage-1
//! branch did not.
extern crate monoruby;
use monoruby::tests::*;

/// The crash, minimized: a `break` inside the block's own `begin`..`ensure`,
/// hot enough to compile.
#[test]
fn a_spliced_break_before_the_regions_value_is_produced() {
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
fn a_spliced_return_before_the_regions_value_is_produced() {
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
fn a_spliced_break_with_a_rescue_on_the_same_region() {
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
