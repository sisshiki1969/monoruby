//! A `rescue` never intercepts a non-local exit — and an `ensure` always
//! does.
//!
//! Two halves of the same lookup, both from issue #1185.
//!
//! **The VM half was wrong.** `handle_error`'s non-local-exit arms asked
//! `get_exception_dest`, which answers with the *innermost* exception-table
//! entry covering the suspended pc, whatever that entry is. Nest a
//! rescue-only region inside an `ensure` region and the innermost entry
//! carries no `ensure`, so the unwind concluded there was none to run and
//! the body was silently skipped:
//!
//! ```ruby
//! [1].each do
//!   begin
//!     begin
//!       return 1        # or `break`, or `throw`
//!     rescue TypeError  # never matches — but it is the innermost entry
//!     end
//!   ensure
//!     puts "skipped"    # CRuby runs this
//!   end
//! end
//! ```
//!
//! `ISeqInfo::covering_ensure` answers with the innermost entry that *has*
//! an `ensure` instead; running that one chains the rest, because its
//! `EnsureEnd` re-delivers the exit from a pc outside the region.
//!
//! **The JIT half was over-conservative.** `in_protected_region` /
//! `check_exception_handler` refused the specialized teardown whenever
//! *any* table entry covered the exit, so a `begin`..`rescue` the exit
//! merely passes through cost a full generic unwind. A rescue-only region
//! is not a reason to refuse: nothing of it runs on the way out. Both
//! checks now ask `ISeqInfo::nonlocal_exit_needs_vm_unwind` — a covering
//! `ensure`, or a `$!` restore owed because the frame stopped inside a
//! `rescue` clause, which only `restore_errinfo_on_exit` replays.
//!
//! The tests below pin the shapes the narrowed checks now let through,
//! and the `$!` each of them must still produce — including the one frame
//! that must *not* be restored, the one a `break` returns into.
extern crate monoruby;
use monoruby::tests::*;

/// A non-local `return` crossing a rescue-only region nested inside an
/// `ensure` region. Before the fix the `ensure` never ran.
#[test]
fn a_return_runs_an_ensure_hidden_behind_a_nested_rescue() {
    run_test(
        r#"
        $log = []
        def m(a)
          a.each do |x|
            begin
              begin
                return x if x > 2
              rescue TypeError
                :unreachable
              end
            ensure
              $log << x
            end
          end
          :none
        end
        [m([1, 2, 3, 4]), $log]
        "#,
    );
}

/// The `break` shape of the same hole.
#[test]
fn a_break_runs_an_ensure_hidden_behind_a_nested_rescue() {
    run_test(
        r#"
        $log = []
        def m(a)
          a.each do |x|
            begin
              begin
                break x * 10 if x > 2
              rescue TypeError
                :unreachable
              end
            ensure
              $log << x
            end
          end
        end
        [m([1, 2, 3, 4]), $log]
        "#,
    );
}

/// `throw` takes the same arm of `handle_error` (`MonorubyErrKind::Throw`)
/// and had the same hole.
#[test]
fn a_throw_runs_an_ensure_hidden_behind_a_nested_rescue() {
    run_test(
        r#"
        $log = []
        def m(a)
          catch(:done) do
            begin
              begin
                a.each { |x| throw :done, x if x > 2 }
              rescue TypeError
                :unreachable
              end
            ensure
              $log << :ens
            end
          end
        end
        [m([1, 2, 3, 4]), $log]
        "#,
    );
}

/// Two `ensure` bodies with a rescue-only region between them: the exit
/// must run both, innermost first.
#[test]
fn a_return_chains_every_ensure_it_crosses() {
    run_test(
        r#"
        $log = []
        def m(a)
          a.each do |x|
            begin
              begin
                begin
                  return x if x > 2
                rescue TypeError
                  :unreachable
                end
              ensure
                $log << :inner
              end
            ensure
              $log << :outer
            end
          end
          :none
        end
        [m([1, 2, 3, 4]), $log]
        "#,
    );
}

/// The find-first shape the specialized teardown is now allowed to take:
/// the `return` leaves a frame suspended inside a `begin`..`rescue` whose
/// handler can never see it. Value and `$!` must be what CRuby produces.
#[test]
fn a_return_crossing_a_rescue_only_region_is_unchanged() {
    run_test(
        r#"
        def m(a)
          begin
            a.each { |x| return x * 3 if x > 2 }
          rescue TypeError
            :rescued
          end
          :none
        end
        [m([1, 2, 3, 4]), m([1, 2]), $!.inspect]
        "#,
    );
}

/// The `break` counterpart, with the rescue-only region in the frame the
/// break passes *through* (`inner`), not the one it returns into.
#[test]
fn a_break_crossing_a_rescue_only_region_is_unchanged() {
    run_test(
        r#"
        def inner(a)
          begin
            a.each { |x| yield x }
          rescue TypeError
            :rescued
          end
          :none
        end
        def m(a)
          inner(a) { |x| break x * 7 if x > 2 }
        end
        [m([1, 2, 3, 4]), $!.inspect]
        "#,
    );
}

/// A `return` leaving a frame that is suspended *inside a rescue clause*
/// owes a `$!` restore — the region-entry save, not the exception it
/// caught. That restore is why such a frame keeps the generic unwind
/// rather than joining the narrowing above.
#[test]
fn a_return_out_of_a_rescue_clause_restores_errinfo() {
    run_test(
        r#"
        def m(a)
          begin
            raise ArgumentError, "boom"
          rescue ArgumentError
            a.each { |x| return [x, $!.class] if x > 2 }
          end
        end
        [m([1, 2, 3, 4]), $!.inspect]
        "#,
    );
}

/// The same restore owed by the frame being compiled: the `break` is
/// written inside the block's own rescue clause.
#[test]
fn a_break_out_of_its_own_rescue_clause_restores_errinfo() {
    run_test(
        r#"
        def m(a)
          a.each do |x|
            begin
              raise ArgumentError, "z"
            rescue ArgumentError
              break [x, $!.class] if x > 2
            end
          end
        end
        [m([1, 2, 3, 4]), $!.inspect]
        "#,
    );
}

/// Nested saves: the outermost one wins, so the restores must run
/// innermost-first. Here the outer clause caught a `RuntimeError` and the
/// inner an `ArgumentError`; after the `return`, `$!` is the outer one.
#[test]
fn nested_rescue_clauses_restore_the_outermost_save() {
    run_test(
        r#"
        def m(a)
          begin
            raise "outer"
          rescue RuntimeError
            begin
              raise ArgumentError, "inner"
            rescue ArgumentError
              a.each { |x| return [x, $!.class] if x > 2 }
            end
          end
        end
        r = m([1, 2, 3, 4])
        [r, $!.inspect]
        "#,
    );
}

/// The exception: a `break` does **not** restore `$!` in the frame it
/// returns into. That frame *resumes* inside its rescue clause and must
/// still see the exception it caught — so the restore walk stops one
/// frame short of the block's defining frame.
#[test]
fn a_break_keeps_errinfo_in_the_frame_it_returns_to() {
    run_test(
        r#"
        def m(a)
          begin
            raise ArgumentError, "u"
          rescue ArgumentError
            r = a.each { |x| break x * 2 if x > 2 }
            [r, $!.class]
          end
        end
        [m([1, 2, 3, 4]), $!.inspect]
        "#,
    );
}

/// A rescue-only region outside and an `ensure` inside: the `ensure`
/// still runs (spliced or generic), the rescue-only one still contributes
/// nothing.
#[test]
fn an_ensure_inside_a_rescue_only_region_still_runs() {
    run_test(
        r#"
        def m(a)
          log = []
          begin
            a.each do |x|
              begin
                return [x, log] if x > 2
              ensure
                log << x
              end
            end
          rescue TypeError
            :rescued
          end
          [:none, log]
        end
        m([1, 2, 3, 4])
        "#,
    );
}
