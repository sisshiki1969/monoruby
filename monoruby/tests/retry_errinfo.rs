//! `retry` leaves the rescue clause, and owes what leaving it owes.
//!
//! Completing a rescue clause normally emits two things: the `ensure`
//! bodies of any regions opened inside the clause, and a restore of `$!`
//! to the value the region saved on entry (`gen_begin`'s `errinfo_save` —
//! CRuby restores the *previous* exception, which for a nested rescue is
//! the outer one, not nil). `retry` re-enters the begin body, which leaves
//! the clause just the same, but `NodeKind::Retry` emitted the jump alone
//! (issue #1357):
//!
//! ```ruby
//! n = 0
//! begin
//!   n += 1
//!   raise "b" if n < 2
//! rescue
//!   retry
//! end
//! p $!            # CRuby nil; monoruby kept #<RuntimeError: b>
//! ```
//!
//! and the leak was permanent — every later `rescue`/`ensure` in the
//! program saw the stale exception. The `ensure` half was the same
//! omission: a `begin`..`ensure` opened *inside* the clause never ran its
//! body on the way round.
//!
//! What `retry` does *not* owe is the retried region's own `ensure`: it
//! stays inside that region, so the body runs once at the end, not per
//! round. `redo` is unaffected — it goes through
//! `gen_all_pending_ensures`, which already did both — and the last test
//! pins that.
extern crate monoruby;
use monoruby::tests::*;

/// The plain shape from the issue: after a retry that eventually
/// succeeds, `$!` is back to nil.
#[test]
fn a_successful_retry_clears_errinfo() {
    run_test(
        r#"
        n = 0
        begin
          n += 1
          raise "b" if n < 2
        rescue
          retry
        end
        [n, $!.inspect]
        "#,
    );
}

/// Restored to the region's *entry* value, not to nil: an inner retry
/// nested inside an outer rescue clause leaves `$!` as the outer
/// exception, which is what the inner `begin` saved.
#[test]
fn a_nested_retry_restores_the_outer_exception() {
    run_test(
        r#"
        seen = nil
        begin
          raise "outer"
        rescue
          m = 0
          begin
            m += 1
            raise "inner" if m < 2
          rescue
            retry
          end
          seen = $!.inspect
        end
        [seen, $!.inspect]
        "#,
    );
}

/// The restore happens before the body is re-entered, so every pass sees
/// the entry value rather than the exception the previous pass raised.
#[test]
fn the_retried_body_sees_the_entry_errinfo() {
    run_test(
        r#"
        seen = []
        j = 0
        begin
          j += 1
          seen << $!.inspect
          raise "e" if j < 3
        rescue
          retry
        end
        [seen, $!.inspect]
        "#,
    );
}

/// An `ensure` region opened *inside* the clause runs on every round, and
/// sees its own region's `$!` — the exception just rescued — because its
/// body runs before the retried region's restore.
#[test]
fn an_ensure_inside_the_clause_runs_on_every_retry() {
    run_test(
        r#"
        log = []
        m = 0
        begin
          m += 1
          raise "y" if m < 3
        rescue
          begin
            log << [:before, $!.inspect]
            retry
          ensure
            log << [:ens, $!.inspect]
          end
        end
        [log, m, $!.inspect]
        "#,
    );
}

/// The retried region's *own* `ensure` is not one of them: `retry` stays
/// inside that region, so the body runs once, at the end.
#[test]
fn the_retried_regions_own_ensure_runs_once() {
    run_test(
        r#"
        log = []
        n = 0
        begin
          n += 1
          raise "x" if n < 3
        rescue
          retry
        ensure
          log << :E
        end
        [log, n, $!.inspect]
        "#,
    );
}

/// The same inside a block, where the frame the `retry` runs in is not
/// the method frame.
#[test]
fn a_retry_in_a_blocks_rescue_clause_restores_errinfo() {
    run_test(
        r#"
        def blk
          out = [1, 2].map do |x|
            k = 0
            begin
              k += 1
              raise "c" if k < 2
            rescue
              retry
            end
            [x, k, $!.inspect]
          end
          [out, $!.inspect]
        end
        blk
        "#,
    );
}

/// `redo` restarts the whole block, so it leaves *every* rescue clause
/// open in it — the outermost save wins, where `retry`'s is the innermost.
/// Unchanged by this fix; here to keep the two apart.
#[test]
fn redo_still_restores_the_outermost_save() {
    run_test(
        r#"
        out = []
        c = 0
        begin
          raise "PRE"
        rescue
          [1].each do |x|
            c += 1
            begin
              raise "A"
            rescue
              begin
                raise "B"
              rescue
                redo if c == 1
              end
            end
            out << $!.inspect
          end
        end
        [out, c, $!.inspect]
        "#,
    );
}
