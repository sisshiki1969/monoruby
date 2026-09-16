//! A non-local exit crossing **more than one** intermediate `ensure`
//! (issue #1185). Stage 2 (#1363) handled exactly one host and refused
//! anything more (`hosts.len() != 1`), sending those exits down the
//! generic unwind: `handle_error`, the chain-deopt walk, the VM.
//!
//! Now each host's `EnsureEnd` hands the exit on: it re-keys the deferral
//! on the next host owed an `ensure`, tears down to the frame that host
//! called and `ret`s the kind's marker into its landing — the tail of the
//! exit's own hop, run from the `EnsureEnd` — and the last host delivers
//! through the teardown arm as before. Bodies run innermost first, and
//! the exit value survives all of them.
//!
//! What must still refuse, and produce CRuby's answer either way: a host
//! that already routes the same kind of exit somewhere else (arms are
//! static, one destination per kind — the second route used to be
//! silently overwritten), and a body that raises mid-chain, which
//! supersedes the exit.
extern crate monoruby;
use monoruby::tests::*;

/// Two hosts on the way out of a `break`; both bodies run, inner first.
#[test]
fn a_break_through_two_intermediate_ensures() {
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
            inner(arr) { |x| yield x }
          ensure
            $log << :outer
          end
        end
        def find(arr, t); outer(arr) { |x| break x * 10 if x == t }; end
        r = []
        40.times { |i| $log.clear; r << [find([1, 2, 3], i % 3 + 1), $log.dup] }
        r.uniq.sort_by(&:to_s)
    "#,
    );
}

/// The non-local `return` twin.
#[test]
fn a_return_through_two_intermediate_ensures() {
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
            inner(arr) { |x| yield x }
          ensure
            $log << :outer
          end
        end
        def find(arr, t)
          outer(arr) { |x| return x * 10 if x == t }
          :not_found
        end
        r = []
        40.times { |i| $log.clear; r << [find([1, 2, 3], i % 4 + 1), $log.dup] }
        r.uniq.sort_by(&:to_s)
    "#,
    );
}

/// Three hosts: the hand-on repeats.
#[test]
fn a_break_through_three_intermediate_ensures() {
    run_test(
        r#"
        $log = []
        def l1(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << 1
          end
        end
        def l2(arr)
          begin
            l1(arr) { |x| yield x }
          ensure
            $log << 2
          end
        end
        def l3(arr)
          begin
            l2(arr) { |x| yield x }
          ensure
            $log << 3
          end
        end
        def find(arr, t); l3(arr) { |x| break x * 10 if x == t }; end
        r = []
        40.times { |i| $log.clear; r << [find([1, 2, 3], i % 3 + 1), $log.dup] }
        r.uniq.sort_by(&:to_s)
    "#,
    );
}

/// The exit value crosses every body intact, including a Float held
/// unboxed at the exit, and an object.
#[test]
fn the_exit_value_survives_every_hop() {
    run_test(
        r#"
        $n = 0
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $n += 1
          end
        end
        def outer(arr)
          begin
            inner(arr) { |x| yield x }
          ensure
            $n += 1
          end
        end
        def ff(arr, t); outer(arr) { |x| break x * 1.5 if x == t }; end
        def fs(arr, t); outer(arr) { |x| break "v#{x}" if x == t }; end
        s = 0.0
        strs = []
        40.times { |i| s += ff([1, 2, 3], i % 3 + 1); strs << fs([1, 2, 3], i % 3 + 1) }
        [s, strs.uniq.sort, $n]
    "#,
    );
}

/// `break` and `return` handed on through the same two hosts: each
/// host's arms dispatch on the parked kind.
#[test]
fn break_and_return_handed_on_through_the_same_hosts() {
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
            inner(arr) { |x| yield x }
          ensure
            $log << :outer
          end
        end
        def find(arr, t)
          v = outer(arr) { |x|
            return [:ret, x] if x == t && t.odd?
            break [:brk, x] if x == t
          }
          [:fell, v]
        end
        r = []
        40.times { |i| $log.clear; r << [find([1, 2, 3], i % 3 + 1), $log.dup] }
        r.uniq.sort_by(&:to_s)
    "#,
    );
}

/// A body raises mid-chain: the raise supersedes the exit (CRuby), the
/// outer body still runs, and the exit value is never delivered.
#[test]
fn a_body_raising_mid_chain_supersedes_the_exit() {
    run_test(
        r#"
        $log = []
        def inner(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << :inner
            raise "from inner" if $boom
          end
        end
        def outer(arr)
          begin
            inner(arr) { |x| yield x }
          ensure
            $log << :outer
          end
        end
        def find(arr, t); outer(arr) { |x| break x * 10 if x == t }; end
        r = []
        40.times do |i|
          $log.clear
          $boom = (i % 5 == 0)
          v = begin
            find([1, 2, 3], i % 3 + 1)
          rescue => e
            e.message
          end
          r << [v, $log.dup]
        end
        r.uniq.sort_by(&:to_s)
    "#,
    );
}

/// Two `break`s of different homes through the same host: the host can
/// route one kind one way only, so the second must refuse (and still be
/// right).
#[test]
fn two_breaks_with_different_homes_through_one_host() {
    run_test(
        r#"
        $log = []
        def mid(arr)
          begin
            arr.each { |y| yield y }
          ensure
            $log << :mid
          end
        end
        # break A: home is `wrap`'s call to `mid` — one host (`mid`) then deliver
        def wrap(arr, t); mid(arr) { |x| break [:a, x] if x == t }; end
        # break B: written one level deeper, home is `deep`'s call to `wrap2`,
        # and `mid` is a host it merely passes through before `wrap2`'s own ensure
        def wrap2(arr)
          begin
            mid(arr) { |x| yield x }
          ensure
            $log << :wrap2
          end
        end
        def deep(arr, t); wrap2(arr) { |x| break [:b, x] if x == t }; end
        r = []
        40.times { |i| $log.clear; r << [wrap([1, 2, 3], i % 3 + 1), $log.dup]; $log.clear; r << [deep([1, 2, 3], i % 3 + 1), $log.dup] }
        r.uniq.sort_by(&:to_s)
    "#,
    );
}
