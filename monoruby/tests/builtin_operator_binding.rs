//! Redefining a basic operator must not reach inside monoruby's own
//! Ruby-written core.
//!
//! `Integer#times`, `Array#each`, `Enumerable#map` and ~9,000 further lines
//! are Ruby here where CRuby writes C, which is a deliberate trade (the JIT
//! compiles the `while` loop per call site and lowers `yield` to a direct
//! block call). The cost showed up once redefinition actually started
//! binding: their own loop counters went through it too.
//!
//! ```ruby
//! Integer.class_eval("def +(o); 999; end")
//! r = []; 5.times { |i| r << i }; r    # CRuby [0,1,2,3,4]; monoruby [0]
//! ```
//!
//! `i += 1` returned 999, `999 < 5` was false, and the loop ended after one
//! round — no exception, no warning, a silently short answer (issue #1135).
//! A redefined `Integer#<` broke `Array#each` outright.
//!
//! The fix gives those frames what CRuby gets from being C: their basic
//! operators are bound to the definition the basic-op table armed at
//! bootstrap. One `Meta` bit marks the frames (`set_internal_builtin`,
//! already used to keep them out of `$~`/`$_` and out of refinement
//! resolution — #1066 is the same fix for refinements), and three places
//! read it: `Executor::dispatch_redefined_op` for the VM helpers,
//! `JitContext::resolve_basic_op` for the JIT's fast-path lookup, and
//! `basic_op_assumable` for its licence to inline.
//!
//! **Where this diverges from CRuby**, deliberately: `5.times` under a
//! redefined `Integer#<`. CRuby answers `[]`, because CRuby's own
//! `Integer#times` is written in Ruby too —
//! `Integer.instance_method(:times).source_location` is
//! `["<internal:numeric>", 255]`, where `upto` and `Array#each` answer nil,
//! and its iseq disassembles to `opt_succ` for the increment and `opt_lt`
//! for the condition, with no `+` anywhere:
//!
//! ```text
//! 0027 opt_succ  <calldata!mid:succ, argc:0>     # i = i.succ
//! 0034 opt_lt    <calldata!mid:<, argc:1>        # while i < self
//! ```
//!
//! So redefining `<` truncates it there and redefining `succ` does too
//! (`def succ; self + 2; end` makes CRuby's `times` yield `0, 2, 4`), while
//! `+` cannot reach it at all. monoruby's is now immune to all three. Every
//! other shape in the differential sweep moves toward CRuby (30 differing
//! cells to 1), so that cell is the price of not reproducing a CRuby wart
//! on purpose. It has no test here: `run_test` compares against CRuby.
extern crate monoruby;
use monoruby::tests::*;

/// The issue's first reproduction: redefining `Integer#+` part-way through
/// a `times` loop must not shorten it.
#[test]
fn a_redefined_add_does_not_truncate_times() {
    run_test(
        r#"
        $flag = false
        def noraise
          Integer.class_eval("def +(o); 999; end") if $flag
          nil
        end
        res = []
        120.times do |i|
          $flag = (i == 100)
          noraise
          res << i
        end
        [res.length, res[0], res[100], res[119]]
        "#,
    );
}

/// And with the redefinition already in place before the loop starts.
#[test]
fn a_redefined_add_before_the_loop_is_also_ignored() {
    run_test(
        r#"
        Integer.class_eval("def +(o); 999; end")
        r = []
        5.times { |i| r << i }
        [r, 1 + 1]
        "#,
    );
}

/// `Array#each`'s `while i < size` is the library's own code. A redefined
/// `Integer#<` used to empty it — and `Array#map`, which builds on it,
/// raised `RangeError` from a bignum-sized `Array.new`.
#[test]
fn a_redefined_lt_does_not_break_each_and_map() {
    run_test(
        r#"
        class Integer; def <(o); :OV; end; end
        [[10, 20, 30].each { |x| x }, [1, 2, 3].map { |x| x }, (1 < 2)]
        "#,
    );
}

/// The user's own operators still bind, in the very same program: the
/// binding is a property of the frame, not of the program.
#[test]
fn user_code_still_sees_its_own_redefinition() {
    run_test(
        r#"
        class Integer; def +(o); 999; end; end
        r = []
        3.times { |i| r << (i + 1) }
        [r, 2 + 2]
        "#,
    );
}

/// A block the library yields to is the user's frame, so the operators it
/// writes dispatch normally even though `each` is running above it.
#[test]
fn a_block_yielded_from_a_builtin_is_user_code() {
    run_test(
        r#"
        class Integer; def *(o); :mul; end; end
        [[1, 2, 3].map { |x| x * 2 }, [1, 2, 3].each_with_index.map { |x, i| [x, i] }]
        "#,
    );
}

/// `Enumerable` sits on top of `each`, so it exercises the binding through
/// two library frames at once.
#[test]
fn enumerable_survives_a_redefined_operator() {
    run_test(
        r#"
        class Integer; def +(o); 999; end; end
        [
          [1, 2, 3].take_while { true },
          [1, 2, 3].each_slice(2).to_a,
          [1, 2, 3].each_with_index.to_a,
          {a: 1, b: 2}.map { |k, v| k },
          (1..5).to_a,
          1.upto(5).to_a,
        ]
        "#,
    );
}

/// `Comparable#<` is Ruby here and `res < 0` is its own comparison; before
/// this it recursed into the redefinition.
#[test]
fn comparable_does_not_recurse_through_a_redefined_lt() {
    run_test(
        r#"
        class Integer; def <(o); :OV; end; end
        [5.clamp(1, 3), 5.between?(1, 10), [3, 1, 2].min]
        "#,
    );
}

/// Unary and index fast paths take the same route: `-x`, `!x`, `a[i]`.
#[test]
fn unary_and_index_fast_paths_are_bound_too() {
    run_test(
        r#"
        class Integer; def -@; :neg; end; end
        class Array; def [](i); :idx; end; end
        [[1, 2, 3].first(2), (1..3).to_a, [1, 2, 3].size, -5]
        "#,
    );
}

/// What a redefinition *must* still reach: a comparison the library makes
/// on the user's behalf. CRuby's `sort` calls `<=>` through `rb_funcall`,
/// so a redefined `Integer#<=>` decides the order there — the binding is
/// about the library's own bookkeeping, not about operations it performs
/// for the caller.
#[test]
fn sort_still_dispatches_the_users_spaceship() {
    run_test(
        r#"
        class Integer; def <=>(o); 0; end; end
        [[3, 1, 2].sort, [1, 2, 3].include?(2)]
        "#,
    );
}
