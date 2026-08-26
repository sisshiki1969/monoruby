//! Branch relaxation for frames past the Imm19 / Adr reach (aarch64).
//!
//! A frame whose AsmIr exceeds the `far_branch_mode` threshold emits its
//! BB-edge conditional branches in a long form (same condition taken to a
//! local trampoline that continues with `b`, ±128 MiB — the condition is
//! never inverted, because after an `fcmp` each condition encodes its own
//! NaN behaviour), routes class-dispatch misses through a local veneer
//! (their guards may use `TBZ`, ±32 KiB), and lays `OptCase` jump tables
//! inline, nop-reserved and patched with absolute dest addresses at the
//! frame's end (`a64_patch_jump_tables`) — the const-area table `adr`
//! normally reaches sits past ±1 MiB in such frames.
//!
//! The shapes below build methods big enough to cross the threshold by
//! `eval`ing generated source. Before the relaxation they either failed to
//! compile (a caught panic parking the loop in the interpreter — or, through
//! the double-panic in the recovery path, an outright abort) or could not
//! exist at all: the pre-#1183 layout overflowed `TBZ`'s reach first.
//!
//! On x86-64 the same shapes compile through the ordinary path (its ±2 GiB
//! branches need no relaxation) and simply pin the results.
extern crate monoruby;
use monoruby::tests::*;

/// A while-loop body of ~18k AsmInsts with an if/else split: the `condbr`
/// across each arm spans megabytes of code, and every `a + k` carries an
/// overflow deopt, so the side-exit accumulator sees thousands of handlers
/// (the touched-only thunk selection keeps the islands within imm14).
#[test]
fn a_frame_past_the_imm19_reach() {
    run_test(
        r#"
        add = (0...9000).map { |k| "a = a + #{k % 7 + 1}" }.join("\n")
        sub = (0...9000).map { |k| "a = a - #{k % 5 + 1}" }.join("\n")
        eval "def big\n a = 0\n i = 0\n while i < 200\n if i.odd?\n #{add}\n else\n #{sub}\n end\n i += 1\n end\n a\nend"
        big
        "#,
    );
}

/// The same scale with a `case`/`when` in the loop: in a far frame the
/// dispatch's jump table is laid inline and patched at frame end, since
/// the const-area copy would sit past `adr`'s reach.
#[test]
fn an_opt_case_in_a_far_frame() {
    run_test(
        r#"
        add = (0...9000).map { |k| "a = a + #{k % 7 + 1}" }.join("\n")
        eval "def big(v)\n a = 0\n i = 0\n while i < 200\n #{add}\n case v\n when 1 then a += 10\n when 2 then a += 20\n when 3 then a += 30\n else a += 1\n end\n i += 1\n end\n a\nend"
        big(2) + big(9)
        "#,
    );
}

/// Floats across the far split: the long form keeps the original fcmp
/// condition (an inverted one flips the unordered/NaN direction), so the
/// float compares must behave identically at either scale.
#[test]
fn float_compares_in_a_far_frame() {
    run_test(
        r#"
        add = (0...9000).map { |k| "a = a + #{k % 7 + 1}" }.join("\n")
        eval "def big(x)\n a = 0\n f = 0.0\n i = 0\n while i < 200\n if x > 0.5\n #{add}\n f += 0.25\n else\n a -= 1\n end\n f = 0.0 / 0.0 if i == 100\n i += 1\n end\n [a, f.nan?]\nend"
        big(1.5)
        "#,
    );
}
