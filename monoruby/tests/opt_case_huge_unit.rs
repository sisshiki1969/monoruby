//! Regression coverage for the aarch64 branch-relaxation decision: an
//! `opt_case` inside a small *inlined* frame of a huge compilation unit.
//!
//! `far_branch_mode` used to be decided per frame, but near-form opt_case
//! jump tables live in the constant area, which is emitted at finalize
//! behind the *entire* unit — so a small inlined frame's `adr` had to reach
//! past everything emitted after it, and in a unit past ±1 MiB it could
//! not ("ADR displacement out of range", first hit by a dewasm-generated
//! sqlite build). The decision is unit-wide now; this test builds the
//! same shape synthetically.

use monoruby::tests::*;

#[test]
fn opt_case_in_inlined_frame_of_huge_unit() {
    // A dense-integer `case` that lowers to opt_case, in its own small
    // method. The call site below passes a literal argument, which is what
    // makes the site specializable (see `specialized_iseq`'s gate), so the
    // body is inlined into `big`'s unit rather than compiled standalone.
    let mut src = String::from("class Picker\n  def pick(n, mask)\n    case n & mask\n");
    for i in 0..64 {
        src.push_str(&format!("    when {i} then {}\n", i * 3 + 1));
    }
    src.push_str("    else -1\n    end\n  end\nend\n");

    // A method big enough that its one compilation unit exceeds the ±1 MiB
    // `adr` reach by a wide margin (30000 statements emit roughly 2.7 MiB
    // of aarch64 code), with the inlined opt_case at the very top so its
    // table reference spans the whole rest of the unit.
    src.push_str("class Big\n  def initialize\n    @p = Picker.new\n  end\n");
    src.push_str("  def big(a)\n    a = a + @p.pick(a, 63)\n");
    for i in 0..30000 {
        src.push_str(&format!("    a = a + {} - {}\n", i % 9, (i + 3) % 5));
    }
    src.push_str("    a & 0xffffff\n  end\nend\n");

    // Enough calls to take `big` through the method JIT and then run the
    // compiled code (the crash fired when the finished unit was finalized,
    // so compiling at all is the point; the calls after it prove the
    // inlined dispatch still computes the right thing). Integration tests
    // link the library without cfg(test), so the production compile
    // threshold (20 calls) applies, not the lowered test one.
    src.push_str("b = Big.new\nr = 0\n40.times { r = b.big(r) }\nr\n");

    run_test_once(&src);
}
