extern crate monoruby;
use monoruby::tests::*;

// `Range#step`, `Range#%` and `Numeric#step` with a step that is not a
// Numeric (#1988). CRuby does not convert the step and does not reject it
// by its class: `Range#step` adds it to the begin (`Integer#+` / `Float#+`
// raise "can't be coerced" or go through its `coerce`), `Numeric#step`
// asks it `step > 0`. monoruby compared it with zero itself
// (NoMethodError for `>`), or raised "step must be numeric".

const PRELUDE: &str = r##"
    def st_t
      r = yield
      r.is_a?(Enumerator) ? r.class.to_s : r
    rescue Exception => e
      "#{e.class}: #{e.message}"
    end
    class StNoCmp; def to_int = 2; end
    class StCo; def coerce(o) = [o, 2]; end
    class StGt; def >(o) = true; end
    class StLt; def >(o) = false; end
    class StPl; def +(o) = self; end
    class StObj; def inspect = "#<o>"; end
"##;

#[test]
fn range_step_non_numeric_step() {
    run_test_with_prelude(
        r##"
        [
          st_t { (1..5).step(StNoCmp.new).first(2) },
          st_t { (1..5).step(Object.new).first(2) },
          st_t { (1..5).step("a").first(2) },
          st_t { (1.0..5.0).step(Object.new).first(2) },
          st_t { (1..5).step(Object.new) { } },
          st_t { (1.0..5.0).step(Object.new) { } },
          st_t { (1..5).step(Object.new) },
          st_t { (1..5).step(Object.new).size },
          st_t { (1..5).step(StObj.new).inspect },
          st_t { (1..).step(Object.new).first(2) },
          st_t { (1..).step(Object.new) { } },
          st_t { (1..5).step(StPl.new).first(2) },
          # A step with `coerce` is added through it, every time.
          st_t { a = []; (1..5).step(StCo.new) { |x| a << x }; a },
          st_t { a = []; (1...5).step(StCo.new) { |x| a << x }; a },
          st_t { (1..).step(StCo.new).first(3) },
          st_t { (1..5).step(StCo.new) },
          st_t { (1.0..5.0).step(StCo.new).first(2) },
          # nil is a step like any other once it is written.
          st_t { a = []; (1..3).step(nil) { |x| a << x }; a },
          st_t { (1..3).step(nil) },
          st_t { (1..3).step(nil).first(2) },
          st_t { (1..5).step(1, 2) },
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn range_percent() {
    run_test_with_prelude(
        r##"
        [
          st_t { (1..5) % Object.new },
          st_t { ((1..5) % Object.new).first(1) },
          st_t { ((1..5) % StObj.new).inspect },
          st_t { ((1..5) % 2).inspect },
          st_t { ((1..5) % 2).class.to_s },
          # With a block it iterates, as `step` does.
          st_t { a = []; r = (1..5).%(2) { |x| a << x }; [a, r] },
          st_t { (("a".."e") % 2).inspect },
          st_t { (("a".."e") % 2).to_a },
          st_t { [1, 2, 3, 4, 5, 6][(..-1) % 2] },
          st_t { [1, 2, 3, 4, 5, 6][(1..) % 2] },
          st_t { ((1..3) % 2) == (1..3).step(2) },
          st_t { (1..3).% },
          st_t { (1..3).%(1, 2) },
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn range_step_numeric_unchanged() {
    run_test_with_prelude(
        r##"
        [
          st_t { (1..3).step(0) },
          st_t { (1..3).step(0.0) { } },
          st_t { (1..3).step.to_a },
          st_t { (1..3).step.inspect },
          st_t { (1..3).step(2).inspect },
          st_t { (1.0..3.0).step(0.5).inspect },
          st_t { (1..10).step(3).size },
          st_t { (5..1).step(-2).to_a },
          st_t { a = []; (5..1).step(-2) { |x| a << x }; a },
          st_t { a = []; (3..1).step(1) { |x| a << x }; a },
          st_t { a = []; (3.0..1.0).step(-0.5) { |x| a << x }; a },
          st_t { a = []; (1..2).step(Rational(1, 2)) { |x| a << x }; a },
          st_t { a = []; (2**64..2**64 + 4).step(2) { |x| a << x }; a },
          st_t { a = []; (1..2.5).step(1) { |x| a << x }; a },
          st_t { (1..).step(2).first(3) },
          st_t { a = []; (1..).step(2) { |x| a << x; break if a.size == 3 }; a },
          st_t { (1..3).step(1) { } },
          st_t { (1..3).%(1) { } },
          st_t { (..5).step(2) },
          st_t { (..5).step(2) { } },
          st_t { (..5).step(Object.new) },
          st_t { (.."a").step(2) },
          st_t { (.."a").step(2) { } },
          st_t { (nil..nil).step(1) },
          st_t { (nil..nil).step },
        ]
        "##,
        PRELUDE,
    );
}

// Everything that is neither a numeric progression nor a String / Symbol
// range with a Fixnum step is `begin + step` repeated.
#[test]
fn range_step_generic() {
    run_test_with_prelude(
        r##"
        t1 = Time.at(0).utc
        [
          st_t { (t1..t1 + 3).step { } },
          st_t { (t1..t1 + 3).step },
          st_t { a = []; (t1..t1 + 3).step(1) { |x| a << x.to_i }; a },
          st_t { a = []; (t1..t1 + 3).step(1.5) { |x| a << x.to_f }; a },
          st_t { a = []; (t1...t1 + 3).step(1) { |x| a << x.to_i }; a },
          st_t { (t1..t1 + 3).step(1) },
          st_t { (t1..t1 + 3).step(1).size },
          st_t { a = []; (t1 + 3..t1).step(-1) { |x| a << x.to_i }; a },
          st_t { a = []; (t1..t1 + 3).step(-1) { |x| a << x.to_i }; a },
          st_t { a = []; (t1..t1 + 1).step(0) { |x| a << x.to_i }; a },
          st_t { a = []; (t1..t1).step(1) { |x| a << x.to_i }; a },
          st_t { a = []; (t1...t1).step(1) { |x| a << x.to_i }; a },
          st_t { ("a".."aaa").step("a").to_a },
          st_t { ("a".."e").step(2.0) { } },
          st_t { ("a".."e").step([]) { } },
          st_t { ("a".."e").step(nil).to_a },
          st_t { ("a".."e").step(Rational(2)).to_a },
          st_t { ("a".."e").step(2**70).to_a },
          st_t { (:a..:e).step(:a).to_a },
        ]
        "##,
        PRELUDE,
    );
}

// String and Symbol ranges with a Fixnum step keep the pre-3.4 reading:
// every n-th element of what `String#upto` walks, counted down from the
// first — which is therefore always yielded, alone when the step is zero
// or negative.
#[test]
fn range_step_string_symbol() {
    run_test_with_prelude(
        r##"
        [
          st_t { ("a".."e").step(2).to_a },
          st_t { ("a".."e").step.to_a },
          st_t { ("a".."e").step(2) },
          st_t { ("a".."e").step(2).size },
          st_t { ("a".."e").step(2).inspect },
          st_t { ("a".."e").step.inspect },
          st_t { ("a"..."e").step(2).to_a },
          st_t { ("9".."11").step(1).to_a },
          st_t { ("8".."11").step(2).to_a },
          st_t { ("a"..).step(2).first(3) },
          st_t { ("a".."e").step(0).to_a },
          st_t { ("a".."e").step(-1).to_a },
          st_t { ("a"..."a").step(0).to_a },
          st_t { ("b".."a").step(0).to_a },
          st_t { a = []; ("a"..).step(0) { |x| a << x; break }; a },
          st_t { (:a..:e).step(2).to_a },
          st_t { (:a..:c).step.to_a },
          st_t { (:a...:e).step(2).to_a },
          st_t { (:a..).step(2).first(3) },
          st_t { (:a..:e).step(0).to_a },
          st_t { (:a..:e).step(-2).to_a },
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn numeric_step_non_numeric_step() {
    run_test_with_prelude(
        r##"
        [
          st_t { 1.step(5, Object.new).first(2) },
          st_t { 1.step(5, Object.new) { } },
          st_t { 1.step(5, Object.new) },
          st_t { 1.step(5, Object.new).size },
          st_t { 1.step(5, "a") { } },
          st_t { 1.step(5, "a").size },
          st_t { 1.step(5, "a").first(2) },
          st_t { 1.step(5, true) { } },
          st_t { 1.step(5, StNoCmp.new) { } },
          st_t { 1.step(5, StCo.new) { } },
          st_t { 1.step(by: Object.new, to: 5) { } },
          st_t { 1.0.step(5.0, Object.new) { } },
          # A step that answers `>` is counted with: upwards until `+`
          # refuses it, downwards not at all here.
          st_t { a = []; r = (1.step(5, StGt.new) { |x| a << x } rescue "#{$!.class}: #{$!.message}"); [a, r] },
          st_t { 1.step(5, StLt.new) { } },
          st_t { 1.step(5, StGt.new).size },
          st_t { 1.0.step(5.0, StGt.new) { } },
          st_t { 1.step(by: StGt.new) { break } },
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn numeric_step_arguments() {
    run_test_with_prelude(
        r##"
        [
          # A written nil step is rejected once there is a block.
          st_t { 1.step(5, nil) { break 1 } },
          st_t { 1.step(5, nil) },
          st_t { 1.step(5, nil).size },
          st_t { 1.step(5, nil).inspect },
          st_t { 1.step(5).inspect },
          st_t { 1.step(5, 2).inspect },
          st_t { a = []; 1.step(by: nil, to: 3) { |x| a << x }; a },
          st_t { 1.step(Object.new, 1) { } },
          st_t { 1.step("a", 1) { } },
          # A limit that is not a Numeric makes a plain Enumerator.
          st_t { 1.step("a", 1) },
          st_t { 1.step(by: "a", to: 3) },
          st_t { 1.step(by: 1, to: Object.new) },
          st_t { 1.step(5, 0) { } },
          st_t { 1.step(5, 0) },
          st_t { 1.step(5, to: 3) { } },
          st_t { 1.step(5, 1, by: 3) { } },
          st_t { 1.step(by: 0, to: 3) },
          st_t { 1.step(by: 0) { break :x } },
          st_t { a = []; 1.step(7, 3) { |x| a << x }; a },
          st_t { a = []; 7.step(1, -3) { |x| a << x }; a },
          st_t { a = []; 1.0.step(2.0, 0.5) { |x| a << x }; a },
          st_t { 1.step(by: 2, to: 6).to_a },
        ]
        "##,
        PRELUDE,
    );
}
