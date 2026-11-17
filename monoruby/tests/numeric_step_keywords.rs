//! `Numeric#step`'s keywords and the ArithmeticSequence it answers
//! (#2019).
//!
//! - `#inspect` shows the arguments as they were written: keywords in
//!   their own order (`1.step(by: 2, to: 7)`), a `nil` that was given.
//! - Unknown keywords, and `to:` / `by:` next to the positional argument
//!   they repeat, are ArgumentErrors (`num_step_extract_args`).
//! - `ArithmeticSequence` has no `to_a` of its own: it is
//!   `Enumerable#to_a`.

use monoruby::tests::*;

const HELPER: &str = r####"
def try = (yield).inspect rescue "#{$!.class}: #{$!.message}"
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("try {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// `inspect` of the sequence for each way of writing the limit and
/// the step.
#[test]
fn inspect_shows_the_written_arguments() {
    probe(&[
        r####"1.step(to: 7, by: 2).inspect"####,
        r####"1.step(by: 2, to: 7).inspect"####,
        r####"1.step(to: nil, by: 2).inspect"####,
        r####"1.step(by: nil, to: 3).inspect"####,
        r####"1.step(by: nil).inspect"####,
        r####"1.step(10, by: 3).inspect"####,
        r####"1.step(by: 2, to: 1.0).inspect"####,
        r####"1.step(5, to: nil).inspect"####,
        r####"1.step(5, nil, by: nil).inspect"####,
        r####"e = 1.step(by: 2); [e.inspect, e.first(3)]"####,
        r####"1.0.step(by: 2, to: 5).inspect"####,
        r####"1.step(nil, by: 1, to: 3).inspect"####,
        r####"1r.step(to: 3, by: 1).inspect"####,
        r####"1.step(by: 2r, to: 7).inspect"####,
        r####"1.step(7r, 2r).inspect"####,
        r####"1.step(to: 7.0, by: 2r).inspect"####,
        r####"1.step(by: 2, to: Float::INFINITY).inspect"####,
        r####"1.step(to: 3, by: 1, **{}).inspect"####,
    ]);
}

/// Unknown and repeated keywords, a zero or non-numeric step.
#[test]
fn keyword_errors() {
    probe(&[
        r####"1.step(foo: 1)"####,
        r####"1.step(foo: 1, bar: 2)"####,
        r####"1.step(to: 5, foo: 1) { }"####,
        r####"1.step("a" => 1)"####,
        r####"1.step(10, 3, to: 5)"####,
        r####"1.step(10, 3, by: 5)"####,
        r####"1.step(to: 3, by: 0)"####,
        r####"1.step(to: "a", by: 1).inspect"####,
    ]);
}

/// The values, with and without a block, and the Enumerable methods
/// on top of them.
#[test]
fn values_and_enumeration() {
    probe(&[
        r####"1.step(3).method(:to_a).owner"####,
        r####"Enumerator::ArithmeticSequence.instance_methods(false).sort - [:==, :===, :begin, :each, :end, :eql?, :exclude_end?, :first, :hash, :inspect, :last, :size, :step]"####,
        r####"[1.step(10, 3).to_a, 1.step(by: 2, to: 6).to_a, (1..10).step(3).to_a, 1.0.step(2.0, 0.5).to_a, (1..10).%(4).to_a]"####,
        r####"a = []; 1.step(to: 7, by: 2) { a << _1 }; a"####,
        r####"a = []; 1.step(by: -2, to: -5) { a << _1 }; a"####,
        r####"h = {to: 4, by: 1}; [1.step(**h).inspect, 1.step(**h).to_a]"####,
        r####"[1.step(3).to_a, 1.step(3).entries, 1.step(3, 1).to_a(), (1.step(3).to_a(1) rescue $!.class)]"####,
        r####"1.step(to: 3, by: 1).size"####,
        r####"[1.step(by: 2, to: 7) == 1.step(to: 7, by: 2), 1.step(by: 2, to: 7).hash == 1.step(to: 7, by: 2).hash]"####,
        r####"1.step(to: 7, by: 2).each_slice(2).to_a"####,
        r####"1.step(to: 7, by: 2).with_index.to_a"####,
        r####"[1.step(by: 2, to: 5).begin, 1.step(by: 2, to: 5).end, 1.step(by: 2, to: 5).step, 1.step(by: nil, to: 5).step, 1.step(by: nil, to: 5).to_a]"####,
        r####"1.step(by: 2, to: 7).each_with_index.to_a"####,
        r####"a = []; 1.step(by: nil, to: 3) { a << _1 }; a"####,
        r####"a = []; 1.step(3, nil) { a << _1 }; a"####,
        r####"o = 1.step(to: 3, by: 1); m = Marshal.load(Marshal.dump(o)) rescue $!.class"####,
    ]);
}
