//! Leftovers of #2016–#2025 (#2033).
//!
//! - `NilClass` defines `nil?` itself.
//! - `Set` has no `dup` / `clone` of its own: `Kernel#dup` copies through
//!   `Set#initialize_copy`.
//! - `Enumerable#to_a` on an `Enumerator::ArithmeticSequence` iterates
//!   with whatever `each` it has.
//! - A String name no Symbol exists for makes none in `Thread#[]`,
//!   `#key?`, `#fetch`, `inject`, `define_method` and
//!   `define_singleton_method`.

use monoruby::tests::*;

const HELPER: &str = r####"
def try = (yield).inspect.gsub(/0x\h+/, "0x") rescue "#{$!.class}: #{$!.message.gsub(/0x\h+/, "0x")}"
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("try {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

#[test]
fn nil_class_defines_nil_p() {
    probe(&[
        r####"[nil.method(:nil?).owner, NilClass.instance_methods(false).include?(:nil?), nil.method(:nil?).arity]"####,
        r####"NilClass.instance_method(:nil?).inspect"####,
        r####"a = []; 30.times { a << nil.nil? << 1.nil? << Object.new.nil? }; a.uniq"####,
    ]);
}

#[test]
fn set_copies_through_initialize_copy() {
    probe(&[
        r####"[Set.instance_methods(false) & [:dup, :clone], Set.instance_method(:dup).owner]"####,
        r####"Set.private_instance_methods(false) & [:initialize_copy]"####,
        r####"s = Set[1, 2]; d = s.dup; d << 3; [s, d, d.class]"####,
        r####"s = Set[1].freeze; [s.dup.frozen?, s.clone.frozen?, s.clone(freeze: false).frozen?]"####,
        r####"c = Class.new(Set); s = c[1]; [s.dup.class == c, s.dup == s]"####,
        r####"s = Set[[1]].compare_by_identity; d = s.dup; [d.compare_by_identity?, d.size]"####,
        r####"Set[1].send(:initialize_copy, [1])"####,
        r####"s = Set[1]; s.send(:initialize_copy, s)"####,
        r####"s = Set[1]; s.send(:initialize_copy, Set[5, 6]); s"####,
    ]);
}

#[test]
fn arithmetic_sequence_to_a_uses_each() {
    probe(&[
        r####"e = 1.step(7, 2); def e.each; yield 9; end; [e.to_a, e.entries, e.map { _1 }]"####,
        r####"[1.step(7, 2).to_a, (1..7).step(3).to_a, 1.step(7, 2).entries]"####,
    ]);
}

#[test]
fn unknown_names_make_no_symbol() {
    probe(&[
        r####"t = Thread.current; [t["zq2033a"], t.key?("zq2033b"), (t.fetch("zq2033c") rescue $!.class), t.fetch("zq2033d") { |k| k }, t.fetch("zq2033e", 3)]"####,
        r####"t = Thread.current; t["zq2033f"] = 1; [t["zq2033f"], t.key?("zq2033f"), t.fetch("zq2033f")]"####,
        r####"[([1, 2].inject("zq2033g") rescue $!.message), ([1, 2].each.inject("zq2033h") rescue $!.message)]"####,
        r####"[([1, 2].inject("zq2033i") rescue $!.private_call?), ([1, 2].each.inject("zq2033j") rescue $!.private_call?)]"####,
        r####"[[1].inject("zq2033k"), [].each.inject("zq2033l"), [1, 2].inject(:+), [1, 2].each.inject("+")]"####,
        r####"o = Object.new; def o.method_missing(n, *a) = [n, a]; [[o, 1].inject("zq2033m"), [o, 1].each.inject("zq2033n")]"####,
        r####"[(Object.new.define_singleton_method("zq2033o") rescue $!), (Object.new.define_singleton_method("zq2033p", 3) rescue $!)]"####,
        r####"[(Class.new.define_method("zq2033q") rescue $!), (Class.new.define_method("zq2033r", 3) rescue $!), (Class.new.define_method(3) rescue $!)]"####,
        r####"Class.new.define_method("zq2033s") { 1 }"####,
        r####"(Struct.new(:a).new(1).dig("zq2033t") rescue $!.class)"####,
        r####"Symbol.all_symbols.map(&:to_s).grep(/zq2033/).sort"####,
    ]);
}
