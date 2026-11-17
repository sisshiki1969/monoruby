//! Methods CRuby defines where monoruby inherited them, and aliases it
//! records as aliases (#2017).
//!
//! - `nil` / `true` / `false` have their own `===` (`case_equal`), which
//!   is identity or `==` called on the *argument*.
//! - `String.new` is a singleton method of String (`rb_str_s_new`).
//! - `Set#===` and `Set#member?` are aliases of `Set#include?`, which is
//!   what `Method#original_name` and `==` between them report.
//! - `Method#name` of a refinement's missing method.

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

/// `NilClass#===`, `TrueClass#===` and `FalseClass#===`.
#[test]
fn case_equality_of_nil_true_false() {
    probe(&[
        r####"[nil.method(:===).owner, true.method(:===).owner, false.method(:===).owner]"####,
        r####"[nil === nil, nil === false, true === true, true === 1, false === false, false === nil]"####,
        r####"o = Object.new; def o.inspect = "x"; def o.==(x) = true; [nil === o, true === o, false === o]"####,
        r####"[nil.method(:===).unbind == NilClass.instance_method(:===), nil.method(:===).arity, NilClass.instance_method(:===).inspect[0,40]]"####,
    ]);
}

/// `Set#===` / `#member?` as aliases of `#include?`.
#[test]
fn set_aliases() {
    probe(&[
        r####"[Set[1].method(:===).name, Set[1].method(:===).original_name]"####,
        r####"[Set[1].method(:include?).name, Set[1].method(:include?).original_name, Set[1].method(:member?).original_name, Set.instance_method(:===) == Set.instance_method(:include?)]"####,
        r####"[Set.instance_method(:===).inspect[0,40], Set[1].method(:member?).name]"####,
    ]);
}

/// `String.new` belongs to String itself, and still allocates the
/// subclass it is called on.
#[test]
fn string_new_is_a_singleton_method() {
    probe(&[
        r####"String.method(:new).owner"####,
        r####"[String.singleton_methods.sort, String.method(:new).arity]"####,
        r####"c = Class.new(String); [c.new("a").class == c, c.method(:new).owner, c.new("x", capacity: 10)]"####,
        r####"c = Class.new(String) { def initialize(a, b) = super(a + b) }; c.new("a", "b")"####,
        r####"[String.new, String.new("a"), String.new(encoding: "ascii").encoding, String.new("x", capacity: 3), (String.new(1) rescue $!.message), (String.new("a","b") rescue $!.message)]"####,
        r####"class String; class << self; alias old_new new; def new(*a, **k) = old_new("Z"); end; end; r = [String.new("a")]; class String; class << self; alias new old_new; end; end; r << String.new("a")"####,
    ]);
}

/// `method` on a refinement, a module and a class that has none.
#[test]
fn reflection_on_a_missing_method() {
    probe(&[
        r####"r = nil; Module.new { r = refine(String) { def rr; end } }; r.method(:zz)"####,
        r####"r = nil; Module.new { r = refine(String) { def rr; end } }; [r.singleton_class.inspect, r.singleton_methods, r.class]"####,
        r####"Module.new.method(:zz)"####,
        r####"Class.new.method(:zz)"####,
    ]);
}
