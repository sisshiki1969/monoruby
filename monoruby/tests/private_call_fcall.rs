//! `NoMethodError#private_call?` says whether the call was written as a
//! function call (#2016).
//!
//! CRuby records it from the call site (`MISSING_FCALL`): a call with
//! no receiver or with `self` as the receiver — an attribute assignment
//! or an operator on `self` included — is one; a call on any other
//! expression is not, even when that expression is `self` by another
//! name. `send` and `__send__` are function calls whatever they are
//! sent to (`rb_f_send`), `public_send` never is, and a `super` that
//! finds nothing is one.

use monoruby::tests::*;

const HELPER: &str = r####"
def try = (yield).inspect rescue "#{$!.class}: #{$!.message}"
class Foo; def priv; end; private :priv; def prot; end; protected :prot
  def mm = (self.zork1 rescue $!).private_call?
  def mm2 = (zork2() rescue $!).private_call?
  def mm3 = (zork3 rescue $!).class
  def sup = (super rescue $!).private_call?
  def sup2 = (super() rescue $!).private_call?
end
class BO < BasicObject; def t = (zork5() rescue $!).private_call?; def t2 = (self.zork5 rescue $!).private_call?; end
class MM; def method_missing(n, *a) = n == :ok ? 1 : super; def t = (zork6() rescue $!).private_call?; def t2 = (zork7 rescue $!).class; end
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("try {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The shapes of a call, written at the top level.
#[test]
fn top_level_call_shapes() {
    probe(&[
        r####"(self.foo rescue $!).private_call?"####,
        r####"(foo() rescue $!).private_call?"####,
        r####"(1.foo rescue $!).private_call?"####,
        r####"(foo(1, 2) rescue $!).private_call?"####,
        r####"(send(:foo) rescue $!).private_call?"####,
        r####"(1.send(:foo) rescue $!).private_call?"####,
        r####"(public_send(:foo) rescue $!).private_call?"####,
        r####"(__send__(:foo) rescue $!).private_call?"####,
        r####"(Foo.new.priv rescue $!).private_call?"####,
        r####"(Foo.new.prot rescue $!).private_call?"####,
    ]);
}

/// From inside a method body, and `super`.
#[test]
fn inside_methods_and_super() {
    probe(&[
        r####"Foo.new.mm"####,
        r####"Foo.new.mm2"####,
        r####"Foo.new.mm3"####,
        r####"Foo.new.sup"####,
        r####"Foo.new.sup2"####,
    ]);
}

/// Attribute assignment, index, operators, `&.`, a block argument,
/// `instance_eval`, `Symbol#to_proc`, and what the error carries.
#[test]
fn operators_assignments_blocks() {
    probe(&[
        r####"(self.foo = 1 rescue $!).private_call?"####,
        r####"(self.foo += 1 rescue $!).private_call?"####,
        r####"(self[1] rescue $!).private_call?"####,
        r####"(nil.foo rescue $!).private_call?"####,
        r####"(method(:foo).call rescue $!).class"####,
        r####"(foo { } rescue $!).private_call?"####,
        r####"(self&.foo rescue $!).private_call?"####,
        r####"(-self rescue $!).private_call?"####,
        r####"(self + 1 rescue $!).private_call?"####,
        r####"(!self.foo rescue $!).private_call?"####,
        r####"x = self; (x.foo rescue $!).private_call?"####,
        r####"(instance_eval { foo() } rescue $!).private_call?"####,
        r####"(Object.new.instance_eval { foo() } rescue $!).private_call?"####,
        r####"([1].each(&:zork9) rescue $!).private_call?"####,
        r####"(:zork9.to_proc.call(1) rescue $!).private_call?"####,
        r####"e = (foo(1, k: 2) rescue $!); [e.args, e.name, e.receiver.equal?(self)]"####,
        r####"e = (self.foo(1) rescue $!); Marshal.load(Marshal.dump(e)).private_call?"####,
    ]);
}

/// The same answers once the call sites are compiled, on a
/// BasicObject, and under a `method_missing` that calls `super`.
#[test]
fn jit_and_special_receivers() {
    probe(&[
        r####"25.times.map { (foo() rescue $!).private_call? }.uniq"####,
        r####"25.times.map { (1.foo rescue $!).private_call? }.uniq"####,
        r####"25.times.map { (self.foo rescue $!).private_call? }.uniq"####,
        r####"[BO.new.t, BO.new.t2]"####,
        r####"[MM.new.t, MM.new.t2, (MM.new.zork8 rescue $!).private_call?]"####,
    ]);
}
