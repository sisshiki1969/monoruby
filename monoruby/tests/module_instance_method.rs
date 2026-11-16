extern crate monoruby;
use monoruby::tests::*;

// `Module#instance_method` / `#public_instance_method` on a *module*
// search the module and the modules it includes, and stop there (#1966).
// monoruby links every module's internal superclass to `Object` so that a
// module function can call `Kernel`'s methods; reflection must not follow
// that link, or `Comparable.instance_method(:to_s)` answers `Kernel#to_s`
// where CRuby raises NameError.

const HELPER: &str = r#"
  t = ->(&b) {
    begin
      r = b.call
      UnboundMethod === r ? [r.owner.name || "anonymous", r.name] : r
    rescue Exception => e
      [e.class.to_s, e.message.gsub(/0x\h+/, "0x")]
    end
  }
  m = Module.new
  m2 = Module.new { def foo; end; private def bar; end }
  m3 = Module.new { include m2; include Comparable }
  class Module; def modm; end; end
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// The issue's program.
#[test]
fn modules_do_not_inherit_from_object() {
    run(r#"
        [
          t.() { Comparable.instance_method(:to_s) },
          t.() { Enumerable.public_instance_method(:object_id) },
          t.() { m.instance_method(:inspect) },
          t.() { m.instance_method("==") },
          t.() { m.instance_method(:initialize) },
          t.() { m.public_instance_method(:initialize) },
          t.() { m3.instance_method(:hash) },
          t.() { m3.instance_method(:modm) },
          t.() { m3.public_instance_method(:modm) },
          t.() { Kernel.instance_method(:!=) },
          t.() { Kernel.instance_method(:zzz) },
          t.() { Comparable.instance_method(:respond_to_missing?) },
          Comparable.method_defined?(:to_s),
          Module.new.method_defined?(:hash),
          Kernel.method_defined?(:!=),
        ]
    "#);
}

/// What a module does define, itself or through an included module, is
/// still found — and classes are unaffected.
#[test]
fn own_and_included_methods_are_found() {
    run(r#"
        [
          t.() { m3.instance_method(:foo) },
          t.() { m3.instance_method(:bar) },
          t.() { m3.public_instance_method(:bar) },
          t.() { m3.public_instance_method(:foo) },
          t.() { m3.instance_method(:between?) },
          t.() { Comparable.instance_method(:clamp) },
          t.() { Comparable.public_instance_method(:clamp) },
          t.() { Kernel.instance_method(:to_s) },
          t.() { Kernel.instance_method(:puts) },
          t.() { Kernel.public_instance_method(:puts) },
          t.() { Class.new.instance_method(:to_s) },
          t.() { Class.new.public_instance_method(:puts) },
          t.() { BasicObject.instance_method(:to_s) },
          t.() { o = Object.new; o.extend(Comparable); o.method(:to_s).owner },
          t.() { o = Object.new; o.singleton_class.instance_method(:to_s) },
          t.() { Kernel.singleton_class.instance_method(:to_s) },
          t.() { Comparable.method(:to_s).owner },
        ]
    "#);
}

/// `Kernel`'s own methods are `Kernel`'s: the ones monoruby used to define
/// on `Object` are found on the module, and a copy of `Kernel` can undefine
/// them (what delegate.rb does at load time).
#[test]
fn kernel_owns_its_methods() {
    run(r#"
        names = %i[=== <=> freeze frozen? itself then yield_self to_s inspect hash !~ class dup tap]
        k = Kernel.dup
        [
          names.map { |n| t.() { Kernel.instance_method(n) } },
          names.map { |n| Object.instance_method(n).owner },
          %i[to_s inspect !~ === <=> hash].map { |n| t.() { k.send(:undef_method, n); :ok } },
          1.frozen?, "a".dup.frozen?, Object.new.freeze.frozen?,
          (Object.new <=> 1), (o = Object.new; o <=> o), 5.itself, 5.then { _1 + 1 },
          Object.new.then.size, (Object === 1), (Object.new === 1),
        ]
    "#);
    // delegate.rb undefines them on a copy of Kernel.
    run_test_once(
        r#"
        require "delegate"
        d = SimpleDelegator.new([1, 2])
        [d.to_s, d.inspect, d === [1, 2], d.frozen?, d.size]
        "#,
    );
}

/// The other reflective entry points agree: `undef_method` / `undef` of a
/// name the module does not have is a NameError; a visibility change or an
/// alias of an `Object` method still works, as on CRuby (those do look at
/// `Object` for a module).
#[test]
fn undef_alias_and_visibility_on_modules() {
    run(r#"
        [
          t.() { Module.new.send(:undef_method, :to_s) },
          t.() { x = Module.new; x.module_eval { undef to_s }; x.instance_methods },
          t.() { x = Module.new; x.module_eval { undef nonexist } },
          t.() { Module.new.send(:remove_method, :to_s) },
          t.() { m.send(:alias_method, :ts, :to_s); m.instance_methods },
          t.() { x = Module.new; x.module_eval { alias zz to_s }; x.instance_methods },
          t.() { x = Module.new; x.send(:alias_method, :zz, :nonexist) },
          t.() { Module.new.send(:public, :to_s).class },
          t.() { Module.new.send(:private, :to_s).class },
          t.() { Module.new { public :to_s }.instance_method(:to_s) },
          t.() { Module.new { private :to_s }.instance_method(:to_s) },
          t.() { Module.new { private :to_s }.private_instance_methods },
          Comparable.instance_methods.include?(:to_s),
          Comparable.private_instance_methods.include?(:puts),
          Comparable.public_method_defined?(:to_s),
          Comparable.private_method_defined?(:puts),
          Kernel.private_method_defined?(:puts),
          Kernel.method_defined?(:to_s),
        ]
    "#);
}

/// A refinement's search continues in the refined class.
#[test]
fn refinements() {
    run(r#"
        [
          t.() { r = nil; Module.new { r = refine(Comparable) { def rr; end } }; r.instance_method(:rr).name },
          (begin
             r = nil; Module.new { r = refine(Comparable) { def rr; end } }; r.instance_method(:to_s)
           rescue NameError => e
             [e.class, e.name, e.receiver.equal?(r)]
           end),
          t.() { r = nil; Module.new { r = refine(String) { def rr; end } }; r.instance_method(:upcase) },
          t.() { r = nil; Module.new { r = refine(Comparable) { def rr; end } }; r.instance_method(:clamp) },
        ]
    "#);
}
