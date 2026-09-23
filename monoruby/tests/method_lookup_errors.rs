extern crate monoruby;
use monoruby::tests::*;

// The reflection API reports a method it cannot hand out as CRuby's
// `rb_method_name_error` does (#1625, #1642): a plain `NameError` about
// the class the lookup ran in — `undefined method 'x' for class 'C'`,
// `method 'x' for module 'M' is private` — whose `#receiver` is that
// class. monoruby raised the `NoMethodError` a *call* would raise, so a
// `rescue NoMethodError` around `method(:x)` fired where CRuby's does not.

const HELPER: &str = r#"
  t = ->(&b) do
    b.call
  rescue Exception => e
    rc = (e.receiver rescue :none)
    rc = rc.is_a?(Module) ? rc.inspect : rc.class.to_s
    [e.class.to_s, e.message.gsub(/0x\h+/, "0x"), (e.name rescue :none), rc.gsub(/0x\h+/, "0x")]
  end
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// `Object#method` / `#public_method`: the class is the receiver's
/// singleton class when it has one (always, for a class) and its class
/// otherwise; a private or protected method under `public_method` says so.
#[test]
fn method_and_public_method_raise_name_error_about_the_class() {
    run(r#"
        o = Object.new
        s = Object.new; def s.x = 1
        class P1625; private def priv = 1; protected def prot = 1; end
        module Q1625; def self.a = 1; end
        module N1625; extend Comparable; end
        res = []
        res << t.() { o.method(:nope) }
        res << t.() { o.method("nope") }
        res << t.() { s.method(:nope) }
        [1, 1.5, nil, true, :a].each { |v| res << t.() { v.method(:nope) } }
        res << t.() { Kernel.method(:nope) }
        res << t.() { Integer.method(:nope) }
        res << t.() { Class.new.method(:nope) }
        res << t.() { Module.new.method(:nope) }
        res << t.() { Q1625.method(:nope) }
        res << t.() { N1625.method(:nope) }
        res << t.() { Class.new.new.method(:nope) }
        res << t.() { Class.new { def respond_to_missing?(n, p = false) = false; def method_missing(n, *) = 42 }.new.method(:dyn) }
        res << t.() { Class.new { def respond_to_missing?(n, p = false) = n == :dyn; def method_missing(n, *) = n == :dyn ? 42 : super }.new.method(:dyn).call }
        res << t.() { o.public_method(:nope) }
        res << t.() { o.public_method(:puts) }
        res << t.() { P1625.new.public_method(:priv) }
        res << t.() { P1625.new.public_method(:prot) }
        res << t.() { s.public_method(:nope) }
        res << t.() { P1625.new.method(:priv).call }
        res << t.() { begin; o.method(:nope); rescue NoMethodError; :no_method_error; rescue NameError; :name_error; end }
        res
    "#);
}

/// `Module#instance_method` / `#public_instance_method`: the same error,
/// naming a module as a module, with the module as `#receiver`.
#[test]
fn instance_method_names_the_module_and_carries_it() {
    run(r#"
        class P1625b; protected def prot = 1; end
        module M1625; end
        s = Object.new; def s.x = 1
        res = []
        res << t.() { Object.instance_method(:nope) }
        res << t.() { Kernel.instance_method(:nope) }
        res << t.() { M1625.instance_method(:nope) }
        res << t.() { s.singleton_class.instance_method(:nope) }
        res << t.() { Class.new.instance_method(:nope) }
        res << t.() { Thread::Queue.instance_method(:initialize_copy) }
        res << t.() { Object.public_instance_method(:nope) }
        res << t.() { Object.public_instance_method(:puts) }
        res << t.() { P1625b.public_instance_method(:prot) }
        res << t.() { Kernel.public_instance_method(:puts) }
        res << t.() { M1625.public_instance_method(:nope) }
        res
    "#);
}

/// `Kernel#singleton_method` finds only what the singleton class itself
/// and the modules mixed into it define — private ones included — and
/// reports anything else as `undefined singleton method 'x' for '…'`
/// with the object as `#receiver`.
#[test]
fn singleton_method_looks_only_at_the_singleton_class() {
    run(r#"
        module M1625c; def mm = 1; end
        class Base1625; def self.cm = 1; end
        class Sub1625 < Base1625; end
        o = Object.new
        s = Object.new; def s.a = 1
        res = []
        res << t.() { o.singleton_method(:nope) }
        res << t.() { s.singleton_method(:nope) }
        res << t.() { s.singleton_method(:to_s) }
        res << t.() { s.singleton_method(:a).call }
        res << t.() { Integer.singleton_method(:nope) }
        res << t.() { Integer.singleton_method(:sqrt).call(16) }
        res << t.() { Integer.singleton_method(:new) }
        res << t.() { Kernel.singleton_method(:nope) }
        res << t.() { Sub1625.singleton_method(:cm) }
        res << t.() { Base1625.singleton_method(:cm).call }
        [1, nil, true, "s"].each { |v| res << t.() { v.singleton_method(:nope) } }
        res << t.() { o.singleton_method("nope") }
        res << t.() { x = Object.new; x.extend(M1625c); x.singleton_method(:mm).owner }
        res << t.() { x = Class.new; x.extend(M1625c); x.singleton_method(:mm).owner }
        res << t.() { x = Object.new; x.singleton_class.prepend(Module.new { def pp1 = 2 }); x.singleton_method(:pp1).call }
        res << t.() { x = Object.new; class << x; private def pv = 1; end; x.singleton_method(:pv).class }
        res
    "#);
}
