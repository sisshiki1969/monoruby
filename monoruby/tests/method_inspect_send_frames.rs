//! `Method#inspect` for a module receiver, the name a class or module
//! takes in a NameError, and the frames `send` leaves (#2008).
//!
//! - `Comparable.method(:to_s)` is `#<Method: Module(Kernel)#to_s()>`:
//!   CRuby creates a module's singleton class lazily, and until something
//!   asks for it the receiver's class in the display is its real class.
//!   monoruby gives every module one up front, so the display asks
//!   whether it has been "materialized" (`singleton_class`, a singleton
//!   method).
//! - A refinement is `#<refinement:String@M>`, and "undefined method 'x'
//!   for class 'C'" shows a class by its name rather than its `inspect`.
//! - `send` / `__send__` are frameless in CRuby (an optimized method):
//!   no `Kernel#send` line in a backtrace or in `caller`. `__send__`
//!   belongs to BasicObject alone.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) {
    begin
      b.call.inspect.gsub(/0x\h+/, "0x").gsub(/ [^ ]*:\d+>/, " LOC>")
    rescue Exception => e
      "#{e.class}: #{e.message.gsub(/0x\h+/, "0x")}"
    end
  }
  bt = ->(&b) { begin; b.call; rescue Exception => e; e.backtrace.map { |x| x[/in '(.*)'/, 1] }; end }
  class Thrower; def foo = raise("z"); end
  ref = -> { r = nil; Module.new { r = refine(String) { def rr; end } }; r }
  cref = -> { r = nil; Module.new { r = refine(Comparable) { def rr; end } }; r }
"##;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// The issue's examples: a method of a module, taken from the module.
#[test]
fn method_of_a_module() {
    probe(&[
        "Comparable.method(:to_s)",
        "Kernel.method(:to_s)",
        "m = Module.new; m.method(:to_s)",
        "m = Module.new; m.singleton_class; m.method(:to_s)",
        "m = Module.new; def m.foo; end; m.method(:to_s)",
        "m = Module.new; def m.foo; end; m.method(:foo)",
        "Enumerable.method(:name)",
        "Enumerable.method(:instance_method)",
        "String.method(:name)",
        "Class.new.method(:name)",
        "Math.method(:to_s)",
        "GC.method(:name)",
        "ObjectSpace.method(:name)",
        "Comparable.method(:to_s).unbind",
        "Comparable.method(:to_s).owner",
        "Comparable.method(:to_s).receiver",
        "Comparable.singleton_methods",
    ]);
}

/// Receivers that are not modules are unaffected.
#[test]
fn method_of_an_object() {
    probe(&[
        "o = Object.new; o.method(:to_s)",
        "o = Object.new; def o.foo; end; o.method(:to_s)",
        "o = Object.new; def o.foo; end; o.method(:foo)",
        "Math.method(:sqrt)",
        "Process.method(:pid)",
        "1.method(:+).unbind",
        "class MiA; def foo; end; end; class MiB < MiA; end; MiB.new.method(:foo)",
        "class MiA; def foo; end; end; class MiB < MiA; end; MiB.instance_method(:foo)",
        "module MiM; def mm; end; end; class MiC; include MiM; end; MiC.new.method(:mm)",
        "module MiM; def mm; end; end; o = Object.new; o.extend(MiM); o.method(:mm)",
    ]);
}

/// A refinement's name, in `inspect` and in the errors that mention it.
#[test]
fn refinement_names() {
    probe(&[
        "cref.().instance_method(:rr)",
        "cref.().instance_method(:to_s)",
        "cref.().instance_method(:rr).owner",
        "ref.().instance_method(:rr)",
        "ref.().instance_method(:zz)",
        "ref.().public_instance_method(:zz)",
        "ref.().send(:remove_method, :zz)",
        "ref.().send(:undef_method, :zz)",
        "ref.().send(:alias_method, :a, :zz)",
        "ref.().send(:public, :zz)",
        "ref.().const_get(:Zz)",
        "ref.().class_variable_get(:@@zz)",
        "ref.().to_s",
        "ref.().inspect",
        "ref.().name",
        "ref.().zork",
        "ref.().instance_method(:rr).bind(\"a\")",
        "module RefNamed; RefR = refine(String) { def rr; end }; end; [RefNamed::RefR, RefNamed::RefR.name, RefNamed::RefR.to_s]",
        "module RefNamed2; RefR = refine(String) { def rr; end }; end; RefNamed2::RefR.instance_method(:zz)",
        "module RefNamed3; RefR = refine(String) { def rr; end }; end; RefNamed3::RefR.instance_method(:rr)",
    ]);
}

/// A class or module in a NameError is shown by its name.
#[test]
fn name_error_class_names() {
    probe(&[
        "class NeK; def self.inspect = \"custom\"; end; NeK.instance_method(:zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.send(:remove_method, :zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.send(:undef_method, :zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.send(:alias_method, :a, :zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.send(:public, :zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.class_variable_get(:@@zz)",
        "class NeK; def self.inspect = \"custom\"; end; NeK.zork",
        "class NeK; def self.inspect = \"custom\"; end; NeK.new.zork",
        "c = Class.new; c.instance_method(:zz)",
        "m = Module.new; m.instance_method(:zz)",
        "String.instance_method(:zz)",
        "Comparable.instance_method(:zz)",
        "String.singleton_class.instance_method(:zz)",
    ]);
}

/// `send` and `__send__`: where they live.
#[test]
fn send_method_entries() {
    probe(&[
        "BasicObject.instance_method(:__send__).owner",
        "Kernel.instance_method(:__send__)",
        "Object.instance_method(:__send__).owner",
        "Object.instance_method(:send).owner",
        "Object.new.method(:__send__).owner",
        "Object.new.method(:send).owner",
        "Kernel.instance_methods(false).include?(:__send__)",
        "Kernel.instance_methods(false).include?(:send)",
        "BasicObject.instance_methods(false).sort",
        "BasicObject.new.__send__(:__id__).class",
        "BasicObject.instance_method(:__send__).bind_call(3, :+, 4)",
        "Kernel.method_defined?(:__send__)",
        "Object.new.respond_to?(:__send__)",
        "[1.send(:+, 2), 1.__send__(:+, 2), 1.public_send(:+, 2), 1.send(:send, :+, 2)]",
    ]);
}

/// No `Kernel#send` frame: the callee sits directly on the caller.
#[test]
fn send_leaves_no_frame() {
    run_test_once(&format!(
        r#"{HELPER}
        def cl = caller(0).map {{ |x| x[/in '(.*)'/, 1] }}
        [
          bt.() {{ Thrower.new.send(:foo) }},
          bt.() {{ Thrower.new.__send__(:foo) }},
          bt.() {{ Thrower.new.public_send(:foo) }},
          bt.() {{ Thrower.new.send(:send, :foo) }},
          bt.() {{ Thrower.new.send(:nomethod) }},
          bt.() {{ Thrower.new.send }},
          bt.() {{ Thrower.new.__send__ }},
          Object.new.send(:cl),
          Object.new.__send__(:cl),
        ]
        "#
    ));
}

/// The same once the caller is compiled.
#[test]
fn send_leaves_no_frame_compiled() {
    run_test(
        r#"
        class SfT
          def foo(x) = x > 28 ? raise("z") : x
          def cl = caller(0).map { |l| l[/in '(.*)'/, 1] }
        end
        def via_send(o, x) = o.send(:foo, x)
        def via_usend(o, x) = o.__send__(:foo, x)
        def via_psend(o, x) = o.public_send(:foo, x)
        o = SfT.new
        res = []
        30.times do |i|
          [:via_send, :via_usend, :via_psend].each do |m|
            begin
              res << send(m, o, i)
            rescue => e
              res << e.backtrace.map { |l| l[/in '(.*)'/, 1] }
            end
          end
        end
        res << o.send(:cl) << o.__send__(:cl)
        res.uniq
        "#,
    );
}
