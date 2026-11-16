extern crate monoruby;
use monoruby::tests::*;

// Issues #1952 and #1954: the NameError / NoMethodError a Module method
// raises for a name it cannot find — its message, `name` and `receiver` —
// and the name in such a message, which CRuby formats when the message is
// read (so through a redefined `Symbol#to_s`).

const ATTRS: &str = r##"
    def attrs
      r = yield
      [:ok, r]
    rescue NameError => e
      [e.class, e.message, e.name, (e.receiver rescue :no_receiver)].inspect.gsub(/0x\h+/, "0x")
    end
    class C; def m; end; def self.cm; end; end
    module M; def mm; end; end
    o = Object.new
"##;

fn attrs(body: &str) {
    run_test_once(&format!("{ATTRS}\n[{body}]"));
}

#[test]
fn alias_of_a_missing_method() {
    attrs(
        r##"
        attrs { C.send(:alias_method, :a, :nope) },
        attrs { C.send(:alias_method, :a, "nope") },
        attrs { M.send(:alias_method, :a, :nope) },
        attrs { C.singleton_class.send(:alias_method, :a, :nope) },
        attrs { o.singleton_class.send(:alias_method, :a, :nope) },
        attrs { class C; alias a nope; end },
        attrs { module M; alias a nope; end },
        attrs { class << C; alias a nope; end }
        "##,
    );
}

#[test]
fn undef_of_a_missing_method() {
    // `rb_method_name_error`: the name as it was given (a String stays
    // one), and the singleton class of a class named by that class.
    attrs(
        r##"
        attrs { C.send(:undef_method, :nope) },
        attrs { C.send(:undef_method, "nope") },
        attrs { M.send(:undef_method, :nope) },
        attrs { C.singleton_class.send(:undef_method, :nope) },
        attrs { M.singleton_class.send(:undef_method, :nope) },
        attrs { o.singleton_class.send(:undef_method, :nope) },
        attrs { class C; undef nope; end },
        attrs { module M; undef nope; end },
        attrs { class << C; undef nope; end }
        "##,
    );
}

#[test]
fn remove_of_a_missing_method() {
    attrs(
        r##"
        attrs { C.send(:remove_method, :nope) },
        attrs { C.send(:remove_method, "nope") },
        attrs { M.send(:remove_method, :nope) },
        attrs { C.singleton_class.send(:remove_method, :nope) },
        attrs { C.send(:remove_method, :to_s) }
        "##,
    );
}

#[test]
fn visibility_of_a_missing_method() {
    attrs(
        r##"
        attrs { C.send(:public, :nope) },
        attrs { C.send(:public, "nope") },
        attrs { C.send(:public, :m, :nope) },
        attrs { C.send(:protected, :nope) },
        attrs { C.send(:private, :nope) },
        attrs { M.send(:private, :nope) },
        attrs { M.send(:private, :to_s) },
        attrs { M.send(:public, :puts) },
        attrs { C.send(:private, [:m, :nope]) },
        attrs { C.send(:private, :cm) },
        attrs { C.private_class_method(:nope) },
        attrs { C.public_class_method(:nope) },
        attrs { M.private_class_method(:nope) },
        attrs { M.send(:module_function, :nope) },
        attrs { M.send(:module_function, "nope") },
        attrs { Module.new { module_function :nope } }[0, 60],
        attrs { C.send(:ruby2_keywords, :nope) },
        attrs { C.send(:public_instance_method, :nope) },
        attrs { C.instance_method(:nope) },
        attrs { o.method(:nope) },
        attrs { o.singleton_method(:nope) }
        "##,
    );
}

#[test]
fn constants_and_variables_not_defined() {
    attrs(
        r##"
        attrs { C.send(:private_constant, :Nope) },
        attrs { C.send(:public_constant, :Nope) },
        attrs { C.send(:deprecate_constant, :Nope) },
        attrs { C.send(:remove_const, :Nope) },
        attrs { C.send(:remove_const, "Nope") },
        attrs { M.send(:remove_const, :Nope) },
        attrs { Object.send(:remove_const, :Nope) },
        attrs { C.send(:remove_const, :String) },
        attrs { C.send(:remove_class_variable, :@@nope) },
        attrs { o.send(:remove_instance_variable, :@nope) },
        attrs { C.class_variable_get(:@@nope) },
        attrs { C.class_variable_get(:nope) },
        attrs { C.const_get(:Nope) },
        attrs { C::Nope },
        attrs { Nope },
        attrs { o.instance_variable_get(:nope) },
        attrs { binding.local_variable_get(:nope) }.sub(/#<Binding[^>]*>/, "BINDING"),
        attrs { Struct.new(:a).new(1)[:nope] },
        attrs { Struct.new(:a).new(1)["nope"] }
        "##,
    );
}

#[test]
fn const_missing_is_a_method_of_module() {
    run_test_once(
        r##"
        class C; end
        res = []
        begin; C.const_missing(:Zork); rescue NameError => e; res << [e.message, e.name, e.receiver]; end
        begin; Object.const_missing(:Zork); rescue NameError => e; res << [e.message, e.name, e.receiver]; end
        begin; C.const_missing("zork"); rescue NameError => e; res << [e.message, e.name, e.receiver]; end
        begin; C.const_missing(1); rescue => e; res << [e.class, e.message]; end
        begin; C.const_missing; rescue => e; res << [e.class, e.message]; end
        m = Module.instance_method(:const_missing)
        res << m.arity << m.source_location << m.owner
        def f = Nope
        begin; f; rescue NameError => e; res << e.backtrace.size << e.backtrace_locations.map(&:label); end
        res
        "##,
    );
}

#[test]
fn receiver_description() {
    // `name_err_mesg_to_str`: nil / true / false by name, a class or a
    // module by kind, an object with a singleton class as `rb_any_to_s`
    // renders it (`main` for the top-level object), any other object as
    // "an instance of" its class — whatever ran the code (`instance_eval`
    // with a block gives the receiver no singleton class).
    run_test_once(
        r##"
        def t; yield.inspect; rescue Exception => e; "#{e.class}: #{e.message}".sub(/0x\h+/, "0x"); end
        class Foo; def m; bar; end; def self.k; baz; end; end
        module Mod; def self.k; baz; end; end
        [
          t { Foo.new.m }, t { Foo.k }, t { Mod.k }, t { bar },
          t { nil.instance_eval { bar } }, t { 1.instance_eval { bar } },
          t { :s.instance_eval { bar } }, t { true.instance_eval { bar } },
          t { 1.5.instance_eval { bar } },
          t { Object.new.instance_eval { bar } },
          t { Object.new.instance_exec { bar } },
          t { "str".instance_eval { bar } }, t { [1].instance_eval { bar } },
          t { o = Foo.new; def o.x = 1; o.instance_eval { bar } },
          t { o = Object.new; o.singleton_class; o.bar },
          t { o = "str".dup; o.singleton_class; o.bar },
          t { o = Object.new; o.instance_eval { def qq = 1 }; o.bar },
          t { o = Object.new.freeze; o.instance_eval { bar } },
          t { Class.new.new.instance_eval { bar } }.sub(/0x\h+/, "0x"),
          t { Class.new.instance_eval { bar } }.sub(/0x\h+/, "0x"),
          t { Struct.new(:a).new(1).instance_eval { bar } }.sub(/0x\h+/, "0x"),
          t { c = Class.new { def self.name = "Fake" }; c.new.instance_eval { bar } },
          t { c = Class.new { def self.name = "Fake" }; c.new.bar },
          t { c = Class.new { def self.name = "Fake" }; c.instance_eval { bar } },
          t { c = Class.new { def self.name = "Fake" }; c.bar },
          t { c = Class.new { def self.name = 1 }; c.new.instance_eval { bar } },
          t { c = Class.new { def self.inspect = "INS" }; c.new.instance_eval { bar } },
          t { BasicObject.new.instance_eval { bar } },
          t { Foo.new.instance_eval { super_duper() } },
          t { Foo.new.instance_eval { self.bar } },
        ]
        "##,
    );
}

#[test]
fn the_name_is_formatted_when_the_message_is_read() {
    // #1954: `%1$s` of the message is `rb_obj_as_string(name)`, evaluated
    // by `message` / `to_s`, every time.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        class C; def m; end; end
        res = []
        class Symbol; def to_s = "SYM"; end
        res << t { C.class_variable_get(:@@nope) }
        res << t { Object.send(:remove_const, :Nope) }
        res << t { C.send(:alias_method, :a, :nope) }
        res << t { C.send(:undef_method, :nope) }
        res << t { C.send(:remove_method, :nope) }
        res << t { C.send(:private, :nope) }
        res << t { nil.foo }
        res << t { foo }
        res << t { Nope }
        res << t { C::Nope }
        res << t { Struct.new(:a).new(1)[:nope] }
        res << t { Object.new.method(:nope) }.sub(/0x\h+/, "0x")
        res << t { binding.local_variable_get(:nope) }.sub(/#<Binding[^>]*>/, "BINDING")
        class Symbol; n = 0; define_method(:to_s) { "S#{n += 1}" }; end
        e = (nil.foo rescue $!)
        res << [e.message, e.message, e.to_s, e.name.equal?(:foo)]
        e = (Nope rescue $!)
        res << [e.message, e.message]
        e = (C::Nope rescue $!)
        res << [e.message, e.full_message(highlight: false).lines.first.include?("S")]
        class Symbol; def to_s = raise("boom"); end
        res << t { (nil.foo rescue $!).message }
        class Symbol; def to_s = 1; end
        res << t { (nil.foo rescue $!).message }.sub(/0x\h+/, "0x")
        res
        "##,
    );
}

#[test]
fn a_message_set_by_the_program_is_left_alone() {
    run_test_once(
        r##"
        class Symbol; def to_s = "SYM"; end
        e = NameError.new("my foo", :foo)
        f = (nil.foo rescue $!).exception("replaced")
        g = NoMethodError.new("undefined method 'foo' for nil", :foo)
        [e.message, f.message, g.message, (raise e rescue $!.message)]
        "##,
    );
}
