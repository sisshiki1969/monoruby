extern crate monoruby;
use monoruby::tests::*;

// Issue #1953: how a singleton class names the object it is attached to,
// which Strings are fstrings (no singleton class), and what a FrozenError
// about a frozen object's singleton class names.

const T: &str = r##"
    def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
    class Foo; end
    module M; end
"##;

fn t(body: &str) {
    run_test_once(&format!("{T}\n[{body}]"));
}

#[test]
fn singleton_class_names_its_object_by_any_to_s() {
    t(r##"
        t { "xyz".dup.singleton_class }, t { [1].singleton_class },
        t { {}.singleton_class }, t { Object.new.singleton_class },
        t { Foo.new.singleton_class }, t { Foo.new.singleton_class.to_s },
        t { self.singleton_class }, t { Foo.singleton_class },
        t { M.singleton_class }, t { nil.singleton_class },
        t { Foo.singleton_class.singleton_class },
        t { Foo.new.singleton_class.singleton_class },
        t { o = Object.new; def o.inspect = "INS"; def o.to_s = "TOS"; o.singleton_class },
        t { o = Object.new; o.singleton_class.instance_method(:nope) },
        t { o = Object.new; def o.x = 1; o.method(:x).owner },
        t { o = Object.new; def o.x = 1; o.method(:x).unbind.inspect.sub(/\(.*/, "") }
    "##);
}

#[test]
fn fstrings_have_no_singleton_class() {
    t(r##"
        t { String.name.singleton_class }, t { Foo.name.singleton_class },
        t { M.name.singleton_class }, t { Foo.to_s.singleton_class },
        t { "abc".freeze.singleton_class }, t { (-"abc").singleton_class },
        t { (-("ab" + "c")).singleton_class },
        t { :abc.to_s.singleton_class }, t { :abc.name.singleton_class },
        t { :abc.name.dup.singleton_class }, t { :abc.name.clone.singleton_class },
        t { :abc.name.frozen? }, t { :abc.name.equal?(:abc.name) },
        t { :abc.name.extend(Comparable) },
        t { :abc.name.define_singleton_method(:x) { } },
        t { s = :abc.name; def s.x = 1 },
        t { s = :abc.name; class << s; end },
        t { :abc.name.instance_eval { 1 } },
        t { :abc.name.instance_eval { def zz = 1 } },
        t { :abc.name.instance_eval("size") },
        t { :abc.name.instance_eval("def zz = 1") },
        t { Foo.name.frozen? }, t { Foo.name.equal?(Foo.name) }
    "##);
}

#[test]
fn hash_key_copies_are_fstrings() {
    // `rb_hash_key_str`: the copy of a bare unfrozen String is an
    // fstring; a key with a singleton class or instance variables gets a
    // plain frozen copy, a frozen key is stored as it is, and an identity
    // Hash copies nothing.
    t(r##"
        t { {"k".dup => 1}.keys[0].singleton_class },
        t { {"k" => 1}.keys[0].singleton_class },
        t { h = {}; h["kk"] = 1; h.keys[0].singleton_class },
        t { h = {}; h.store("kk".dup, 1); h.keys[0].singleton_class },
        t { h = {}; h["kk".dup] = 1; h.keys[0].frozen? },
        t { h = {}.compare_by_identity; h["kk".dup] = 1; h.keys[0].singleton_class },
        t { s = "sing".dup; def s.x = 1; h = {s => 1}; [h.keys[0].singleton_class, h.keys[0].respond_to?(:x)] },
        t { s = "sing".dup; s.instance_variable_set(:@a, 1); h = {s => 1}; h.keys[0].singleton_class },
        t { "abc".dup.freeze.singleton_class },
        t { k = "abc".dup.freeze; h = {k => 1}; [h.keys[0].singleton_class, h.keys[0].equal?(k)] },
        t { k = Class.new(String).new("sub"); h = {k => 1}; [h.keys[0].equal?(k), h.keys[0].frozen?] },
        t { h = {"k".dup => 1}; h.keys[0].dup.singleton_class }
    "##);
}

#[test]
fn frozen_error_names_the_attached_object() {
    // `rb_class_modify_check` for a method or an ancestor; the class
    // itself (`rb_check_frozen`) for visibility, a constant or a variable.
    t(r##"
        t { o = Object.new.freeze; class << o; def y = 1; end },
        t { o = Object.new.freeze; class << o; include M; end },
        t { o = Object.new.freeze; class << o; prepend M; end },
        t { o = Object.new.freeze; class << o; attr_accessor :z; end },
        t { o = Object.new.freeze; class << o; alias zz to_s; end },
        t { o = Object.new.freeze; class << o; undef to_s; end },
        t { o = Object.new.freeze; o.instance_eval { def x = 1 } },
        t { o = Object.new.freeze; o.instance_eval("def x = 1") },
        t { o = Object.new.freeze; def o.x = 1 },
        t { o = Object.new.freeze; o.define_singleton_method(:x) {} },
        t { o = Object.new.freeze; o.extend(Comparable) },
        t { o = Object.new.freeze; o.singleton_class.send(:define_method, :x) {} },
        t { o = Object.new.freeze; o.singleton_class.send(:attr_reader, :x) },
        t { o = Object.new.freeze; o.singleton_class.send(:attr_writer, :x) },
        t { o = Object.new.freeze; o.singleton_class.send(:attr_accessor, :x) },
        t { o = Object.new.freeze; o.singleton_class.send(:alias_method, :x, :to_s) },
        t { o = Object.new.freeze; o.singleton_class.send(:remove_method, :x) },
        t { o = Object.new.freeze; o.singleton_class.send(:undef_method, :to_s) },
        t { o = Object.new.freeze; o.singleton_class.send(:include, M) },
        t { o = Object.new.freeze; o.singleton_class.send(:private, :to_s) },
        t { o = Object.new.freeze; o.singleton_class.send(:public, :to_s) },
        t { o = Object.new.freeze; o.singleton_class.const_set(:X, 1) },
        t { o = Object.new.freeze; o.singleton_class.class_variable_set(:@@x, 1) },
        t { o = Object.new.freeze; o.singleton_class.instance_variable_set(:@x, 1) },
        t { o = Object.new.freeze; o.singleton_class.frozen? },
        t { o = "s".dup.freeze; class << o; def y = 1; end },
        t { o = Object.new; o.singleton_class.freeze; def o.x = 1 },
        t { o = Object.new; o.singleton_class.freeze; o.frozen? }
    "##);
}

#[test]
fn frozen_class_and_module() {
    t(r##"
        t { c = Class.new.freeze; c.class_eval { def y = 1 } },
        t { c = Class.new.freeze; c.send(:define_method, :y) {} },
        t { c = Class.new.freeze; c.send(:attr_accessor, :y) },
        t { c = Class.new.freeze; c.send(:include, M) },
        t { c = Class.new.freeze; c.send(:alias_method, :y, :to_s) },
        t { c = Class.new.freeze; c.send(:undef_method, :to_s) },
        t { c = Class.new.freeze; c.send(:remove_method, :to_s) },
        t { c = Class.new.freeze; c.send(:private, :to_s) },
        t { c = Class.new.freeze; c.send(:private) },
        t { c = Class.new.freeze; c.send(:private_constant, :X) },
        t { c = Class.new.freeze; c.send(:ruby2_keywords, :to_s) },
        t { c = Class.new.freeze; class << c; def y = 1; end },
        t { c = Class.new.freeze; def c.y = 1 },
        t { m = Module.new.freeze; m.module_eval { def y = 1 } },
        t { m = Module.new.freeze; m.send(:include, M) }
    "##);
}

#[test]
fn string_instance_eval_on_a_receiver_without_a_singleton_class() {
    // The body runs; only a definition in it asks for the singleton
    // class.
    t(r##"
        t { 1.instance_eval("self + 1") }, t { :a.instance_eval("to_s") },
        t { nil.instance_eval("inspect") }, t { 1.5.instance_eval("floor") },
        t { (2**70).instance_eval("self - 1") },
        t { "lit".freeze.instance_eval("size") },
        t { 1.instance_eval("def zz = 1") },
        t { :a.instance_eval("def zz = 1") },
        t { 1.instance_eval("MAX rescue :none") },
        t { 1.instance_eval { def q3 = 3 } },
        t { 1.instance_exec { def q3 = 3 } },
        t { o = Object.new; o.instance_eval("def q = 1"); o.q },
        t { o = Object.new; o.instance_eval("def self.w = 1"); o.w }
    "##);
}
