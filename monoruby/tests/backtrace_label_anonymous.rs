extern crate monoruby;
use monoruby::tests::*;

// Since Ruby 3.4 a backtrace label names the method's owner only when the
// owner has a *permanent* name (`rb_gen_method_name`): a method of an
// anonymous class or module, of a class nested under an anonymous one, or
// of a class that only has a temporary name, is labelled with the bare
// method name. monoruby printed the owner's `inspect` form there —
// `#<Class:0x…>#foo` (#1983).

#[test]
fn anonymous_owner_labels() {
    run_test_once(
        r##"
        def bl_lab
          yield
        rescue => e
          l = e.backtrace_locations[0]
          [l.label, l.base_label, e.backtrace[0].sub(/\A.*:in /, "")]
        end
        r = []
        c = Class.new do
          def foo = raise("z")
          def self.sfoo = raise("z")
          def blk = [1].each { raise "z" }
          def cl = caller_locations(0)[0].label
          def c0 = caller(0)[0].sub(/\A.*:in /, "")
        end
        m = Module.new do
          def mfoo = raise("z")
          def self.msfoo = raise("z")
        end
        r << bl_lab { c.new.foo }
        r << bl_lab { c.sfoo }
        r << bl_lab { c.new.blk }
        r << bl_lab { Class.new { include m }.new.mfoo }
        r << bl_lab { m.msfoo }
        r << c.new.cl << c.new.c0
        # Nested under an anonymous class: a name, but not a permanent one.
        c.const_set(:Inner, Class.new { def bar = raise("z"); def self.sbar = raise("z") })
        r << c::Inner.name.sub(/0x\h+/, "0x") << bl_lab { c::Inner.new.bar } << bl_lab { c::Inner.sbar }
        # A temporary name is not one either.
        t = Class.new { def baz = raise("z"); def self.sbaz = raise("z") }
        t.set_temporary_name("fake<name>")
        r << t.name << bl_lab { t.new.baz } << bl_lab { t.sbaz }
        # An anonymous Struct class.
        r << bl_lab { Struct.new(:a) { def sm = raise("z") }.new(1).sm }
        # The whole backtrace: only the anonymous owner's frames change.
        begin
          c.new.blk
        rescue => e
          r << e.backtrace.map { _1.sub(/\A.*:in /, "") }
        end
        r
        "##,
    );
}

#[test]
fn owner_named_later_is_labelled() {
    run_test_once(
        r##"
        def bl_lab
          yield
        rescue => e
          l = e.backtrace_locations[0]
          [l.label, l.base_label]
        end
        r = []
        c = Class.new do
          def foo = raise("z")
          def self.sfoo = raise("z")
          def blk = [1].each { raise "z" }
        end
        c.const_set(:Inner, Class.new { def bar = raise("z"); def self.sbar = raise("z") })
        r << bl_lab { c.new.foo } << bl_lab { c::Inner.new.bar }
        # Assigning the class to a constant makes its name, and the names
        # nested under it, permanent.
        BlNamed = c
        r << bl_lab { c.new.foo } << bl_lab { c.sfoo } << bl_lab { c.new.blk }
        r << bl_lab { c::Inner.new.bar } << bl_lab { c::Inner.sbar }
        r
        "##,
    );
}

#[test]
fn named_and_singleton_owner_labels() {
    run_test_once(
        r##"
        def bl_lab
          yield
        rescue => e
          l = e.backtrace_locations[0]
          [l.label, l.base_label]
        end
        class BlPerm
          def i = raise("z")
          def self.s = raise("z")
          class << self
            class << self
              def meta = raise("z")
            end
          end
          class Deep
            def d = raise("z")
          end
        end
        BlS = Struct.new(:a) { def sm = raise("z") }
        module BlMf
          module_function
          def mfn = raise("z")
        end
        r = []
        r << bl_lab { BlPerm.new.i } << bl_lab { BlPerm.s } << bl_lab { BlPerm::Deep.new.d }
        # The singleton class of a singleton class has no name at all.
        r << bl_lab { BlPerm.singleton_class.meta }
        # Nor does a plain object's singleton class.
        o = Object.new
        def o.sing = raise("z")
        r << bl_lab { o.sing }
        r << bl_lab { BlS.new(1).sm }
        r << bl_lab { BlMf.mfn } << bl_lab { Class.new { include BlMf; def call = mfn }.new.call }
        # An anonymous module with `module_function`: bare both ways.
        mf = Module.new { module_function; def amfn = raise("z") }
        r << bl_lab { mf.amfn }
        # Builtin owners are permanent.
        r << bl_lab { Integer("x") } << bl_lab { [1].fetch(5) }
        r
        "##,
    );
}
