//! Method-table changes invalidate compiled code by *name*, not globally.
//!
//! A compiled unit guards its resolved calls with the JIT's own class
//! version word, and a definition, removal or visibility change poisons
//! only the units the method-name index says resolved that name
//! (`Store::jit_method_changed`). Every scenario here defines, removes or
//! re-scopes a method *after* a site calling it has been compiled, and
//! checks the next call sees the change — against CRuby.
//!
//! `run_test` repeats the code 25 times in one VM, so every scenario
//! builds its classes afresh (`Class.new` / `Module.new`): a change made
//! in one repetition must not leak into the next.

extern crate monoruby;
use monoruby::tests::*;

/// A singleton method defined on an object *after* a compiled site called
/// the object's class method: the site must now dispatch to the singleton.
#[test]
fn singleton_def_after_compiled_call() {
    run_test(
        r##"
        c = Class.new { def m; :class; end }
        o = c.new
        res = []
        40.times { res << o.m }
        def o.m; :singleton; end
        40.times { res << o.m }
        res.uniq
        "##,
    );
}

/// A parent-class redefinition reached through a subclass receiver.
#[test]
fn parent_redefinition_seen_through_subclass_receiver() {
    run_test(
        r##"
        p = Class.new { def m; 1; end }
        q = Class.new(p).new
        res = []
        40.times { res << q.m }
        p.class_eval { def m; 2; end }
        40.times { res << q.m }
        res.uniq
        "##,
    );
}

/// A method defined in the subclass *shadows* the inherited one a compiled
/// site had resolved.
#[test]
fn subclass_shadowing_after_compile() {
    run_test(
        r##"
        p = Class.new { def m; :parent; end }
        qc = Class.new(p)
        q = qc.new
        res = []
        40.times { res << q.m }
        qc.class_eval { def m; :child; end }
        40.times { res << q.m }
        res.uniq
        "##,
    );
}

/// `remove_method` in the subclass exposes the inherited method again;
/// `undef_method` then makes the call raise.
#[test]
fn remove_and_undef_after_compile() {
    run_test(
        r##"
        p = Class.new { def m; :parent; end }
        qc = Class.new(p) { def m; :child; end }
        q = qc.new
        res = []
        40.times { res << q.m }
        qc.send(:remove_method, :m)
        40.times { res << q.m }
        qc.send(:undef_method, :m)
        res << (q.m rescue :undefined)
        res.uniq
        "##,
    );
}

/// `define_method` and `alias_method` install under a name a compiled site
/// resolved to `method_missing`.
#[test]
fn method_missing_then_define() {
    run_test(
        r##"
        c = Class.new do
          def method_missing(name, *a); name == :m ? :missing : super; end
          def respond_to_missing?(name, priv = false); name == :m || super; end
        end
        o = c.new
        res = []
        40.times { res << o.m }
        c.define_method(:m) { :defined }
        40.times { res << o.m }
        c.alias_method(:m, :object_id)
        res << (o.m == o.object_id)
        res.uniq
        "##,
    );
}

/// A `super` site: the grandparent's method is redefined after the
/// compile. The site depends on the enclosing method's *name*.
#[test]
fn super_site_sees_ancestor_redefinition() {
    run_test(
        r##"
        g = Class.new { def m; :g1; end }
        p = Class.new(g) { def m; super; end }
        q = Class.new(p) { def m; super; end }.new
        res = []
        40.times { res << q.m }
        g.class_eval { def m; :g2; end }
        40.times { res << q.m }
        res.uniq
        "##,
    );
}

/// A visibility change makes a compiled public call raise, and back.
#[test]
fn visibility_change_after_compile() {
    run_test(
        r##"
        c = Class.new { def m; :ok; end }
        o = c.new
        res = []
        40.times { res << o.m }
        c.send(:private, :m)
        res << (o.m rescue :private)
        c.send(:public, :m)
        40.times { res << o.m }
        res.uniq
        "##,
    );
}

/// `include` / `prepend` after the compile (the coarse path): the
/// module's method now shadows the one the site had resolved.
#[test]
fn include_and_prepend_after_compile() {
    run_test(
        r##"
        p = Class.new { def m; :parent; end }
        inc = Module.new { def m; :module; end }
        pre = Module.new { def m; :prepended; end }
        qc = Class.new(p)
        q = qc.new
        res = []
        40.times { res << q.m }
        qc.include(inc)
        40.times { res << q.m }
        qc.prepend(pre)
        40.times { res << q.m }
        res.uniq
        "##,
    );
}

/// `extend` on the receiver itself after the compile.
#[test]
fn extend_after_compile() {
    run_test(
        r##"
        c = Class.new { def m; :class; end }
        ext = Module.new { def m; :extended; end }
        o = c.new
        res = []
        40.times { res << o.m }
        o.extend(ext)
        40.times { res << o.m }
        res.uniq
        "##,
    );
}

/// The htmlentities shape: a fresh object per iteration gets singleton
/// methods via `instance_eval`; the caller sees each object's own.
#[test]
fn per_object_singleton_methods_stay_correct() {
    run_test(
        r##"
        enc = Class.new do
          def initialize(i)
            @i = i
            instance_eval "def tag; :t#{@i}; end"
          end
        end
        res = []
        60.times { |i| res << enc.new(i % 3).tag }
        res.uniq.sort
        "##,
    );
}

/// An OSR loop body, its callee redefined mid-way.
#[test]
fn loop_unit_sees_redefinition() {
    run_test(
        r##"
        c = Class.new { def m; 1; end }
        o = c.new
        s = 0
        i = 0
        while i < 400
          s += o.m
          c.class_eval { def m; 2; end } if i == 200
          i += 1
        end
        s
        "##,
    );
}

/// Redefining a name a compiled unit never resolved leaves that unit
/// alone (nothing to observe but the answers), and the redefined name is
/// seen where it *is* called.
#[test]
fn unrelated_definition_is_harmless() {
    run_test(
        r##"
        c = Class.new { def m; :m; end; def n; :n1; end }
        o = c.new
        res = []
        40.times { res << o.m }
        c.class_eval { def n; :n2; end }
        40.times { res << o.m; res << o.n }
        res.uniq
        "##,
    );
}

/// `super` inside a `define_method` body resolves under the name the body
/// was installed as, not the name of the method whose block it is; such a
/// unit is filed under every name, so the ancestor's redefinition reaches
/// it.
#[test]
fn super_in_define_method_block_sees_ancestor_redefinition() {
    run_test(
        r##"
        g = Class.new { def m; :g1; end }
        p = Class.new(g)
        p.class_eval do
          define_method(:m) { super() }
        end
        q = p.new
        res = []
        40.times { res << q.m }
        g.class_eval { def m; :g2; end }
        40.times { res << q.m }
        res.uniq
        "##,
    );
}

/// A site compiled as the `method_missing` dispatch (the receiver's class
/// had no such method) must be reached by a later definition of the
/// name: each iteration builds a fresh object and defines the method on
/// it *after* the site was compiled against a class without it.
#[test]
fn missing_method_site_sees_definition() {
    run_test(
        r##"
        res = []
        40.times do
          obj = Object.new
          def (obj).qux; 4; end
          res << obj.qux
        end
        c = Class.new do
          def method_missing(name, *a); name == :zap ? :missing : super; end
          def respond_to_missing?(name, priv = false); name == :zap || super; end
        end
        o = c.new
        40.times { res << o.zap }
        c.class_eval { def zap; :defined; end }
        40.times { res << o.zap }
        res.uniq
        "##,
    );
}

/// `class << obj` on an object whose class the unit had proved (it was
/// just allocated there): the singleton class it creates is the object's
/// class from then on, so a call after it must not resolve against the
/// old class.
#[test]
fn singleton_class_body_after_known_allocation() {
    run_test(
        r##"
        res = []
        40.times do
          obj = Object.new
          class << obj
            def tag; :sc; end
          end
          res << obj.tag
          sc = obj.singleton_class
          res << (sc == obj.singleton_class)
        end
        res.uniq
        "##,
    );
}
