//! A call site whose method does not exist dispatches `method_missing`
//! from compiled code, instead of leaving the JIT on every execution.
//!
//! The compiled form is guarded by the receiver class and the class
//! version, so everything that can make the name resolve — a definition on
//! the class or an ancestor, `define_method`, another receiver class —
//! has to take the call back off it.

extern crate monoruby;
use monoruby::tests::*;

/// The dispatch itself, from a loop hot enough to compile: the name, the
/// arguments, a block, and keywords all arrive as they do in the
/// interpreter.
#[test]
fn method_missing_from_compiled_code() {
    run_test(
        r##"
        class MM
          def method_missing(name, *args, **kw, &blk)
            [name, args, kw, blk ? blk.call : nil]
          end
          def respond_to_missing?(*) = true
        end
        m = MM.new
        res = []
        i = 0
        while i < 60
          res << m.whatever
          res << m.whatever(1, 2)
          res << m.whatever(i % 2)
          res << m.whatever(k: 1)
          res << (m.whatever { :from_block })
          res << m.whatever(*[3, 4])
          i += 1
        end
        res.uniq
        "##,
    );
}

/// Defining the method afterwards has to take the call site off the
/// compiled `method_missing` — the class version guard is what does it.
#[test]
fn a_later_definition_takes_over() {
    run_test_once(
        r##"
        class Late
          def method_missing(name, *args) = [:missing, name]
        end
        o = Late.new
        before = []
        i = 0
        while i < 60
          before << o.arrives
          i += 1
        end
        class Late
          def arrives = :defined
        end
        after = []
        i = 0
        while i < 60
          after << o.arrives
          i += 1
        end
        # ... and removing it again goes back to method_missing.
        class Late
          remove_method :arrives
        end
        again = []
        i = 0
        while i < 60
          again << o.arrives
          i += 1
        end
        [before.uniq, after.uniq, again.uniq]
        "##,
    );
    // The definition can arrive on a superclass, or through define_method.
    run_test_once(
        r##"
        class Base; end
        class Sub < Base
          def method_missing(name, *args) = [:missing, name]
        end
        o = Sub.new
        before = []
        i = 0
        while i < 60
          before << o.later
          i += 1
        end
        Base.define_method(:later) { :from_base }
        after = []
        i = 0
        while i < 60
          after << o.later
          i += 1
        end
        [before.uniq, after.uniq]
        "##,
    );
}

/// One call site, two receiver classes, only one of which has the method:
/// the receiver class guard keeps them apart.
#[test]
fn one_site_with_and_without_the_method() {
    run_test(
        r##"
        class Has
          def poke(x) = [:has, x]
        end
        class Hasnt
          def method_missing(name, *args) = [:hasnt, name, args]
        end
        objs = [Has.new, Hasnt.new]
        res = []
        i = 0
        while i < 60
          objs.each { |o| res << o.poke(1) }
          i += 1
        end
        res.uniq
        "##,
    );
}

/// With no `method_missing` anywhere, the compiled call raises what the
/// interpreter raises — including the wording and the class, which differ
/// between a receiver-qualified call (NoMethodError) and a receiverless
/// one (NameError, "undefined local variable or method").
#[test]
fn without_method_missing_the_error_matches() {
    run_test(
        r##"
        class Plain
          def call_it
            i = 0
            msg = nil
            while i < 60
              begin
                self.no_such_method
              rescue NoMethodError => e
                msg = [e.class.to_s, e.message]
              end
              i += 1
            end
            msg
          end
          def vcall_it
            i = 0
            msg = nil
            while i < 60
              begin
                no_such_method
              rescue NameError => e
                # The receiver's `inspect` carries its address; the part
                # before it is what this test is about.
                msg = [e.class.to_s, e.message.sub(/ for .*/, "")]
              end
              i += 1
            end
            msg
          end
        end
        [Plain.new.call_it, Plain.new.vcall_it]
        "##,
    );
}

/// `super` with no super method reaches `method_missing` too — it names
/// the running method rather than a call-site name, so it stays on the
/// interpreter's path; it must still answer.
#[test]
fn super_without_a_super_method() {
    run_test(
        r##"
        class Parent
          def method_missing(name, *args) = [:missing, name, args]
        end
        class Child < Parent
          def orphan(x)
            i = 0
            r = nil
            while i < 60
              r = super
              i += 1
            end
            r
          end
        end
        Child.new.orphan(7)
        "##,
    );
}
