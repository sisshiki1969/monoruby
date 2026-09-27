//! Self-generic method bodies (`MONORUBY_GENERIC_SELF`).
//!
//! Once a method has been specialized for the threshold number of self
//! classes, its next whole-method compile produces one body that runs for
//! any *self*: instance variables go through the global `(class, name)`
//! slot table instead of a baked `IvarId`, and the body ends the
//! class-guard chain unguarded. These tests drive a method through many
//! classes whose ivar layouts disagree, so a slot resolved for the wrong
//! class shows up as a wrong value.

extern crate monoruby;
use monoruby::tests::*;

fn enable() {
    // SAFETY: every test in this binary sets the same value before its
    // first JIT compile; the threshold is read once per thread.
    unsafe { std::env::set_var("MONORUBY_GENERIC_SELF", "2") };
}

/// Classes that assign their ivars in different orders, so `@v` and `@w`
/// sit at different slots in each. One method reads and writes both.
#[test]
fn generic_body_resolves_ivar_slots_per_class() {
    enable();
    run_test(
        r##"
        class Base
          def bump(n); @v += n; @w = @v * 2; self; end
          def val; @v + @w; end
          def unset; @never_assigned; end
        end
        classes = 8.times.map do |i|
          Class.new(Base) do
            define_method(:initialize) do |v|
              i.times { |k| instance_variable_set("@pad#{k}", k) }
              if i.even? then @w = 0; @v = v else @v = v; @w = 0 end
            end
          end
        end
        objs = classes.each_with_index.map { |c, i| c.new(i) }
        sum = 0
        200.times do |k|
          o = objs[k % objs.size]
          sum += o.bump(1).val
          sum += 1 if o.unset.nil?
        end
        [sum, objs.map(&:val)]
        "##,
    );
}

/// A store into a frozen *self* raises from the generic body, and an
/// immediate *self* reads `nil`.
#[test]
fn generic_body_frozen_and_immediate_self() {
    enable();
    run_test(
        r##"
        module Tag
          def set_tag(t); @tag = t; end
          def tag; @tag; end
        end
        classes = 6.times.map { Class.new { include Tag } }
        [Integer, Symbol].each { |c| c.include(Tag) }
        res = []
        objs = classes.map(&:new)
        40.times do |k|
          objs.each_with_index { |o, i| o.set_tag(i + k) }
          res << objs.map(&:tag).sum
          res << 1.tag << :a.tag
        end
        f = classes[0].new.freeze
        res << (begin; f.set_tag(1); rescue FrozenError; :frozen; end)
        res
        "##,
    );
}

/// Blocks inside a generic body see the same unknown *self*, and a call on
/// *self* from the generic body dispatches per receiver class.
#[test]
fn generic_body_blocks_and_self_calls() {
    enable();
    run_test(
        r##"
        class Base
          def items; [1, 2, 3].map { |x| x * factor + (@off ||= 0) }; end
          def factor; 1; end
        end
        classes = 8.times.map do |i|
          Class.new(Base) do
            define_method(:factor) { i + 1 }
            define_method(:initialize) { @off = i; @extra = -i }
          end
        end
        objs = classes.map(&:new)
        30.times.map { objs.map(&:items) }.last
        "##,
    );
}

/// `super` in a generic body: its target depends on self's ancestry, so
/// the body resolves it at run time (a generic send) instead of deopting
/// on every class outside its dispatch arms. The module sits over bases
/// whose `init` differ, and some classes insert a second module between,
/// so the same `super` reaches different targets. zsuper forwards the
/// parameters (optional and rest included); explicit `super(...)` and a
/// block passed through ride along.
#[test]
fn generic_body_super_resolves_per_receiver() {
    enable();
    run_test(
        r##"
        class BaseA
          def init(a, b = 10, *r); @log = [:a, a, b, r]; yield(a) if block_given?; self; end
          def pick(x) = [:a, x]
        end
        class BaseB
          def init(a, b = 20, *r); @log = [:b, a, b, r]; self; end
          def pick(x) = [:b, x * 2]
        end
        module Mid
          def init(a, b = 30, *r); super; @log << :mid; self; end
          def pick(x) = super(x + 100)
        end
        module M
          def init(a, b = 40, *r)
            @m = a
            super
            @log << :m
            self
          end
          def pick(x) = super(x) + [:m]
          def log = @log
        end
        classes = 8.times.map do |i|
          base = i.even? ? BaseA : BaseB
          Class.new(base) do
            include Mid if i % 3 == 0
            include M
          end
        end
        res = []
        seen = 0
        200.times do |k|
          c = classes[k % classes.size]
          o = c.new
          o.init(k, k + 1, k + 2) { |v| seen += v }
          res << o.log
          res << o.pick(k)
          res << c.new.init(k).log
        end
        [res.last(48), seen, res.size]
        "##,
    );
}

/// A method whose own body appears twice in the receiver's ancestry
/// (included at two levels): each `super` reaches the next occurrence, so
/// the target depends on the frame, never just the class.
#[test]
fn generic_body_super_repeated_ancestor() {
    enable();
    run_test(
        r##"
        module Twice
          def depth(n) = n > 5 ? [:end] : [n] + super(n + 1)
        end
        class Root
          def depth(n) = [:root, n]
        end
        classes = 6.times.map do |i|
          mid = Class.new(Root) { include Twice }
          Class.new(mid) do
            define_method(:tag) { i }
            prepend(Module.new { def depth(n) = [:p] + super })
            include Twice if i.odd?
          end
        end
        out = []
        300.times { |k| out << classes[k % classes.size].new.depth(0) }
        out.last(12)
        "##,
    );
}
