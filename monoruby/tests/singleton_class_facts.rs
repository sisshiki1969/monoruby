//! Class proofs of heap objects vs. singleton classes.
//!
//! The JIT carries "the object in this slot is of class C" across calls,
//! and folds the class of a constant it loads. Giving the object a
//! singleton class (`def obj.m`, `extend`, `define_singleton_method`,
//! `instance_eval` with a string, `singleton_class`) changes its class
//! under that proof. Every scenario here does that from inside a call (or
//! to a folded constant) *after* the site has been compiled, and checks the
//! compiled continuation sees the new class — against CRuby. See
//! `doc/singleton_class_facts.md`.
//!
//! `run_test` repeats the code 25 times in one VM. The latch that stops
//! proofs of a class from being carried is per class and one-way, so a
//! scenario that builds its class with `Class.new` sees it flip on every
//! repetition, while one using a named class sees it flip once and then
//! runs latched.

extern crate monoruby;
use monoruby::tests::*;

/// A local proved to be an Array before a call; the callee gives it a
/// singleton `[]`. The inlined `Array#[]` must not survive the call.
#[test]
fn local_array_index_after_callee_singleton_def() {
    run_test(
        r##"
        def sc_h1(a, i)
          def a.[](i) = :s if i == 30
        end
        def sc_g1(i)
          a = [7]
          sc_h1(a, i)
          a[0]
        end
        res = []
        40.times { |i| res << sc_g1(i) }
        res.uniq
        "##,
    );
}

/// The same with a String and an inlined `+`.
#[test]
fn local_string_plus_after_callee_singleton_def() {
    run_test(
        r##"
        def sc_h2(s, i)
          def s.+(o) = :s if i == 30
        end
        def sc_g2(i)
          s = "x"
          sc_h2(s, i)
          s + "y"
        end
        res = []
        40.times { |i| res << sc_g2(i) }
        res.uniq
        "##,
    );
}

/// A folded `respond_to?` on a local after the callee extends it, with a
/// class that is fresh on every repetition.
#[test]
fn respond_to_after_callee_extend_fresh_class() {
    run_test(
        r##"
        c = Class.new
        m = Module.new { def tag = :m }
        ext = ->(o, i) { o.extend(m) if i == 30 }
        g = ->(i) { o = c.new; ext.(o, i); [o.respond_to?(:tag), (o.tag rescue :nm)] }
        res = []
        40.times { |i| res << g.(i) }
        res.uniq
        "##,
    );
}

/// `define_singleton_method` / `instance_eval "def"` / `def self.x` by a
/// callee on the receiver the caller holds.
#[test]
fn callee_attaches_to_callers_object() {
    run_test(
        r##"
        class ScFoo
          def attach(i) = (def self.tag = :sdef if i == 30)
          def dsm(i) = (define_singleton_method(:tag) { :dsm } if i == 30)
          def ie(i) = (instance_eval("def tag = :ie") if i == 30)
        end
        def sc_run(m, i)
          o = ScFoo.new
          o.send(m, i)
          [o.respond_to?(:tag), o.singleton_methods]
        end
        res = []
        %i[attach dsm ie].each { |m| 40.times { |i| res << [m, sc_run(m, i)] } }
        res.uniq
        "##,
    );
}

/// `self` gets a singleton method from a callee: the rest of the method
/// must see it (a folded `respond_to?` and a self call).
#[test]
fn self_attached_by_callee() {
    run_test(
        r##"
        class ScSelf
          def att(i) = (define_singleton_method(:hello2) { :s } if i >= 30)
          def run(i)
            att(i)
            [respond_to?(:hello2), (hello2 rescue :nm)]
          end
        end
        res = []
        40.times { |i| res << ScSelf.new.run(i) }
        res.uniq
        "##,
    );
}

/// `self` extended by a callee, over and over, on a named class: the
/// first repetition flips the latch, the later ones are compiled with it
/// set (a run-time class guard on `self` after the call).
#[test]
fn self_extended_after_latch() {
    run_test(
        r##"
        module ScM; def hello = :m; end
        class ScLatched
          def hello = :base
          def ext(i) = (extend(ScM) if i % 7 == 3)
          def run(i) = (ext(i); hello)
        end
        res = []
        60.times { |i| res << ScLatched.new.run(i) }
        res
        "##,
    );
}

/// A folded constant receiver given a singleton method with no call in
/// between: the compiled site resolved against the constant's old class.
#[test]
fn constant_receiver_singleton_def() {
    run_test(
        r##"
        class ScConst; def bar = :base; end
        Object.send(:remove_const, :SC_OBJ) if defined?(SC_OBJ)
        SC_OBJ = ScConst.new
        def sc_cg = [SC_OBJ.bar, SC_OBJ.respond_to?(:baz)]
        res = []
        40.times do |i|
          if i == 30
            def SC_OBJ.bar = :s
            def SC_OBJ.baz = :z
          end
          res << sc_cg
        end
        res.uniq
        "##,
    );
}

/// The proof lives in the frame of a method whose block is inlined, and
/// the call happens inside the block.
#[test]
fn outer_frame_local_across_call_in_inlined_block() {
    run_test(
        r##"
        def sc_h3(a, i)
          def a.[](i) = :s if i == 30
        end
        def sc_g3(i)
          a = [7]
          [1].each { sc_h3(a, i) }
          a[0]
        end
        res = []
        40.times { |i| res << sc_g3(i) }
        res.uniq
        "##,
    );
}

/// A loop (OSR) body carrying the proof across the call.
#[test]
fn loop_body_across_call() {
    run_test(
        r##"
        def sc_h4(a, i)
          def a.size = :s if i == 40
        end
        def sc_g4
          a = [1, 2]
          res = []
          i = 0
          while i < 60
            sc_h4(a, i)
            res << a.size
            i += 1
          end
          res.uniq
        end
        sc_g4
        "##,
    );
}

/// A frozen object's `singleton_class` changes its class but can never
/// change its behaviour; and a clone copies an existing singleton class.
/// Both must stay correct either way.
#[test]
fn frozen_singleton_class_and_clone() {
    run_test(
        r##"
        class ScFrozen; def v = :base; end
        def sc_h5(f, o, i)
          f.singleton_class if i == 30
          o.clone if i == 31
        end
        def sc_g5(i)
          f = ScFrozen.new.freeze
          o = Object.new
          def o.x = 1
          sc_h5(f, o, i)
          [f.v, f.frozen?, f.respond_to?(:v), o.x]
        end
        res = []
        40.times { |i| res << sc_g5(i) }
        res.uniq
        "##,
    );
}
