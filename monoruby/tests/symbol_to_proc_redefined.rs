//! `&:sym` once `Symbol#to_proc` is redefined: the block is what the
//! user's `to_proc` answers, as in CRuby (#2180). A refinement of
//! `Symbol#to_proc` is not consulted (CRuby asks the method table
//! without one).
//!
//! Each test redefines `Symbol#to_proc` for good, so each is one run.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn redefined_symbol_to_proc_is_called() {
    run_test_once(
        r##"
        class Symbol
          def to_proc = proc { |x| 42 }
        end
        def m = yield(3)
        def blk(&b) = b
        [[1, 2].map(&:even?), m(&:odd?), blk(&:odd?).call(5)]
        "##,
    );
}

#[test]
fn redefined_symbol_to_proc_after_compile() {
    run_test_once(
        r##"
        def m = yield(3)
        def f = m(&:odd?)
        def g = [1, 2].map(&:even?)
        res = []
        30.times { res << [f, g] }
        class Symbol
          def to_proc = proc { |x| 42 }
        end
        30.times { res << [f, g] }
        class Symbol
          remove_method :to_proc
        end
        r = begin; f; rescue TypeError => e; e.message; end
        [res.uniq, r]
        "##,
    );
}

// A callee redefines it while a compiled loop runs.
#[test]
fn redefined_symbol_to_proc_inside_a_compiled_loop() {
    run_test_once(
        r##"
        def m = yield(3)
        def redef
          Symbol.class_eval { def to_proc = proc { |x| 42 } }
        end
        def loop1
          res = []
          i = 0
          while i < 60
            redef if i == 40
            res << m(&:odd?)
            i += 1
          end
          res.uniq
        end
        loop1
        "##,
    );
}

#[test]
fn prepended_symbol_to_proc() {
    run_test_once(
        r##"
        module P
          def to_proc = proc { |x| :prep }
        end
        Symbol.prepend P
        [1].map(&:even?)
        "##,
    );
}

#[test]
fn redefined_symbol_to_proc_errors() {
    run_test_once(
        r##"
        class Symbol
          def to_proc
            raise "boom" if self == :boom
            return 1 if self == :one
            return lambda { |x| [:lam, x] } if self == :lam
            s = self
            proc { |x| [:user, s, x] }
          end
        end
        def m(&b) = b
        res = []
        res << [1].map(&:foo)
        res << [1].map(&:lam)
        res << m(&:lam).lambda?
        res << (begin; [1].map(&:boom); rescue => e; e.message; end)
        res << (begin; m(&:one); rescue TypeError => e; e.message; end)
        res
        "##,
    );
}

#[test]
fn refined_symbol_to_proc_is_not_consulted() {
    run_test_once(
        r##"
        module R
          refine Symbol do
            def to_proc = proc { |x| :refined }
          end
        end
        using R
        def m = yield(3)
        res = []
        30.times { res << [[1, 2].map(&:even?), m(&:odd?)] }
        res.uniq
        "##,
    );
}

// Refinements active at the `&:sym` site, then a redefinition: the
// refined method was what the bare Symbol called, the user's `to_proc`
// is what is called after.
#[test]
fn redefined_symbol_to_proc_with_refinements_active() {
    run_test_once(
        r##"
        module R
          refine Integer do
            def foo = :refined_foo
          end
        end
        using R
        def run = [1, 2].map(&:foo)
        res = []
        30.times { res << run }
        class Symbol
          def to_proc = proc { |x| :user }
        end
        30.times { res << run }
        res.uniq
        "##,
    );
}
