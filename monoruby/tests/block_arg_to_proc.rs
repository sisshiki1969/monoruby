//! `m(&obj)` with an `obj` that is neither `nil`, a Proc nor a Symbol
//! calls `obj.to_proc` at the call site, once per call, whether or not
//! the callee uses the block, as CRuby does (#2277).
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn to_proc_runs_at_the_call_site() {
    run_test(
        r##"
        $log = []
        o = Object.new
        def o.to_proc = ($log << :to_proc; proc { |x| x * 2 })
        def none = :none
        def one = yield(4)
        def take(&b) = b
        res = []
        i = 0
        while i < 30
          res << none(&o) << one(&o) << take(&o).call(5)
          i += 1
        end
        [res.uniq, $log.size]
        "##,
    );
}

#[test]
fn to_proc_errors_at_the_call_site() {
    run_test(
        r##"
        def none = :none
        o1 = Object.new
        def o1.to_proc = 1
        o2 = Object.new
        def o2.to_proc = nil
        mm = Object.new
        def mm.method_missing(n, *a) = n == :to_proc ? nil : super
        res = []
        [o1, o2, mm, Object.new, BasicObject.new, 1, 1.5, true, false, "s", [1], 2**70].each do |v|
          res << begin; none(&v); rescue TypeError => e; e.message; end
        end
        r = Object.new
        def r.to_proc = raise("boom")
        res << begin; none(&r); rescue => e; e.message; end
        res
        "##,
    );
}

#[test]
fn to_proc_protocol() {
    run_test(
        r##"
        def take(&b) = b
        def none = :none
        # method_missing, with and without respond_to_missing?
        mm = Object.new
        def mm.respond_to_missing?(n, p = false) = n == :to_proc || super
        def mm.method_missing(n, *a) = n == :to_proc ? proc { :mm } : super
        mm2 = Object.new
        def mm2.method_missing(n, *a) = n == :to_proc ? proc { :mm2 } : super
        # private, and a respond_to? that says no
        pv = Object.new
        class << pv; private def to_proc = proc { :priv }; end
        rt = Object.new
        def rt.respond_to?(*) = false
        def rt.to_proc = proc { :rt }
        # a lambda, a Proc subclass, a Hash, a Method
        l = Object.new
        def l.to_proc = lambda { |a, b| [a, b] }
        class MyProc < Proc; end
        mp = MyProc.new { :my }
        def foo(x) = x * 3
        m = method(:foo)
        pr = proc { :pr }
        o3 = Object.new
        o3.define_singleton_method(:to_proc) { pr }
        [take(&mm).call, take(&mm2).call, take(&pv).call, take(&rt).call,
         take(&l).lambda?, take(&mp).class, take(&mp).call, [1, 2].map(&{1 => :a}),
         take(&m).lambda?, [1, 2].map(&m), take(&o3).equal?(pr), take(&nil),
         none(&mm), none(&m)]
        "##,
    );
}

#[test]
fn to_proc_of_builtin_classes() {
    run_test_once(
        r##"
        class Integer; def to_proc = proc { :int }; end
        class String; def to_proc = proc { :str }; end
        def take(&b) = b
        res = []
        30.times { res << take(&1).call << take(&"x").call }
        res.uniq
        "##,
    );
}

#[test]
fn refined_to_proc() {
    run_test(
        r##"
        class Foo; end
        module R
          refine Foo do
            def to_proc = proc { :refined }
          end
        end
        def take(&b) = b
        module M
          using R
          def self.run = take(&Foo.new).call
        end
        [M.run, (begin; take(&Foo.new); rescue TypeError => e; e.message; end)]
        "##,
    );
}

// The JIT passes a Proc or `nil` inline and calls only for anything
// else; a Float live across the call survives it.
#[test]
fn to_proc_mixed_values_in_a_compiled_loop() {
    run_test(
        r##"
        def take(&b) = b ? b.call(2) : :none
        o = Object.new
        def o.to_proc = proc { |x| x + 100 }
        def foo(x) = x * 3
        vals = [proc { |x| x }, nil, o, method(:foo), :odd?, lambda { |x| -x }]
        res = []
        f = 1.5
        i = 0
        while i < 60
          v = vals[i % vals.size]
          res << take(&v) << f
          f += 0.5
          i += 1
        end
        res
        "##,
    );
}

// `to_proc` redefines a basic operator, defines a method and assigns a
// constant while a compiled caller runs: each is seen afterwards.
#[test]
fn to_proc_redefines_while_a_compiled_loop_runs() {
    run_test_once(
        r##"
        $n = 0
        o = Object.new
        def o.to_proc
          $n += 1
          if $n == 40
            Integer.class_eval { def -(o) = 100 }
            Object.class_eval { def g = :new_g }
            Object.send(:remove_const, :K)
            Object.const_set(:K, 2)
          end
          proc { |x| x }
        end
        def g = :g
        K = 1
        def m = yield(3)
        def loop1(o)
          res = []
          i = 0
          while i < 60
            x = m(&o)
            res << (x - 1) << g << K
            i += 1
          end
          res
        end
        loop1(o).each_slice(3).to_a.uniq
        "##,
    );
}
