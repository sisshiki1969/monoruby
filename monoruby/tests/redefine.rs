extern crate monoruby;
use monoruby::tests::*;

#[test]
fn redefine_test1() {
    run_test_once(
        r##"
        a = [100 * 100]
        class Integer
          def *(other)
            42
          end
        end
        a << 100 * 100
        a
        "##,
    );
}

#[test]
fn redefine_test2() {
    run_test_once(
        r##"
        res = []
        50.times do |x|
          res << 100 * 100
          if x == 25
            class Integer
              def *(other)
                42
              end
            end
          end
        end
        res
        "##,
    );
}

#[test]
fn redefine_test3() {
    run_test_once(
        r##"
        a = 0
        20.times do |x|
          20.times do |y|
            20.times do |z|
              s = if x == 18 && y == 18 && z == 18
                "def *(other); 42; end;"
              else
                ""
              end
              Integer.class_eval(s)
              a += 100 * 100
            end
            a += 100 * 100
          end
          a += 100 * 100
        end
        a
        "##,
    );
}

#[test]
fn redefine_test4() {
    run_test_once(
        r##"
        a = 0
        for x in 0..20
          for y in 0..20
            for z in 0..20
              s = if x == 18 && y == 18 && z == 18
                "def *(other); 42; end;"
              else
                ""
              end
              Integer.class_eval(s)
              a += 100 * 100
            end
            a += 100 * 100
          end
          a += 100 * 100
        end
        a
        "##,
    );
}

// Regression: a basic-op redefinition must be observed by an *already
// JIT-compiled* method that was **off the stack** at redefinition time.
//
// The pre-existing `redefine_test*` above only redefine `Integer#*` and use
// `100 * 100`, a compile-time **fold**, while the redefining `class Integer`
// body runs *inside* the warm-up loop (on-stack) — so they were satisfied on
// aarch64 by the fold-only `CheckBOP` guard + on-stack `immediate_eviction`,
// and never exercised the **non-fold** inline integer add of an off-stack
// method. That path carries no per-op guard on either arch, so correctness
// relies entirely on `set_bop_redefine` reverting every compiled method to the
// VM: x86 rewrites each entry to `vm_entry`; aarch64 zeroes each method's
// dispatch slot in `invalidate_jit_code`. Before slot-zeroing, aarch64 kept
// dispatching an off-stack method into its stale JIT body and returned the
// pre-redefinition result (e.g. `5 + 3 == 8` instead of the redefined `111`).
//
// `run_test` doubles as the guard: it evaluates the snippet 25× in one process
// and `__assert`s run 1 (which JIT-compiles then redefines off-stack) against
// later VM runs, so a divergence between the stale JIT body and the VM fails
// the test.
#[test]
fn redefine_bop_offstack_nonfold() {
    run_test(
        r##"
        def add_fold; 5 + 3; end
        def add_dyn(a, b); a + b; end
        res = []
        50.times { add_fold; add_dyn(5, 3) }
        class Integer
          def +(o); 111; end
        end
        res << add_fold          # folded 5+3, must observe redefine -> 111
        res << add_dyn(5, 3)     # non-fold inline add, off-stack -> 111
        res
        "##,
    );
}

#[test]
fn redefine_bop_offstack_ops() {
    run_test(
        r##"
        def add_fold; 5 + 3; end
        def mul_fold; 100 * 100; end
        def sub_fold; 10 - 4; end
        def add_dyn(a, b); a + b; end
        def mul_dyn(a, b); a * b; end
        def sub_dyn(a, b); a - b; end
        res = []
        50.times { add_fold; mul_fold; sub_fold; add_dyn(5,3); mul_dyn(100,100); sub_dyn(10,4) }
        class Integer
          def +(o); 111; end
          def *(o); 222; end
          def -(o); 333; end
        end
        res << add_fold << mul_fold << sub_fold
        res << add_dyn(5,3) << mul_dyn(100,100) << sub_dyn(10,4)
        res
        "##,
    );
}

// The dispatch slot is per receiver class: an inherited method compiled for
// several subclasses builds a guard *chain* of slots (one HashMap entry each).
// A BOP redefinition must zero **every** slot, not just the head class's.
#[test]
fn redefine_bop_polymorphic_chain() {
    run_test(
        r##"
        class BopBase; def calc(a, b); a + b; end; end
        class BopS1 < BopBase; end
        class BopS2 < BopBase; end
        s1 = BopS1.new
        s2 = BopS2.new
        50.times { s1.calc(5, 3); s2.calc(5, 3) }
        class Integer
          def +(o); 111; end
        end
        [s1.calc(5, 3), s2.calc(5, 3)]
        "##,
    );
}

// Regression (bug B): an off-stack method that stays VM-interpreted but has an
// OSR/loop-JIT'd hot loop must not re-enter the stale loop body after a
// basic-op redefinition. The compiled loop entry lives in the bytecode
// (`BytecodePtr::write2`, `[pc+8]`) and the VM's `loop_start` handler branches
// to it with no version check, and nothing reverts OSR bytecode-PC entries —
// so the protection is the `remove_vm_bop_optimization` dispatch-table swap
// that replaces `loop_start` with the no-opt handler (plain advance+dispatch),
// making stale OSR entries unreachable. x86 always had that swap
// (`dispatch[14] = vm_loop_start_no_opt`); the aarch64
// `remove_vm_bop_optimization` was missing it, so `x * x` here kept returning
// the pre-redefinition product (14700 instead of 300000).
#[test]
fn redefine_bop_offstack_osr_loop() {
    run_test(
        r##"
        def osr_sum(x)
          total = 0
          i = 0
          while i < 300
            total = total + (x * x)
            i = i + 1
          end
          total
        end
        osr_sum(7)
        class Integer
          def *(o); 1000; end
        end
        osr_sum(7)
        "##,
    );
}

// Same, with a *folded* loop body (`7 * 7` bakes 49 at compile time with no
// runtime trace). Historically this exact case was protected on aarch64 by a
// compensating per-execution `CheckBOP` before every folded op; that guard is
// now gone (the dispatch-table swap covers it method-wide), so this test pins
// the fold variant against regressions in the swap.
#[test]
fn redefine_bop_offstack_osr_loop_fold() {
    run_test(
        r##"
        def osr_fold_sum
          total = 0
          i = 0
          while i < 300
            total = total + (7 * 7)
            i = i + 1
          end
          total
        end
        osr_fold_sum
        class Integer
          def *(o); 1000; end
        end
        osr_fold_sum
        "##,
    );
}

// Regression (bug C): an ON-STACK caller must not resume its stale compiled
// body after a callee redefines a basic op. Two aarch64 gaps conspired here:
// (1) `emit_call` did not register normal calls' return addresses, so
// `immediate_eviction` could not patch the caller's return continuation
// (x86 registers every call); (2) arch-neutral: the `check_bop_redefine`
// eviction ran *before* the store mutation (and only on the `def` path), so
// the very definition that set the BOP flags never triggered it — it only ran
// on the *next* def, one redefinition too late. The eviction now fires inside
// the `Executor::add_method` / `add_method_with_original` /
// `alias_method_for_class` funnel (after the store mutation, before the
// `method_added` hook), covering every definition route. With both fixed, the
// caller deopts when the callee returns, and the redefined `+` is observed
// (999, not the stale inline 8).
#[test]
fn redefine_bop_onstack_caller() {
    run_test(
        r##"
        $flag = false
        def maybe_redefine
          if $flag
            Integer.class_eval("def +(o); 999; end")
          end
          nil
        end
        def parent_dyn(a, b)
          maybe_redefine
          a + b        # non-fold inline add after the call
        end
        def parent_fold
          maybe_redefine
          5 + 3        # constant fold after the call
        end
        100.times { parent_dyn(5, 3); parent_fold }
        $flag = true
        [parent_dyn(5, 3), parent_fold]
        "##,
    );
}

// Same on-stack scenario reached through a GENERIC `yield` instead of a method
// call: the yielded block redefines the basic op, and the yielding frame must
// deopt on resume instead of running its stale compiled continuation. On
// aarch64, `emit_yield` historically ignored its `evict` (only method calls /
// specialized calls registered return addresses) — which both left
// yield-suspended frames un-evictable AND broke `emit_immediate_evict`'s
// same-block-registration invariant (AsmEvict ids restart per block; a generic
// yield's `ImmediateEvict` would read a stale same-id entry from an earlier
// block's call and hijack that unrelated call site's patch point). The fix
// registers the yield's block-call return address exactly like x86.
#[test]
fn redefine_bop_onstack_yield() {
    run_test(
        r##"
        def m_dyn(a, b)
          yield
          a + b
        end
        def m_fold
          yield
          5 + 3
        end
        100.times { m_dyn(5, 3) {}; m_fold {} }
        r1 = m_dyn(5, 3) { Integer.class_eval("def +(o); 999; end") }
        r2 = m_fold {}
        [r1, r2]
        "##,
    );
}

// Same on-stack scenario, but the basic op is redefined WITHOUT the `def`
// keyword: via `Module#define_method` and via `alias_method`. These set the
// BOP flags through `insert_method` exactly like `def`, but historically only
// the `def` bytecode path ran the eviction check — the funnel placement in
// `Executor::add_method` / `alias_method_for_class` covers them.
#[test]
fn redefine_bop_onstack_caller_define_method_alias() {
    run_test(
        r##"
        $flag = false
        def maybe_redefine
          if $flag
            Integer.class_eval { define_method(:+) { |o| 999 } }
          end
          nil
        end
        def parent_dyn(a, b)
          maybe_redefine
          a + b
        end
        100.times { parent_dyn(5, 3) }
        $flag = true
        [parent_dyn(5, 3), parent_dyn(5, 3)]
        "##,
    );
    run_test(
        r##"
        class Integer
          def sub_impl(o); 777; end
        end
        $flag = false
        def maybe_redefine
          if $flag
            Integer.class_eval { alias_method :-, :sub_impl }
          end
          nil
        end
        def parent_dyn(a, b)
          maybe_redefine
          a - b
        end
        100.times { parent_dyn(5, 3) }
        $flag = true
        [parent_dyn(5, 3), parent_dyn(5, 3)]
        "##,
    );
}

// Regression for #730: a `class`/`module`/`class << obj` definition used
// in *expression* position (its value feeds a surrounding operation)
// must not clobber the accumulator that holds a live operand. Here `a`
// (a freshly built Array) is the receiver of `<<` while the argument is a
// class definition; the JIT used to leave `a` in the accumulator across
// the class-body call, so the `<<` read a clobbered register and tripped
// a non-Array assertion. `run_test` exercises the JIT (≥ warmup runs).
#[test]
fn class_def_in_expression_position_730() {
    run_test(
        r##"
        out = nil
        50.times do
          a = []
          a << (class C730A; 42; end)
          a << (class C730B; def m; 7; end; end; "z")
          obj = Object.new
          a << (class << obj; 99; end)
          out = a
        end
        out
        "##,
    );
    // module form, and the value flowing into arithmetic
    run_test(
        r##"
        r = nil
        50.times { r = 1 + (module M730; 41; end) }
        r
        "##,
    );
}
