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

// KNOWN, PRE-EXISTING, CROSS-ARCH BUG (bug B): an off-stack method that stays
// VM-interpreted but has an OSR/loop-JIT'd hot loop keeps entering the stale
// loop body after a basic-op redefinition. The compiled loop entry lives in the
// bytecode (`BytecodePtr::write2`, `[pc-8]`) and `vm_loop_start` jumps to it
// (`jmp rax` / `br x10`) with no `jit_invalidated`/class-version check, and
// `set_bop_redefine` reverts method entries but never the OSR bytecode-PC
// entries — on BOTH x86 and aarch64. Slot-zeroing (which fixed the method-JIT
// twin, bug A) cannot reach these entries. Fixing this needs a cross-arch design
// decision (revert every loop's `[pc-8]`, or guard the loop entry); until then
// this miscompiles (`x * x` here reads the pre-redefinition product). Un-ignore
// once OSR-entry invalidation lands.
#[test]
#[ignore = "bug B: off-stack OSR loop-JIT ignores BOP redefine (pre-existing, cross-arch)"]
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
