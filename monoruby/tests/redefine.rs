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
// aarch64 by the fold-only `CheckBOP` guard + the on-stack conversion walk,
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
// to it with no version check. The protection is `clear_loop_jit_entries`:
// evicting an iseq's JIT code also zeroes its `LoopStart` operands, so the
// site returns to its "not compiled yet" state and recompiles on the next
// iteration. This used to be a process-wide dispatch-table swap
// (`remove_vm_bop_optimization` replacing `loop_start` with a no-opt handler);
// aarch64 was once missing that swap, and `x * x` here kept returning the
// pre-redefinition product (14700 instead of 300000).
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

/// Only the loops that inlined the operator lose their compiled body. A
/// second loop that never used it keeps running its OSR body -- when the
/// invalidation was process-wide, one redefinition stopped the VM entering
/// *any* compiled loop for the rest of the process.
#[test]
fn redefine_bop_osr_loop_spares_unrelated_loops() {
    // `run_test_once`, not `run_test`: aliasing `*` a second time in the same
    // process would capture the replacement and recurse. The loop counts are
    // already past the test-mode OSR threshold on the first pass.
    run_test_once(
        r##"
        def osr_mul(n); s = 0; i = 0; while i < n; s = s + (i * 2); i = i + 1; end; s; end
        def osr_add(n); s = 0; i = 0; while i < n; s = s + i;       i = i + 1; end; s; end
        warm = [osr_mul(400), osr_add(400)]
        class Integer
          alias __osr_mul *
          def *(o); __osr_mul(o); end
        end
        warm + [osr_mul(400), osr_add(400)]
        "##,
    );
}

// Same as `redefine_bop_offstack_osr_loop`, with a *folded* loop body
// (`7 * 7` bakes 49 at compile time with no runtime trace). Historically this
// exact case was protected on aarch64 by a compensating per-execution
// `CheckBOP` before every folded op; that guard is now gone (the
// dispatch-table swap covers it method-wide), so this test pins the fold
// variant against regressions in the swap.
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
// (1) `emit_call` did not register normal calls' return addresses, so the
// on-stack walk could not reach the caller's suspended frame
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

// The on-stack caller case with an unboxed `Float` local live across the call.
// `acc` stays FPR-resident through `maybe_redefine` (no block at the call site,
// so nothing demotes it), so converting the suspended frame has to box it out
// of its FP home and into the local slot before the interpreter resumes —
// exercising the float half of the eviction write-back, which the integer-only
// cases above never reach. The trailing `3 + 4` is constant-folded by the JIT,
// so it also checks the frame really stopped running its compiled body: a
// resumed fold answers 7, an interpreter answers 999.
//
// This goes through the chain-deopt walk's eager replay, which boxes the
// suspended frame's FP-pool resident out of the call's `FprSave` area
// (`doc/chain_deopt.md` §8.1).
#[test]
fn redefine_bop_onstack_caller_float_local() {
    run_test(
        r##"
        $flag = false
        def maybe_redefine
          if $flag
            Integer.class_eval("def +(o); 999; end")
          end
          nil
        end
        def float_carrier(a)
          acc = 0.0
          acc = acc + a * 1.5
          maybe_redefine
          acc = acc + 0.25
          [acc, 3 + 4]
        end
        100.times { float_carrier(2.0) }
        $flag = true
        float_carrier(2.0)
        "##,
    );
}

// The suspended caller holds MORE live floats than the FP register pool has
// registers, so its write-back has to source them from two different homes:
// the pool residents out of the call's `FprSave` area (indexed by set-bit
// position in the site's `UsingFpr`) and the overflow out of the frame's
// `base`-relative spill slots. Under `stress-spill-pool` the pool is two
// registers, so five live products guarantee both homes are populated. The
// results are `Float#-`, which the `Integer#+` redefinition leaves alone, so
// a float taken from the wrong home reads as a wrong number rather than a
// crash; the trailing `3 + 4` is the fold that pins the frame to the
// interpreter (a resumed compiled body answers 7).
//
// Those two homes are the two arms of `ChainReplay::replay`'s `fpr` loop
// (`doc/chain_deopt.md` §8.1), which every build now runs — the eviction walk
// is the chain-deopt walk.
#[test]
fn redefine_bop_onstack_caller_float_spill() {
    run_test(
        r##"
        $flag = false
        def fmaybe
          Integer.class_eval("def +(o); 999; end") if $flag
          nil
        end
        def float_spill(a)
          p1 = a * 1.5
          p2 = a * 2.5
          p3 = a * 3.5
          p4 = a * 4.5
          p5 = a * 5.5
          fmaybe
          [p1 - p2, p3 - p4, p5 - p1, 3 + 4]
        end
        100.times { float_spill(2.0) }
        $flag = true
        float_spill(2.0)
        "##,
    );
}

// The suspended frame is a specialized `(...)`-forwarding trampoline whose
// rest `Array` — and, in the second snippet, whose `**kwrest` `Hash` — is
// still DEFERRED at the call it is suspended at, so the eviction has to
// materialize both out of argument slots living in the trampoline's *dynamic
// caller* frame rather than its own.
//
// The redefining call is reached through `*_driver` rather than from the
// warm-up loop directly: the deferral is a specialization-only decision
// (`forward_rest_deferral` requires `is_specialized()`), so the trampoline has
// to be entered from JIT-compiled code — entered from the VM it runs its root
// body, where nothing is deferred. The second `*_target(...)` consume is what
// makes a wrong materialization visible: the interpreter builds that call's
// arguments out of the slots the eviction just wrote, so `y` diverges from `x`
// if they were sourced wrongly.
#[test]
fn redefine_bop_onstack_forwarding_trampoline() {
    run_test(
        r##"
        $flag = false
        def fwd_target(a, b)
          Integer.class_eval("def +(o); 999; end") if $flag
          a + b
        end
        def fwd_wrap(...)
          x = fwd_target(...)
          y = fwd_target(...)
          [x, y, 3 + 4]
        end
        def fwd_driver
          fwd_wrap(5, 3)
        end
        100.times { fwd_driver }
        $flag = true
        fwd_driver
        "##,
    );
    run_test(
        r##"
        $flag = false
        def kw_target(a, k: 0, j: 0)
          Integer.class_eval("def +(o); 999; end") if $flag
          [a, k, j]
        end
        def kw_wrap(...)
          x = kw_target(...)
          y = kw_target(...)
          [x, y, 3 + 4]
        end
        def kw_driver
          kw_wrap(5, k: 7, j: 9)
        end
        100.times { kw_driver }
        $flag = true
        kw_driver
        "##,
    );
}

// `Class#new` is the trampoline that carries a deferred `**kwrest` in
// practice: the literal keywords at `KwNode.new(a: 1, b: 2)` are source-routed
// straight into `initialize`'s declared parameters, so `new`'s own `**kwrest`
// is still deferred while `initialize` runs. Redefining the basic op there
// suspends `new` with that deferral outstanding, and the frame below it is a
// SPECIALIZED call rather than the ordinary send the snippets above suspend
// at.
#[test]
fn redefine_bop_onstack_new_trampoline_kwrest() {
    run_test(
        r##"
        $flag = false
        class KwNode
          attr_reader :v
          def initialize(a: 0, b: 0, c: 0)
            Integer.class_eval("def +(o); 999; end") if $flag
            @v = [a, b, c]
          end
        end
        def kwnode_driver
          [KwNode.new(a: 1, b: 2).v, 3 + 4]
        end
        100.times { kwnode_driver }
        $flag = true
        kwnode_driver
        "##,
    );
}

// TWO suspended JIT frames below the redefining one instead of one, so the
// eviction walk has to carry a chain rather than a single frame, and the two
// call-site shapes are mixed: `lvl2` is suspended at a call whose result is
// discarded (no destination slot), `lvl1` at one whose result feeds a local.
// Each frame carries its own post-call fold, so a frame that resumed its
// compiled body contributes a stale 2 / 7 / 11 and its position in the result
// array names which one it was.
#[test]
fn redefine_bop_onstack_multi_frame_chain() {
    run_test(
        r##"
        $flag = false
        def lvl3
          Integer.class_eval("def +(o); 999; end") if $flag
          nil
        end
        def lvl2
          lvl3
          [1 + 1, 3 + 4]
        end
        def lvl1
          r = lvl2
          r << (5 + 6)
        end
        100.times { lvl1 }
        $flag = true
        lvl1
        "##,
    );
}

// The suspended call site is an OPERATOR (`r + 5`, a 1-unit `BinOp` bytecode)
// rather than a 2-unit send — it reaches the generic `send` emitter because
// the receiver is neither `Integer` nor `Float`. A post-call continuation that
// assumed the send's size would advance the resume pc one whole instruction
// too far (`doc/chain_deopt.md` §8.2 is the record of what that costs), so the
// trailing `3 + 4` is the instruction the frame must resume AT.
#[test]
fn redefine_bop_onstack_operator_site() {
    run_test(
        r##"
        $flag = false
        class OpRedef
          def +(o)
            Integer.class_eval("def +(o); 999; end") if $flag
            o * 2
          end
        end
        def op_caller(r)
          v = r + 5
          [v, 3 + 4]
        end
        o = OpRedef.new
        100.times { op_caller(o) }
        $flag = true
        op_caller(o)
        "##,
    );
}

// The redefining callee RAISES instead of returning, so control leaves the
// suspended frames by unwinding: `mid_frame` never resumes at all and
// `outer_frame` resumes in its `rescue`. Both were evicted while suspended, so
// this pins the eviction against the unwind path (`doc/chain_deopt.md` §8.4)
// as well as the ordinary return path every other case here takes.
#[test]
fn redefine_bop_onstack_unwind_through_suspended() {
    run_test(
        r##"
        $flag = false
        def raiser
          if $flag
            Integer.class_eval("def +(o); 999; end")
            raise "boom"
          end
          nil
        end
        def mid_frame
          raiser
          [1 + 1, 2 + 2]
        end
        def outer_frame
          begin
            mid_frame
          rescue => e
            [e.message, 3 + 4]
          end
        end
        100.times { outer_frame }
        $flag = true
        outer_frame
        "##,
    );
}

// Same on-stack scenario reached through a GENERIC `yield` instead of a method
// call: the yielded block redefines the basic op, and the yielding frame must
// deopt on resume instead of running its stale compiled continuation. On
// aarch64, `emit_yield` historically ignored its `evict` (only method calls /
// specialized calls registered return addresses) — which both left
// yield-suspended frames unconvertible AND broke `register_chain_exit`'s
// same-block-registration invariant (AsmEvict ids restart per block; a generic
// yield's `ChainExit` would read a stale same-id entry from an earlier
// block's call and give that unrelated call site the wrong replay data). The
// fix registers the yield's block-call return address exactly like x86.
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

// Guard-free slot dispatch (aarch64) — a shared method dispatched through the
// guard-free slot must stay correct across a whole-method recompile
// (class-version bump) that RE-POINTS the slot at the recompiled body. Uses
// chained receiver classes (A head, B chained) so both a head slot and a
// chained-class slot participate, and avoids `+` entirely (`.times`/`<<`/
// `.succ`) because `run_test` re-runs the snippet 25× in one process — a
// mid-snippet `Integer#+` redefine would break loop counters on the re-runs.
// (The BOP-zeroing + wrapper-fallback leg is covered by the redefine_bop_*
// tests above, which already run with guard-free dispatch active.) On x86 this
// is plain dispatch; on aarch64 it drives the jit_guard_free_slot machinery.
#[test]
fn guard_free_dispatch_across_recompile() {
    run_test(
        r##"
        class Base
          def helper(x); x.succ; end
          def m(v); [helper(v), helper(v)]; end
        end
        class A < Base; end
        class B < Base; end
        a = A.new; b = B.new
        res = 0
        300.times { |i| res += a.m(i)[0]; res += b.m(i)[1] }   # compile m for A (head) + B (chained)
        def caller(o)
          r = []
          50.times { |j| r << o.m(j) }
          r.length
        end
        3.times { caller(a); caller(b) }                       # callers bake the guard-free slots
        # class-version bumps drive whole-method recompiles that re-point the
        # slots; every subsequent guard-free dispatch must reach the new body.
        3.times { |k| eval("class Dummy#{k}; def zz; #{k}; end; end"); a.m(1); b.m(1) }
        [res, caller(a), caller(b), a.m(5), b.m(7)]
        "##,
    );
}

///
/// A block that folds a constant must keep working after the global constant
/// version moves — and must go on working, call after call.
///
/// A block-style root cannot take the salvaging *recompile* entry (its
/// whole-method entry would rebuild the wrong frame shape), so its
/// constant-version guard used to be a bare deopt with no healing at all.
/// Since the global version never moves back, a single unrelated constant
/// assignment left such a block failing its guard on every later call for the
/// rest of the process — 69% of all constant-version deopts on the
/// activerecord benchmark. The guard now calls the salvage-only entry, which
/// re-validates the folds and re-stamps the unit's version word without
/// touching a frame.
///
/// The answer was always right (a deopt is only slow, not wrong), so what
/// this pins is the new runtime entry itself: it runs on a block frame, with
/// that frame's `self`, and must find the unit's record and survive doing so.
///
#[test]
fn const_version_block_root_heals() {
    run_test_once(
        r##"
        LIMIT = 10
        def sum_under
          total = 0
          (1..30).each { |x| total += x if x < LIMIT }
          total
        end
        res = []
        # Warm the block past the JIT thresholds, so it is compiled with
        # LIMIT folded and the unit's constant-version word stamped.
        200.times { res << sum_under }
        # Move the global constant version out from under it.
        OTHER = 1
        200.times { res << sum_under }
        # ...and again, to catch a heal that only works once.
        ANOTHER = 2
        200.times { res << sum_under }
        [res.uniq, OTHER, ANOTHER]
        "##,
    );
}

///
/// The same shape with the constant read inside a nested block, so the
/// salvage entry is reached from a frame whose `self` comes from an outer
/// block rather than the method.
///
#[test]
fn const_version_nested_block_root_heals() {
    run_test_once(
        r##"
        FACTOR = 3
        def scaled
          out = []
          (1..5).each do |a|
            (1..5).each { |b| out << a * b * FACTOR }
          end
          out.sum
        end
        res = []
        200.times { res << scaled }
        BUMP1 = :x
        200.times { res << scaled }
        [res.uniq, BUMP1]
        "##,
    );
}

#[test]
fn version_guard_restamp_survives_repeated_defs() {
    // Every 7th iteration defines a fresh (unrelated) method, bumping the
    // global class version while `target`'s compiled unit is hot. Each bump
    // fails the unit's class-version guard; salvage re-validates the
    // unchanged call sites and re-stamps the guard's version snapshot —
    // on x86-64 by patching the imm32 baked into every guard's
    // `movl rax, imm32` (see `check_version`). A mis-patched site would
    // crash or wedge the method into a permanent per-call deopt.
    run_test(
        r##"
        def target(x) = x + 1
        s = 0
        i = 0
        while i < 200
          s += target(i)
          eval("def zz_#{s} = 1") if i % 7 == 0
          i += 1
        end
        s
        "##,
    );
}

#[test]
fn loop_with_eval_does_not_storm_the_loop_jit() {
    // A loop body containing `eval` cannot be loop-JIT-compiled (the
    // front-end bails). The bail must be remembered via the LoopStart
    // 1-sentinel — without it (the x86 bug this guards), the counter
    // sitting past the threshold re-fired the aborting compiler on
    // every iteration: ~100k aborted compiles for a 100k-iteration
    // loop (28.5s instead of 0.16s for this shape at 100k).
    run_test_once(
        r##"
        def target(x) = x + 1
        s = 0
        i = 0
        while i < 2000
          s += target(i)
          eval("1 + 1") if i == 0
          i += 1
        end
        s
        "##,
    );
}
