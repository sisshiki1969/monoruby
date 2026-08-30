//! What survives of a frame's *float* registers across the call that hands
//! out its block.
//!
//! At such a call every unboxed local is boxed into its slot, because the
//! block can read the slot behind the compiler's back. Boxing an `F` — a
//! slot whose only copy is an fpr — leaves both copies live, so the binding
//! lands on `Sf` and the frame keeps reading the float out of the register
//! after the call instead of decoding it out of the box again.
//!
//! That is only correct while the register and the slot still agree, and
//! the block is exactly the thing that can make them disagree. Each case
//! below reports a stale float if the view is kept where it should have
//! been dropped, or reads an unwritten slot if the box is not stored.
extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r#"
    def call_block
      yield
    end
    def call_block_twice
      yield
      yield
    end
"#;

/// The block reads the outer float: the box has to be in the slot, since
/// a `LoadDynVar` reads the slot and never the caller's register.
#[test]
fn the_block_reads_the_float() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = call_block { a + 1.0 }
          [a, b, a + 0.25]
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// The block writes it, so the register the caller kept is stale and the
/// binding has to go.
#[test]
fn the_block_writes_the_float() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          call_block { a = a + 2.0 }
          a
        end
        def g(n)
          a = n * 1.5
          call_block_twice { a += 2.0 }
          a * 2.0
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The block replaces the float with something that is not a float, so
/// what has to be dropped is not only the register but the `Float` type
/// the `Sf` binding asserts about the slot.
#[test]
fn the_block_changes_the_type() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          call_block { a = "str" }
          a
        end
        def g(n)
          a = n * 1.5
          call_block { a = 7 }
          a + 1
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The callee is a builtin, so the block is a unit of its own and its
/// stores never reach the compiler: nothing about the float may be kept.
#[test]
fn a_builtin_callee() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          3.times { a += 1.0 }
          a
        end
        def g(n)
          a = n * 1.5
          [1, 2].each { a *= 2.0 }
          a
        end
        [f(3), g(3)]
        "#,
        PRELUDE,
    );
}

/// The store is two frames down, and reaches past its immediate outer
/// frame to a float the frame above that was holding in a register.
#[test]
fn a_store_through_two_outer_levels() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = n * 0.25
          call_block { call_block { a += b } }
          [a, b, a - b]
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// A loop whose body hands a block out: the merge at the loop head sees
/// one path with the register view still held and one without.
#[test]
fn a_block_call_inside_a_loop() {
    run_test_with_prelude(
        r#"
        def g(n)
          total = 0.0
          5.times do |i|
            a = n * 1.5 + i
            call_block { total += a }
          end
          total
        end
        g(3)
        "#,
        PRELUDE,
    );
}

/// The same with a `while` loop, which used to abort the compiler with
///
/// ```text
/// unreachable code: %1 S(Value)->C(3)
/// ```
///
/// `%1` is `n`. It reached the loop entry as `C(3)`, folded from the call
/// site, and reached the back edge as `S(Value)`, given up by the
/// `forget_constants` on the path through the block-passing call — and
/// `bridge` has no `S -> C`, because nothing can prove a slot holds the
/// constant the target claims.
///
/// The disagreement was between the two passes, not between the paths:
/// the loop-entry target comes from the back-edge fixpoint, which runs in
/// analysis mode, where a guarded unbox emits nothing and so never set
/// `had_deopt` — the very flag `specialized_iseq` reads to decide whether
/// the call may keep the frame's constants. The analysis predicted a kept
/// `C(3)`; codegen gave it up. See `AsmIr::note_analysis_deopt`.
///
/// Compiling it at all then uncovered two further, unrelated defects,
/// both since fixed.
///
/// The first was not about the spill pool at all, and is fixed:
/// `Codegen::set_class_version` patched a unit's version snapshot word —
/// a `const_i32`, which monoasm emits into a `MAP_JIT` *code* page, not
/// the always-writable data page — without flipping the page writable, so
/// every successful class-version salvage was a SIGBUS on Apple Silicon at
/// *any* pool size. See `Codegen::set_const_version`'s comment.
///
/// The second was a frame-layout disagreement, now fixed. On aarch64 with
/// `stress-spill-pool` this segfaulted in the `[x29 + offset]` load that
/// `LoadDynVarSpecialized` emits for the block's `a` — but the offset was
/// right: breaking on that one instruction and walking the frame chain on
/// every execution, 288 of 289 hits measured exactly the emitted 352, and
/// the odd one measured 368, with the same call chain and byte-identical
/// `bl` targets. It was `sp` that moved, not the offset.
///
/// Breaking instead on `a64_op_loop_start`'s `br x10` found one and the
/// same compiled loop entered at *two* depths — `x29 - sp` of 144 and of
/// 160, with `x29 - lfp` an invariant 24 — which is the loop-JIT entry
/// counting this unit's spill region twice on one of its two entry paths.
/// `LoopJitRspBump` subtracts `total - base` (the spill region) from the
/// `sp` it inherits. From a VM frame that is right: `init_method` reserves
/// the bytecode's `FnInitInfo::stack_offset * 16` and knows nothing of
/// spill slots. From a JIT-prologue frame it is not: that prologue
/// reserves `total - PROLOGUE_OVERHEAD`, which already *includes* the
/// spill region. Both kinds of frame reach a `loop_start`, so the same
/// body ran at two depths, `spill_bytes` apart — 16 at pool 2, 0 at pool
/// 14, hence the pool dependence. The inlined specialized frames it builds
/// below `sp` moved with it, while the `x29`-relative offsets addressing
/// them are fixed at compile time.
///
/// The fix is to pin `sp` to the frame's canonical depth, `total -
/// PROLOGUE_OVERHEAD`, rather than subtracting from what it inherits, so
/// both producers agree on the depth the compile assumed.
///
/// An earlier attempt instead *raised* every frame to `total -
/// CONTINUATION_FRAME_SIZE`, on the premise that `init_method` reserves
/// `iseq.stack_offset()`. It does not: `ISeqInfo::stack_offset()` is the
/// bytecode operand's expression plus a further 16, so the prologue's
/// `total - PROLOGUE_OVERHEAD` already matched the VM. Raising it broke
/// that match and needed a compensating `+ 16` per frame in
/// `resolve_specialized_id_chain`, which then over-counted for every
/// loop-JIT root a chain crossed — on x86-64 that surfaced as a nil `i`
/// read out of `Array#repeated_combination`'s outer frame
/// (`builtins::array::tests::sort`).
///
/// Still open, and unrelated to this test (it has no explicit `return` or
/// `break`): `a64_method_ret` and `a64_block_break` `b raise` without
/// restoring `sp`, unlike every sibling exit, which all take
/// `loop_jit_spill_bytes`.
#[test]
fn a_while_loop_around_a_block_passing_call() {
    run_test_with_prelude(
        r#"
        def f(n)
          total = 0.0
          i = 0
          while i < 5
            a = n * 1.5
            call_block { a *= 2.0 }
            total += a
            i += 1
          end
          total
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// The float is live across the call but the block never touches it, so
/// the register is the only thing that has to still be right — this is
/// the case the change exists for.
#[test]
fn a_float_untouched_by_the_block() {
    run_test_with_prelude(
        r#"
        def f(n)
          a = n * 1.5
          b = n * 2.5
          c = call_block { 1 }
          [a + 1.0, b * 2.0, a * b, c]
        end
        f(3)
        "#,
        PRELUDE,
    );
}

/// The float lives only in a *spill* home across the call, so the boxed
/// slot store is deferred — and then a side exit inside the callee hands
/// the rest of it, the `yield` included, back to the VM. The block reads
/// the caller's local off the slot the deferral left unwritten, and sees
/// the `nil` the frame was set up with.
///
/// Reported from a JIT-compiled DOOM renderer as
/// `comparison of Float with NilClass failed` (`sprite_scale` read as
/// `nil` inside a `reverse_each` block).
#[test]
fn a_deferred_home_read_after_a_side_exit() {
    run_test_once(
        r#"
        class C
          def initialize
            @a = [0.0, 1.0, 2.0]
            @hit = 0
          end
          def poison(v) = @a[1] = v
          def f(d)
            s = 160.0 / d
            @a.each do |x|
              @hit += 1 if !x.nil? && x < s
            end
            @hit
          end
        end
        c = C.new
        res = 0
        5000.times do |i|
          c.poison(i % 97 == 96 ? nil : (i % 17).to_f)
          res = c.f(2.0 + (i % 13))
        end
        res
        "#,
    );
}
