//! A loop-JIT unit whose region-final `loop_end` sat past an unbalanced
//! nesting counter used to fall off the end of the compiled code.
//!
//! The `TraceIr::LoopEnd` handler emitted the loop's exit bridge (the
//! deopt back to the interpreter) only when its dynamic loop_start /
//! loop_end nesting counter returned to zero. An inner loop whose
//! `loop_start` is statically reachable — so it is compiled and counted —
//! but whose body dies at an unconditional deopt (its comparison was
//! never profiled, so the operand cache is INVALID) hides that inner
//! loop's `loop_end` in dead code: the counter never comes back to zero,
//! the region's own `loop_end` emits nothing, and execution runs off the
//! end of the unit into unemitted (zeroed) memory — a hang or segfault
//! depending on what the zero bytes do. Found by dewasm's DOOM port,
//! whose generated `memcpy` (`Doom#_f28`) puts exactly this shape inside
//! its hot copy loop.
//!
//! The fix backs the counter with a positional check: the `loop_end`
//! sitting in the compile region's final basic block always emits the
//! exit bridge.
extern crate monoruby;
use monoruby::tests::*;

/// The outer `while` trips the loop threshold and is OSR-compiled. The
/// `else` arm never runs during warmup, so `while j < n` inside it is
/// compiled up to an unconditional deopt (INVALID operand cache) and its
/// `loop_end` is dead code — before the fix, the outer loop's own
/// `loop_end` then emitted no exit and the process hung or crashed here.
#[test]
fn dead_inner_loop_end_still_emits_region_exit() {
    run_test(
        r##"
        def f(n, k)
          i = 0
          r = 0
          while i < n
            if i < 1000
              r += 1
            else
              j = k
              while j < n
                j += 1
                r -= j
              end
            end
            i += 1
          end
          r
        end
        a = 0
        30.times { a += f(150, 0) }
        a
        "##,
    );
}
