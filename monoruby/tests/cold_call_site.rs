//! Call sites the VM has never run, compiled while the receiver class is
//! already proven.
//!
//! A proven receiver class lets the compile-time lookup resolve such a
//! site, and resolving it is what makes a never-taken branch charge the
//! branch that *is* taken: the resolved call reaches the join with its
//! result in a stack slot, so the meet against the hot edge's unboxed
//! float has to box, and the hot path then reloads, guards and decodes a
//! value that never left its register. dewasm's converted wasm puts one
//! of these in every float subtraction (a NaN-normalising `quiet_nan`
//! that is never reached), which cost `benchmarks/c/mandelbrot` about
//! half its compute.
//!
//! Such a site now takes the same recompile-once path an unproven
//! receiver takes. These tests pin the semantics either way: a branch
//! that never runs must still answer correctly, and one that starts
//! running must heal to the resolved call and keep answering correctly
//! across the recompile its first executions ask for.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn never_taken_branch_on_a_proven_receiver() {
    // `self` has a known class, so `probe` resolves at compile time even
    // though the guard never lets it run.
    run_test(
        r##"
        class C
          def probe(x) = x * 3
          def run(n)
            s = 0
            i = 0
            while i < n
              s += (i < 0 ? probe(i) : 1)
              i += 1
            end
            s
          end
        end
        C.new.run(300)
        "##,
    );
}

#[test]
fn cold_branch_becomes_hot_and_heals() {
    // Cold for the first 1000 iterations (long past the loop-JIT
    // threshold), then taken for the rest: the site must resolve on the
    // recompile and keep answering correctly through the transition.
    run_test(
        r##"
        class C
          def probe(x) = x * 3
          def run(n)
            s = 0
            i = 0
            while i < n
              s += (i < 1000 ? 1 : probe(i))
              i += 1
            end
            s
          end
        end
        C.new.run(2000)
        "##,
    );
}

#[test]
fn float_merge_against_a_never_taken_call() {
    // The dewasm shape: a float carried in a register meets the result of
    // a call that is never made. Both the loop-carried floats and the
    // guarded value must survive the join.
    run_test(
        r##"
        class C
          def quiet(x) = x + 0.5
          def run(n)
            total = 0.0
            k = 0
            while k < n
              zr = 0.0
              zi = 0.0
              j = 0
              while j < 20
                a = zr * zr
                b = zi * zi
                break if a + b > 4.0
                zi = 2.0 * zr * zi + 0.25
                # The false arm never runs; `r` is a float in a register
                # on the arm that does.
                zr = ((r = a - b) == r ? r : quiet(r)) + (k % 7) * 0.125 - 1.0
                j += 1
              end
              total += zr + zi + j
              k += 1
            end
            total
          end
        end
        C.new.run(400)
        "##,
    );
}

#[test]
fn never_taken_branch_reached_after_warmup() {
    // The same site, first reached only after the method is compiled and
    // its class-proven resolution has been abandoned: the answer must not
    // depend on when the branch starts running.
    run_test(
        r##"
        class C
          def a = 7
          def b = 11
          def pick(flag) = (flag ? a : b)
        end
        c = C.new
        out = []
        300.times { |i| out << c.pick(i > 250) }
        [out.sum, out.first, out.last]
        "##,
    );
}
