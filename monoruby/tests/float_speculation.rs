extern crate monoruby;
use monoruby::tests::*;

// The unboxed-locals speculation (doc/chain_deopt.md §5 steps 4–5, §11):
// at a qualifying block-passing specialized call, Float locals stay
// unboxed across the call and the specialized blocks access them in the
// frame's FP save/spill area. These tests pin the semantics every path
// of the speculation must preserve — the guard-fired conversion, the
// compile-time poison, capture, and multi-local interplay.

#[test]
fn speculated_float_accumulation() {
    // The motivating aobench shape: a Float local mutated two blocks
    // deep, plus a read-only Float local, through JIT warmup.
    run_test(
        r#"
        def occl(nphi, ntheta)
          eps = 0.5
          occlusion = 0.0
          nphi.times do |j|
            ntheta.times do |i|
              occlusion += eps + (i + j) * 0.25
            end
          end
          occlusion
        end
        r = 0.0
        10.times { r += occl(8, 8) }
        r
        "#,
    );
}

#[test]
fn speculated_store_of_integer_kills_cleanly() {
    // A compile-time-known Integer store into the speculated local: the
    // speculation must die (poison → unspeculated recompile), and the
    // Integer must land as an Integer — not smuggled in as 1.0.
    run_test(
        r#"
        def kill_int(n)
          acc = 0.0
          hits = []
          n.times do |i|
            acc = 1 if i == 3
            acc += 2.0
            hits << acc
          end
          hits
        end
        r = nil
        10.times { r = kill_int(6) }
        r
        "#,
    );
}

#[test]
fn speculated_store_guard_fires_at_runtime() {
    // The stored value is Float-guarded but occasionally an Integer at
    // runtime: the Float guard's escalated deopt must convert the chain
    // *before* the store, and the interpreter's re-execution must store
    // the real (non-coerced) value.
    run_test(
        r#"
        def kill_dyn(n, vals)
          acc = 0.0
          n.times do |i|
            v = vals[i]
            acc = v if v
            acc += 1.0
          end
          acc
        end
        r = nil
        10.times { r = kill_dyn(6, [nil, 2.5, nil, 3, nil, nil]) }
        r
        "#,
    );
}

#[test]
fn speculation_poisoned_by_capture() {
    // A proc created inside the block captures the chain: the site must
    // fall back to boxed locals and the captured proc must observe the
    // final value.
    run_test(
        r#"
        def cap(n)
          acc = 0.0
          saved = nil
          n.times do |i|
            acc += 1.0
            saved = proc { acc } if i == 2
          end
          acc += 100.0
          saved.call
        end
        r = nil
        10.times { r = cap(5) }
        r
        "#,
    );
}

#[test]
fn multiple_speculated_locals() {
    // Several Float locals kept at once — mixed mutated/read-only, with
    // enough float traffic in the block to exercise both pool and spill
    // homes (`stress-spill-pool` shrinks the pool to 2, forcing the
    // spill-slot variant through the suite's stress runs).
    run_test(
        r#"
        def multi(n)
          a = 0.5
          b = 1.5
          c = 2.5
          d = 3.5
          n.times do |i|
            a += b * 0.5
            c += d + a
            t0 = a * b + c
            t1 = t0 * d - b
            a += t1 * 0.0625
          end
          [a, b, c, d]
        end
        r = nil
        10.times { r = multi(12) }
        r
        "#,
    );
}

#[test]
fn speculated_local_read_after_exception_in_block() {
    // A raise inside the block unwinds through the speculating frame:
    // the escalated error exit converts the chain, so the rescue sees
    // the boxed, current value.
    run_test(
        r#"
        def exc(n)
          acc = 0.0
          begin
            n.times do |i|
              acc += 1.0
              raise "stop" if i == 3
            end
          rescue
          end
          acc
        end
        r = nil
        10.times { r = exc(10) }
        r
        "#,
    );
}
