extern crate monoruby;
use monoruby::tests::*;

// A class guard emitted while the assembler is already on the cold page —
// an outline loop-entry bridge that reloads a Float from its stack slot —
// lays its "failing value -> rdi" stub in place and jumps over it. The stub
// has to bind the label the guard branches to; when it bound a fresh one
// instead, the guard's `jne`s pointed at a label that never got an address
// and the unit aborted at finalize ("unresolved relocation(s)").
//
// The bridge only carries such guards when Floats are spilled at the loop
// head, which the `stress-spill-pool` build (two FP registers) reaches with
// three Float arguments.

const BODY: &str = r#"
        def q(beg, unit, last, n, f)
          i = 0
          r = nil
          if f
            nil
          else
            while i < n
              d = unit + beg
              d = last if d < last
              r = d
              i += 1
            end
          end
          r
        end
"#;

#[test]
fn loop_entry_bridge_guards_compile() {
    run_test_with_prelude(
        r#"
        a = []
        30.times { a << q(1.0, 1.0, 3.0, 3, true) }
        30.times { a << q(3.0, -1.0, 1.0, 3, false) }
        a.uniq
        "#,
        BODY,
    );
}

#[test]
fn loop_entry_bridge_guards_fail_over_to_the_interpreter() {
    // Each of the bridge's guards missing in turn: a Fixnum, a heap value
    // and a Bignum where the compiled loop expects a Float.
    run_test_with_prelude(
        r#"
        a = []
        30.times { a << q(1.0, 1.0, 3.0, 3, true) }
        30.times { a << q(3.0, -1.0, 1.0, 3, false) }
        a << q(3, -1, 1, 3, false)
        a << q(3r, -1, 1.0, 3, false)
        a << q(3.0, -1, 2**70, 3, false)
        a << q(3.0, -1.0, 1, 3, false)
        30.times { a << q(3.0, -1.0, 1.0, 3, false) }
        a << (q(nil, -1.0, 1.0, 3, false) rescue $!.class)
        a.uniq
        "#,
        BODY,
    );
}
