extern crate monoruby;
use monoruby::tests::*;

// Non-local `return` out of an inlined block: when the home method frame
// is inside the compilation unit — including as the compile root — the
// return compiles to a static frame teardown (`MethodRetSpecialized`)
// joining the home's return, instead of the runtime unwind (which, with
// unconditional side-exit escalation, ran a chain-deopt walk per return —
// the `throw` benchmark regression). These pin value correctness on the
// static path and the ensure semantics that must keep the runtime path.

#[test]
fn return_through_one_inlined_frame() {
    // The throw-benchmark shape: home method is the compile root.
    run_test(
        r#"
        def nlr_foo
          yield
        end
        def nlr_bar
          nlr_foo { return 1 }
          :not_reached
        end
        s = 0
        200.times { s += nlr_bar }
        s
        "#,
    );
}

#[test]
fn return_through_two_inlined_frames() {
    run_test(
        r#"
        def nlr_a
          yield
        end
        def nlr_b
          nlr_a { yield }
        end
        def nlr_deep(x)
          nlr_b { return x * 2 }
          :not_reached
        end
        r = []
        100.times { |i| r << nlr_deep(i) }
        [r.first, r.last, r.sum]
        "#,
    );
}

#[test]
fn return_value_kinds_and_dst_join() {
    // The returned value must reach the home's caller intact for every
    // representation the JIT treats specially (Fixnum, Float, String).
    run_test(
        r#"
        def nlr_y
          yield
        end
        def pick(k)
          nlr_y { return 1.5 } if k == 0
          nlr_y { return 42 } if k == 1
          nlr_y { return "s" } if k == 2
          :none
        end
        out = []
        60.times { |i| out << pick(i % 4) }
        out.uniq
        "#,
    );
}

#[test]
fn ensure_in_intermediate_frame_still_runs() {
    // An `ensure` in a flown-over frame must run during the unwinding, so
    // this shape must keep the runtime path (the exception-handler check
    // rejects the static teardown).
    run_test(
        r#"
        $nlr_log = []
        def nlr_mid
          yield
        ensure
          $nlr_log << :mid
        end
        def nlr_home
          nlr_mid { return :v }
          :not_reached
        end
        r = []
        50.times { r << nlr_home }
        [r.uniq, $nlr_log.size]
        "#,
    );
}

#[test]
fn ensure_in_home_method_still_runs() {
    run_test(
        r#"
        $nlr_log2 = 0
        def nlr_y2
          yield
        end
        def nlr_home2
          nlr_y2 { return 5 }
          :not_reached
        ensure
          $nlr_log2 += 1
        end
        r = 0
        50.times { r += nlr_home2 }
        [r, $nlr_log2]
        "#,
    );
}

#[test]
fn return_out_of_loop_jit_block() {
    // A hot loop inside the block makes the block a loop-JIT root; the
    // home method is then outside that unit and the return must take the
    // runtime path correctly.
    run_test_once(
        r#"
        def nlr_y3
          yield
        end
        def find_it(n)
          nlr_y3 do
            i = 0
            while i < 200
              return i if i == n
              i += 1
            end
          end
          :absent
        end
        [find_it(150), find_it(199), find_it(500)]
        "#,
    );
}
