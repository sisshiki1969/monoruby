//! A polymorphic send whose speculative fast arm gives up must leave the
//! compiler where it found it (#1666).
//!
//! `method_call_with_residual` compiles the hot class's call as a fast arm
//! that may specialize, and rolls the attempt back when it does not end in
//! `Continue` — including a `CompileError` raised somewhere down the
//! specialized chain. The rollback restored the IR and the abstract state
//! but not the specialization stack: every frame the aborted nested
//! compiles had pushed stayed behind, so the caller went on compiling its
//! own bytecode positions against the innermost leftover frame's iseq, and
//! the first `Ret` it met there indexed its state chain past the end
//! (`range end index 3 out of range for slice of length 1` in
//! `AbstractState::chain_prefix`, aborting the process).
//!
//! The shape: one iseq target shared by two receiver classes (so the PIC
//! declines and the residual arm is tried), and a chain under it that
//! reaches a `class_eval` send with a block literal (an eval-capable
//! callee, which `compile_method_call` refuses with `CompileError`). This
//! is `Object#stub!` → `Mock.install_method` → `suppress_warning { ... }`
//! from mspec, as a ruby/spec `before :each` block runs it.

extern crate monoruby;
use monoruby::tests::*;

#[test]
fn a_failed_fast_arm_unwinds_the_specialization_stack() {
    run_test(
        r##"
        class Base
          def go(m)
            install(m)
            7
          end
          def install(m)
            meta = singleton_class
            wrap { meta.class_eval { define_method(m) { |*a, &b| 1 } } }
          end
          def wrap
            v = $VERBOSE
            $VERBOSE = nil
            yield
          ensure
            $VERBOSE = v
          end
        end
        class C1 < Base; end
        class C2 < Base; end
        objs = [C1.new, C2.new]
        def drive(objs)
          s = 0
          objs.each { |o| s += o.go(:x) }
          s
        end
        t = 0
        300.times { t += drive(objs) }
        t
        "##,
    );
}
