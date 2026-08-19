use crate::bytecodegen::UnOpK;
use crate::executor::inline::InlineFuncInfo;

use super::*;

impl<'a> JitContext<'a> {
    ///
    /// Attempt guard-free inline emission of `recv_class#op` at this unary
    /// operator site through the registered unary inline generator — the
    /// unary mirror of
    /// [`fire_binary_inline`](Self::fire_binary_inline).
    ///
    /// Returns `false` when the method doesn't resolve, has no unary
    /// generator, the basic-op license is gone (redefinition / refinement in
    /// scope), or the generator declined; the caller then takes the ordinary
    /// method call. **No class-version guard and no receiver-slot guard** are
    /// emitted: soundness comes from the recorded bop_dep (a redefinition
    /// evicts every dependent body via `set_bop_redefine`) plus the fact that
    /// the generator is found by the *resolved* FuncId — a receiver class
    /// whose `-@` / `~` / `+@` resolves anywhere else has no generator and
    /// falls back. The operand guard is the generator's own responsibility
    /// (`load_fixnum` / the float-load discipline).
    ///
    fn fire_unary_inline(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: IdentId,
        recv: SlotId,
        recv_class: ClassId,
        bc_pos: BcIndex,
    ) -> bool {
        let Some((fid, _visibility)) = self.jit_check_method(recv_class, op) else {
            return false;
        };
        let Some(InlineFuncInfo::InlineGenUnary(f)) = self.store.inline_info.get_inline(fid) else {
            return false;
        };
        if !self.basic_op_assumable(recv_class, op) {
            return false;
        }
        let callid = self.store.get_callsite_id(self.iseq_id(), bc_pos).unwrap();
        debug_assert_eq!(self.store[callid].recv, recv);
        debug_assert_eq!(self.store[callid].pos_num, 0);
        debug_assert!(self.store[callid].block_fid.is_none());
        if self.inline_asm_unary(state, ir, f, callid, recv_class) {
            self.record_bop_dep(recv_class, op);
            true
        } else {
            false
        }
    }

    ///
    /// `TraceIr::UnOp` — `-x` / `+x` / `~x` / `!x`.
    ///
    /// The unary mirror of [`binary_op`](Self::binary_op): resolve the
    /// receiver class (abstract state first, inline cache second), try the
    /// registered unary generator guard-free, and fall back to the ordinary
    /// method call. `!` has no generator, so it always takes the call.
    ///
    pub(super) fn unary_op(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        kind: UnOpK,
        dst: SlotId,
        src: SlotId,
        ic: Option<ClassId>,
        bc_pos: BcIndex,
    ) -> JitResult<CompileResult> {
        let Some(recv_class) = state.class(src).or(ic) else {
            return Ok(CompileResult::Recompile(RecompileReason::NotCached));
        };
        if self.fire_unary_inline(state, ir, kind.into(), src, recv_class, bc_pos) {
            // ④-b: the Integer/Float inline unaries are pure arithmetic
            // (guards exit the trace) — the unfrozen-slot proofs survive.
            if recv_class == INTEGER_CLASS || recv_class == FLOAT_CLASS {
                self.restore_unfrozen(Some(dst));
            }
            return Ok(CompileResult::Continue);
        }
        self.call_unary_method(state, ir, src, recv_class, kind, bc_pos)
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn unop_constant_folds() {
        // Regression: `y = +x` on a state-known fixnum used to leave `dst`
        // undefined once the method was JIT-compiled, so `y` kept its
        // previous value. Every fold path must define the destination.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              j = 0
              while j < 30
                x = 5
                f = 2.5
                y = 99
                y = +x
                res << y
                z = 99
                z = +f
                res << z
                res << -x
                res << ~x
                res << -f
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn unop_variable_operands() {
        // The register / xmm paths (operand not a compile-time constant),
        // including the fixnum-overflow deopt of `-@` at FIXNUM_MIN.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def u(a, b)
              [-a, +a, ~a, -b, +b]
            end
            def drive
              res = []
              j = 0
              while j < 30
                res << u(7, 1.5)
                res << u(-3, -0.25)
                res << u(-4611686018427387904, 0.0)
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn unop_explicit_send_parity() {
        // The explicit-send spelling fires the same generators through
        // compile_method_call.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            def drive
              res = []
              j = 0
              while j < 30
                res << 7.-@
                res << 7.+@
                res << 7.~
                res << 1.5.-@
                res << 1.5.+@
                j = j + 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn unop_redefinition_evicts() {
        // The generator is found by the *resolved* FuncId and the body
        // records a bop_dep, so a post-warmup redefinition both evicts the
        // compiled body and stops the site from re-inlining.
        run_test_once(
            r##"
            def n(x) = -x
            a = []
            40.times { a << n(3) }
            class Integer
              def -@ = :redefined
            end
            a << n(3)
            a
        "##,
        );
    }

    #[test]
    fn unop_non_numeric_receiver() {
        // A receiver whose unary operator resolves elsewhere (String#+@ /
        // a user-defined `-@`) has no generator on the resolved FuncId, so
        // the site takes the ordinary call.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class Neg
              def -@ = :neg
              def +@ = :pos
            end
            def drive
              res = []
              n = Neg.new
              s = +"abc"
              j = 0
              while j < 30
                res << -n
                res << +n
                res << (+s).frozen?
                j = j + 1
              end
              res
            end
        "##,
        );
    }
}
