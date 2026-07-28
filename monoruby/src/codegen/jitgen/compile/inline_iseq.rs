use super::*;
use crate::codegen::jitgen::state::Guarded;

/// Upper bound on the number of bytecode instructions an inlinable body may
/// have. The whole body is re-scanned per call site at JIT-compile time, so
/// keep this small.
const MAX_INLINE_BYTECODE_LEN: usize = 32;

///
/// A compile-time symbolic value flowing through an inlined body.
///
/// Because the admitted instruction set is pure data movement, every callee
/// slot can be described as one of these; no callee frame slot ever needs a
/// physical home.
///
#[derive(Debug, Clone, Copy, PartialEq)]
enum InlineOperand {
    /// A literal (immediate or frozen heap value owned by the callee iseq).
    Const(Value),
    /// An alias of a caller slot (the receiver or a positional argument).
    /// Caller slots are never written during the inlined body, so the alias
    /// stays valid until the final result store.
    CallerSlot(SlotId),
    /// An ivar load that has not been materialized yet. Loads are pure, so
    /// each consumer re-loads; this is only valid while no store to the same
    /// ivar has intervened (enforced by the analysis).
    PendingIvar(IvarId),
    /// An alias of a slot in the *dynamic caller's* frame, one level up —
    /// a D1 source-routed forwarded argument (`g(...)` whose `...` rest was
    /// deferred). The D1 gate guarantees the caller is exactly one
    /// (outermost) level up and `set_arguments`' `defer_rest` spill makes
    /// the slot memory-resident for the whole window; like `CallerSlot`,
    /// nothing writes it during the inlined body.
    OuterSlot(SlotId),
}

struct InlineStore {
    ivarid: IvarId,
    src: InlineOperand,
}

pub(super) struct InlinePlan {
    stores: Vec<InlineStore>,
    ret: InlineOperand,
}

enum InlineAnalysis {
    Inlinable(InlinePlan),
    NotInlinable,
    /// An ivar name in the body has no `IvarId` for the receiver class yet.
    /// Deopt-and-recompile: interpreting the body registers the id.
    IvarIdNotFound,
}

impl<'a> JitContext<'a> {
    ///
    /// Try to inline the callee *iseq* into the caller without creating a
    /// frame (see doc/method_inlining.md).
    ///
    /// Only bodies consisting of `FrozenLiteral` / `Mov` / `LoadIvar` /
    /// `StoreIvar` / `Ret` qualify: such a body cannot raise, cannot deopt
    /// mid-body (the only guard — frozen receiver — is hoisted to the call
    /// site), contains no GC poll and cannot capture the frame. Therefore
    /// the interpreter never needs to resume inside the callee, and every
    /// side exit re-executes the whole call from the caller's `MethodCall`
    /// pc, where no side effect has happened yet.
    ///
    /// Returns `None` when the body does not qualify (the caller falls
    /// through to specialization / generic dispatch).
    ///
    /// Two call-site shapes are admitted (`simple` distinguishes them):
    ///
    /// * a simple call (`simple_fold` held at the hook) — arguments are the
    ///   caller's own arg slots;
    /// * a D1 source-routed forwarding site (`g(lead.., ...)` whose `...`
    ///   rest was deferred — notably `Class#new`'s
    ///   `o.__builtin_initialize__(...)` when `new` is compiled specialized
    ///   for a concrete outer call site) — forwarded arguments alias the
    ///   dynamic caller's slots (`OuterSlot`).
    ///
    /// The class-version guard and the receiver-class guard have already
    /// been emitted by `compile_method_call`.
    ///
    pub(super) fn try_inline_iseq(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        fid: FuncId,
        iseq: ISeqId,
        simple: bool,
    ) -> Option<CompileResult> {
        let (arg_ops, forwarded) = self.inline_call_shape(state, callid, simple)?;
        match self.analyze_inline_iseq(&arg_ops, callid, recv_class, fid, iseq) {
            InlineAnalysis::Inlinable(plan) => {
                #[cfg(feature = "jit-log")]
                eprintln!(
                    "    inline_iseq: {} stores={} forwarded={forwarded}",
                    self.store[fid].name().map_or_else(String::new, |n| n.to_string()),
                    plan.stores.len(),
                );
                if forwarded {
                    // Inlining *is* the forwarding consume and reads the
                    // arguments straight from the source slots, so keep the
                    // caller-side `create_array` skip on. The D1 annotation
                    // is left in place: side exits still rebuild the rest
                    // `Array` for an interpreter resuming inside the
                    // trampoline frame.
                    ir.set_deferred_rest();
                }
                Some(self.compile_inline_iseq(state, ir, callid, recv_class, plan))
            }
            InlineAnalysis::IvarIdNotFound => Some(CompileResult::Recompile(
                RecompileReason::IvarIdNotFound,
            )),
            InlineAnalysis::NotInlinable => None,
        }
    }

    ///
    /// Classify the call-site shape and bind the callee's positional
    /// parameters to symbolic operands. Returns `(operands, forwarded)`,
    /// or `None` when the shape is not inlinable.
    ///
    fn inline_call_shape(
        &self,
        state: &AbstractState,
        callid: CallSiteId,
        simple: bool,
    ) -> Option<(Vec<InlineOperand>, bool)> {
        let callsite = &self.store[callid];
        if callsite.block_fid.is_some() {
            return None;
        }
        if simple {
            if callsite.block_arg.is_some() {
                return None;
            }
            let ops = (0..callsite.pos_num)
                .map(|i| InlineOperand::CallerSlot(callsite.args + i))
                .collect();
            Some((ops, false))
        } else if callsite.forwarding
            && callsite.pos_num >= 1
            && callsite.splat_pos.as_slice() == [callsite.pos_num - 1]
            && callsite.kw_args.is_empty()
            && callsite.hash_splat_pos.len() == 1
        {
            // `g(lead.., ...)` whose trailing `...` rest is D1-deferred:
            // the forwarded positionals are the dynamic caller's source
            // slots, and the structural gate guarantees the forwarded
            // `**kwrest` is statically nil (no keywords reached the
            // trampoline). The `...` also forwards `&blk` (`block_arg` is
            // always `Some` here) — the callee body cannot observe a block
            // (no `yield`, no calls in the admitted set), so ignoring it
            // is exact.
            let lead_num = callsite.pos_num - 1;
            let (src, len) = state.deferred_rest_src(callsite.args + lead_num)?;
            let mut ops: Vec<InlineOperand> = (0..lead_num)
                .map(|i| InlineOperand::CallerSlot(callsite.args + i))
                .collect();
            ops.extend((0..len as usize).map(|i| InlineOperand::OuterSlot(src + i)));
            Some((ops, true))
        } else {
            None
        }
    }

    ///
    /// Abstract-interpret the callee body at compile time.
    ///
    /// The environment maps every callee slot to an `InlineOperand`;
    /// `ivar_env` performs store-to-load forwarding so a load of an ivar
    /// stored earlier in the body never re-reads the object.
    ///
    fn analyze_inline_iseq(
        &self,
        arg_ops: &[InlineOperand],
        callid: CallSiteId,
        recv_class: ClassId,
        fid: FuncId,
        iseq: ISeqId,
    ) -> InlineAnalysis {
        use InlineOperand::*;
        let callsite = &self.store[callid];
        let info = &self.store[fid];
        // required-positional-only callee, exact arity match.
        if info.is_block_style()
            || info.reqopt_num() != info.req_num()
            || info.post_num() != 0
            || info.is_rest()
            || !info.no_keyword()
            || arg_ops.len() != info.req_num()
        {
            return InlineAnalysis::NotInlinable;
        }
        let iseq_info = &self.store[iseq];
        // No exception table: guarantees the admitted-opcode scan below never
        // meets a handler-only opcode (`RescueTEq` panics in the decoder).
        if iseq_info.block_param().is_some() || iseq_info.has_exception_handler() {
            return InlineAnalysis::NotInlinable;
        }
        let len = iseq_info.bytecode_len();
        if len > MAX_INLINE_BYTECODE_LEN {
            return InlineAnalysis::NotInlinable;
        }

        // slot 0 = self = receiver; params occupy slots 1..=req; all other
        // locals and temps start as nil (`InitMethod` nil-fills them).
        let mut env = vec![Const(Value::nil()); iseq_info.total_reg_num()];
        env[0] = CallerSlot(callsite.recv);
        for (i, op) in arg_ops.iter().enumerate() {
            env[i + 1] = *op;
        }
        // last stored value per ivar, for store-to-load forwarding.
        let mut ivar_env: Vec<(IvarId, InlineOperand)> = vec![];
        let mut stores: Vec<InlineStore> = vec![];
        for i in 0..len {
            let pc = iseq_info.get_pc(BcIndex::from(i));
            match TraceIr::from_pc(pc, self.store) {
                TraceIr::InitMethod(_) => {}
                TraceIr::FrozenLiteral(dst, val) => env[dst.0 as usize] = Const(val),
                TraceIr::Mov(dst, src) => env[dst.0 as usize] = env[src.0 as usize],
                TraceIr::LoadIvar(dst, name, _) => {
                    if recv_class.is_always_frozen() {
                        return InlineAnalysis::NotInlinable;
                    }
                    let Some(id) = self.store[recv_class].get_ivarid(name) else {
                        return InlineAnalysis::IvarIdNotFound;
                    };
                    env[dst.0 as usize] = ivar_env
                        .iter()
                        .rev()
                        .find(|(stored, _)| *stored == id)
                        .map(|(_, v)| *v)
                        .unwrap_or(PendingIvar(id));
                }
                TraceIr::StoreIvar(src, name, _) => {
                    if recv_class.is_always_frozen() {
                        return InlineAnalysis::NotInlinable;
                    }
                    let Some(id) = self.store[recv_class].get_ivarid(name) else {
                        return InlineAnalysis::IvarIdNotFound;
                    };
                    // A pending (not yet materialized) load of this ivar must
                    // not survive the store: its later materialization would
                    // re-read the NEW value. (e.g. `tmp = @a; @a = @b; @b = tmp`)
                    if env.iter().any(|op| *op == PendingIvar(id))
                        || ivar_env.iter().any(|(_, op)| *op == PendingIvar(id))
                    {
                        return InlineAnalysis::NotInlinable;
                    }
                    let val = env[src.0 as usize];
                    stores.push(InlineStore { ivarid: id, src: val });
                    ivar_env.push((id, val));
                }
                TraceIr::Ret(ret) => {
                    return InlineAnalysis::Inlinable(InlinePlan {
                        stores,
                        ret: env[ret.0 as usize],
                    });
                }
                _ => return InlineAnalysis::NotInlinable,
            }
        }
        // fell off the end without a `Ret` terminal.
        InlineAnalysis::NotInlinable
    }

    ///
    /// Emit the inlined body.
    ///
    /// Shape: hoisted frozen guard (deopting to the caller's `MethodCall`
    /// pc, where no side effect has happened yet) → the stores in body
    /// order → the result definition. No instruction below may create a
    /// deopt point: past the frozen guard the body is uninterruptible.
    ///
    fn compile_inline_iseq(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: ClassId,
        plan: InlinePlan,
    ) -> CompileResult {
        let (recv, dst) = {
            let callsite = &self.store[callid];
            (callsite.recv, callsite.dst)
        };
        let is_object_ty = self.store[recv_class].is_object_ty_instance();

        if !plan.stores.is_empty() {
            // The receiver cannot become frozen mid-body (no calls), so one
            // hoisted guard covers every store.
            state.load(ir, recv, GP::Rdi);
            let deopt = ir.new_deopt(state);
            ir.guard_frozen(deopt);
        }
        for InlineStore { ivarid, src } in &plan.stores {
            let src_gp = self.materialize_inline_operand(state, ir, *src, recv, is_object_ty);
            // Reload the receiver: materialization may clobber rdi. This
            // load itself cannot clobber `src_gp` — the receiver is a heap
            // value, so no boxing call is involved.
            state.load(ir, recv, GP::Rdi);
            if is_object_ty && ivarid.is_inline() {
                ir.push(AsmInst::StoreIVarInline {
                    src: src_gp,
                    ivarid: *ivarid,
                });
            } else {
                // The bounds-checked store: its cold path calls `set_ivar`
                // (which may grow the ivar table — an allocation, but no
                // raise and no inline GC), so flush the GP pool first,
                // exactly like `attr_writer`.
                let using_fpr = state.get_using_fpr(ir);
                ir.push(AsmInst::StoreIVarHeap {
                    src: src_gp,
                    ivarid: *ivarid,
                    is_object_ty,
                    using_fpr,
                });
            }
        }
        if !plan.stores.is_empty() {
            state.unset_side_effect_guard();
        }

        if let Some(dst) = dst {
            match plan.ret {
                InlineOperand::Const(v) => {
                    if let Some(imm) = v.is_immediate() {
                        state.def_C(dst, imm);
                    } else {
                        state.def_lit2gp(ir, dst, v);
                    }
                }
                InlineOperand::CallerSlot(slot) => state.copy_slot(ir, slot, dst),
                InlineOperand::PendingIvar(ivarid) => {
                    // Load recv before `alloc_gp_for`: dst may alias recv,
                    // and the alloc discards dst's old link.
                    state.load(ir, recv, GP::Rdi);
                    let gp = state.alloc_gp_for(ir, dst, Guarded::Value);
                    self.push_inline_ivar_load(ir, ivarid, is_object_ty, gp);
                    state.bind_gp_resident(gp, dst);
                }
                InlineOperand::OuterSlot(slot) => {
                    let gp = state.alloc_gp_for(ir, dst, Guarded::Value);
                    ir.push(AsmInst::LoadCallerFrameSlot { slot, dst: gp });
                    state.bind_gp_resident(gp, dst);
                }
            }
        }
        CompileResult::Continue
    }

    ///
    /// Materialize an operand into a scratch register for an ivar store.
    ///
    /// ### out
    /// - the returned GP holds the operand value (rax, or a live GP-pool
    ///   resident for `CallerSlot`).
    ///
    fn materialize_inline_operand(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        op: InlineOperand,
        recv: SlotId,
        is_object_ty: bool,
    ) -> GP {
        match op {
            InlineOperand::Const(v) => {
                ir.lit2reg(v, GP::Rax);
                GP::Rax
            }
            InlineOperand::CallerSlot(slot) => state.load_or_reg(ir, slot, GP::Rax),
            InlineOperand::PendingIvar(ivarid) => {
                state.load(ir, recv, GP::Rdi);
                self.push_inline_ivar_load(ir, ivarid, is_object_ty, GP::Rax);
                GP::Rax
            }
            InlineOperand::OuterSlot(slot) => {
                ir.push(AsmInst::LoadCallerFrameSlot {
                    slot,
                    dst: GP::Rax,
                });
                GP::Rax
            }
        }
    }

    ///
    /// ### in
    /// - rdi: receiver (&RValue)
    ///
    /// ### destroy
    /// - rdi, rsi, rdx
    ///
    fn push_inline_ivar_load(&mut self, ir: &mut AsmIr, ivarid: IvarId, is_object_ty: bool, dst: GP) {
        if is_object_ty && ivarid.is_inline() {
            ir.push(AsmInst::LoadIVarInline { ivarid, dst });
        } else {
            // Bounds-checked (`self_: false`): an out-of-range or absent
            // ivar table yields nil, no call, no raise.
            ir.push(AsmInst::LoadIVarHeap {
                ivarid,
                is_object_ty,
                self_: false,
                dst,
            });
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // NOTE: the hot loop lives inside a driver method defined in the
    // prelude: call sites are JIT-compiled (and thus inlinable) only when
    // their *caller* is compiled, and the top-level script body is not.

    #[test]
    fn inline_iseq_basic() {
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class Point
              def initialize(x, y)
                @x = x
                @y = y
              end
              def x
                @x
              end
              def y
                @y
              end
              def set(a, b)
                @x = a
                @y = b
                self
              end
              def defaults
                @x = 0
                @y = nil
                42
              end
              def through(v)
                t = v
                t
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                pt = Point.new(i, i * 2)
                res << pt.x << pt.y
                res << pt.set(i + 1, i + 2).x
                res << pt.defaults
                res << pt.x << pt.y
                res << pt.through(i)
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_heap_ivar() {
        // > OBJECT_INLINE_IVAR ivars: exercises LoadIVarHeap / StoreIVarHeap
        // including the table-growing cold path on the first stores.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class S
              def initialize(v)
                @a = v
                @b = v
                @c = v
                @d = v
                @e = v
                @f = v
                @g = v
                @h = v
              end
              def a
                @a
              end
              def h
                @h
              end
              def bump(v)
                @h = v
                @g = v
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                s = S.new(i)
                res << s.h << s.a
                s.bump(i * 10)
                res << s.h
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_non_object_ty() {
        // Receiver whose instance type is not ObjTy::OBJECT (Array subclass):
        // always takes the heap-ivar path.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class S < Array
              def initialize(v)
                @v = v
              end
              def v
                @v
              end
              def setv(v)
                @v = v
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                s = S.new(i)
                res << s.v
                s.setv(i * 3)
                res << s.v
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_swap_rejected() {
        // `tmp = @x; @x = @y; @y = tmp` must NOT be inlined (the pending
        // load of @x would cross the store to @x) — but it must still run
        // correctly via the normal path.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class P
              def initialize(x, y)
                @x = x
                @y = y
              end
              def x
                @x
              end
              def y
                @y
              end
              def rot
                tmp = @x
                @x = @y
                @y = tmp
              end
            end
            def drive
              res = []
              pt = P.new(1, 2)
              i = 0
              while i < 50
                pt.rot
                res << pt.x << pt.y
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_frozen_deopt() {
        // Freezing the receiver mid-loop: the hoisted frozen guard deopts to
        // the call site; the interpreter re-executes the call and raises
        // FrozenError from inside the callee.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class P
              def initialize(x, y)
                @x = x
                @y = y
              end
              def x
                @x
              end
              def y
                @y
              end
              def set(a, b)
                @x = a
                @y = b
                self
              end
            end
            def drive
              res = []
              obj = P.new(1, 2)
              i = 0
              while i < 40
                begin
                  obj.set(i, i + 1)
                rescue FrozenError => e
                  res << e.class.to_s
                end
                obj.freeze if i == 25
                i += 1
              end
              res << obj.x << obj.y
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_redefinition() {
        // Redefining the inlined method between two driver runs: the
        // class-version guard deopts and the site recompiles against the
        // new definition. The original definition is restored at the end
        // because the harness runs the code many times in the same VM.
        run_test_with_prelude(
            r##"
            a = drive(30)
            class R
              def v
                99
              end
            end
            b = drive(30)
            class R
              def v
                @a = 5
                @a
              end
            end
            [a, b]
        "##,
            r##"
            class R
              def v
                @a = 5
                @a
              end
            end
            $obj = R.new
            def drive(n)
              res = []
              i = 0
              while i < n
                res << $obj.v
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_float_and_literal() {
        // Float arguments (possibly unboxed in the caller) stored to ivars,
        // frozen literals, and an ivar never stored anywhere (reads as nil).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class F
              def initialize
                @a = nil
                @b = nil
                @c = nil
              end
              def set(x, y)
                @a = x
                @b = y
                @c = 100
                self
              end
              def a
                @a
              end
              def b
                @b
              end
              def c
                @c
              end
              def never
                @unset
              end
            end
            def drive
              res = []
              i = 0
              f = 0.0
              while i < 50
                o = F.new
                o.set(f, :sym)
                res << o.a << o.b << o.c << o.never
                f += 1.5
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_polymorphic_site() {
        // Two receiver classes at the same call site: the receiver-class
        // guard deopts for the non-compiled class.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class A
              def initialize(v)
                @v = v
              end
              def v
                @v
              end
            end
            class B
              def initialize(v)
                @v = v * 10
              end
              def v
                @v
              end
            end
            def drive
              res = []
              objs = [A.new(1), B.new(2)]
              i = 0
              while i < 50
                o = objs[i % 2]
                res << o.v
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_forwarded_new() {
        // `Class#new`'s `o.__builtin_initialize__(...)` under a specialized
        // `new`: initialize is inlined frame-free at the D1 source-routed
        // forwarding site. Includes a block-passing `new` (the forwarded
        // block is ignored by an initialize that takes none — exact).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class Point
              def initialize(x, y)
                @x = x
                @y = y
              end
              def x
                @x
              end
              def y
                @y
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                pt = Point.new(i, i * 2)
                res << pt.x << pt.y
                pt2 = Point.new(i, i) { |a| a }
                res << pt2.x << pt2.y
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_forwarded_trampoline() {
        // A pure forwarding trampoline whose target is inline-eligible:
        // the target is inlined at the forwarded site, both with and
        // without leading arguments (CallerSlot + OuterSlot mix).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class C
              def initialize
                @a = nil
                @b = nil
              end
              def set(a, b)
                @a = a
                @b = b
                self
              end
              def a
                @a
              end
              def b
                @b
              end
              def tset(...)
                set(...)
              end
              def tset2(a, ...)
                set(a, ...)
              end
            end
            def drive
              res = []
              o = C.new
              i = 0
              while i < 50
                o.tset(i, i + 1)
                res << o.a << o.b
                o.tset2(i * 2, i * 3)
                res << o.a << o.b
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_store_load_forwarding() {
        // A load of an ivar stored earlier in the same body is forwarded at
        // compile time; a store overwritten later must still be observable
        // outside the body only in its final state.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class C
              def work(v)
                @a = v
                x = @a
                @b = x
                @a = 7
                @b
              end
              def a
                @a
              end
              def b
                @b
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                o = C.new
                res << o.work(i)
                res << o.a << o.b
                i += 1
              end
              res
            end
        "##,
        );
    }
}
