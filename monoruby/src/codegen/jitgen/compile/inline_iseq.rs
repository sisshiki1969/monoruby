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
    /// Emission-internal (never appears in `env`): the value currently in
    /// the cycle-breaking scratch register loaded by `ScheduledOp::Hold`.
    /// Placed by the store scheduler when a pending ivar load must survive
    /// the store to its own ivar (a swap/rotate cycle, or a returned value
    /// read before an overwrite).
    Held,
}

struct InlineStore {
    ivarid: IvarId,
    src: InlineOperand,
    /// `true` when this store lowers to the bounds-checked `StoreIVarHeap`,
    /// whose cold path (`set_ivar`) clobbers caller-saved registers — a
    /// `Hold` cannot stay live across it.
    heap: bool,
}

enum ScheduledOp {
    /// Load the *initial* value of `ivarid` into an unbound pool scratch
    /// register (`alloc_scratch_gp`); subsequent `InlineOperand::Held`
    /// operands consume it. At most one `Hold` is live at a time, and no
    /// heap-type store may appear in a plan containing a `Hold` (see the
    /// scheduler).
    Hold { ivarid: IvarId },
    Store(InlineStore),
}

pub(super) struct InlinePlan {
    ops: Vec<ScheduledOp>,
    ret: InlineOperand,
}

impl InlinePlan {
    fn has_store(&self) -> bool {
        self.ops
            .iter()
            .any(|op| matches!(op, ScheduledOp::Store(_)))
    }
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
                    "    inline_iseq: {} stores={} holds={} forwarded={forwarded}",
                    self.store[fid].name().map_or_else(String::new, |n| n.to_string()),
                    plan.ops
                        .iter()
                        .filter(|op| matches!(op, ScheduledOp::Store(_)))
                        .count(),
                    plan.ops
                        .iter()
                        .filter(|op| matches!(op, ScheduledOp::Hold { .. }))
                        .count(),
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
        let is_object_ty = self.store[recv_class].is_object_ty_instance();
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
                    let val = env[src.0 as usize];
                    stores.push(InlineStore {
                        ivarid: id,
                        src: val,
                        heap: !(is_object_ty && id.is_inline()),
                    });
                    ivar_env.push((id, val));
                }
                TraceIr::Ret(ret) => {
                    // The result operand is irrelevant when the call site
                    // discards it — don't let it force a Hold.
                    let ret = if callsite.dst.is_some() {
                        env[ret.0 as usize]
                    } else {
                        Const(Value::nil())
                    };
                    return match Self::schedule_inline_stores(stores, ret) {
                        Some(plan) => InlineAnalysis::Inlinable(plan),
                        None => InlineAnalysis::NotInlinable,
                    };
                }
                _ => return InlineAnalysis::NotInlinable,
            }
        }
        // fell off the end without a `Ret` terminal.
        InlineAnalysis::NotInlinable
    }

    ///
    /// Order the stores so that every `PendingIvar(X)` operand (a read of
    /// X's *initial* value, materialized right before its consuming store)
    /// is emitted before the store to X.
    ///
    /// Because the body is uninterruptible, intermediate ivar states are
    /// unobservable: only the final store per ivar is kept (dead-store
    /// elimination), and the surviving stores may be freely reordered
    /// under the read-before-overwrite constraint above. Each store has
    /// exactly one source, so the "reads the initial value of" graph is
    /// functional and its cycles (swap / rotate patterns) are disjoint;
    /// one cycle is broken by holding the initial value of its entry ivar
    /// in a scratch register (`ScheduledOp::Hold`), after which the rest
    /// of the cycle chains in reverse dependency order.
    ///
    /// Bails out (`None`, falling back to specialization) when:
    /// * two `Hold`s would have to be live at once (e.g. a cycle plus a
    ///   returned initial value of another overwritten ivar), or
    /// * a plan needing a `Hold` contains a heap-type store, whose cold
    ///   path (`set_ivar`) clobbers the scratch register.
    ///
    fn schedule_inline_stores(
        stores: Vec<InlineStore>,
        mut ret: InlineOperand,
    ) -> Option<InlinePlan> {
        use InlineOperand::*;
        // dead-store elimination: keep the final store per ivar, in the
        // order of the final occurrences.
        let mut remaining: Vec<InlineStore> = vec![];
        for s in stores.into_iter() {
            remaining.retain(|t| t.ivarid != s.ivarid);
            remaining.push(s);
        }
        let any_heap = remaining.iter().any(|s| s.heap);

        let mut ops: Vec<ScheduledOp> = vec![];
        let mut held_consumers = 0usize; // Held uses not yet scheduled (ret excluded)
        let mut ret_holds = false; // ret consumes the Held value (live to the end)
        while !remaining.is_empty() {
            // A store to X is emittable when no *other* remaining operand
            // (nor the result) still reads X's initial value. Its own
            // source is materialized before the store executes, so a
            // self-reference (`@a = @a`) does not block.
            let pos = (0..remaining.len()).find(|&i| {
                let x = remaining[i].ivarid;
                !remaining
                    .iter()
                    .enumerate()
                    .any(|(j, s)| j != i && s.src == PendingIvar(x))
                    && ret != PendingIvar(x)
            });
            if let Some(pos) = pos {
                let s = remaining.remove(pos);
                if s.src == Held {
                    held_consumers -= 1;
                }
                ops.push(ScheduledOp::Store(s));
            } else {
                // Every remaining store is blocked: a cycle. Break it by
                // holding the initial value of the first store's ivar.
                if held_consumers != 0 || ret_holds || any_heap {
                    return None;
                }
                let x = remaining[0].ivarid;
                for s in remaining.iter_mut() {
                    if s.src == PendingIvar(x) {
                        s.src = Held;
                        held_consumers += 1;
                    }
                }
                if ret == PendingIvar(x) {
                    ret = Held;
                    ret_holds = true;
                }
                ops.push(ScheduledOp::Hold { ivarid: x });
            }
        }
        // A returned initial value of an overwritten ivar reaches here as a
        // still-pending `ret` only via the cycle path above (the emittable
        // check includes `ret`), so no post-pass is needed.
        Some(InlinePlan { ops, ret })
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

        let has_store = plan.has_store();
        if has_store {
            // The receiver cannot become frozen mid-body (no calls), so one
            // hoisted guard covers every store.
            state.load(ir, recv, GP::Rdi);
            let deopt = ir.new_deopt(state);
            ir.guard_frozen(deopt);
        }
        // The cycle-breaking scratch register (unbound in the register
        // file). Nothing between the `Hold` and its last consumer can
        // clobber it: the scheduler excludes heap-type stores from a plan
        // with a `Hold`, and everything else in the window (inline stores,
        // pure loads, `lit2reg`) leaves pool registers alone.
        let mut held_gp: Option<GP> = None;
        for op in &plan.ops {
            match op {
                ScheduledOp::Hold { ivarid } => {
                    state.load(ir, recv, GP::Rdi);
                    let gp = state.alloc_scratch_gp(ir);
                    self.push_inline_ivar_load(ir, *ivarid, is_object_ty, gp);
                    held_gp = Some(gp);
                }
                ScheduledOp::Store(InlineStore { ivarid, src, heap }) => {
                    let src_gp = if *src == InlineOperand::Held {
                        held_gp.unwrap()
                    } else {
                        self.materialize_inline_operand(state, ir, *src, recv, is_object_ty)
                    };
                    // Reload the receiver: materialization may clobber rdi.
                    // This load itself cannot clobber `src_gp` — the
                    // receiver is a heap value, so no boxing call is
                    // involved.
                    state.load(ir, recv, GP::Rdi);
                    if !heap {
                        ir.push(AsmInst::StoreIVarInline {
                            src: src_gp,
                            ivarid: *ivarid,
                        });
                    } else {
                        // The bounds-checked store: its cold path calls
                        // `set_ivar` (which may grow the ivar table — an
                        // allocation, but no raise and no inline GC), so
                        // flush the GP pool first, exactly like
                        // `attr_writer`.
                        let using_fpr = state.get_using_fpr(ir);
                        ir.push(AsmInst::StoreIVarHeap {
                            src: src_gp,
                            ivarid: *ivarid,
                            is_object_ty,
                            using_fpr,
                        });
                    }
                }
            }
        }
        if has_store {
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
                InlineOperand::Held => {
                    // The scheduler placed a `Hold` whose last consumer is
                    // the result; store it straight to dst's home.
                    state.def_reg2acc(ir, held_gp.unwrap(), dst);
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
            InlineOperand::Held => unreachable!("Held is consumed by the caller"),
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
    fn inline_iseq_swap() {
        // `tmp = @x; @x = @y; @y = tmp`: a 2-cycle in the read-initial
        // graph, broken by one `Hold` (the initial @x is kept in a scratch
        // register across the store to @x).
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
    fn inline_iseq_swap_variants() {
        // Scheduler coverage: a 3-rotation (one Hold, rest chains), an
        // acyclic chain (no Hold), dead stores, a returned initial value
        // of an overwritten ivar (ret-Hold), and two sequential swaps
        // (two Holds, never live at once).
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class P
              def initialize
                @x = 1
                @y = 2
                @z = 3
                @a = 4
                @b = 5
              end
              def rot3
                t = @x
                @x = @y
                @y = @z
                @z = t
              end
              def chain
                @a = @b
                @b = @x
                self
              end
              def dead
                @a = 100
                @a = @b
                @a
              end
              def take
                t = @a
                @a = 0
                t
              end
              def dswap
                t = @x
                @x = @y
                @y = t
                u = @a
                @a = @b
                @b = u
                self
              end
              def all
                [@x, @y, @z, @a, @b]
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                o = P.new
                o.rot3
                res << o.all
                res << o.chain.all
                res << o.dead
                res << o.take << o.all
                res << o.dswap.all
                i += 1
              end
              res
            end
        "##,
        );
    }

    #[test]
    fn inline_iseq_swap_fallbacks() {
        // Shapes the scheduler refuses (heap-type stores with a Hold; two
        // Holds live at once): must still run correctly via specialization.
        run_test_with_prelude(
            r##"
            drive
        "##,
            r##"
            class H
              def initialize
                @a = 1
                @b = 2
                @c = 3
                @d = 4
                @e = 5
                @f = 6
                @g = 7
                @h = 8
              end
              def swap_heap
                t = @g
                @g = @h
                @h = t
              end
              def all
                [@g, @h]
              end
            end
            class Q
              def initialize
                @a = 1
                @b = 2
              end
              def cycle_and_ret
                t = @b
                x = @a
                @a = @b
                @b = x
                t
              end
              def all
                [@a, @b]
              end
            end
            def drive
              res = []
              i = 0
              while i < 50
                h = H.new
                h.swap_heap
                res << h.all
                q = Q.new
                res << q.cycle_and_ret << q.all
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
