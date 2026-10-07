//! Frameless specialization: calling a small specialized callee without a
//! Ruby frame of its own.
//!
//! An ordinary specialized call (`send_specialized`) still pays for a
//! whole method frame: the stack check, the call-site pc for
//! `Kernel#caller`, the control-frame push and pop, the callee's entry
//! poll, and the eviction / chain-deopt bookkeeping that lets a suspended
//! frame be converted later. For a small loop-free body that does none of
//! the things those pieces exist for, they are most of the call.
//!
//! A frameless callee keeps its native frame — it is still a separate
//! function, reached by `call`, with its own prologue and its own
//! rbp-relative slots — and its local frame header (`SetupMethodFrame`),
//! so everything that reads the LFP finds what it expects. What it drops
//! is everything that makes the frame *visible*: no control frame is
//! linked, so nothing can ever find the callee suspended, report it in a
//! backtrace, or rescue in it.
//!
//! # Exits
//!
//! That only holds if the callee never needs to be found. It must not
//! suspend (no safepoint, no call that pushes a frame), raise, or deopt
//! into the interpreter in its own frame. Every side exit is therefore
//! turned into a *redo*: the callee returns 0 in rax, and the caller
//! deopts at the call instruction, so the interpreter performs the whole
//! call again, with a real frame. That is sound while the callee has not
//! done anything observable yet — exactly `side_effect_guard` — so an exit
//! compiled after a side effect, an error exit, an eviction point or a
//! poll makes the callee ineligible ([`AsmIr::set_frameless_violation`]);
//! the call site then rolls back and calls it the ordinary way.
//!
//! The static check below is only a cheap pre-filter for that compile:
//! the violation tracking is what makes the result sound.

use super::*;

///
/// Bytecode budget for a frameless callee. Every call site gets its own
/// copy of the body, so this bounds the code a unit can grow by per site.
///
const FRAMELESS_MAX_BYTECODES: usize = 40;

///
/// Can the call `callid` to *iseq_id* be tried frameless?
///
/// The callee must be a plain method — no outer frame, no block
/// parameter, required positionals only, no exception handler, no loop —
/// small, and built only of instructions that need no frame of their own.
/// The call site must pass exactly those positionals and no block.
///
pub(super) fn eligible(
    store: &Store,
    func_id: FuncId,
    iseq_id: ISeqId,
    callid: CallSiteId,
) -> bool {
    let iseq = &store[iseq_id];
    let func = &store[func_id];
    let cs = &store[callid];
    if iseq.outer.is_some()
        || iseq.block_param().is_some()
        || !func.meta().is_simple()
        || func.opt_num() != 0
        || func.post_num() != 0
        || func.is_rest()
        || func.kw_rest().is_some()
        || !func.kw_names().is_empty()
        || iseq.has_exception_handler()
        || iseq.bb_info.has_loop()
        || iseq.bytecode().len() > FRAMELESS_MAX_BYTECODES
    {
        return false;
    }
    if cs.block_fid.is_some()
        || cs.block_arg.is_some()
        || cs.forwarding
        || !cs.splat_pos().is_empty()
        || !cs.hash_splat_pos().is_empty()
        || cs.kw_may_exists()
        || cs.pos_num != func.req_num()
        || !store.is_simple_call(func_id, callid)
    {
        return false;
    }
    (0..iseq.bytecode().len()).all(|i| {
        let pc = iseq.get_pc(BcIndex::from(i));
        match TraceIr::from_pc(pc, store) {
            TraceIr::Br(..)
            | TraceIr::CondBr(..)
            | TraceIr::NilBr(..)
            | TraceIr::OptCase { .. }
            | TraceIr::FrozenLiteral(..)
            | TraceIr::StringFreeze(..)
            | TraceIr::Literal(..)
            | TraceIr::Array { .. }
            | TraceIr::Hash { .. }
            | TraceIr::Range { .. }
            | TraceIr::LoadConst(..)
            | TraceIr::LoadIvar(..)
            | TraceIr::StoreIvar(..)
            | TraceIr::UnOp { .. }
            | TraceIr::BinOp { .. }
            | TraceIr::BinCmp { .. }
            | TraceIr::Index { .. }
            | TraceIr::IndexAssign { .. }
            | TraceIr::Ret(..)
            | TraceIr::Mov(..)
            | TraceIr::InitMethod(..)
            | TraceIr::InlineCache
            | TraceIr::TypeIc(..) => true,
            // A call is compiled as whatever the call site resolves to; one
            // that needs a frame or can raise is caught as a violation.
            TraceIr::MethodCall { callid, .. } => {
                store[callid].block_fid.is_none() && store[callid].block_arg.is_none()
            }
            _ => false,
        }
    })
}

impl<'a> JitContext<'a> {
    ///
    /// Compile the call `callid` to *iseq* as a frameless specialized
    /// call. `None` when the callee turned out not to be frameless: the
    /// compile is then rolled back, leaving `state` and `ir` as they were,
    /// and the caller compiles the site some other way.
    ///
    pub(super) fn try_frameless_iseq(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        recv_class: CachedClass,
        func_id: FuncId,
        iseq: ISeqId,
    ) -> Option<CompileResult> {
        if self.frameless_rejected.contains(&iseq) {
            return None;
        }
        // The same rollback as `method_call_with_residual`'s.
        let ir_save = ir.save();
        let state_save = state.clone();
        let depth = self.stack_frame_len();
        let unfrozen_save = (self.unfrozen_slots.clone(), self.instr_unfrozen.clone());
        let fused_skip_save = self.fused_skip;
        match self.specialized_iseq(
            state, ir, callid, recv_class, func_id, iseq, true, None, true,
        ) {
            Ok(res) => Some(res),
            Err(_) => {
                assert_eq!(
                    self.stack_frame_len(),
                    depth,
                    "a rolled-back frameless compile left frames on the specialization stack"
                );
                ir.restore(ir_save);
                *state = state_save;
                (self.unfrozen_slots, self.instr_unfrozen) = unfrozen_save;
                self.fused_skip = fused_skip_save;
                self.discard_cond_results();
                self.frameless_rejected.insert(iseq);
                None
            }
        }
    }
}

impl AbstractState {
    ///
    /// Call a frameless specialized callee — the frameless counterpart of
    /// `send_specialized`.
    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn send_frameless(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        entry: JitLabel,
        using_fpr: UsingFpr,
        arg_hints: &[(GP, SlotId)],
        float_args: &[(SlotId, FPReg)],
    ) {
        // Taken when the callee hands the call back: deopt to this very
        // call instruction, with this frame as it stands before the call.
        // Made after `get_using_fpr`'s flush, so it reads nothing the call
        // clobbers but the fprs `fpr_restore_cont` brings back first.
        let redo = ir.new_deopt(self);
        ir.fpr_save_cont(using_fpr);
        self.set_arguments(store, ir, callid, callee_fid, false, arg_hints, float_args);
        self.discard(store[callid].dst);
        self.clear_above_next_sp();
        // The header still goes in: whatever reads the callee's LFP
        // (`block_given?`, `$~`, the GC if it ever scans it) finds a
        // well-formed frame, just one that is linked to nothing.
        ir.push(AsmInst::SetupMethodFrame {
            meta: store[callee_fid].meta(),
            callid,
            outer_lfp: None,
        });
        ir.push(AsmInst::FramelessCall { entry });
        ir.fpr_restore_cont(using_fpr);
        ir.push(AsmInst::FramelessRedo { deopt: redo });
        // `side_effect_guard` is left alone: the callee's return state
        // carries its own, which the result store joins in. A callee that
        // did nothing observable leaves this frame's guard standing — and
        // a frameless caller free to make another frameless call after it.
    }
}
