//! Frameless specialization: running a small specialized callee inline,
//! without a Ruby frame of its own.
//!
//! An ordinary specialized call (`send_specialized`) still pays for a
//! whole method frame: the stack check, the call-site pc for
//! `Kernel#caller`, the control-frame push and pop, the callee's entry
//! poll, and the eviction / chain-deopt bookkeeping that lets a suspended
//! frame be converted later — and then the `call` / `ret` pair and a
//! prologue around a body of a few instructions. For a small loop-free
//! body that does none of the things those pieces exist for, they are
//! most of the call.
//!
//! A frameless callee has no frame at all. Its slots, its LFP header and
//! its spill slots live in a *window* the caller reserves inside its own
//! native frame, right below the caller's slot area
//! (`JitContext::note_inline_window`); the caller writes `self`, the
//! arguments and, when the body reads it, the header into the window as
//! plain slot stores of its own (`AbstractState::fill_window`), then
//! moves the native frame pointer and the LFP down into it and runs the
//! callee's body in place (`AsmInst::InlineCall`,
//! `Codegen::gen_inline_call`). With both pointers shifted, the body is
//! emitted exactly as it would be for a frame of its own, prologue and
//! `ret` aside, and everything that reads the LFP finds what it expects.
//! What is gone is everything that makes the frame *visible*: no control
//! frame is linked, so nothing can ever find the callee suspended, report
//! it in a backtrace, or rescue in it.
//!
//! # Exits
//!
//! That only holds while the callee is running compiled. The moment it
//! has to leave for the interpreter it needs a frame, and a side exit
//! gives it one in one of two ways:
//!
//! - A *redo* exit leaves the body for the caller's deopt at the call
//!   instruction, so the interpreter performs the whole call again, with
//!   a real frame. That is sound while the callee has not done anything
//!   observable yet — exactly `side_effect_guard`.
//! - A *materializing* exit (`LSideExitKind::Materialize`,
//!   `Codegen::gen_frameless_materialize`) is what an exit after the
//!   callee's first side effect takes, and what every error exit takes:
//!   it turns the window into the frame the interpreter would have built
//!   — the window already sits where that frame would be
//!   (`JitContext::inline_window_delta`) — links it as the current control
//!   frame, converts the caller as a chain deopt converts a suspended call
//!   site, and resumes the callee in the interpreter where it is. A frame
//!   that materializes this way is never nil-filled, so the exit's
//!   write-back stores `nil` into every slot the body has not written
//!   (`AbstractFrame::get_write_back_with_void`).
//!
//! What a frameless body still cannot contain is anything that needs the
//! frame while it keeps running compiled: a safepoint poll or an eviction
//! point (the window is invisible to the collector and to the walks that
//! convert suspended frames), a call that pushes a frame, and an error
//! exit whose path can run Ruby code before it is taken (`AsmIr::new_error`
//! versus `new_error_pure`). Each of those makes the callee ineligible
//! ([`AsmIr::set_frameless_violation`]); the call site then rolls back and
//! calls it the ordinary way.
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

///
/// Does the body of *iseq_id* read its own LFP header? The window of an
/// inline callee gets a header (outer, meta, svar, block) only when
/// something in the body can look at it: a method call, which may be
/// lowered to an inline builtin that reads the block word or the meta
/// (`block_given?`, `__method__`), an index operation, whose runtime
/// helpers can re-enter Ruby, or anything that touches `$~` / `$_`. A body
/// of loads, stores, arithmetic and branches reads `self` and its
/// arguments from the slots and nothing above them.
///
fn reads_frame_header(store: &Store, iseq_id: ISeqId) -> bool {
    let iseq = &store[iseq_id];
    (0..iseq.bytecode().len()).any(|i| {
        let pc = iseq.get_pc(BcIndex::from(i));
        !matches!(
            TraceIr::from_pc(pc, store),
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
                | TraceIr::Ret(..)
                | TraceIr::Mov(..)
                | TraceIr::InitMethod(..)
                | TraceIr::InlineCache
                | TraceIr::TypeIc(..)
        )
    })
}

impl AbstractState {
    ///
    /// Call a frameless specialized callee — the frameless counterpart of
    /// `send_specialized`: fill the callee's window in this frame and run
    /// its body inline (`AsmInst::InlineCall`).
    ///
    /// *delta* is where the window is (`JitContext::inline_window_delta`):
    /// the callee's slot `s` is this frame's pseudo slot `s + delta / 8`,
    /// and the four header words sit just above its slot 0. They are
    /// written with the frame pointer still this frame's, as plain slot
    /// stores; the `InlineCall` then moves the frame pointer and the LFP
    /// down by *delta* for the body.
    ///
    pub(super) fn send_frameless(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        spec_id: context::SpecializedId,
        delta: i32,
        using_fpr: UsingFpr,
        arg_hints: &[(GP, SlotId)],
        float_args: &[(SlotId, FPReg)],
        redo: Option<AsmDeopt>,
    ) {
        // Taken when the callee hands the call back: deopt to this very
        // call instruction, with this frame as it stands before the call.
        // Made after `get_using_fpr`'s flush, so it reads nothing the body
        // clobbers but the fprs the `InlineCall` brings back first. A
        // caller that already changed this frame on the way to the call
        // (`inline_class_new`) supplies one taken before it did.
        let redo = redo.unwrap_or_else(|| ir.new_deopt(self));
        ir.fpr_save(using_fpr);
        self.fill_window(store, ir, callid, callee_fid, delta, arg_hints, float_args);
        let dst = store[callid].dst;
        self.discard(dst);
        self.clear_above_next_sp();
        // What a materializing exit of the callee writes back into this
        // frame: its state at the call, `dst` untouched, the same snapshot
        // a framed call registers for the chain walk (`ChainExitSpec::new`).
        // The fprs it reads are the ones the exit restores from the save
        // above before using it. Unlike the redo, it describes the frame
        // *after* everything the site did on the way in (`inline_class_new`
        // allocates the receiver before the body runs; the interpreter
        // resumes after the call, with the object in its slot).
        let spec = ChainExitSpec::new_with(ir.exit_write_back(self), using_fpr, dst, self.pc());
        ir.push(AsmInst::InlineCall {
            spec_id,
            delta,
            redo,
            using_fpr,
            spec,
        });
        // `side_effect_guard` is left alone: the callee's return state
        // carries its own, which the result store joins in. A callee that
        // did nothing observable leaves this frame's guard standing — and
        // a frameless caller free to make another frameless call after it.
    }

    ///
    /// The window counterpart of `set_arguments` for the one call shape a
    /// frameless callee accepts (`eligible`): `self` and the required
    /// positionals, plus the header when the body reads it. Same order as
    /// `set_arguments`: the GP-pool residents go straight to their slots
    /// first, before a boxing call can take the pool with them, and the
    /// register-passed floats move last.
    ///
    fn fill_window(
        &mut self,
        store: &Store,
        ir: &mut AsmIr,
        callid: CallSiteId,
        callee_fid: FuncId,
        delta: i32,
        arg_hints: &[(GP, SlotId)],
        float_args: &[(SlotId, FPReg)],
    ) {
        let callee = &store[callee_fid];
        let callsite = &store[callid];
        let args = callsite.args;
        let req = callee.req_num();
        debug_assert_eq!(delta % 8, 0);
        debug_assert_eq!(callsite.pos_num, req);
        // The callee's slot `s` as this frame's pseudo slot; the header
        // words are the (negative) slots above `self`.
        let window = |s: i32| SlotId((s + delta / 8) as u16);

        if reads_frame_header(store, callee.as_iseq()) {
            ir.push(AsmInst::U64ToStack(0, window(-(LFP_SELF - LFP_OUTER) / 8)));
            ir.push(AsmInst::U64ToStack(
                callee.meta().get(),
                window(-(LFP_SELF - LFP_META) / 8),
            ));
            ir.push(AsmInst::U64ToStack(0, window(-(LFP_SELF - LFP_SVAR) / 8)));
            ir.push(AsmInst::U64ToStack(0, window(-(LFP_SELF - LFP_BLOCK) / 8)));
        }

        let hinted = |slot: SlotId| {
            arg_hints
                .iter()
                .find(|(reg, s)| *s == slot && *reg != GP::R11)
                .map(|(reg, _)| *reg)
        };
        let mut direct_filled = vec![];
        for i in 0..req {
            if let Some(reg) = hinted(args + i) {
                ir.push(AsmInst::RegToStack(reg, window(1 + i as i32)));
                direct_filled.push(i);
            }
        }
        for i in 0..req {
            // A parameter handed over in a register leaves its slot
            // unwritten: the body binds it `F` and reads the register,
            // and no safepoint or write-back ever looks at the slot
            // (see the `Init` lowering for the same argument about the
            // locals). Boxing it here would be the one allocation of the
            // call.
            let in_register = float_args.iter().any(|(param, _)| param.0 as usize == 1 + i);
            // A constant is bound `C` in the callee too
            // (`LinkMode::from_caller`, `SlotState::new_method`), and a
            // frameless body reads nothing from the slot of a `C` local:
            // its calls write their arguments from the state, and its
            // side exits hand the whole call back to this frame, whose
            // own state has the constant. Nothing is stored.
            let constant = matches!(self.mode(args + i), LinkMode::C(_));
            if !direct_filled.contains(&i) && !in_register && !constant {
                self.fetch_to_slot(ir, args + i, window(1 + i as i32));
            }
        }
        // Last: a boxing above can call out, and a call takes the whole
        // pool with it.
        for (param, dst) in float_args {
            let i = param.0 as usize - 1;
            let src = match self.mode(args + i) {
                LinkMode::F(x) | LinkMode::Sf(x, _) => x,
                mode => unreachable!("float-passed argument is not fpr-resident: {mode:?}"),
            };
            self.use_as_float_at(args + i);
            ir.float_arg_move(src, *dst);
        }
        // `self`, through rdi and after everything that could call out:
        // the body enters with rdi still holding it (nothing between here
        // and its first instruction touches rdi — the frame shift and the
        // fpr saves use other registers), and its entry notes that
        // (`note_rdi_holds` in the frameless `InitMethod`), so the first
        // use of `self` reads no slot.
        self.load(ir, callsite.recv, GP::Rdi);
        ir.push(AsmInst::RegToStack(GP::Rdi, window(0)));
    }

    /// `[dst] <- slot`, through rax; *dst* is a pseudo slot of this frame
    /// that no state tracks, so this bypasses `reg2stack`.
    fn fetch_to_slot(&mut self, ir: &mut AsmIr, slot: SlotId, dst: SlotId) {
        self.load(ir, slot, GP::Rax);
        ir.push(AsmInst::RegToStack(GP::Rax, dst));
    }
}
