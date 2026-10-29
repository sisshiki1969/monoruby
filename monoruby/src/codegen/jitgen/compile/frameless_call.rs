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
//!
//! # Blocks
//!
//! A specialized `yield` (`compile_yield_specialized`) runs the same way
//! when the block's body passes the same checks (`yield_eligible`): the
//! block of `100.times { @a += 1 }` is loads, stores and arithmetic on
//! `self`, and needs none of the block frame `SetupYieldFrame` builds. What
//! a block adds to the method case is its home: `self` is the home frame's
//! `self`, and a frame of the block carries that frame's LFP at
//! `LFP_OUTER`. The yield site walks the control-frame chain to the home
//! as `setup_yield_frame` does (`AsmInst::YieldOuterLfp`) and stores both
//! into the window — the LFP as the header word a materializing exit then
//! keeps (`InlineCtx::block`), `self` as slot 0. The body itself reads
//! nothing through that word: a block that reaches its outer frame
//! (`LoadDynVar` / `StoreDynVar`, a `yield` or `block_given?` of its own,
//! `break`, a non-local `return`) is not in the instruction allow-list and
//! takes the framed yield. The distance to the home is the same on the
//! specialization stack and on the runtime chain (`JitBlockInfo::outer`):
//! no frameless frame can sit between a `yield` and the block's home,
//! since a frameless body neither yields nor passes a block.

use super::*;
use super::method_call::SpecializedCompileResult;
use crate::codegen::jitgen::context::JitBlockInfo;

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
    iseq.outer.is_none()
        && callee_shape_ok(store, func_id, iseq_id)
        && call_site_ok(store, func_id, callid)
        && cs.pos_num == func.req_num()
        && body_ok(store, iseq_id)
}

///
/// Can the `yield` at `callid` run the block *iseq_id* frameless?
///
/// The block is held to what [`eligible`] asks of a method, with the
/// positional count free: a block drops the arguments it has no parameter
/// for and binds `nil` to the parameters it is not given (`apply_args`,
/// `LinkMode::from_caller_inner`), which `fill_yield_window` follows. A
/// block that reaches its outer frame in any way fails `body_ok`.
///
pub(super) fn yield_eligible(
    store: &Store,
    block_fid: FuncId,
    iseq_id: ISeqId,
    callid: CallSiteId,
) -> bool {
    callee_shape_ok(store, block_fid, iseq_id)
        && call_site_ok(store, block_fid, callid)
        && body_ok(store, iseq_id)
}

/// The callee's parameter list and body shape: no block parameter,
/// required positionals only, no exception handler, no loop, small.
fn callee_shape_ok(store: &Store, func_id: FuncId, iseq_id: ISeqId) -> bool {
    let iseq = &store[iseq_id];
    let func = &store[func_id];
    !(iseq.block_param().is_some()
        || !func.meta().is_simple()
        || func.opt_num() != 0
        || func.post_num() != 0
        || func.is_rest()
        || func.kw_rest().is_some()
        || !func.kw_names().is_empty()
        || iseq.has_exception_handler()
        || iseq.bb_info.has_loop()
        || iseq.bytecode().len() > FRAMELESS_MAX_BYTECODES)
}

/// The call site: plain positionals, no block, no splat, no keywords.
fn call_site_ok(store: &Store, func_id: FuncId, callid: CallSiteId) -> bool {
    let cs = &store[callid];
    !(cs.block_fid.is_some()
        || cs.block_arg.is_some()
        || cs.forwarding
        || !cs.splat_pos().is_empty()
        || !cs.hash_splat_pos().is_empty()
        || cs.kw_may_exists()
        || !store.is_simple_call(func_id, callid))
}

/// Is the body built only of instructions that need no frame of their own?
fn body_ok(store: &Store, iseq_id: ISeqId) -> bool {
    let iseq = &store[iseq_id];
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
            | TraceIr::InlineCache => true,
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
                self.discard_cond_results();
                self.frameless_rejected.insert(iseq);
                None
            }
        }
    }

    ///
    /// Compile the `yield` at `callid` to the block *iseq* as a frameless
    /// specialized call — the yield counterpart of [`Self::try_frameless_iseq`],
    /// with the same rollback when the block turns out not to be frameless.
    /// Only called when [`yield_eligible`] passed.
    ///
    pub(super) fn try_frameless_yield(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        block: &JitBlockInfo,
        iseq: ISeqId,
    ) -> Option<CompileResult> {
        if self.frameless_rejected.contains(&iseq) {
            return None;
        }
        let ir_save = ir.save();
        let state_save = state.clone();
        let depth = self.stack_frame_len();
        let unfrozen_save = (self.unfrozen_slots.clone(), self.instr_unfrozen.clone());
        match self.compile_yield_frameless(state, ir, callid, block, iseq) {
            Ok(res) => Some(res),
            Err(_) => {
                assert_eq!(
                    self.stack_frame_len(),
                    depth,
                    "a rolled-back frameless yield left frames on the specialization stack"
                );
                ir.restore(ir_save);
                *state = state_save;
                (self.unfrozen_slots, self.instr_unfrozen) = unfrozen_save;
                self.discard_cond_results();
                self.frameless_rejected.insert(iseq);
                None
            }
        }
    }

    ///
    /// The frameless half of `compile_yield_specialized`: compile the block
    /// frameless, then fill its window and run it inline. The block's
    /// specialization frame is pushed one level up from the yielding frame
    /// (`block.add(1)`), exactly as the framed yield pushes it, so the
    /// lexical bookkeeping of the compile is the same; what differs is the
    /// call emitted afterwards.
    ///
    fn compile_yield_frameless(
        &mut self,
        state: &mut AbstractState,
        ir: &mut AsmIr,
        callid: CallSiteId,
        block: &JitBlockInfo,
        iseq: ISeqId,
    ) -> JitResult<CompileResult> {
        let dst = self.store[callid].dst;
        // The control-frame hops from the yielding frame to the block's
        // home, for `YieldOuterLfp` (`setup_yield_frame` walks the same
        // count: its `outer - 1`, from the `add(1)`'d value).
        let hops = block.outer;
        let JitBlockInfo {
            block_fid: callee_fid,
            self_class,
            outer,
        } = block.add(1);
        debug_assert!(self.store.is_simple_call(callee_fid, callid));
        let args_info = JitArgumentInfo::new(
            LinkMode::from_caller_yield(&self.store, callee_fid, callid, state, self_class),
            vec![],
        );
        // A frameless callee never asks for a stage-2 splice landing
        // (`specialized_iseq`).
        self.current_frame_mut().landing_sink = false;
        let SpecializedCompileResult {
            entry: _,
            return_state,
            kw_routed: _,
            kw_vetoed: _,
            had_deopt: _,
            generic_yield: _,
            spec_id,
            using_fpr: frozen_using_fpr,
            float_return,
            // A block's plain return never hands an object over
            // (`return_virtual_in_caller`).
            return_virtual: _,
        } = self.compile_specialized_func(
            state,
            iseq,
            self_class,
            args_info,
            Some(outer),
            callid,
            false,
            true,
        )?;
        // The block's body runs in a window of this frame: reserve it, and
        // find where it is.
        self.note_inline_window(spec_id);
        let delta = self.inline_window_delta();
        let arg_hints = state.peek_gp_residents();
        // The save set was frozen when the block's compile began — see the
        // framed yield site and `specialized_compile`.
        let live = state.get_using_fpr(ir);
        let using_fpr = frozen_using_fpr;
        debug_assert!(using_fpr.is_superset_of(&live), "{using_fpr:?} < {live:?}");
        self.record_call_site_fpr_save(spec_id, using_fpr);
        state.yield_frameless(
            ir,
            &self.store,
            callid,
            callee_fid,
            spec_id,
            delta,
            using_fpr,
            &arg_hints,
            hops,
        );
        let res = state.def_rax2acc_return(ir, dst, return_state, float_return);
        state.capture_guard_after_call(ir, &self.store, dst);
        Ok(res)
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
            // An elided object (`doc/escape_analysis.md`) is the same: the
            // callee's entry state holds it over this frame's slots, and
            // its window slot is never read (the collector does not see
            // the window either).
            let elided = self.virtual_at(args + i).is_some();
            if !direct_filled.contains(&i) && !in_register && !constant && !elided {
                self.fetch_to_slot(ir, args + i, window(1 + i as i32));
            }
        }
        // The elided arguments are the callee's now (see `set_arguments`).
        for slot in args..args + req {
            if let Some(src) = self.take_virtual(slot) {
                self.discard_dead_elements(src, slot);
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

    ///
    /// Run a frameless block — the yield counterpart of
    /// [`Self::send_frameless`]: fill the block's window in this frame and
    /// run its body inline (`AsmInst::InlineCall`).
    ///
    pub(super) fn yield_frameless(
        &mut self,
        ir: &mut AsmIr,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        spec_id: context::SpecializedId,
        delta: i32,
        using_fpr: UsingFpr,
        arg_hints: &[(GP, SlotId)],
        hops: usize,
    ) {
        // The redo: deopt to this very `yield`, with this frame as it
        // stands before it. Made after `get_using_fpr`'s flush, as in
        // `send_frameless`.
        let redo = ir.new_deopt(self);
        ir.fpr_save(using_fpr);
        self.fill_yield_window(store, ir, callid, callee_fid, delta, arg_hints, hops);
        let dst = store[callid].dst;
        self.discard(dst);
        self.clear_above_next_sp();
        // What a materializing exit of the block writes back into this
        // frame (`send_frameless`).
        let spec = ChainExitSpec::new_with(ir.exit_write_back(self), using_fpr, dst, self.pc());
        ir.push(AsmInst::InlineCall {
            spec_id,
            delta,
            redo,
            using_fpr,
            spec,
        });
    }

    ///
    /// The window counterpart of `set_arguments` for a `yield`
    /// (`yield_eligible`): the positionals the block has parameters for —
    /// the rest are dropped, and a parameter the yield does not supply is
    /// bound `C(nil)` in the block (`LinkMode::from_caller_inner`), which
    /// a frameless body never reads from its slot — then the header, and
    /// last the block's home: its LFP as the `LFP_OUTER` word and its
    /// `self` as slot 0, both from `YieldOuterLfp`.
    ///
    fn fill_yield_window(
        &mut self,
        store: &Store,
        ir: &mut AsmIr,
        callid: CallSiteId,
        callee_fid: FuncId,
        delta: i32,
        arg_hints: &[(GP, SlotId)],
        hops: usize,
    ) {
        let callee = &store[callee_fid];
        let callsite = &store[callid];
        let args = callsite.args;
        let filled = callsite.pos_num.min(callee.req_num());
        debug_assert_eq!(delta % 8, 0);
        let window = |s: i32| SlotId((s + delta / 8) as u16);

        let hinted = |slot: SlotId| {
            arg_hints
                .iter()
                .find(|(reg, s)| *s == slot && *reg != GP::R11)
                .map(|(reg, _)| *reg)
        };
        let mut direct_filled = vec![];
        for i in 0..filled {
            if let Some(reg) = hinted(args + i) {
                ir.push(AsmInst::RegToStack(reg, window(1 + i as i32)));
                direct_filled.push(i);
            }
        }
        for i in 0..filled {
            // A constant is bound `C` in the block too; nothing is stored
            // (see `fill_window`). An elided argument never reaches a yield
            // (`from_caller_yield` hands none over).
            let constant = matches!(self.mode(args + i), LinkMode::C(_));
            debug_assert!(self.virtual_at(args + i).is_none());
            if !direct_filled.contains(&i) && !constant {
                self.fetch_to_slot(ir, args + i, window(1 + i as i32));
            }
        }
        if reads_frame_header(store, callee.as_iseq()) {
            ir.push(AsmInst::U64ToStack(
                callee.meta().get(),
                window(-(LFP_SELF - LFP_META) / 8),
            ));
            ir.push(AsmInst::U64ToStack(0, window(-(LFP_SELF - LFP_SVAR) / 8)));
            ir.push(AsmInst::U64ToStack(0, window(-(LFP_SELF - LFP_BLOCK) / 8)));
        }
        // The home's LFP always: a materializing exit of the block keeps it
        // as the frame's outer (`InlineCtx::block`). `self` through rdi and
        // last, as `fill_window` does: the body enters with rdi still
        // holding it (`note_rdi_holds` in the frameless `InitMethod`).
        ir.push(AsmInst::YieldOuterLfp { outer: hops });
        ir.push(AsmInst::RegToStack(GP::Rax, window(-(LFP_SELF - LFP_OUTER) / 8)));
        ir.push(AsmInst::RegToStack(GP::Rdi, window(0)));
    }

    /// `[dst] <- slot`, through rax; *dst* is a pseudo slot of this frame
    /// that no state tracks, so this bypasses `reg2stack`.
    fn fetch_to_slot(&mut self, ir: &mut AsmIr, slot: SlotId, dst: SlotId) {
        self.load(ir, slot, GP::Rax);
        ir.push(AsmInst::RegToStack(GP::Rax, dst));
    }
}
