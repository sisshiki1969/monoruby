use super::*;
#[cfg(target_arch = "x86_64")]
use monoasm_macro::monoasm;

impl JitModule {
    #[cfg(target_arch = "x86_64")]
    pub fn jit_check_stack(&mut self, wb: &jitgen::WriteBack, error: &DestLabel, base: usize) {
        let overflow = self.jit.label();
        assert_eq!(0, self.jit.get_page());
        monoasm! { &mut self.jit,
            cmpq rsp, [rbx + (EXECUTOR_STACK_LIMIT)];
            jle  overflow;
        }
        self.jit.select_page(1);
        self.jit.bind_label(overflow);
        self.gen_write_back(wb, base);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rax, (stack_overflow);
            call rax;
            jmp error;
        }
        self.jit.select_page(0);
    }
}

pub(in crate::codegen) extern "C" fn unimplemented_inst(
    vm: &mut Executor,
    _: &mut Globals,
    opcode: u16,
) -> Option<Value> {
    vm.set_error(MonorubyErr::fatal(format!(
        "internal error: unimplemented instruction. {:04x}",
        opcode
    )));
    None
}

#[repr(C)]
pub(super) struct ErrorReturn {
    value: Option<Value>,
    dest: Option<BytecodePtr>,
}

impl ErrorReturn {
    fn return_err() -> Self {
        Self {
            value: None,
            dest: None,
        }
    }

    fn return_normal(val: Value) -> Self {
        Self {
            value: Some(val),
            dest: None,
        }
    }

    fn goto(dest: BytecodePtr) -> Self {
        Self {
            value: None,
            dest: Some(dest),
        }
    }
}

/// Restore `$!` from the region-entry saves of every rescue clause the
/// suspended *pc* sits inside (innermost first; the outermost save
/// wins) — used when a non-local exit (`return` from a block, `break`)
/// leaves a frame whose execution stopped inside a rescue clause.
fn restore_errinfo_on_exit(
    vm: &mut Executor,
    info: &crate::globals::ISeqInfo,
    pc: crate::bytecodegen::BcIndex,
    lfp: Lfp,
) {
    for slot in info.errinfo_restore_slots(pc) {
        if let Some(v) = lfp.register(slot) {
            vm.set_errinfo(v);
        }
    }
}

pub(super) extern "C" fn handle_error(
    vm: &mut Executor,
    globals: &mut Globals,
    meta: Meta,
    pc: BytecodePtr,
) -> ErrorReturn {
    if vm.exception().is_none() {
        runtime::_dump_stacktrace(vm, globals);
        vm.set_error(MonorubyErr::fatal(
            "internal error: unknown exception.",
        ));
    }
    let func_info = &globals.store[meta.func_id()];
    match &func_info.kind {
        FuncKind::ISeq(info) => {
            let bc_base = globals.store[*info].get_top_pc();
            // check Retry/Redo first.
            // Retry/Redo opcode encodes disp in the lower 32 bits of op1.
            // handle_error receives pc pointing to the retry/redo instruction itself
            // (entry_raise subtracts 16 from r13).
            // dest = pc + 1 + disp
            if let MonorubyErrKind::Retry | MonorubyErrKind::Redo = vm.exception().unwrap().kind() {
                vm.take_error();
                let disp = pc.op1() as i32;
                let dest = pc + (1 + disp as isize);
                return ErrorReturn::goto(dest);
            }
            let pc = pc - bc_base;
            let mut lfp = vm.cfp().lfp();
            let iseq_id = *info;
            let info = &globals.store[*info];
            // check exception table.
            // First, we check method_return.
            let method_return = match vm.exception().unwrap().kind() {
                MonorubyErrKind::MethodReturn(val, target_lfp) => Some((*val, *target_lfp)),
                _ => None,
            };
            if let Some((val, target_lfp)) = method_return {
                // Leaving a frame suspended inside a rescue clause
                // (whether passing through or returning from it)
                // restores `$!` to the region-entry save — the inline
                // bytecodegen restore only covers local exits. Not
                // needed on the error path: a propagating exception
                // overwrites `$!` wherever it is caught.
                restore_errinfo_on_exit(vm, info, pc, lfp);
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    // Suspend the non-local return across the ensure body so
                    // the body can `raise` (set_error) without tripping the
                    // empty-exception guard; `EnsureEnd` restores it.
                    vm.defer_unwind(lfp);
                    ErrorReturn::goto(bc_base + ensure)
                } else if lfp == target_lfp {
                    // Stop unwinding at the matching target LFP. (A
                    // `return` written inside a `define_method` body
                    // already targets that frame — `Lfp::outermost`
                    // stops at the proc-method boundary — so a
                    // MethodReturn merely *passing through* a
                    // define_method frame toward an outer target, e.g.
                    // a closure's `return` routed through
                    // `define_method(:m) { |&b| b.call }`, must NOT be
                    // stopped here.)
                    vm.take_error();
                    // This frame returns now; abandon any deferral it owns.
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_normal(val)
                } else {
                    // Propagating past this frame; its deferral (if any) can
                    // no longer reach its `EnsureEnd`.
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_err()
                };
            }
            if let MonorubyErrKind::Throw(..) = vm.exception().unwrap().kind() {
                return if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                    vm.defer_unwind(lfp);
                    ErrorReturn::goto(bc_base + ensure)
                } else {
                    vm.discard_deferred_unwind(lfp);
                    ErrorReturn::return_err()
                };
            }
            // `break` escaping a block (see `MonorubyErrKind::BlockBreak`):
            // stop at the block's defining frame. If that frame's
            // in-progress call site is the one that received this block
            // literal, resume it with the call returning the break value
            // (CRuby's BREAK catch table). Otherwise the block escaped as
            // a Proc — degrade to LocalJumpError and let the normal
            // rescue machinery below handle it in this very frame.
            let block_break = match vm.exception().unwrap().kind() {
                MonorubyErrKind::BlockBreak(val, block_fid, outer) => {
                    Some((*val, *block_fid, *outer))
                }
                _ => None,
            };
            if let Some((val, block_fid, outer)) = block_break {
                if lfp != outer {
                    // An intermediate frame: run its ensure body (the
                    // break passes through it), then keep unwinding.
                    // Same `$!` restore as the MethodReturn path — but
                    // only for frames being *exited*; the defining
                    // frame below resumes inside its rescue clause and
                    // must keep the caught exception in `$!`.
                    restore_errinfo_on_exit(vm, info, pc, lfp);
                    if let Some((_, Some(ensure), _)) = info.get_exception_dest(pc) {
                        vm.defer_unwind(lfp);
                        return ErrorReturn::goto(bc_base + ensure);
                    }
                    vm.discard_deferred_unwind(lfp);
                    return ErrorReturn::return_err();
                }
                // The defining frame: check the call site *before* any
                // ensure handling — a matching break resumes right after
                // the call, still inside the same begin region, so this
                // frame's ensure must NOT run now (it runs when control
                // leaves the region normally).
                // The suspended pc may point at the call instruction or
                // its trailing InlineCache slot — check both.
                let mut candidates = vec![pc];
                if pc.to_usize() > 0 {
                    candidates.push(pc + (-1i64));
                }
                let call_pos = candidates.into_iter().find(|cand| {
                    globals.store.get_callsite_id(iseq_id, *cand).is_some_and(|id| {
                        globals.store[id].block_fid == Some(block_fid)
                    })
                });
                if let Some(call_pos) = call_pos {
                    let dst = {
                        let id = globals.store.get_callsite_id(iseq_id, call_pos).unwrap();
                        globals.store[id].dst
                    };
                    vm.take_error();
                    vm.discard_deferred_unwind(lfp);
                    if let Some(dst) = dst {
                        unsafe { lfp.set_register(dst, Some(val)) };
                    }
                    // Skip the call instruction + its InlineCache slot.
                    return ErrorReturn::goto(bc_base + call_pos + 2isize);
                }
                // Defining frame reached via a materialized Proc: the
                // original receiving call is gone.
                #[cfg(feature = "emit-bc")]
                eprintln!(
                    "block-break no-match: pc={pc:?} candidates checked, block_fid={block_fid:?}, map={:?}",
                    (0..40).filter_map(|i| globals.store.get_callsite_id(iseq_id, crate::bytecodegen::BcIndex::from(i)).map(|id| (i, globals.store[id].block_fid))).collect::<Vec<_>>()
                );
                vm.take_error();
                vm.set_error(MonorubyErr::new(
                    MonorubyErrKind::LocalJump,
                    "break from proc-closure".to_string(),
                ));
                // fall through to the ordinary exception handling below
                // (rescue table of this frame, or propagate).
            }
            let sourceinfo = info.sourceinfo.clone();
            let loc = info.sourcemap[pc.to_usize()];
            let fid = info.func_id();
            vm.push_error_location(loc, sourceinfo, fid);
            // Fatal errors (Rust panics caught at an `extern "C"` boundary)
            // must never be caught by `rescue`, and we also skip `ensure`
            // since the interpreter/VM state may be inconsistent after a
            // panic. Propagate straight up to the top.
            if vm.exception().unwrap().is_fatal() {
                vm.discard_deferred_unwind(lfp);
                return ErrorReturn::return_err();
            }
            if let Some((Some(rescue), _, err_reg)) = info.get_exception_dest(pc) {
                let err_val = vm.take_ex_obj(globals);
                vm.set_errinfo(err_val);
                if let Some(err_reg) = err_reg {
                    unsafe { lfp.set_register(err_reg, Some(err_val)) };
                }
                return ErrorReturn::goto(bc_base + rescue);
            }
        }
        FuncKind::Builtin { .. } => {
            let lfp = vm.cfp().lfp();
            // First, we check method_return.
            if let MonorubyErrKind::MethodReturn(val, target_lfp) = vm.exception().unwrap().kind() {
                return if lfp == *target_lfp {
                    let val = *val;
                    vm.take_error();
                    ErrorReturn::return_normal(val)
                } else {
                    ErrorReturn::return_err()
                };
            }
            if let MonorubyErrKind::Throw(..) | MonorubyErrKind::BlockBreak(..) =
                vm.exception().unwrap().kind()
            {
                return ErrorReturn::return_err();
            }
            vm.push_internal_error_location(meta.func_id());
        }
        _ => unreachable!(),
    }
    // A normal exception is propagating out of this frame: its `EnsureEnd`
    // (if any) will not run, so drop a deferral it may still own.
    let lfp = vm.cfp().lfp();
    vm.discard_deferred_unwind(lfp);
    ErrorReturn::return_err()
}
