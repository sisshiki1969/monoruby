use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Lower `SetupMethodFrame`: write the callee frame's outer/meta/svar
    /// and block fields at `[sp - (RSP_LOCAL_FRAME + LFP_*)]`. Mirrors x86
    /// `setup_method_frame`.
    pub(super) fn a64_setup_method_frame(
        &mut self,
        meta: Meta,
        outer_lfp: Option<Lfp>,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
    ) {
        let outer = match outer_lfp {
            Some(lfp) => lfp.as_ptr() as u64,
            None => 0,
        };
        monoasm_arm64!(&mut self.jit, mov x9, (outer););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
        monoasm_arm64!(&mut self.jit, mov x9, (meta.get()););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_META) as u32);
        monoasm_arm64!(&mut self.jit, mov x9, (0u64););
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
        self.a64_set_block(block_fid, block_arg);
    }

    /// Write the callee frame's block-handler slot. Mirrors x86 `set_block`.
    pub(super) fn a64_set_block(&mut self, block_fid: Option<FuncId>, block_arg: Option<SlotId>) {
        let block_off = (RSP_LOCAL_FRAME + LFP_BLOCK) as u32;
        if let Some(func_id) = block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm_arm64!(&mut self.jit, mov x9, (bh.id()););
        } else if let Some(block) = block_arg {
            let lfp = GP::R14.a64().0;
            let off = block.0 as u32 * 8 + LFP_SELF as u32;
            self.a64_frame_load(9, lfp, off);
        } else {
            monoasm_arm64!(&mut self.jit, mov x9, (0u64););
        }
        self.a64_store_x9_below_sp(block_off);
    }

    /// Lower `SetArguments`: one C call to `jit_generic_set_arguments(vm,
    /// globals, callid, callee_lfp, fid)` which massages the caller's args into
    /// the callee frame. Returns rax==0 (None) on error (followed by a
    /// HandleError in the IR). `offset` (callee frame size, 16-aligned) is
    /// reserved below sp around the call (large frames go through `a64_sp_*`).
    pub(super) fn a64_set_arguments(&mut self, callid: CallSiteId, fid: FuncId, offset: usize) -> bool {
        let f = crate::runtime::jit_generic_set_arguments as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, sp, #(RSP_LOCAL_FRAME as u32); // callee_lfp (call-site sp)
            mov x4, (fid.get() as u64);        // callee fid
            str x30, [sp, #-16]!;              // save LR
        );
        self.a64_sp_sub(offset as u32);        // reserve callee scratch
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;
        );
        self.a64_sp_add(offset as u32);
        monoasm_arm64!(&mut self.jit,
            ldr x30, [sp], #16;                // restore LR
        );
        true
    }

    /// Lower the D1 source-routed `SetArgumentsForwarded` fast path (the
    /// deferred `...`-rest case). The trampoline `f`'s `...` rest `Array` was
    /// elided at frame entry, so the forwarded positionals are copied straight
    /// from the *caller* frame into the callee's argument slots — no array is
    /// ever built. A literal port of x86 `jit_set_arguments_forwarded`'s
    /// `deferred_src` branch:
    ///
    /// * `recv` and the `lead_num` leading args are `f`'s own slots
    ///   (`x22`/LFP-relative via `conv`).
    /// * The `expected_len` forwarded positionals live in the caller frame:
    ///   `f` saved the caller's frame pointer at `[x29]`, and the structural
    ///   gate guarantees the caller is exactly one (outermost, non-specialized)
    ///   level up, so the source slots are `[caller_fp - rbp_local(src + j)]`
    ///   (the fp→local displacement is arch-neutral —
    ///   `RBP_LOCAL_FRAME == (BP_CFP + CFP_LFP) + 8`, matching
    ///   `load_dyn_var_specialized`).
    /// * The `none_fill` trailing optional slots (statically not covered by the
    ///   forwarded args) get `0`, exactly as `fill_positional_args` writes for
    ///   an absent optional — the callee prologue's `CheckLocal` then runs the
    ///   defaults.
    ///
    /// Statically-bound arity and a nil forwarded `**kwrest` are gate
    /// invariants, so there is no length/kw guard and no fallback. Leaves
    /// `x0 = NIL_VALUE` (the success sentinel the following `HandleError`
    /// checks). Scratch: x9..x15 are reserved lowering temps (never GP-mapped);
    /// x10 is used internally by `a64_frame_load/store` for large offsets, so
    /// the fixed temps below avoid it.
    pub(super) fn a64_set_arguments_forwarded_deferred(
        &mut self,
        recv: SlotId,
        args: SlotId,
        lead_num: usize,
        expected_len: usize,
        none_fill: usize,
        src: SlotId,
    ) -> bool {
        // Fixed lowering temps (x9..x15 are never GP-mapped; x10 is the
        // internal scratch of `a64_frame_load/store`, so it is left free):
        //   x13 = callee LFP base (sp - RSP_LOCAL_FRAME), stable across the copy
        //   x12 = value in transit
        //   x11 = caller frame pointer
        const CLFP: u32 = 13;
        const VAL: u32 = 12;
        const CALLER_FP: u32 = 11;
        let lfp = GP::R14.a64().0; // x22: f's own LFP
        monoasm_arm64!(&mut self.jit,
            sub x13, sp, #(RSP_LOCAL_FRAME as u32);
        );
        // self <- f's own recv slot
        self.a64_frame_load(VAL, lfp, conv(recv) as u32);
        self.a64_frame_store(VAL, CLFP, LFP_SELF as u32);
        // leading args (f's own slots)
        for i in 0..lead_num {
            self.a64_frame_load(VAL, lfp, conv(args + i) as u32);
            self.a64_frame_store(VAL, CLFP, (LFP_ARG0 + 8 * i as i32) as u32);
        }
        // forwarded positionals routed straight from the caller frame
        if expected_len != 0 {
            monoasm_arm64!(&mut self.jit, ldr x11, [x29];);
            for j in 0..expected_len {
                self.a64_frame_load(VAL, CALLER_FP, rbp_local(src + j) as u32);
                self.a64_frame_store(VAL, CLFP, (LFP_ARG0 + 8 * (lead_num + j) as i32) as u32);
            }
        }
        // None-fill the statically-uncovered trailing optionals (0 sentinel)
        if none_fill != 0 {
            monoasm_arm64!(&mut self.jit, mov x12, (0u64););
            for j in 0..none_fill {
                self.a64_frame_store(
                    VAL,
                    CLFP,
                    (LFP_ARG0 + 8 * (lead_num + expected_len + j) as i32) as u32,
                );
            }
        }
        monoasm_arm64!(&mut self.jit, mov x0, (NIL_VALUE as u64););
        true
    }

    /// Lower the *eager* (non-deferred) `SetArgumentsForwarded` fast path: the
    /// `...` rest `Array` was materialized in `f`'s rest slot and is forwarded
    /// into a req-only callee. Port of the x86
    /// `jit_set_arguments_forwarded`'s non-deferred branch — copy `recv`/lead
    /// from `f`'s own slots, then, if the rest slot really holds an `Array` of
    /// exactly `expected_len` elements (and no keyword is forwarded), two-copy
    /// its elements straight into the callee arg slots, skipping the runtime
    /// helper entirely. Any guard miss (not an Array, wrong length, or a live
    /// forwarded keyword) falls through to the proven
    /// `jit_forwarded_set_arguments` helper. Scratch: x9 (+ x10, used internally
    /// by `a64_frame_load/store`/`a64_field_load` for large offsets), x11 = the
    /// rest array, x12 = value, x13 = callee LFP base, x14 = element base,
    /// x15 = length.
    pub(super) fn a64_set_arguments_forwarded_eager(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
        recv: SlotId,
        args: SlotId,
        lead_num: usize,
        expected_len: usize,
        kwrest_guard: Option<SlotId>,
    ) -> bool {
        const CLFP: u32 = 13;
        const VAL: u32 = 12;
        const ARR: u32 = 11;
        const BASE: u32 = 14;
        const LEN: u32 = 15;
        let lfp = GP::R14.a64().0; // x22: f's own LFP
        let fallback = self.jit.label();
        let done = self.jit.label();
        let heap = self.jit.label();
        let got = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            sub x13, sp, #(RSP_LOCAL_FRAME as u32);
        );
        // self + leading args from f's own slots
        self.a64_frame_load(VAL, lfp, conv(recv) as u32);
        self.a64_frame_store(VAL, CLFP, LFP_SELF as u32);
        for i in 0..lead_num {
            self.a64_frame_load(VAL, lfp, conv(args + i) as u32);
            self.a64_frame_store(VAL, CLFP, (LFP_ARG0 + 8 * i as i32) as u32);
        }
        // load the forwarded `...` rest slot (= args + lead_num) and guard it is
        // a heap Array (tag == 0, ty == ARRAY).
        self.a64_frame_load(ARR, lfp, conv(args + lead_num) as u32);
        monoasm_arm64!(&mut self.jit,
            mov x9, #7;
            and x9, x(ARR), x9;
            cbnz x9, fallback;                    // immediate (not a heap ptr) -> fallback
            ldrb w9, [x(ARR), #(RVALUE_OFFSET_TY as u32)];
            cmp x9, #(ObjTy::ARRAY.get() as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &fallback);
        // length + element base: inline (capa <= INLINE_CAPA) vs heap.
        monoasm_arm64!(&mut self.jit,
            ldr x(LEN), [x(ARR), #(RVALUE_OFFSET_ARY_CAPA as u32)]; // capa == len when inline
            cmp x(LEN), #(ARRAY_INLINE_CAPA as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &heap);
        monoasm_arm64!(&mut self.jit,
            add x(BASE), x(ARR), #(RVALUE_OFFSET_INLINE as u32);
            b got;
        );
        self.jit.bind_label(heap);
        monoasm_arm64!(&mut self.jit,
            ldr x(LEN), [x(ARR), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            ldr x(BASE), [x(ARR), #(RVALUE_OFFSET_HEAP_PTR as u32)];
        );
        self.jit.bind_label(got);
        // speculative length guard (expected_len is a compile-time constant).
        monoasm_arm64!(&mut self.jit,
            mov x9, (expected_len as u64);
            cmp x(LEN), x9;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &fallback);
        // only fast-path when no keyword is actually forwarded (nil kwrest).
        if let Some(kw) = kwrest_guard {
            self.a64_frame_load(VAL, lfp, conv(kw) as u32);
            monoasm_arm64!(&mut self.jit,
                mov x9, (NIL_VALUE as u64);
                cmp x(VAL), x9;
            );
            self.jit.bcond_label(monoasm::Cond::Ne, &fallback);
        }
        // copy the `expected_len` elements into the callee arg slots.
        for j in 0..expected_len {
            self.a64_field_load(VAL, BASE, (8 * j) as u32);
            self.a64_frame_store(VAL, CLFP, (LFP_ARG0 + 8 * (lead_num + j) as i32) as u32);
        }
        monoasm_arm64!(&mut self.jit,
            mov x0, (NIL_VALUE as u64);           // success sentinel
            b done;
        );
        // guard miss: the proven specialized forwarding helper (handles the
        // subtle keyword / shape cases and re-parses the call site).
        self.jit.bind_label(fallback);
        self.jit_set_arguments_forwarded_helper(callid, fid, offset);
        self.jit.bind_label(done);
        true
    }

    /// Lower `SetArgumentsForwardedHelper`: same asm shape as
    /// `a64_set_arguments`, but dispatches to the specialized
    /// `jit_forwarded_set_arguments` runtime helper (forwarding `g(x.., ...)`
    /// into a no-keyword iseq with opt/post/rest). Large callee frames go
    /// through `a64_sp_*`.
    pub(in crate::codegen::jitgen) fn jit_set_arguments_forwarded_helper(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
    ) -> bool {
        let f = crate::runtime::jit_forwarded_set_arguments as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                       // vm
            mov x1, x20;                       // globals
            mov x2, (callid.get() as u64);     // callid
            sub x3, sp, #(RSP_LOCAL_FRAME as u32); // callee_lfp (call-site sp)
            mov x4, (fid.get() as u64);        // callee fid
            str x30, [sp, #-16]!;              // save LR
        );
        self.a64_sp_sub(offset as u32);        // reserve callee scratch
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            blr x9;
        );
        self.a64_sp_add(offset as u32);
        monoasm_arm64!(&mut self.jit,
            ldr x30, [sp], #16;                // restore LR
        );
        true
    }

    /// Lower `Call` (the call itself): set the callee LFP, push a control
    /// frame, set PC, `blr` the callee codeptr, then restore the caller's
    /// cfp/lfp. Mirrors x86 `do_call` (set_lfp + push_frame + call + pop_frame)
    /// and the VM invoker's `aftargs` sequence. The eviction-on-return
    /// patching (`set_deopt_with_return_addr`) is x86-only (runtime branch
    /// patching), so it is skipped — class-version changes are caught by
    /// `GuardClassVersion` deopts instead.
    pub(super) fn a64_do_call(
        &mut self,
        codeptr: CodePtr,
        is_iseq: bool,
        callee_pc: Option<BytecodePtrBase>,
        call_site_bc_ptr: BytecodePtr,
    ) {
        let codeptr_addr = codeptr.as_ptr() as u64;
        // set_lfp + push_frame (EXEC=x19, LFP=x22).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)]; // prev cfp
            sub x11, sp, #(RSP_CFP as u32);          // new cfp addr
            str x10, [x11];                          // new_cfp.prev = prev
            str x11, [x19, #(EXECUTOR_CFP as u32)];  // exec.cfp = new cfp
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);  // callee LFP
            stur x22, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];  // new_cfp.lfp = LFP
        );
        if is_iseq {
            // iseq: PC <- callee pc (read by the VM tier / prologue).
            if let Some(pc) = callee_pc {
                let pc_ptr = pc.as_ptr() as u64;
                monoasm_arm64!(&mut self.jit, mov x21, (pc_ptr););
            }
        } else {
            // builtin: x3 is the 4th C-arg = the `pc` parameter, which with-pc
            // builtins use as the call-site bytecode pointer. The native-func
            // wrapper passes x3 through untouched (mirrors x86 do_call setting
            // rcx to the call-site bc ptr).
            let cs = call_site_bc_ptr.as_ptr() as u64;
            monoasm_arm64!(&mut self.jit, mov x3, (cs););
        }
        monoasm_arm64!(&mut self.jit,
            mov x10, (codeptr_addr);
            blr x10;                                 // result in x0
        );
        // pop_frame: restore caller cfp + lfp from x29 (== x86 rbp).
        monoasm_arm64!(&mut self.jit,
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        );
    }

    // ---- specialized (inlined) frame lowering (aarch64) -------------------

    /// `MethodRetSpecialized` / `BlockBreakSpecialized`: a clean return that
    /// unwinds `rbp_offset` bytes of inlined frames at once. Mirrors x86
    /// `method_return_specialized` (`lea rbp,[rbp+off]; leave; ret`): adjust the
    /// native frame base (x29), then run the standard epilogue. No error path —
    /// the value is already in the accumulator and the caller frame is JIT'd.
    pub(in crate::codegen::jitgen::asmir) fn method_return_specialized(&mut self, rbp_offset: usize) {
        monoasm_arm64!(&mut self.jit,
            mov x10, (rbp_offset as u64);
            add x29, x29, x10;          // rbp += off (skip inlined frames)
            mov sp, x29;                // leave
            ldp x29, x30, [sp], #(16);  // restore caller fp/lr
            ret;
        );
    }

    /// `SpecializedCall` / `SpecializedYield`: a direct branch-with-link into
    /// an inlined method/block entry already emitted in this code buffer.
    /// Mirrors x86 `do_specialized_call`: set_lfp + push_frame, optionally bind
    /// the deopt re-entry `patch_point`, `bl entry`, then pop_frame. Returns the
    /// post-`bl` address (the return continuation); the caller records it via
    /// `set_deopt_with_return_addr` so `immediate_eviction` can later overwrite
    /// the continuation with a `B deopt` on BOP redefinition.
    pub(in crate::codegen::jitgen::asmir) fn do_specialized_call(
        &mut self,
        entry: DestLabel,
        patch_point: Option<DestLabel>,
    ) -> CodePtr {
        // set_lfp + push_frame (mirror a64_do_call).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)]; // prev cfp
            sub x11, sp, #(RSP_CFP as u32);          // new cfp addr
            str x10, [x11];                          // new_cfp.prev = prev
            str x11, [x19, #(EXECUTOR_CFP as u32)];  // exec.cfp = new cfp
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);  // callee LFP
            stur x22, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];  // new_cfp.lfp = LFP
        );
        if let Some(patch) = patch_point {
            self.jit.bind_label(patch);
        }
        monoasm_arm64!(&mut self.jit, bl entry;);
        let return_addr = self.jit.get_current_address();
        // pop_frame: restore caller cfp + lfp from x29 (== x86 rbp).
        monoasm_arm64!(&mut self.jit,
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        );
        return_addr
    }

    /// `SetupYieldFrame`: build the callee **block** frame for a specialized
    /// `yield` before `SpecializedYield` branches into it. Walks `outer - 1`
    /// outer-LFP links to the block's defining frame, then writes the callee
    /// frame's outer/meta/svar/block/self slots. A literal translation of
    /// x86 `setup_yield_frame` (x29-free; uses x9 = outer LFP, x11 = value
    /// scratch — neither of which is GP-mapped; `stur`/`ldur` address the
    /// frame fields directly off sp/x9 with no address-scratch register). The cfp
    /// prev/lfp it also writes are immediately overwritten by the following
    /// `SpecializedYield`'s push_frame, exactly as on x86.
    pub(in crate::codegen::jitgen::asmir) fn setup_yield_frame(&mut self, meta: Meta, outer: usize) {
        let outer = outer - 1;
        monoasm_arm64!(&mut self.jit, ldr x9, [x19, #(EXECUTOR_CFP as u32)];);
        for _ in 0..outer {
            monoasm_arm64!(&mut self.jit, ldr x9, [x9];);
        }
        monoasm_arm64!(&mut self.jit,
            ldur x9, [x9, #(-(CFP_LFP as i32))];              // x9 <- outer LFP
            // new_cfp.prev = exec.cfp
            ldr x11, [x19, #(EXECUTOR_CFP as u32)];
            stur x11, [sp, #(-(RSP_CFP as i32))];
            // new_cfp.lfp = rsp + (24 - RSP_LOCAL_FRAME) = sp - 16
            sub x11, sp, #(16u32);
            stur x11, [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
            // frame.outer = outer LFP
            stur x9, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            // frame.meta
            mov x11, (meta.get());
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
            // svar / block = 0 (block callee resolves via outer chain;
            // zeroed so the GC mark walker stays sound)
            mov x11, (0u64);
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];
            // frame.self = [outer LFP - LFP_SELF]
            ldur x11, [x9, #(-(LFP_SELF as i32))];
            stur x11, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
        );
    }

    /// `RestKw`: build a const-data table of (name: i32, slot-id: i32) pairs
    /// terminated by (0, 0), then call `correct_rest_kw(&table, lfp)` which
    /// reads the listed slots and returns the `**kwrest` Hash in x0. Mirrors
    /// the x86 `RestKw` arm; the const-table emission is arch-neutral and the
    /// table address is taken with PC-relative `adr` (as in OptCase).
    pub(in crate::codegen::jitgen) fn emit_rest_kw(&mut self, rest_kw: Vec<(SlotId, IdentId)>) {
        let data = self.jit.const_align8();
        for (i, name) in rest_kw.into_iter() {
            self.jit.const_i32(name.get() as i32);
            self.jit.const_i32(i.0 as i32);
        }
        self.jit.const_i32(0);
        self.jit.const_i32(0);
        let f = runtime::correct_rest_kw as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            adr x0, data;          // &table
            mov x1, x22;           // lfp (R14)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                // x0 = kwrest Hash
            ldr x30, [sp], #16;
        );
    }

    /// Dense-integer `case` dispatch — aarch64 twin of x86 `emit_opt_case`. The
    /// condition (a tagged fixnum) is in x4 (`GP::Rdi`), placed by the
    /// front-end. Untag, range-check `[min, max]` (signed, both < 2048 so they
    /// fit a 12-bit `cmp` immediate), then index a jump table of absolute
    /// branch-target addresses by `cond - min` and branch indirectly.
    ///
    /// The table is built with `const_align8` + `abs_address`, exactly as on
    /// x86; `resolve_constants` emits it into this method's own code page right
    /// after the body, so it is well within `adr`'s ±1MB reach. Terminates the
    /// basic block (the `br` is an unconditional indirect branch).
    pub(in crate::codegen::jitgen) fn emit_opt_case(
        &mut self,
        max: u16,
        min: u16,
        else_dest: DestLabel,
        branch_dests: Box<[DestLabel]>,
    ) {
        let jump_table = self.jit.const_align8();
        for dest_label in branch_dests.iter() {
            self.jit.abs_address(dest_label.clone());
        }
        let cond = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            asr x(cond), x(cond), #1;   // untag fixnum: x4 = n
            cmp x(cond), #(max as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &else_dest); // n > max -> else
        monoasm_arm64!(&mut self.jit,
            cmp x(cond), #(min as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &else_dest); // n < min -> else
        monoasm_arm64!(&mut self.jit,
            sub x(cond), x(cond), #(min as u32);     // index = n - min
            adr x10, jump_table;                     // table base (PC-relative)
            ldr x10, [x10, x(cond), lsl #3];         // table[n - min]
            br x10;
        );
    }

    pub(in crate::codegen::jitgen::asmir) fn set_deopt_with_return_addr(
        &mut self,
        return_addr: CodePtr,
        evict: AsmEvict,
        evict_label: &DestLabel,
    ) {
        self.asm_return_addr_table.insert(evict, return_addr);
        self.return_addr_table
            .insert(return_addr, (None, evict_label.clone()));
    }

    /// Write the callee frame's meta/outer/block fields before a call.
    pub(in crate::codegen::jitgen) fn emit_setup_method_frame(
        &mut self,
        meta: Meta,
        outer_lfp: Option<Lfp>,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
    ) {
        self.a64_setup_method_frame(meta, outer_lfp, block_fid, block_arg);
    }

    /// Marshal the call arguments into the callee frame (`offset` is the callee
    /// scratch-area size, pre-resolved by the dispatcher).
    pub(in crate::codegen::jitgen) fn emit_set_arguments(
        &mut self,
        callid: CallSiteId,
        callee_fid: FuncId,
        offset: usize,
    ) {
        self.a64_set_arguments(callid, callee_fid, offset);
    }

    /// Emit the FP-pool-preserving C call to
    /// `jit_recompile_specialized(globals, idx, reason)`. The caller-saved
    /// d2-d7 pool is saved around the call because the following deopt's
    /// write-back reads it (d8-d15 are callee-saved); x19-x23 are AAPCS64
    /// callee-saved so the VM globals survive. `global_idx` is the resolved
    /// `specialized_base + idx` slot in `specialized_info`.
    /// Emit an (uncounted) call to the non-specialized recompiler:
    /// `jit_recompile_loop(vm, globals, lfp, pc, reason)` when `position` is a
    /// loop-header pc, else `jit_recompile_method(vm, globals, lfp, reason)`.
    /// Twin of `a64_call_recompile_specialized`. The caller-saved d2-d7 FP pool
    /// and x5-x8 GP pool (R8-R11) are saved around the `blr` because the deopt
    /// write-back that follows reads both (x19-x23 / d8-d15 / x19-x28 are
    /// callee-saved). Leaves the `Option<Value>` result in x0 — x0 == 0 means
    /// the recompile panicked and set a FatalError.
    pub(super) fn a64_call_recompile(
        &mut self,
        position: Option<BytecodePtr>,
        reason: RecompileReason,
    ) {
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(80);
            str d2, [sp, #(0)];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            stp x5, x6, [sp, #(48)];      // GP pool (R8-R11), read by the deopt write-back
            stp x7, x8, [sp, #(64)];
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, x22;                  // lfp
        );
        let f = if let Some(pc) = position {
            let pc_ptr = pc.as_ptr() as u64;
            monoasm_arm64!(&mut self.jit,
                mov x3, (pc_ptr);         // loop pc
                mov x4, (reason as u64);
            );
            crate::codegen::compiler::jit_recompile_loop as *const () as u64
        } else {
            monoasm_arm64!(&mut self.jit,
                mov x3, (reason as u64);
            );
            crate::codegen::compiler::jit_recompile_method as *const () as u64
        };
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                       // -> Option<Value>: None (x0 == 0) = panic
            ldr x30, [sp], #16;
            ldr d2, [sp, #(0)];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            ldp x5, x6, [sp, #(48)];
            ldp x7, x8, [sp, #(64)];
            add sp, sp, #(80);
        );
    }

    pub(super) fn a64_call_recompile_specialized(&mut self, global_idx: usize, reason: RecompileReason) {
        let f = crate::codegen::compiler::jit_recompile_specialized as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(80);
            str d2, [sp, #(0)];
            str d3, [sp, #(8)];
            str d4, [sp, #(16)];
            str d5, [sp, #(24)];
            str d6, [sp, #(32)];
            str d7, [sp, #(40)];
            stp x5, x6, [sp, #(48)];      // GP pool (R8-R11), read by the deopt write-back
            stp x7, x8, [sp, #(64)];
            mov x0, x20;                  // globals
            mov x1, (global_idx as u64);  // specialized_info index
            mov x2, (reason as u64);      // RecompileReason
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
            ldr d2, [sp, #(0)];
            ldr d3, [sp, #(8)];
            ldr d4, [sp, #(16)];
            ldr d5, [sp, #(24)];
            ldr d6, [sp, #(32)];
            ldr d7, [sp, #(40)];
            ldp x5, x6, [sp, #(48)];
            ldp x7, x8, [sp, #(64)];
            add sp, sp, #(80);
        );
    }

    /// The call itself. aarch64 has no runtime branch patching, so the `evict`
    /// return-address patch point is not registered (class-version guards cover
    /// the staleness it would otherwise catch); the x86-only params are unused.
    /// aarch64 always calls `codeptr` directly (no JIT-entry dispatch or
    /// return-address patching — class-version guards cover invalidation), so
    /// `jit_entry` / `evict` / `evict_label` are unused here.
    pub(in crate::codegen::jitgen) fn emit_call(
        &mut self,
        codeptr: CodePtr,
        is_iseq: bool,
        callee_pc: Option<BytecodePtrBase>,
        call_site_bc_ptr: BytecodePtr,
        _jit_entry: Option<DestLabel>,
        _evict: AsmEvict,
        _evict_label: &DestLabel,
    ) {
        self.a64_do_call(codeptr, is_iseq, callee_pc, call_site_bc_ptr);
    }

    /// Keyword-rest fixup: if the `slot` is nil, replace it with a fresh empty
    /// Hash (runtime::empty_hash, no args, result in x0). Mirrors the x86 inline
    /// path (no xmm save — no xmm is live at kw-rest setup). Bails on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_check_kw_rest(&mut self, slot: SlotId) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = slot.0 as u32 * 8 + LFP_SELF as u32;
        let exit = self.jit.label();
        let f = runtime::empty_hash as *const () as u64;
        self.a64_frame_load(11, lfp, off);
        monoasm_arm64!(&mut self.jit,
            cmp x11, #(NIL_VALUE);       // slot == nil ?
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &exit); // not nil -> keep
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                      // x0 = {}
            ldr x30, [sp], #16;
        );
        self.a64_frame_store(0, lfp, off); // slot = {}
        monoasm_arm64!(&mut self.jit,
        exit:
        );
        true
    }

    /// Lower the generic `Yield` (block call whose target is resolved at runtime
    /// via `get_yield_data`). Mirrors x86 `gen_yield`: fetch the block's
    /// ProcData, build the callee block frame, massage arguments, then call the
    /// block's funcdata indirectly. The eviction-on-return patching is x86-only
    /// (runtime branch patching), so it is skipped — class-version guards cover
    /// it. `error` catches a missing block, an argument error, or a callee
    /// raise. Bails on an out-of-range callee-frame offset.
    pub(in crate::codegen::jitgen) fn emit_yield(
        &mut self,
        callid: CallSiteId,
        error: &DestLabel,
        _evict: AsmEvict,
        _evict_label: &DestLabel,
    ) -> bool {
        // Closely mirrors the proven VM `a64_op_yield`. x25/x26 are callee-saved
        // and used by neither the JIT global set (x19-x23) nor JIT'd code, so
        // they survive the C calls and hold the outer LFP / funcdata. The
        // continuation frame is already reserved by the surrounding
        // fpr_save_cont, so no extra push here.
        let f_yield = runtime::get_yield_data as *const () as u64;
        let f_args = runtime::jit_handle_arguments_no_block as *const () as u64;
        // get_yield_data(vm, globals) -> x0 = outer Lfp, x1 = FuncId.
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;
            mov x1, x20;
            str x30, [sp, #-16]!;
            mov x9, (f_yield);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.a64_resolve_invalidated_outer(0);
        self.emit_handle_error(error); // null outer (no block given) -> error
        monoasm_arm64!(&mut self.jit, mov x25, x0;); // outer (callee-saved)
        // get_func_data: FuncId (x1) -> &FuncData (x9 -> x26).
        monoasm_arm64!(&mut self.jit, mov x2, x1;);
        self.a64_get_func_data_x2(); // x9 = &FuncData (clobbers x10, x11)
        monoasm_arm64!(&mut self.jit, mov x26, x9;);
        // Build the callee block frame fields below sp (outer/svar/block/
        // self/meta). self is inherited from the outer frame.
        monoasm_arm64!(&mut self.jit,
            mov x12, (0u64);
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
            str x25, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
            str x12, [x11];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_BLOCK) as u32);
            str x12, [x11];
            sub x10, x25, #(LFP_SELF as u32);
            ldr x10, [x10];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            str x10, [x11];
            ldr x10, [x26, #(FUNCDATA_META as u32)];
            sub x11, sp, #((RSP_LOCAL_FRAME + LFP_META) as u32);
            str x10, [x11];
        );
        // jit_handle_arguments_no_block(vm, globals, caller_lfp, callee_lfp,
        // callid). callee_lfp is computed before the dynamic callee-scratch
        // reservation; the pre-reservation sp is saved in x25 and restored
        // afterwards (x26 survives as fdata).
        monoasm_arm64!(&mut self.jit,
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);   // callee_lfp
            mov x25, sp;                             // save sp (outer no longer needed)
            ldrh w10, [x26, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);                     // 16-aligned reservation
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x19;
            mov x1, x20;
            mov x2, x22;                             // caller LFP
            mov x4, (callid.get() as u64);
            mov x9, (f_args);
            blr x9;                                  // x0 = Option<Value>
            mov sp, x25;                             // restore sp
        );
        self.emit_handle_error(error); // argument error -> error
        // call_funcdata (indirect, fdata in x26): push the control frame, set
        // the callee LFP/PC, blr the codeptr, then restore the caller frame
        // (cfp from the saved prev slot, lfp from x29 == the JIT frame pointer).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x19, #(EXECUTOR_CFP as u32)];
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x22, [x10];
            sub x3, x21, #(16u32);                       // x3 = pc arg (call-site bc ptr) for with-pc callees
            ldr x21, [x26, #(FUNCDATA_PC as u32)];        // PC <- callee pc
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;                                       // result in x0
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
        true
    }

    // ---- callee-frame argument stores ([sp + (ofs - RSP_LOCAL_FRAME)]) ------
    // Used by the inline argument-setup fast path (fetch_for_callee).

    /// `&block` proxy: materialize the current method's block handler into
    /// `ret`. Walk `outer` outer-frame links to reach the method LFP (x0), load
    /// its block slot ([lfp - LFP_BLOCK]); if the low bit is set (already a
    /// BlockHandler proxy rather than a frame pointer) bump the nesting tag by
    /// `(outer << 2) + 2`. No runtime call, no xmm pressure. Bails on an
    /// out-of-range frame offset or nesting tag immediate.
    pub(in crate::codegen::jitgen) fn emit_block_arg_proxy(
        &mut self,
        ret: SlotId,
        outer: usize,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let rax = GP::Rax.a64().0; // x0
        let off = ret.0 as u32 * 8 + LFP_SELF as u32;
        let tag = ((outer << 2) + 2) as u32;
        // get_method_lfp(outer): x0 <- method LFP (walk `outer` outer links).
        if outer == 0 {
            monoasm_arm64!(&mut self.jit, mov x(rax), x(lfp););
        } else {
            monoasm_arm64!(&mut self.jit, ldr x(rax), [x(lfp)];);
            for _ in 0..outer - 1 {
                monoasm_arm64!(&mut self.jit, ldr x(rax), [x(rax)];);
            }
        }
        // block_arg_proxy(outer): x0 <- [x0 - LFP_BLOCK]; if (x0 & 1) bump tag.
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldur x(rax), [x(rax), #(-(LFP_BLOCK as i32))];
            mov x11, (1u64);
            tst x(rax), x11;             // Z = ((x0 & 1) == 0)
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &exit);
        if tag <= 4095 {
            monoasm_arm64!(&mut self.jit, add x(rax), x(rax), #(tag););
        } else {
            monoasm_arm64!(&mut self.jit, mov x11, (tag as u64); add x(rax), x(rax), x11;);
        }
        monoasm_arm64!(&mut self.jit, exit:);
        // store_rax(ret): [lfp - off] <- x0
        self.a64_frame_store(rax, lfp, off);
        true
    }

    /// `&block` captured as a Proc value: runtime::block_arg(vm, globals, lfp,
    /// call_site) materializes the current frame's block handler into a Proc
    /// (promoting the frame to the heap if needed). The Option<Value> result is
    /// stored to `ret` after a HandleError. The live xmm pool is saved/restored
    /// around the C call (restore placed before the HandleError branch so the
    /// side exit writes the live floats back from the pool); bails only on an
    /// out-of-range frame offset.
    pub(in crate::codegen::jitgen) fn emit_block_arg(
        &mut self,
        ret: SlotId,
        using_fpr: UsingFpr,
        call_site_bc_ptr: BytecodePtr,
        error: &DestLabel,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = ret.0 as u32 * 8 + LFP_SELF as u32;
        let cs = call_site_bc_ptr.as_ptr() as u64;
        let f = runtime::block_arg as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;          // vm
            mov x1, x20;          // globals
            mov x2, x(lfp);       // caller LFP
            mov x3, (cs);         // call-site bc ptr
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;               // x0 = Option<Value>
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
        let rax = GP::Rax.a64().0;
        self.a64_frame_store(rax, lfp, off); // ret <- Proc
        true
    }
}
