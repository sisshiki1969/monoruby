use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// op 30-33 `send`/`send_simple`: method call. Reads the monomorphic inline
    /// cache on the VM fast path (mirrors x86 `vm_send`): if the receiver class
    /// and `class_version` match the cached pair, the cached FuncId is used
    /// directly; otherwise the slow path calls `find_method` and refills the
    /// cache (FuncId/class/version via [`Self::a64_save_method_cache`]), which
    /// also lets the JIT specialize the site. A cached FuncId of 0 means
    /// method_missing. Handles the simple case (no kw/block/splat). Bytecode
    /// (32 bytes): `+0` callid, `+4` ret slot, `+8` pos_num, `+10` arg slot,
    /// `+12` recv slot; inline cache `+16` FuncId, `+24` class, `+28` version.
    pub(in crate::codegen) fn a64_op_send(&mut self, is_simple: bool) -> CodePtr {
        let p = self.jit.get_current_address();
        let mm = self.jit.label();
        let argloop = self.jit.label();
        let argdone = self.jit.label();
        let generic = self.jit.label();
        let docall = self.jit.label();
        let skip = self.jit.label();
        let after_call = self.jit.label();
        let exec = self.jit.label();
        let slow_class = self.jit.label();
        let slow_ver = self.jit.label();
        let raise = self.entry_raise.clone();
        let get_class = self.get_class.clone();
        // Absolute address of the global `class_version` (aarch64 has no
        // RIP-relative addressing, so we bake the data label's address in).
        let cv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        // Raise SystemStackError before pushing the new frame (so the caller's
        // LFP is still intact when entry_raise inspects it for a rescue).
        // No call-site GC/preempt poll here: the callee entry
        // (`a64_op_init_method` / JIT `InitMethod`) polls on every
        // invocation, which covers all Ruby-level callees; native callees
        // are bounded between the caller's loop-edge/entry polls.
        self.a64_check_stack();
        // push_cont_frame: save caller PC (sp -= 16; [sp] = PC)
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(16);
            str x(PC.0), [sp];
        // receiver
            ldrh x10, [x(PC.0), #(12)];
        );
        self.a64_load_slot(X10, X4, X11); // X4 = recv
        // callee self slot
        monoasm_arm64!(&mut self.jit,
            stur x4, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
        // Monomorphic inline-cache fast path (mirrors x86 `vm_send`): compute
        // the receiver class and compare it against the cached (class,
        // class_version); on a hit, use the cached FuncId directly instead of
        // calling find_method. Inline-cache layout is PC-relative and absolute
        // (aarch64 does not pre-advance PC): +16 FuncId, +24 class, +28 version.
        // get_class: x0 = recv in, w0 = class out (clobbers x1/x2/x9/lr; x4 is
        // preserved but recv is reloaded from its slot where needed).
            mov x0, x4;
            bl get_class;
            ldr w11, [x(PC.0), #(24)];  // CACHED_CLASS
            cmp w0, w11;
        );
        self.jit.bcond_label(Cond::Ne, &slow_class); // class mismatch (maybe polymorphic)
        monoasm_arm64!(&mut self.jit,
            mov x11, (cv_addr);
            ldr w11, [x11];             // current class_version
            ldr w12, [x(PC.0), #(28)];  // CACHED_VERSION
            cmp w11, w12;
        );
        self.jit.bcond_label(Cond::Ne, &slow_ver); // version mismatch -> plain refill
        monoasm_arm64!(&mut self.jit,
        // cache hit: load the cached FuncId. 0 means method_missing was cached.
            exec:
            ldr w0, [x(PC.0), #(16)];   // CACHED_FUNCID
            cbz x0, mm;
        // get_func_data: X15 = funcinfo_base + funcid*64 + FUNCINFO_DATA
            lsl x10, x0, #(6);
            mov x11, (GLOBALS_FUNCINFO as u64);
            add x11, x(GLOBALS.0), x11;
            ldr x11, [x11];
            add x10, x10, x11;
            add x15, x10, #(FUNCINFO_DATA as u32);
        // set_method_outer: zero outer/svar; set meta (kept in X14).
            mov x12, (0);
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            ldr x14, [x15, #(FUNCDATA_META as u32)];
            stur x14, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
        // Simple-send opcodes (no block/splat/kw at the call site) may take
        // the fast positional-copy path when the callee is also simple and
        // arity matches. The full-send opcodes always go generic so that
        // set_frame_block / splat / keyword handling runs.
            ldrh x9, [x(PC.0), #(8)];  // pos_num
        );
        if is_simple {
            monoasm_arm64!(&mut self.jit,
                lsr x16, x14, #(56);  // kind byte
                tbz x16, #(4), generic;
                ldrh x16, [x15, #(FUNCDATA_MIN as u32)];
                cmp x9, x16;
            );
            self.jit.bcond_label(Cond::Ne, &generic);
        } else {
            monoasm_arm64!(&mut self.jit,
                b generic;
            );
        }
        // --- simple path: zero block + copy positional args directly ---
        monoasm_arm64!(&mut self.jit,
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];  // block = 0
            ldrh x10, [x(PC.0), #(10)];  // arg slot
            neg x10, x10;
            add x10, x(LFP.0), x10, lsl #(3);
            sub x10, x10, #(LFP_SELF as u32);  // args base (caller)
            cbz x9, argdone;
            neg x9, x9;
            argloop:
            add x11, x10, x9, lsl #(3);
            ldr x12, [x11, #(8)];  // src = [base + i*8 + 8]
            sub x13, sp, #((RSP_LOCAL_FRAME + LFP_SELF) as u32);
            add x13, x13, x9, lsl #(3);
            str x12, [x13];  // dst = callee self slot + i*8
            add x9, x9, #(1);
            cbnz x9, argloop;
            argdone:
            b docall;
        // --- generic path: vm_handle_arguments(exec, globals, caller_lfp,
        // callee_lfp, callid). Handles rest/optional/keyword/splat + block. ---
            generic:
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);  // callee lfp
        // Reserve scratch below the callee frame (= ofs*16 + 16, 16-aligned)
        // so the C call's frame can't trample the callee frame being built.
        // Save the pre-reservation SP (X25) and funcdata ptr (X26) in
        // callee-saved registers (AAPCS64 preserves x19-x28); X15 is
        // caller-saved so it would otherwise be lost. Restore SP directly
        // from X25 afterwards.
            mov x25, sp;  // X25 = SP before reservation
            mov x26, x15;
            ldrh x10, [x15, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);  // caller lfp
            ldr w4, [x(PC.0)];  // callid
            mov x9, (runtime::vm_handle_arguments as *const () as u64);
            blr x9;
            mov x15, x26;  // restore funcdata ptr
            mov sp, x25;  // restore SP directly
            cbz x0, raise;
            docall:
        // call_funcdata: push_frame + set_lfp + pc + blr codeptr + restore cfp
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            stur x(LFP.0), [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
        // 4th arg (X3) = call-site BytecodePtr, for with-pc builtins (x86
        // sets `rcx = r13 - 16`); aarch64 PC is already the call site.
            mov x3, x(PC.0);
            ldr x(PC.0), [x15, #(FUNCDATA_PC as u32)];
            ldr x10, [x15, #(FUNCDATA_CODEPTR as u32)];
            blr x10;
        );
        // Chain deopt tails into the frame-restoring continuation that starts
        // here (`doc/chain_deopt.md` §3.1): the address just past the `blr`,
        // *before* the pop_frame below — a hijacked `ret` skips the JIT
        // frame's own pop_frame, so the sequence a converted frame resumes
        // through has to contain one. Carries no per-site state, so this one
        // address serves every send and yield site.
        let call_return_addr = self.jit.get_current_address();
        self.set_vm_call_continuation(call_return_addr);
        monoasm_arm64!(&mut self.jit,
        // pop_frame: EXEC.cfp = (X29 - BP_CFP). Mirrors x86 `lea r14,[rbp-8]`
        // — set EXEC.cfp to the *address* of this frame's CFP descriptor (set
        // up by the caller's push_frame before our vm_entry). We must NOT
        // reload from `[SP - RSP_CFP]`: AAPCS64 has no red zone, so the inner
        // BLR's callee may use that slot as a local and clobber it. The
        // descriptor at `[X29 - BP_CFP]` lives in this frame's "header" area
        // (above the locals/LFP) and is safe across nested calls.
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
        // restore caller LFP from its own frame (x86 `restore_lfp`):
        // LFP = [x29 - (BP_CFP + CFP_LFP)]. The callee clobbers LFP, so we
        // reload it from the caller's stable frame pointer (x29 == x86 rbp).
            ldur x(LFP.0), [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        // pop_cont_frame: restore PC, advance past the 32-byte send
            after_call:
            ldr x(PC.0), [sp];
            add sp, sp, #(16);
            cbz x0, raise;  // result 0 => error
            ldrh x10, [x(PC.0), #(4)];  // ret slot
            add x(PC.0), x(PC.0), #(32);
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
        );
        self.a64_fetch_and_dispatch();
        // method_missing: invoke_method_missing(vm, globals, recv, lfp,
        // callid) -> Option, then join the result path. The receiver register
        // was clobbered by find_method, so reload it from the recv slot.
        // invoke_method_missing manages its own frames and preserves PC/LFP
        // (callee-saved), so no cfp/LFP restore is needed here.
        monoasm_arm64!(&mut self.jit,
            mm:
            ldrh x10, [x(PC.0), #(12)];  // recv slot
        );
        self.a64_slot_value(X10);
        monoasm_arm64!(&mut self.jit,
            mov x2, x10;  // receiver
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x3, x(LFP.0);
            ldr w4, [x(PC.0)];  // callid
            mov x9, (crate::codegen::runtime::invoke_method_missing as *const () as u64);
            blr x9;
            b after_call;
        );
        // --- slow path: inline-cache miss. Look up the method, populate the
        // inline cache (FuncId/class/version via a64_save_method_cache) so the
        // JIT can type the site, then rejoin at `exec`. A method_missing result
        // (FuncId 0) is cached as well, so repeated misses hit the fast path and
        // fall through to `mm` without re-running find_method.
        //
        // `slow_class` is the class-mismatch entry (mirrors x86 `slow_path1`):
        // if a FuncId was already cached, the call site has now seen >=2
        // receiver classes, so mark it polymorphic by writing 1 to the
        // `opcode_sub` byte (offset +7; PC points at the op start on aarch64).
        // The JIT reads this back via `BytecodePtr::opcode_sub()` and emits a
        // non-deoptimizing dispatch instead of a monomorphic class guard. The
        // version-mismatch entry (`slow_ver`, x86 `slow_path2`) skips this:
        // the class still matched, so a stale class_version is not polymorphism.
        monoasm_arm64!(&mut self.jit,
            slow_class:
            ldr w11, [x(PC.0), #(16)];  // CACHED_FUNCID
            cbz w11, slow_ver;          // nothing cached yet -> first resolution
            mov x11, (1);
            strb w11, [x(PC.0), #(7)];  // opcode_sub = 1 (polymorphic)
            slow_ver:
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            ldr w2, [x(PC.0)];  // callid
        // reload recv from the callee self slot (get_class/find_method clobber
        // the caller-saved receiver register; recv was stored there above).
            ldur x3, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
        // Reserve scratch below the callee frame being built: its slots
        // (LFP_SELF in particular) live *below* SP, and AAPCS64 has no
        // red zone, so find_method's C frame would otherwise overwrite
        // them — boot-breaking once the callee's frame happens to reach
        // that depth. Mirrors x86 `subq rsp, 1016` around the same call
        // and the vm_handle_arguments reservation below.
            sub sp, sp, #(1024);
            mov x9, (runtime::find_method as *const () as u64);
            // x0 = (ClassId to tag the cache with) << 32 | FuncId
            // (low 32 = 0 -> method_missing). The tag is the receiver's
            // IC class, or its real class for a bool receiver whose
            // method is not unified across TrueClass/FalseClass (#713).
            blr x9;
            add sp, sp, #(1024);
        );
        // Populate the inline cache (X0 = FuncId, preserved across the call).
        self.a64_save_method_cache();
        monoasm_arm64!(&mut self.jit,
            b exec;
        );
        p
    }

    /// op 34/35 `Yield`: invoke the current block. Bytecode (32 bytes):
    /// `+0` callid, `+4` ret slot. The block's func/outer come from
    /// `get_yield_data`; self is the block's captured self. Args are set up
    /// via the runtime arg massager (callsite-driven).
    pub(in crate::codegen) fn a64_op_yield(&mut self) -> CodePtr {
        let p = self.jit.get_current_address();
        let raise = self.entry_raise.clone();
        let skip = self.jit.label();
        // Stack check only, mirroring x86 `vm_yield` (and `a64_op_send`).
        // The block body's entry poll (`a64_op_init_method` / JIT
        // `InitMethod`) fires on every yield, so signals / GC / preemption
        // get a per-iteration safepoint without a call-site poll here.
        self.a64_check_stack();
        // push_cont_frame: save caller PC
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(16);
            str x(PC.0), [sp];
        // get_yield_data(vm, globals) -> x0 = outer (Lfp), x1 = func_id
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x9, (runtime::get_yield_data as *const () as u64);
            blr x9;
            cbz x1, raise;  // no block -> error set
            mov x25, x0;  // X25 = outer (callee-saved across later calls)
        // get_func_data from func_id (X1) -> X15
            lsl x10, x1, #(32);
            lsr x10, x10, #(32);
            lsl x10, x10, #(6);
            mov x11, (GLOBALS_FUNCINFO as u64);
            add x11, x(GLOBALS.0), x11;
            ldr x11, [x11];
            add x10, x10, x11;
            add x15, x10, #(FUNCINFO_DATA as u32);
        // block frame setup: outer = X25, self = outer.self, svar/block 0.
            mov x12, (0);
            stur x25, [sp, #(-((RSP_LOCAL_FRAME + LFP_OUTER) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_SVAR) as i32))];
            stur x12, [sp, #(-((RSP_LOCAL_FRAME + LFP_BLOCK) as i32))];
            ldur x10, [x25, #(-(LFP_SELF as i32))];  // self = outer.self
            stur x10, [sp, #(-((RSP_LOCAL_FRAME + LFP_SELF) as i32))];
            ldr x14, [x15, #(FUNCDATA_META as u32)];
            stur x14, [sp, #(-((RSP_LOCAL_FRAME + LFP_META) as i32))];
        // generic arg setup: vm_handle_arguments(vm, globals, caller_lfp,
        // callee_lfp, callid). Reserve scratch; preserve SP/funcdata.
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);  // callee_lfp
            mov x25, sp;
            mov x26, x15;
            ldrh x10, [x15, #(FUNCDATA_OFS as u32)];
            lsl x10, x10, #(4);
            add x10, x10, #(16);
            sub x11, x25, x10;
            mov sp, x11;
            mov x0, x(EXEC.0);
            mov x1, x(GLOBALS.0);
            mov x2, x(LFP.0);  // caller lfp
            ldr w4, [x(PC.0)];  // callid
            mov x9, (runtime::vm_handle_arguments as *const () as u64);
            blr x9;
            mov x15, x26;
            mov sp, x25;
            cbz x0, raise;
        // call_funcdata
            ldr x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            sub x(LFP.0), sp, #(RSP_LOCAL_FRAME as u32);
            stur x(LFP.0), [sp, #(-((RSP_CFP + CFP_LFP) as i32))];
            mov x3, x(PC.0);  // call-site pc for with-pc builtins
            ldr x(PC.0), [x15, #(FUNCDATA_PC as u32)];
            ldr x10, [x15, #(FUNCDATA_CODEPTR as u32)];
            blr x10;
            ldur x10, [sp, #(-(RSP_CFP as i32))];
            str x10, [x(EXEC.0), #(EXECUTOR_CFP as u32)];
            ldur x(LFP.0), [x29, #(-((BP_CFP + CFP_LFP) as i32))];  // restore caller LFP
        // pop_cont_frame + store result to ret slot [pc+4]
            ldr x(PC.0), [sp];
            add sp, sp, #(16);
            cbz x0, raise;
            ldrh x10, [x(PC.0), #(4)];  // ret slot
            add x(PC.0), x(PC.0), #(32);
            cbz x10, skip;
            neg x10, x10;
            add x11, x(LFP.0), x10, lsl #(3);
            stur x0, [x11, #(-(LFP_SELF as i32))];
            skip:
        );
        self.a64_fetch_and_dispatch();
        p
    }

    /// Stamp the method-call inline cache so the JIT can specialize this call
    /// site (otherwise `pc.method_cache()` stays `None` and every call deopts).
    /// Mirrors x86 `save_cache`. On entry X0 = `runtime::find_method`'s packed
    /// result: the cache-tag ClassId in the high 32 bits and the resolved
    /// FuncId in the low 32 (the tag is the receiver's IC class, or its real
    /// class for a bool receiver with a non-unified method — #713). Writes
    /// FuncId @ `[PC+16]`, the tag ClassId @ `[PC+24]` and the current
    /// class_version @ `[PC+28]` (the layout `method_cache()` reads). On exit
    /// X0 = the FuncId (zero-extended); clobbers X1/X11.
    pub(in crate::codegen) fn a64_save_method_cache(&mut self) {
        let cv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        monoasm_arm64!(&mut self.jit,
            str w0, [x(PC.0), #(16)];   // CACHED_FUNCID (low 32 bits)
            lsr x1, x0, #(32);
            str w1, [x(PC.0), #(24)];   // CACHED_CLASS (cache tag)
            mov x11, (cv_addr);
            ldr w1, [x11];              // class_version (i32)
            str w1, [x(PC.0), #(28)];   // CACHED_VERSION
            ldr w0, [x(PC.0), #(16)];   // X0 = FuncId (u32, zero-ext)
        );
    }
}
