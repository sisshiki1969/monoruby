use super::*;
use monoasm_macro::monoasm_arm64;

// ---- Object#send inline symbol cache (aarch64 twin of the x86_64 copy in
// arch/x86_64/compile/method_call.rs; the two files are arch-selected so the
// duplicate definitions never collide) ----------------------------------------
const CACHE_SIZE: usize = 4;
const CACHE_METHOD: usize = std::mem::offset_of!(CacheEntry, method);
const CACHE_FID: usize = std::mem::offset_of!(CacheEntry, fid);
const CACHE_COUNTER: usize = std::mem::offset_of!(CacheEntry, counter);

#[repr(C)]
struct CacheEntry {
    method: Option<IdentId>,
    fid: Option<FuncId>,
    counter: usize,
}

#[repr(C)]
struct Cache([CacheEntry; CACHE_SIZE]);

extern "C" fn expect_string(
    vm: &mut Executor,
    globals: &mut Globals,
    v: Value,
) -> Option<IdentId> {
    v.expect_symbol_or_string(globals)
        .map_err(|err| vm.set_error(err))
        .ok()
}

extern "C" fn no_method_name(vm: &mut Executor) -> Option<Value> {
    let err = MonorubyErr::argumenterr("no method name given.");
    vm.set_error(err);
    None
}

extern "C" fn wrong_number_of_arg(vm: &mut Executor, expected: usize, given: usize) -> Option<Value> {
    let err = MonorubyErr::wrong_number_of_arg(expected, given);
    vm.set_error(err);
    None
}

extern "C" fn find(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    func_name: IdentId,
) -> Option<FuncId> {
    vm.find_method(globals, recv, func_name, true)
        .map_err(|err| vm.set_error(err))
        .ok()
}

impl Codegen {
    /// Invalidate the `Object#send` symbol cache when the global class version
    /// moved (a method was (re)defined). aarch64 twin of x86
    /// `check_version_with_cache`: no RIP-relative addressing (bake the data
    /// addresses) and no `select_page` (the clear block is laid out inline).
    /// Does not deopt — it zeroes the 4 cache entries' `method`/`fid` words so a
    /// stale `IdentId -> FuncId` binding can't survive a redefinition.
    fn check_version_with_cache(&mut self, cache_addr: u64) {
        // Per-callsite version stamp. Heap-leaked (like the wrapper's jit_slot)
        // so its absolute address bakes into the stub — aarch64 has no
        // RIP-relative addressing, and a fresh JIT-data label is not yet
        // resolved at emit time (only `class_version_label`, allocated at
        // startup, is).
        let cv_addr = Box::into_raw(Box::new(-1i32)) as u64;
        let gv_addr = self
            .jit
            .get_label_address(&self.class_version_label())
            .as_ptr() as u64;
        let clear = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x9, (gv_addr);
            ldr w10, [x9];           // global class version
            mov x9, (cv_addr);
            ldr w11, [x9];           // cached version stamp
            cmp w10, w11;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &clear);
        monoasm_arm64!(&mut self.jit, b exit;); // match -> keep the cache
        self.jit.bind_label(clear);
        monoasm_arm64!(&mut self.jit,
            mov x9, (cv_addr);
            str w10, [x9];           // re-stamp the cached version
            mov x9, (cache_addr);
            mov x12, #0;
            str x12, [x9];           // zero each entry's method+fid word
            str x12, [x9, #(16)];
            str x12, [x9, #(32)];
            str x12, [x9, #(48)];
        );
        self.jit.bind_label(exit);
    }

    /// `send(*arr)`: extract the method name (arg0 == `arr[0]`, or `arr` itself
    /// when it is not an Array) into x11. Twin of x86 `object_send_splat_arg0`;
    /// reuses the Array-shape guard idiom of `a64_set_arguments_forwarded_eager`.
    /// An empty-array splat raises via `no_method_name`.
    fn object_send_splat_arg0(&mut self, args: SlotId, error: &DestLabel) {
        let lfp = GP::R14.a64().0;
        let exit = self.jit.label();
        let heap = self.jit.label();
        self.a64_frame_load(11, lfp, conv(args) as u32); // x11 = splat value
        monoasm_arm64!(&mut self.jit,
            mov x9, #7;
            and x9, x11, x9;
            cbnz x9, exit;                                // immediate -> arg0 = value
            ldrb w9, [x11, #(RVALUE_OFFSET_TY as u32)];
            cmp x9, #(ObjTy::ARRAY.get() as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &exit);   // not an Array -> arg0 = value
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x11, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x9, #(ARRAY_INLINE_CAPA as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &heap);
        monoasm_arm64!(&mut self.jit,
            ldr x11, [x11, #(RVALUE_OFFSET_INLINE as u32)]; // inline elem0
            cbnz x11, exit;                                 // non-null -> arg0 = elem0
            // empty array: raise "no method name given."
            mov x0, x19;
            str x30, [sp, #-16]!;
            mov x9, (no_method_name as *const () as u64);
            blr x9;
            ldr x30, [sp], #16;
            b error;
        );
        self.jit.bind_label(heap);
        monoasm_arm64!(&mut self.jit,
            ldr x11, [x11, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            ldr x11, [x11];                                 // heap elem0
        );
        self.jit.bind_label(exit);
    }

    /// Resolve the `send` method name (arg0 in x11: a Symbol or String) to a
    /// FuncId in x0. Twin of x86 `object_send_inner`: an LFU symbol cache
    /// (4 entries) keyed by `IdentId`, promoting hot entries by swapping with
    /// the least-used slot; a miss calls `find`, a non-Symbol arg0 is coerced
    /// via `expect_string`. x0 == 0 (None) on error is caught by the caller's
    /// `handle_error`. aarch64: no `select_page`, so the cold blocks are inline;
    /// `csel` replaces `cmovltq`. Regs: x11 IdentId, x12 cur, x13 min, x14/x15/x9
    /// scratch.
    fn object_send_inner(&mut self, recv: SlotId, cache_addr: u64, error: &DestLabel) {
        let lfp = GP::R14.a64().0;
        let not_symbol = self.jit.label();
        let l1 = self.jit.label();
        let found = self.jit.label();
        let not_found = self.jit.label();
        let loop0 = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x9, #(0xff);
            and x9, x11, x9;
            cmp x9, #(TAG_SYMBOL as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &not_symbol);
        monoasm_arm64!(&mut self.jit, lsr x11, x11, #(32);); // untag Symbol -> IdentId
        self.jit.bind_label(l1.clone());
        monoasm_arm64!(&mut self.jit,
            mov x12, (cache_addr);   // cur = &Cache[0]
            mov x13, x12;            // min = &Cache[0]
            ldr w0, [x12, #(CACHE_METHOD as u32)];
            cbz x0, not_found;
            cmp w11, w0;
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &found);
        monoasm_arm64!(&mut self.jit, mov x15, #((CACHE_SIZE - 1) as u64););
        self.jit.bind_label(loop0.clone());
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x12, #(CACHE_COUNTER as u32)];  // cur counter
            ldr x14, [x13, #(CACHE_COUNTER as u32)]; // min counter
            cmp x9, x14;
            csel x13, x12, x13, lt;                  // min tracks the least-used
            add x12, x12, #(16);                     // next entry
            ldr w0, [x12, #(CACHE_METHOD as u32)];
            cbz x0, not_found;
            cmp w11, w0;
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &found);
        monoasm_arm64!(&mut self.jit,
            sub x15, x15, #1;
            cbz x15, not_found;
            b loop0;
        );
        self.jit.bind_label(found.clone());
        monoasm_arm64!(&mut self.jit,
            ldr w0, [x12, #(CACHE_FID as u32)];      // result = fid
            ldr x9, [x12, #(CACHE_COUNTER as u32)];
            add x9, x9, #1;
            str x9, [x12, #(CACHE_COUNTER as u32)];  // bump cur counter
            ldr x14, [x13, #(CACHE_COUNTER as u32)];
            cmp x9, x14;
        );
        self.jit.bcond_label(monoasm::Cond::Le, &exit); // cur <= min -> done
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x12];      ldr x14, [x13];       // swap word0 (method+fid)
            str x14, [x12];     str x9, [x13];
            ldr x9, [x12, #(8)]; ldr x14, [x13, #(8)]; // swap word1 (counter)
            str x14, [x12, #(8)]; str x9, [x13, #(8)];
            b exit;
        );
        self.jit.bind_label(not_found.clone());
        monoasm_arm64!(&mut self.jit,
            stp x11, x12, [sp, #-16]!;                // save IdentId + cur&Cache
            mov x0, x19;                              // vm
            mov x1, x20;                              // globals
        );
        self.a64_frame_load(2, lfp, conv(recv) as u32); // x2 = recv
        monoasm_arm64!(&mut self.jit,
            mov x3, x11;                              // func_name (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (find as *const () as u64);
            blr x9;                                   // x0 = Option<FuncId>
            ldr x30, [sp], #16;
            ldp x11, x12, [sp], #16;
            cbz x0, exit;                             // None -> error (handle_error)
            str w0, [x12, #(CACHE_FID as u32)];
            str w11, [x12, #(CACHE_METHOD as u32)];
            mov x9, #1;
            str x9, [x12, #(CACHE_COUNTER as u32)];
            b exit;
        );
        self.jit.bind_label(not_symbol.clone());
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                              // vm
            mov x1, x20;                              // globals
            mov x2, x11;                              // arg0 value
            str x30, [sp, #-16]!;
            mov x9, (expect_string as *const () as u64);
            blr x9;                                   // x0 = Option<IdentId>
            ldr x30, [sp], #16;
        );
        self.emit_handle_error(error);                // x0 == 0 -> error
        monoasm_arm64!(&mut self.jit,
            mov x11, x0;                              // IdentId = result
            b l1;
        );
        self.jit.bind_label(exit);
    }

    /// C call into a `(vm, globals, caller_lfp, callee_lfp, callid)` argument
    /// marshaller (`jit_handle_arguments_no_block_for_send{,_splat}`). aarch64
    /// twin of x86 `generic_handle_arguments` + `push/pop_stack_offset`: the
    /// callee frame is reserved below sp from the *runtime* callee's
    /// `FUNCDATA_OFS` (held via `x26 = &FuncData`, callee-saved so it survives
    /// the call and the reservation is recomputed to restore sp). Result in x0.
    fn a64_generic_handle_arguments(&mut self, f: u64, callid: CallSiteId) {
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                              // vm
            mov x1, x20;                              // globals
            mov x2, x22;                              // caller LFP
            sub x3, sp, #(RSP_LOCAL_FRAME as u32);    // callee LFP (before reserving)
            mov x4, (callid.get() as u64);            // callid
            str x30, [sp, #-16]!;                     // save LR
            // reserve the callee frame (FUNCDATA_OFS is in 16-byte units).
            ldrh w9, [x26, #(FUNCDATA_OFS as u32)];
            lsl x9, x9, #(4);
            add x9, x9, #(16);
            mov x10, sp;
            sub x10, x10, x9;
            mov sp, x10;
            mov x9, (f);
            blr x9;                                   // x0 = Option<Value>
            // restore sp (recompute the reservation — x26 survives the call).
            ldrh w9, [x26, #(FUNCDATA_OFS as u32)];
            lsl x9, x9, #(4);
            add x9, x9, #(16);
            mov x10, sp;
            add x10, x10, x9;
            mov sp, x10;
            ldr x30, [sp], #16;                       // restore LR
        );
    }

    /// Marshal the forwarded args for a no-splat `send`, splicing out arg0 (the
    /// method name). Twin of x86 `object_send_handle_arguments`. Fast path: a
    /// "simple" iseq callee with matching arity — unrolled copy of the
    /// `pos_num - 1` positionals from `args + 1` into the callee slots. Cold
    /// (inline, no `select_page`): arity mismatch -> `wrong_number_of_arg`;
    /// non-simple callee -> the generic C marshaller. `x26` holds `&FuncData`.
    fn object_send_handle_arguments(
        &mut self,
        args: SlotId,
        pos_num: usize,
        callid: CallSiteId,
        error: &DestLabel,
    ) {
        let lfp = GP::R14.a64().0;
        let not_simple = self.jit.label();
        let arg_error = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldrb w9, [x26, #((FUNCDATA_META + META_KIND) as u32)];
            tbz w9, #(4), not_simple;                 // not a simple iseq -> generic
            ldrh w9, [x26, #(FUNCDATA_MIN as u32)];    // callee min arity
            cmp w9, #((pos_num - 1) as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &arg_error);
        // fast copy: splice out arg0 (source starts at args + 1).
        monoasm_arm64!(&mut self.jit, sub x13, sp, #(RSP_LOCAL_FRAME as u32);); // callee LFP base
        for k in 0..(pos_num - 1) {
            self.a64_frame_load(9, lfp, conv(args + (k + 1)) as u32);
            self.a64_frame_store(9, 13, (LFP_ARG0 + 8 * k as i32) as u32);
        }
        monoasm_arm64!(&mut self.jit, b exit;);
        // arity mismatch (inline): raise. `wrong_number_of_arg(vm, expected, given)`
        // matches x86: expected = pos_num-1, given = the callee min arity in w9.
        self.jit.bind_label(arg_error);
        monoasm_arm64!(&mut self.jit,
            mov x2, x9;                               // given (callee min arity)
            mov x0, x19;                              // vm
            mov x1, #((pos_num - 1) as u64);          // expected
            str x30, [sp, #-16]!;
            mov x9, (wrong_number_of_arg as *const () as u64);
            blr x9;
            ldr x30, [sp], #16;
            b error;
        );
        // non-simple callee (inline): the generic C marshaller.
        self.jit.bind_label(not_simple);
        self.a64_generic_handle_arguments(
            crate::runtime::jit_handle_arguments_no_block_for_send as *const () as u64,
            callid,
        );
        self.emit_handle_error(error);
        monoasm_arm64!(&mut self.jit, b exit;);
        self.jit.bind_label(exit);
    }

    /// aarch64 `Object#send` / `#__send__` inline body. Twin of x86
    /// `object_send_inline`: resolve arg0 (Symbol/String) -> FuncId via the LFU
    /// symbol cache, build the callee frame from the resolved `&FuncData`
    /// (held in x26, callee-saved), marshal the args (splicing out arg0), and
    /// tail into the callee's codeptr. Result in x0. Reached from the shared
    /// `object_send` generator through `ir.inline` once this is registered with
    /// `inline_gen2!`.
    pub(crate) fn object_send_inline(
        &mut self,
        callid: CallSiteId,
        store: &Store,
        using_fpr: UsingFpr,
        error: &DestLabel,
        no_splat: bool,
    ) {
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            block_fid,
            block_arg,
            ..
        } = store[callid];
        let lfp = GP::R14.a64().0;
        // Per-callsite symbol cache. Heap-leaked (64-byte zeroed region ==
        // size_of::<Cache>()) so its absolute address bakes into the stub; a
        // fresh JIT-data label is not resolved at emit time on aarch64.
        assert_eq!(std::mem::size_of::<Cache>(), 64);
        let cache_addr = Box::into_raw(Box::new([0u64; 8])) as u64;
        self.check_version_with_cache(cache_addr);
        self.emit_fpr_save(using_fpr, false);
        // resolve arg0 -> FuncId in x0
        if no_splat {
            if pos_num < 1 {
                let error = error.clone();
                monoasm_arm64!(&mut self.jit,
                    mov x0, x19;
                    str x30, [sp, #-16]!;
                    mov x9, (no_method_name as *const () as u64);
                    blr x9;
                    ldr x30, [sp], #16;
                    b error;
                );
            } else {
                self.a64_frame_load(11, lfp, conv(args) as u32); // x11 = arg0
                self.object_send_inner(recv, cache_addr, error);
            }
        } else {
            self.object_send_splat_arg0(args, error);
            self.object_send_inner(recv, cache_addr, error);
        }
        self.emit_handle_error(error);
        // FuncId (x0) -> &FuncData in x26 (callee-saved, survives the arg C-calls
        // and the callee call).
        monoasm_arm64!(&mut self.jit, mov x2, x0;);
        self.a64_get_func_data_x2(); // x9 = &FuncData
        monoasm_arm64!(&mut self.jit, mov x26, x9;);
        // callee frame fields: OUTER=0, META from funcdata, SVAR=0, block, SELF.
        monoasm_arm64!(&mut self.jit, mov x9, #0;);
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_OUTER) as u32);
        monoasm_arm64!(&mut self.jit, ldr x9, [x26, #(FUNCDATA_META as u32)];);
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_META) as u32);
        monoasm_arm64!(&mut self.jit, mov x9, #0;);
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_SVAR) as u32);
        self.a64_set_block(block_fid, block_arg);
        self.a64_frame_load(9, lfp, conv(recv) as u32);
        self.a64_store_x9_below_sp((RSP_LOCAL_FRAME + LFP_SELF) as u32);
        // marshal arguments (splicing out arg0)
        if no_splat {
            self.object_send_handle_arguments(args, pos_num, callid, error);
        } else {
            self.a64_generic_handle_arguments(
                crate::runtime::jit_handle_arguments_no_block_for_send_splat as *const () as u64,
                callid,
            );
            self.emit_handle_error(error);
        }
        // call the resolved funcdata (x26): push_frame + set LFP + set PC +
        // blr codeptr + pop_frame (the runtime-codeptr call shape from emit_yield).
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x11, sp, #(RSP_CFP as u32);
            str x10, [x11];
            str x11, [x19, #(EXECUTOR_CFP as u32)];
            sub x22, sp, #(RSP_LOCAL_FRAME as u32);       // callee LFP
            sub x10, sp, #((RSP_CFP + CFP_LFP) as u32);
            str x22, [x10];
            sub x3, x21, #(16u32);                        // call-site bc ptr (with-pc builtins)
            ldr x21, [x26, #(FUNCDATA_PC as u32)];         // PC <- callee pc
            ldr x10, [x26, #(FUNCDATA_CODEPTR as u32)];
            blr x10;                                       // x0 = result
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
        self.emit_fpr_restore(using_fpr, false);
        self.emit_handle_error(error);
    }

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
        offset: usize,
        recv: SlotId,
        args: SlotId,
        lead_num: usize,
        src: SlotId,
        layout: ForwardedLayout,
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
        let expected_len = layout.from_src;
        // A `*rest` callee gets its `Array` built straight off the
        // caller's slots (the elements are a contiguous tail of the
        // forwarded window — see `forwarded_deferred_layout`). Emitted
        // *first*, before any callee slot is written and before `x13` is
        // taken: the call clobbers the x9..x15 temps and its LR push
        // would otherwise land in the frame under construction. `sp` is
        // lowered past the whole callee frame for the duration, as the
        // helper path does. GC-safe for the same reason the side-exit
        // materialization is: every element is still live in the
        // caller's frame slots, which are scanned.
        if let Some(rest) = layout.rest {
            monoasm_arm64!(&mut self.jit,
                str x30, [sp, #-16]!;                 // save LR
                ldr x11, [x29];                       // caller frame pointer
                mov x9, (rbp_local(src + rest.src_offset) as u64);
                sub x0, x11, x9;                      // highest-address element
                mov x1, (rest.len as u64);
            );
            self.a64_sp_sub(offset as u32);
            monoasm_arm64!(&mut self.jit,
                mov x9, (crate::runtime::create_array as *const () as u64);
                blr x9;
            );
            self.a64_sp_add(offset as u32);
            monoasm_arm64!(&mut self.jit,
                ldr x30, [sp], #16;                   // restore LR
                sub x13, sp, #(RSP_LOCAL_FRAME as u32);
                mov x12, x0;
            );
            self.a64_frame_store(VAL, CLFP, (LFP_ARG0 + 8 * rest.pos as i32) as u32);
        }
        monoasm_arm64!(&mut self.jit,
            sub x13, sp, #(RSP_LOCAL_FRAME as u32);
        );
        // A bare `**kwrest` gets `nil` — "no keyword arguments" everywhere
        // downstream, matching the runtime's `store_empty_kw_rest`.
        if let Some(kw_rest) = layout.kw_rest {
            monoasm_arm64!(&mut self.jit, mov x12, (NIL_VALUE as u64););
            self.a64_frame_store(VAL, CLFP, (LFP_SELF + 8 * kw_rest.0 as i32) as u32);
        }
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
        if layout.none_fill != 0 {
            monoasm_arm64!(&mut self.jit, mov x12, (0u64););
            for j in 0..layout.none_fill {
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
    /// and the VM invoker's `aftargs` sequence.
    /// Returns the call's return address (the instruction after the `blr`,
    /// i.e. pop_frame's first instruction) so `emit_call` can register it for
    /// BOP-redefinition eviction (mirrors x86 `emit_call`'s
    /// `get_current_address` after the `call`).
    pub(super) fn a64_do_call(
        &mut self,
        codeptr: CodePtr,
        is_iseq: bool,
        callee_pc: Option<BytecodePtrBase>,
        call_site_bc_ptr: BytecodePtr,
    ) -> CodePtr {
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
        let return_addr = self.jit.get_current_address();
        // pop_frame: restore caller cfp + lfp from x29 (== x86 rbp).
        monoasm_arm64!(&mut self.jit,
            sub x10, x29, #(BP_CFP as u32);
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            ldur x22, [x29, #(-((BP_CFP + CFP_LFP) as i32))];
        );
        return_addr
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

    /// The call itself. `jit_entry` is unused — aarch64 always calls `codeptr`
    /// directly (no JIT-entry dispatch).
    ///
    /// Like x86, the call's return address is registered for BOP-redefinition
    /// eviction: when a basic op is redefined *inside* the callee, the caller's
    /// own compiled body (inline integer arithmetic, folds) is stale the moment
    /// the callee returns, and the callee's entry guards cannot protect the
    /// *caller*. `immediate_eviction` finds this frame's return address in
    /// `return_addr_table` and overwrites the recorded patch point (bound by
    /// the following `ImmediateEvict`, after the result store) with a `B evict`
    /// so the frame deopts instead of resuming stale code.
    pub(in crate::codegen::jitgen) fn emit_call(
        &mut self,
        codeptr: CodePtr,
        is_iseq: bool,
        callee_pc: Option<BytecodePtrBase>,
        call_site_bc_ptr: BytecodePtr,
        _jit_entry: Option<DestLabel>,
        evict: AsmEvict,
        evict_label: &DestLabel,
    ) {
        let return_addr = self.a64_do_call(codeptr, is_iseq, callee_pc, call_site_bc_ptr);
        self.set_deopt_with_return_addr(return_addr, evict, evict_label);
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
    /// block's funcdata indirectly. Like every other machine-level call, the
    /// block call's return address is registered for BOP-redefinition eviction
    /// (see `emit_call`) — a yielded block can redefine a basic op, and the
    /// yielding frame must then deopt on resume instead of running its stale
    /// compiled continuation. Registering here also keeps the
    /// `emit_immediate_evict` invariant that every `ImmediateEvict`'s evict id
    /// was registered by its own site in the same block (AsmEvict ids restart
    /// per block, so an unregistered producer would read a stale same-id entry
    /// from `asm_return_addr_table` and hijack an unrelated site's patch
    /// point). `error` catches a missing block, an argument error, or a callee
    /// raise. Bails on an out-of-range callee-frame offset.
    pub(in crate::codegen::jitgen) fn emit_yield(
        &mut self,
        callid: CallSiteId,
        error: &DestLabel,
        evict: AsmEvict,
        evict_label: &DestLabel,
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
        );
        let return_addr = self.jit.get_current_address();
        monoasm_arm64!(&mut self.jit,
            sub x11, sp, #(RSP_CFP as u32);
            ldr x10, [x11];
            str x10, [x19, #(EXECUTOR_CFP as u32)];
            sub x10, x29, #((BP_CFP + CFP_LFP) as u32);
            ldr x22, [x10];
        );
        self.set_deopt_with_return_addr(return_addr, evict, evict_label);
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
