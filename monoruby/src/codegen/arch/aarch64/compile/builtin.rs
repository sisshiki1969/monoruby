use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// `Fiber.yield` with no args: the yielded value (Rsi/x3) is nil.
    pub(crate) fn emit_fiber_yield_value_nil(&mut self) {
        monoasm_arm64!(&mut self.jit,
            mov x3, (Value::nil().id());   // GP::Rsi == x3
        );
    }

    /// `Fiber.yield(*args)` with ≥2 args: build the args array, leaving it in
    /// Rsi (x3). `args_off` is `conv(args)`; any frame offset is addressable
    /// (large offsets materialize through a scratch register via `a64_addr_sub`,
    /// so the caller never has to bail).
    pub(crate) fn emit_fiber_yield_value_array(&mut self, args_off: usize, pos_num: usize) {
        let lfp = GP::R14.a64().0; // x22
        self.a64_addr_sub(0, lfp, args_off as u32); // x0 = &args (lfp - conv)
        monoasm_arm64!(&mut self.jit,
            mov x1, (pos_num);
            mov x9, (crate::runtime::create_array as *const () as u64);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
            mov x3, x0;
        );
    }

    /// `Fiber.yield`: call `yield_fiber(vm, value)` with value in Rsi (x3). The
    /// method's own LR is saved: the invoker's a64_push_callee_save stashes the
    /// *post-blr* x30, not ours, so we restore the real return address after the
    /// fiber resumes back here.
    /// A yield with no parent fiber (main fiber / a green thread's root)
    /// must not reach the switch stub — it would load SP through a null
    /// `parent_fiber` — so route it to the error helper instead (returns
    /// None with a FiberError set; the inline's handle_error picks it up).
    pub(crate) fn emit_fiber_yield_call(&mut self, yield_fiber: u64, no_parent: u64) {
        let none = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;          // vm (EXEC)
            mov x1, x3;           // value (Rsi)
            ldr x9, [x0, #(EXECUTOR_PARENT_FIBER as u32)];
            cbz x9, none;
            mov x9, (yield_fiber);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
            b exit;
        none:
            mov x9, (no_parent);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        exit:
        );
    }

    /// Load a 64-bit tagged fixnum literal into Rsi (x3) for an `Integer` bit-op
    /// whose immediate doesn't fit a 32-bit encoding.
    pub(crate) fn emit_load_tagged_rsi(&mut self, tagged: i64) {
        monoasm_arm64!(&mut self.jit, mov x3, (tagged as u64);); // GP::Rsi == x3
    }

    /// `Integer#|` with a tagged immediate (`(2a+1)|(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitor_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (imm as u64);
            orr x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#|` register-register.
    pub(crate) fn emit_bitor_rr(&mut self) {
        monoasm_arm64!(&mut self.jit, orr x4, x4, x3;); // Rdi==x4, Rsi==x3
    }
    /// `Integer#&` with a tagged immediate (`(2a+1)&(2b+1)` keeps LSB=1).
    pub(crate) fn emit_bitand_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (imm as u64);
            and x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#&` register-register.
    pub(crate) fn emit_bitand_rr(&mut self) {
        monoasm_arm64!(&mut self.jit, and x4, x4, x3;);
    }
    /// `Integer#^` with a tagged immediate (use `imm-1` so lhs's tag survives).
    pub(crate) fn emit_bitxor_imm(&mut self, imm: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, ((imm - 1) as u64);
            eor x4, x4, x9;          // GP::Rdi == x4
        );
    }
    /// `Integer#^` register-register (`(2a+1)^(2b+1)` clears LSB, re-tag +1).
    pub(crate) fn emit_bitxor_rr(&mut self) {
        monoasm_arm64!(&mut self.jit,
            eor x4, x4, x3;          // GP::Rdi == x4, GP::Rsi == x3
            add x4, x4, #(1);
        );
    }

    /// `n << k` / `n >> -k` with `k >= 64`: a non-zero `n` overflows (deopt);
    /// `0` shifts to `0`. lhs in Rdi (x4).
    pub(crate) fn emit_shl_overflow_zero(&mut self, z: i64, deopt: &DestLabel) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (z as u64);
            cmp x4, x9;              // GP::Rdi == x4
        );
        self.jit.bcond_label(monoasm::Cond::Ne, deopt);
        monoasm_arm64!(&mut self.jit,
            mov x4, x9;
        );
    }

    /// `Integer#%` by a positive power of two: `lhs & mask` on the tagged
    /// fixnum in Rdi (x4).
    pub(crate) fn emit_int_rem_pow2_mask(&mut self, mask: i64) {
        monoasm_arm64!(&mut self.jit,
            mov x9, (mask as u64);
            and x4, x4, x9;          // GP::Rdi == x4
        );
    }


    /// Inlined `Float#to_i`: truncate the double in `fsrc` to i64 (`fcvtzs`),
    /// then tag it as a fixnum. `fcvtzs` saturates out-of-range doubles to
    /// i64::MIN/MAX; doubling the result (`adds`) then overflows for both the
    /// saturated case and any value that doesn't fit in a 63-bit fixnum, so a
    /// single signed-overflow branch covers both. aarch64 twin of x86
    /// `cvttsd2siq` + `addq;jo` + `orq 1`. Result Value lands in Rdi (x4).
    pub(crate) fn emit_float_to_int(&mut self, fsrc: FPReg, deopt: &DestLabel, base: usize) {
        let rdi = GP::Rdi.a64().0; // x4
        self.a64_fpr_into_d0(fsrc, base);
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            fcvtzs x(rdi), d0;
            adds x(rdi), x(rdi), x(rdi);   // ×2, set NZCV
        );
        self.jit.bcond_label(monoasm::Cond::Vs, &deopt); // overflow -> deopt
        monoasm_arm64!(&mut self.jit,
            add x(rdi), x(rdi), #(1);      // tag (low bit clear after ×2)
        );
    }

    /// Inlined `BasicObject#object_id`: `i64_to_value(self_id)`. The receiver
    /// (its raw id) is in Rdi (x4); move it to the C ABI arg0 (x0) and call.
    /// Result Value lands in Rax (x0). The FP pool is saved by the surrounding
    /// AsmIr fpr_save/fpr_restore; here we only preserve LR around the `blr`.
    pub(crate) fn emit_object_id(&mut self) {
        let rdi = GP::Rdi.a64().0; // x4
        let f = crate::executor::op::i64_to_value as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // self id -> arg0
            mov x9, (f);
            str x30, [sp, #-16]!; // save LR
            blr x9;               // x0 = i64_to_value(id)
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Hash#[]`: `hashindex(vm, globals, recv, key)`. The receiver is
    /// already in Rdx (x2 == C arg2) and the key in Rcx (x1); move the key to
    /// arg3 first (before x1 is overwritten by globals), then load vm/globals.
    /// Result Value lands in Rax (x0); errors are checked by the trailing
    /// HandleError. FP pool saved by the surrounding fpr_save/restore.
    pub(crate) fn emit_hash_index(&mut self, hashindex: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x3, x1;           // key (Rcx) -> arg3   [recv already in x2 == Rdx]
            mov x0, x19;          // vm (EXEC) -> arg0
            mov x1, x20;          // globals (GLOBALS) -> arg1
            mov x9, (hashindex);
            str x30, [sp, #-16]!; // save LR
            blr x9;               // x0 = hashindex(vm, globals, recv, key)
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Class#allocate`: `alloc_func(class_id, globals)`. The class id
    /// (a u32) and the resolved allocator pointer are embedded as constants;
    /// arg0 = class_id, arg1 = globals (GLOBALS/x20). Result Value in Rax (x0).
    /// FP pool saved by the surrounding fpr_save.
    pub(crate) fn emit_class_allocate(&mut self, class_id: u32, alloc_func: u64) {
        monoasm_arm64!(&mut self.jit,
            mov x0, (class_id as u64); // class_id -> arg0 (low 32 bits read)
            mov x1, x20;               // globals -> arg1
            mov x9, (alloc_func);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Array#clone`: `array_clone_extern(recv)`. recv (Rdi/x4) -> arg0.
    /// Result Value in Rax (x0). FP pool saved by the surrounding fpr_save.
    pub(crate) fn emit_array_clone(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Array#dup`: `array_dup_extern(recv, globals)`. recv (Rdi/x4) ->
    /// arg0, globals (GLOBALS/x20) -> arg1. Result Value in Rax (x0).
    pub(crate) fn emit_array_dup(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x1, x20;          // globals -> arg1
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// Inlined `Array#<<`: `ary_shl(recv, arg)`. recv (Rdi/x4) -> arg0,
    /// arg (Rsi/x3) -> arg1. Result Value in Rax (x0).
    pub(crate) fn emit_array_shl(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        let rsi = GP::Rsi.a64().0; // x3
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x1, x(rsi);       // arg  -> arg1
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// `Integer#succ` / `#next`: fixnum in Rdi (x4); tagged `+1` is `+2` on the
    /// raw bits. Deopts on i63 overflow (interpreter returns a Bignum). aarch64
    /// twin of x86 `addq;jo`.

    /// `String#getbyte`: receiver String in Rdi (x4), fixnum index in Rsi (x3) →
    /// Rax (x0) = byte tagged as a fixnum, or nil when the (negative-adjusted)
    /// index is out of range. aarch64 twin of x86 `emit_string_getbyte`
    /// (`cmovgt` → `csel`, `jae` → `b.ls` on the swapped operands; `ldrb`/`strb`
    /// have no register-offset form here so the address is materialised first).
    ///
    /// ### destroy
    /// - x0 (Rax), x1 (Rcx), x2 (Rdx), x3 (Rsi), x9
    pub(crate) fn emit_string_getbyte(&mut self) {
        let exit = self.jit.label();
        // x4=recv(Rdi), x3=index(Rsi), x0=result(Rax), x1=data ptr(Rcx),
        // x2=tmp(Rdx), x9=scratch
        monoasm_arm64!(&mut self.jit,
            asr x3, x3, #(1);                                // untag index
            // len -> x0 (inline capa vs heap_len select)
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)];
            ldr x9, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, #(STRING_INLINE_CAP as u32);
            csel x0, x9, x0, gt;                             // capa>cap -> heap_len
            // data ptr -> x1 (flags from the cmp above still live)
            add x1, x4, #(RVALUE_OFFSET_INLINE as u32);
            ldr x9, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            csel x1, x9, x1, gt;                             // capa>cap -> heap_ptr
            // negative index counts back from the end
            add x2, x3, x0;                                  // idx + len
            cmp x3, #(0);
            csel x3, x2, x3, lt;                             // idx<0 -> idx+len
            // unsigned bound check (also catches a still-negative index)
            cmp x0, x3;                                      // len vs idx
            mov x0, #(NIL_VALUE);
        );
        self.jit.bcond_label(monoasm::Cond::Ls, &exit); // len<=idx (unsigned) -> nil
        monoasm_arm64!(&mut self.jit,
            add x1, x1, x3;                                  // &data[idx]
            ldrb w0, [x1];
            lsl x0, x0, #(1);
            add x0, x0, #(1);
        );
        self.jit.bind_label(exit);
    }

    /// `String#setbyte`: receiver String in Rdi (x4), fixnum index in Rsi (x3),
    /// fixnum byte value in Rdx (x2). Deopts when the receiver is frozen or
    /// chilled, copy-on-write shared, or the index is out of range (the
    /// interpreter raises / warns there). Keeps the cached code-range
    /// classification consistent with `RStringInner::set_byte`. aarch64 twin of
    /// x86 `emit_string_setbyte`.
    ///
    /// ### destroy
    /// - x0 (Rax), x1 (Rcx), x3 (Rsi), x9
    pub(crate) fn emit_string_setbyte(&mut self, deopt: &DestLabel) {
        let exit = self.jit.label();
        let set_unknown = self.jit.label();
        let deopt = deopt.clone();
        // frozen (0b010) or chilled (0b100) -> deopt
        monoasm_arm64!(&mut self.jit,
            ldrh w0, [x4, #(RVALUE_OFFSET_FLAG as u32)];
            mov x9, (0b110u64);
            tst x0, x9;
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &deopt);
        monoasm_arm64!(&mut self.jit,
            asr x3, x3, #(1);                                // untag index
            asr x2, x2, #(1);                                // untag byte value
            ldr x0, [x4, #(RVALUE_OFFSET_ARY_CAPA as u32)]; // capa / shared tag
            // shared (copy-on-write) buffer -> deopt (interpreter detaches)
            mov x9, (crate::rvalue::STRING_SHARED_TAG as u64);
            cmp x0, x9;
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &deopt);
        monoasm_arm64!(&mut self.jit,
            // len -> x0, data ptr -> x1 (inline vs heap select)
            ldr x9, [x4, #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x0, #(STRING_INLINE_CAP as u32);
            csel x0, x9, x0, gt;
            add x1, x4, #(RVALUE_OFFSET_INLINE as u32);
            ldr x9, [x4, #(RVALUE_OFFSET_HEAP_PTR as u32)];
            csel x1, x9, x1, gt;
            // negative index counts back from the end
            add x9, x3, x0;                                  // idx + len
            cmp x3, #(0);
            csel x3, x9, x3, lt;                             // idx<0 -> idx+len
            // out of range (unsigned, covers still-negative) -> deopt
            cmp x0, x3;                                      // len vs idx
        );
        self.jit.bcond_label(monoasm::Cond::Ls, &deopt); // len<=idx (unsigned)
        monoasm_arm64!(&mut self.jit,
            add x1, x1, x3;                                  // &data[idx]
            strb w2, [x1];
            // code range cache: poking an ASCII byte into a SevenBit string
            // keeps SevenBit; anything else degrades to Unknown.
            ldrb w9, [x4, #(crate::rvalue::STRING_CR_OFFSET as u32)];
            cmp x9, #(crate::rvalue::CodeRange::SevenBit as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &set_unknown);
        monoasm_arm64!(&mut self.jit,
            mov x9, (0x80u64);
            tst x2, x9;                                      // high bit set?
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &exit); // ASCII -> stays SevenBit
        self.jit.bind_label(set_unknown);
        monoasm_arm64!(&mut self.jit,
            mov x9, #(crate::rvalue::CodeRange::Unknown as u32);
            strb w9, [x4, #(crate::rvalue::STRING_CR_OFFSET as u32)];
        );
        self.jit.bind_label(exit);
    }

    /// `Fiddle.___read` integer load: untag the pointer in Rdi (x4), deopt on
    /// NULL, load a `width`-byte value (sign/zero-extended per `signed`), tag
    /// the result as a fixnum in Rax (x0). aarch64 twin of x86
    /// `emit_fiddle_read_int`; signed byte/half loads sign-extend via lsl+asr
    /// since the macro has no `ldrsb`/`ldrsh`.
    pub(crate) fn emit_fiddle_read_int(&mut self, width: u8, signed: bool, deopt: &DestLabel) {
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x4, x4, #(1);     // untag ptr (Rdi == x4)
            cbz x4, deopt;        // NULL -> deopt
        );
        match (width, signed) {
            (1, true) => monoasm_arm64!(&mut self.jit,
                ldrb w0, [x4]; lsl x0, x0, #(56); asr x0, x0, #(56);),
            (1, false) => monoasm_arm64!(&mut self.jit, ldrb w0, [x4];),
            (2, true) => monoasm_arm64!(&mut self.jit,
                ldrh w0, [x4]; lsl x0, x0, #(48); asr x0, x0, #(48);),
            (2, false) => monoasm_arm64!(&mut self.jit, ldrh w0, [x4];),
            (4, true) => monoasm_arm64!(&mut self.jit, ldrsw x0, [x4];),
            (4, false) => monoasm_arm64!(&mut self.jit, ldr w0, [x4];),
            _ => unreachable!(),
        }
        // Tag as Fixnum: x0 = (x0 << 1) | 1.
        monoasm_arm64!(&mut self.jit,
            lsl x0, x0, #(1);
            add x0, x0, #(1);
        );
    }

    /// `Fiddle.___read` f64 load: untag the pointer in Rdi (x4), deopt on NULL,
    /// load the double into `fret`. aarch64 twin of x86 `emit_fiddle_read_f64`.
    pub(crate) fn emit_fiddle_read_f64(&mut self, fret: FPReg, deopt: &DestLabel, base: usize) {
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            asr x4, x4, #(1);
            cbz x4, deopt;
            ldr d0, [x4];
        );
        self.a64_d0_into_fpr(fret, base);
    }

    /// `Fiddle.___write` integer store: save the tagged pointer (the return
    /// value) in Rax (x0), untag the pointer in Rdi (x4), deopt on NULL, untag
    /// the value in Rsi (x3) and store its low `width` bytes. aarch64 twin of
    /// x86 `emit_fiddle_write_int`.
    pub(crate) fn emit_fiddle_write_int(&mut self, width: u8, deopt: &DestLabel) {
        let deopt = deopt.clone();
        monoasm_arm64!(&mut self.jit,
            mov x0, x4;           // rax = tagged ptr (return value)
            asr x4, x4, #(1);     // untag ptr
            cbz x4, deopt;        // NULL -> deopt
            asr x3, x3, #(1);     // untag value (Rsi == x3)
        );
        match width {
            1 => monoasm_arm64!(&mut self.jit, strb w3, [x4];),
            2 => monoasm_arm64!(&mut self.jit, strh w3, [x4];),
            4 => monoasm_arm64!(&mut self.jit, str w3, [x4];),
            _ => unreachable!(),
        }
    }

    /// `Fiddle.___write` f64 store: load the source double into d0, save the
    /// tagged pointer in Rax (x0), untag the pointer in Rdi (x4), deopt on NULL,
    /// store the double. aarch64 twin of x86 `emit_fiddle_write_f64`.
    pub(crate) fn emit_fiddle_write_f64(&mut self, xsrc: FPReg, deopt: &DestLabel, base: usize) {
        let deopt = deopt.clone();
        self.a64_fpr_into_d0(xsrc, base);
        monoasm_arm64!(&mut self.jit,
            mov x0, x4;           // rax = tagged ptr (return value)
            asr x4, x4, #(1);     // untag ptr
            cbz x4, deopt;        // NULL -> deopt
            str d0, [x4];
        );
    }
}
