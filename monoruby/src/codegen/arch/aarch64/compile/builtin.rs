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

    /// `Hash#size`: entry count of the hash in `base`, fixnum-tagged into `dst`.
    /// aarch64 twin of the x86 `gen_hash_len_fixnum`.
    ///
    /// A small hash keeps its length in the header's representation bits; a
    /// boxed one keeps it in the entry vector, behind a pointer that is only a
    /// pointer on that side of the branch — so unlike `Array#size` the two
    /// lengths cannot both be loaded and `csel`ed.
    ///
    /// ### destroy
    /// - x9
    pub(crate) fn gen_hash_len_fixnum(&mut self, dst: GP, base: GP, layout: rubymap::EntriesLayout) {
        let (d, b) = (dst.a64().0, base.a64().0);
        let tag = self.jit.label();
        let ty_flags = (RVALUE_OFFSET_TY + 1) as u32;
        let boxed_rep = HASH_REP_BOXED as u32;
        let map_ptr = HASH_CONTENT_MAP_OFFSET as u32;
        let len_off = layout.len_offset as u32;
        monoasm_arm64!(&mut self.jit,
            ldrb w(d), [x(b), #(ty_flags)];
            mov x9, (HASH_REP_MASK as u64);
            and x(d), x(d), x9;
            cmp x(d), #(boxed_rep);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &tag);
        monoasm_arm64!(&mut self.jit,
            ldr x(d), [x(b), #(map_ptr)];
            ldr x(d), [x(d), #(len_off)];
        );
        self.jit.bind_label(tag);
        monoasm_arm64!(&mut self.jit,
            lsl x(d), x(d), #1;
            add x(d), x(d), #1;
        );
    }

    /// `Hash#compare_by_identity?`: hash in `base`, Ruby bool into `dst`.
    /// aarch64 twin of the x86 `gen_hash_compare_by_identity`.
    ///
    /// Both representations reduce to one bit — a `ty_flags` bit while inline,
    /// and the boxed `HashContent` discriminant (0 = Map, 1 = IdentMap) — so
    /// masking bit 0 of either answers it without a comparison.
    ///
    /// ### destroy
    /// - x9
    pub(crate) fn gen_hash_compare_by_identity(&mut self, dst: GP, base: GP) {
        let (d, b) = (dst.a64().0, base.a64().0);
        let inline_case = self.jit.label();
        let tag_ready = self.jit.label();
        let ty_flags = (RVALUE_OFFSET_TY + 1) as u32;
        let boxed_rep = HASH_REP_BOXED as u32;
        let ident_shift = HASH_INLINE_IDENT_BIT.trailing_zeros();
        let content = HASH_CONTENT_OFFSET as u32;
        monoasm_arm64!(&mut self.jit,
            ldrb w(d), [x(b), #(ty_flags)];
            mov x9, (HASH_REP_MASK as u64);
            and x9, x(d), x9;
            cmp x9, #(boxed_rep);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &inline_case);
        monoasm_arm64!(&mut self.jit,
            ldr x(d), [x(b), #(content)];    // 0 = Map, 1 = IdentMap
            b tag_ready;
        inline_case:
            lsr x(d), x(d), #(ident_shift);
        tag_ready:
            // isolate bit 0, then 0/1 -> false/true (0x14 / 0x1c)
            lsl x(d), x(d), #(63);
            lsr x(d), x(d), #(63);
            lsl x(d), x(d), #(3);
            add x(d), x(d), #(FALSE_VALUE as u32);
        );
    }

    /// `Hash#default` (`want_proc == false`) / `#default_proc`: hash in `base`,
    /// result Value into `dst`. aarch64 twin of the x86 `gen_hash_default`.
    ///
    /// An inline hash never carries a default, a null slot means none is set,
    /// and the other discriminant belongs to the sibling accessor — all three
    /// answer `nil`, matching the builtins' `unwrap_or_default`.
    ///
    /// ### destroy
    /// - x9
    pub(crate) fn gen_hash_default(&mut self, dst: GP, base: GP, want_proc: bool) {
        let (d, b) = (dst.a64().0, base.a64().0);
        let nil_case = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = (RVALUE_OFFSET_TY + 1) as u32;
        let boxed_rep = HASH_REP_BOXED as u32;
        let slot = HASH_DEFAULT_OFFSET as u32;
        let payload = HASH_DEFAULT_PAYLOAD_OFFSET as u32;
        let want_tag = (if want_proc {
            HASH_DEFAULT_TAG_PROC
        } else {
            HASH_DEFAULT_TAG_VALUE
        }) as u64;
        // Isolate the representation bits with a shift pair rather than a
        // mask register: `base` is still live, so `dst` must not be borrowed
        // as scratch here.
        let rep_bits = HASH_REP_MASK.count_ones();
        monoasm_arm64!(&mut self.jit,
            ldrb w9, [x(b), #(ty_flags)];
            lsl x9, x9, #(64 - rep_bits);
            lsr x9, x9, #(64 - rep_bits);
            cmp x9, #(boxed_rep);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &nil_case);
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(b), #(slot)];         // Option<Box<HashDefault>>: null = None
            cmp x9, #(0);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &nil_case);
        monoasm_arm64!(&mut self.jit,
            ldr x(d), [x9];                  // discriminant
            cmp x(d), #(want_tag as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &nil_case);
        monoasm_arm64!(&mut self.jit,
            ldr x(d), [x9, #(payload)];
            b exit;
        nil_case:
            mov x(d), #(NIL_VALUE);
        exit:
        );
    }

    /// `Hash#__key_at` / `#__value_at`: hash in Rdx (x2), fixnum index in Rcx
    /// (x1), result Value in Rax (x0). aarch64 twin of the x86
    /// `gen_hash_entry_at`.
    ///
    /// Total by construction — a negative or out-of-range index answers `nil`
    /// rather than trapping — so there is no generic fallback and no error edge.
    ///
    /// ### destroy
    /// - x0 (Rax), x1 (Rcx), x3 (Rsi), x4 (Rdi), x9
    pub(crate) fn gen_hash_entry_at(&mut self, want_key: bool, layout: rubymap::EntriesLayout) {
        let boxed = self.jit.label();
        let exit = self.jit.label();
        let ty_flags = (RVALUE_OFFSET_TY + 1) as u32;
        let boxed_rep = HASH_REP_BOXED as u32;
        let inline_field = (HASH_INLINE_PAIRS_OFFSET
            + if want_key {
                HASH_INLINE_KEY_OFFSET
            } else {
                HASH_INLINE_VALUE_OFFSET
            }) as u32;
        let stride = HASH_INLINE_PAIR_STRIDE as u64;
        let map_ptr = HASH_CONTENT_MAP_OFFSET as u32;
        let len_off = layout.len_offset as u32;
        let ptr_off = layout.ptr_offset as u32;
        let bucket_size = layout.bucket_size as u64;
        let bucket_field = (if want_key {
            layout.key_offset
        } else {
            layout.value_offset
        }) as u32;
        monoasm_arm64!(&mut self.jit,
            asr x1, x1, #(1);                 // untag the index
            mov x0, #(NIL_VALUE);             // nil unless a path below overwrites it
            cmp x1, #(0);
        );
        self.jit.bcond_label(monoasm::Cond::Lt, &exit); // negative -> nil
        monoasm_arm64!(&mut self.jit,
            ldrb w3, [x2, #(ty_flags)];
            mov x9, (HASH_REP_MASK as u64);
            and x3, x3, x9;                   // representation bits
            cmp x3, #(boxed_rep);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &boxed);
        // Inline: the representation bits double as the length.
        monoasm_arm64!(&mut self.jit,
            cmp x3, x1;                       // len vs idx
        );
        self.jit.bcond_label(monoasm::Cond::Ls, &exit); // len <= idx -> nil
        monoasm_arm64!(&mut self.jit,
            mov x9, (stride);
            mul x1, x1, x9;
            add x1, x1, x2;
            ldr x0, [x1, #(inline_field)];
            b exit;
        boxed:
            ldr x4, [x2, #(map_ptr)];
            ldr x9, [x4, #(len_off)];
            cmp x9, x1;                       // len vs idx
        );
        self.jit.bcond_label(monoasm::Cond::Ls, &exit);
        monoasm_arm64!(&mut self.jit,
            mov x9, (bucket_size);
            mul x1, x1, x9;
            ldr x9, [x4, #(ptr_off)];
            add x1, x1, x9;
            ldr x0, [x1, #(bucket_field)];
        exit:
        );
    }

    /// Inlined `Class#allocate`: `alloc_func(class_id, globals)`. The class id
    /// (a u32) and the resolved allocator pointer are embedded as constants;
    /// arg0 = class_id, arg1 = globals (GLOBALS/x20). Result Value in Rax (x0).
    /// FP pool saved by the surrounding fpr_save.
    ///
    /// With an `inline` payload the whole allocation is emitted here
    /// instead: pop a cell (`emit_alloc_cell`) and write exactly what the
    /// stock allocator would have produced. The runtime call is kept as the
    /// fallback for the page-boundary cases.
    pub(crate) fn emit_class_allocate(
        &mut self,
        class_id: u32,
        alloc_func: u64,
        inline: Option<InlineAlloc>,
    ) {
        let Some(inline) = inline.filter(|_| !self.alloc_free_head_addr.is_null()) else {
            self.class_allocate_call(class_id, alloc_func);
            return;
        };
        let rax = GP::Rax.a64().0; // x0 (result)
        let slow = self.jit.label();
        let cont = self.jit.label();
        // 8-byte object header: flag=1 (live) | ty<<16 | class<<32.
        let ty = match inline {
            InlineAlloc::Object => ObjTy::OBJECT,
            InlineAlloc::Struct(_) => ObjTy::STRUCT,
        };
        let header: u64 = ((class_id as u64) << 32) | ((ty.get() as u64) << 16) | 1;
        self.emit_alloc_cell(CellHeader::Imm(header), &slow);
        monoasm_arm64!(&mut self.jit,
            mov x12, #0;
            str x12, [x(rax), #(RVALUE_OFFSET_VAR as u32)]; // var_table = None
        );
        match inline {
            // `ObjKind::object()` == `[None; OBJECT_INLINE_IVAR]` at the
            // head of the `kind` union (the same `RVALUE_OFFSET_KIND +
            // ivarid * 8` addressing the ivar emitters use), and `None`
            // for `Option<Value>` is a zero word.
            InlineAlloc::Object => {
                for k in 0..OBJECT_INLINE_IVAR {
                    let off = RVALUE_OFFSET_KIND as u32 + (k as u32) * 8;
                    monoasm_arm64!(&mut self.jit,
                        str x12, [x(rax), #(off)];
                    );
                }
            }
            // `StructInner::new(len)` == a `SmallVec` holding `len` nils:
            // the smallvec's capacity field doubles as the inline length,
            // and slots past `len` stay untouched, exactly as in Rust.
            InlineAlloc::Struct(len) => {
                monoasm_arm64!(&mut self.jit,
                    mov x12, (len as u64);
                    str x12, [x(rax), #(RVALUE_OFFSET_ARY_CAPA as u32)];
                    mov x12, (NIL_VALUE);
                );
                for k in 0..len {
                    let off = RVALUE_OFFSET_INLINE as u32 + (k as u32) * 8;
                    monoasm_arm64!(&mut self.jit,
                        str x12, [x(rax), #(off)];
                    );
                }
            }
        }
        monoasm_arm64!(&mut self.jit, b cont;);
        self.jit.bind_label(slow);
        self.class_allocate_call(class_id, alloc_func);
        self.jit.bind_label(cont);
    }

    fn class_allocate_call(&mut self, class_id: u32, alloc_func: u64) {
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

    /// `Array#[]=` slice form, `recv[start, len] = val`. aarch64 twin of x86
    /// `emit_array_slice_assign`; see there for which shape is inlined and
    /// why. Cold blocks stay on this page (aarch64 b/b.cond cannot reach
    /// monoasm's second page).
    ///
    /// ### in
    /// - Rdi (x4): receiver: Array (class- and frozen-guarded)
    /// - Rsi (x3): start: Fixnum (tagged)
    /// - Rdx (x2): val: Value
    ///
    /// ### out
    /// - Rax (x0): non-null on success (the caller's `handle_error` checks it)
    ///
    pub(crate) fn emit_array_slice_assign(&mut self, f: u64, len: usize) {
        let rdi = GP::Rdi.a64().0; // x4  receiver
        let rsi = GP::Rsi.a64().0; // x3  start
        let rdx = GP::Rdx.a64().0; // x2  val
        let slow = self.jit.label();
        let src_heap = self.jit.label();
        let src_ready = self.jit.label();
        let dst_heap = self.jit.label();
        let dst_ready = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            asr x(rsi), x(rsi), #(1u32);       // untag start
            tbnz x(rsi), #(63), slow;          // negative: let the callee wrap it
            // A self-assignment would copy a buffer over itself; hand it to
            // the callee, which snapshots the source first.
            cmp x(rdi), x(rdx);
        );
        self.jit.bcond_label(monoasm::Cond::Eq, &slow);
        monoasm_arm64!(&mut self.jit,
            // `val` must be an Array...
            mov x9, (0b111);
            and x9, x(rdx), x9;
            cbnz x9, slow;
            ldrb w9, [x(rdx), #(RVALUE_OFFSET_TY as u32)];
            cmp x9, #(ObjTy::ARRAY.get() as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &slow);
        monoasm_arm64!(&mut self.jit,
            // ...of exactly `len` elements. x11 <- its data.
            ldr x9, [x(rdx), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x9, #(ARRAY_INLINE_CAPA as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &src_heap);
        monoasm_arm64!(&mut self.jit,
            cmp x9, #(len as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &slow);
        monoasm_arm64!(&mut self.jit,
            add x11, x(rdx), #(RVALUE_OFFSET_INLINE as u32);
        );
        self.jit.bind_label(src_ready.clone());
        monoasm_arm64!(&mut self.jit,
            // x9 <- the receiver's length, x12 <- its data.
            ldr x9, [x(rdi), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x9, #(ARRAY_INLINE_CAPA as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &dst_heap);
        monoasm_arm64!(&mut self.jit,
            add x12, x(rdi), #(RVALUE_OFFSET_INLINE as u32);
        );
        self.jit.bind_label(dst_ready.clone());
        monoasm_arm64!(&mut self.jit,
            // The replaced run must lie inside the receiver.
            add x10, x(rsi), #(len as u32);
            cmp x10, x9;
        );
        self.jit.bcond_label(monoasm::Cond::Gt, &slow);
        monoasm_arm64!(&mut self.jit,
            add x12, x12, x(rsi), lsl #3;      // &recv[start]
        );
        for i in 0..len {
            let disp = (i * 8) as u32;
            monoasm_arm64!(&mut self.jit,
                ldr x9, [x11, #(disp)];
                str x9, [x12, #(disp)];
            );
        }
        // Several children stored at once: remember the receiver wholesale.
        self.emit_write_barrier_bulk(GP::Rdi);
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdx);                    // `[]=` evaluates to the value
            b exit;
        );
        self.jit.bind_label(src_heap);
        monoasm_arm64!(&mut self.jit,
            ldr x10, [x(rdx), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x10, #(len as u32);
        );
        self.jit.bcond_label(monoasm::Cond::Ne, &slow);
        monoasm_arm64!(&mut self.jit,
            ldr x11, [x(rdx), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            b src_ready;
        );
        self.jit.bind_label(dst_heap);
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            ldr x12, [x(rdi), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            b dst_ready;
        );
        self.jit.bind_label(slow);
        // set_array_slice(base, start, len, val, vm, globals). Source regs at
        // entry: base=x4, start=x3, val=x2. Reorder into the C ABI args
        // without clobbering a still-needed source.
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // base  -> arg0  (x4 free after this)
            mov x1, x(rsi);       // start -> arg1  (x3 free after this)
            mov x3, x(rdx);       // val   -> arg3  (x2 free after this)
            mov x2, (len as u64); // len   -> arg2
            mov x4, x19;          // vm      -> arg4
            mov x5, x20;          // globals -> arg5
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
        self.jit.bind_label(exit);
    }

    /// `Array#rotate!`: `ary_rotate_(recv, count)`. recv (Rdi/x4) -> arg0;
    /// the count arrives tagged in Rsi (x3) — or is the implicit `1` — and
    /// the callee takes a plain `i64`. Result Value in Rax (x0).
    pub(crate) fn emit_array_rotate_(&mut self, f: u64, has_arg: bool) {
        let rdi = GP::Rdi.a64().0; // x4
        let rsi = GP::Rsi.a64().0; // x3
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
        );
        if has_arg {
            monoasm_arm64!(&mut self.jit, asr x1, x(rsi), #(1u32););
        } else {
            monoasm_arm64!(&mut self.jit, mov x1, #(1););
        }
        monoasm_arm64!(&mut self.jit,
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
    }

    /// `Array#<<` — append, with the no-grow case emitted inline. aarch64
    /// twin of x86 `emit_array_shl`; see there for why the receiver's
    /// `SmallVec` residency decides which pair of fields holds the length.
    /// Cold blocks are laid out inline rather than on page 1 (aarch64
    /// b/b.cond cannot reach it — see `array_index`).
    ///
    /// ### in
    /// - Rdi (x4): receiver: Array
    /// - Rsi (x3): value: Value
    ///
    /// ### out
    /// - Rax (x0): receiver: Array (`<<` returns self)
    ///
    pub(crate) fn emit_array_shl(&mut self, f: u64) {
        let rdi = GP::Rdi.a64().0; // x4
        let rsi = GP::Rsi.a64().0; // x3
        let heap = self.jit.label();
        let grow = self.jit.label();
        let stored = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x0, [x(rdi), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            cmp x0, #(ARRAY_INLINE_CAPA as u32);
            b.gt heap;
            // Inline buffer: x0 is the length, ARRAY_INLINE_CAPA the capacity.
            b.eq grow;
            add x9, x(rdi), x0, lsl #3;
            str x(rsi), [x9, #(RVALUE_OFFSET_INLINE as u32)];
            add x0, x0, #(1);
            str x0, [x(rdi), #(RVALUE_OFFSET_ARY_CAPA as u32)];
            b stored;
        );
        self.jit.bind_label(heap);
        monoasm_arm64!(&mut self.jit,
            // Spilled buffer: x0 is the capacity, the length lives beside
            // the pointer.
            ldr x10, [x(rdi), #(RVALUE_OFFSET_HEAP_LEN as u32)];
            cmp x10, x0;
            b.ge grow;
            ldr x11, [x(rdi), #(RVALUE_OFFSET_HEAP_PTR as u32)];
            add x9, x11, x10, lsl #3;
            str x(rsi), [x9];
            add x10, x10, #(1);
            str x10, [x(rdi), #(RVALUE_OFFSET_HEAP_LEN as u32)];
        );
        self.jit.bind_label(stored);
        // Write barrier: x4 (Rdi) = the array (parent), x3 (Rsi) = appended value.
        self.emit_write_barrier(GP::Rdi, GP::Rsi);
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);
            b exit;
        );
        self.jit.bind_label(grow);
        // Buffer full: let `ary_shl` reallocate (and run its own barrier).
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);       // recv -> arg0
            mov x1, x(rsi);       // arg  -> arg1
            mov x9, (f);
            str x30, [sp, #-16]!;
            blr x9;
            ldr x30, [sp], #16;
        );
        self.jit.bind_label(exit);
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
