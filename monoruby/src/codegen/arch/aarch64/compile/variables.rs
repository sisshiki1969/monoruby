use super::*;
use monoasm_macro::monoasm_arm64;

impl Codegen {

    /// Walk `outer` outer-LFP links, leaving the target outer LFP in `dst`.
    /// `[lfp]` is the immediately-enclosing frame (LFP_OUTER == 0); each extra
    /// step dereferences again. Mirrors x86 `get_outer`. `outer >= 1`.
    pub(super) fn a64_get_outer(&mut self, outer: usize, lfp: u32, dst: u32) {
        monoasm_arm64!(&mut self.jit, ldr x(dst), [x(lfp)];);
        for _ in 0..outer.saturating_sub(1) {
            monoasm_arm64!(&mut self.jit, ldr x(dst), [x(dst)];);
        }
    }

    /// `LoadDynVarSpecialized`: rax <- outer-scope local at a pre-resolved
    /// frame-base offset. Mirrors x86
    /// `movq rax, [rbp + (offset - (BP_CFP+CFP_LFP) - 8 - conv(reg))]`. The
    /// effective displacement can be negative, so it is materialized in a
    /// scratch and added to x29 (x9..x15 are reserved lowering temps, never a
    /// GP-mapped register).
    pub(in crate::codegen::jitgen::asmir) fn load_dyn_var_specialized(&mut self, offset: usize, reg: SlotId) {
        let e: i64 = offset as i64 - (BP_CFP + CFP_LFP) as i64 - 8 - conv(reg) as i64;
        let rax = GP::Rax.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x10, (e as u64);
            add x10, x29, x10;
            ldr x(rax), [x10];
        );
    }

    /// `StoreDynVarSpecialized`: outer-scope local <- src, symmetric to
    /// `load_dyn_var_specialized`. `src` maps to x0..x8 / x20..x23, never
    /// the x10 scratch, so there is no clobber.
    pub(in crate::codegen::jitgen::asmir) fn store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
        let e: i64 = offset as i64 - (BP_CFP + CFP_LFP) as i64 - 8 - conv(dst) as i64;
        let s = src.a64().0;
        monoasm_arm64!(&mut self.jit,
            mov x10, (e as u64);
            add x10, x29, x10;
            str x(s), [x10];
        );
    }

    // ---- variable-access primitives (aarch64) -----------------------------
    // gvar/cvar go through a runtime C call; dynvar walks the outer-LFP chain.
    // All bail (`false`) on a live xmm / out-of-range offset (no FP save yet).

    /// rax <- $gvar via runtime::get_global_var(vm, globals, name).
    pub(in crate::codegen::jitgen) fn emit_load_gvar(
        &mut self,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let f = runtime::get_global_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            str x30, [sp, #-16]!;         // save LR across the call
            mov x9, (f);
            blr x9;                       // result in x0 (= rax)
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// $gvar <- src via runtime::set_global_var(vm, globals, name, val).
    pub(in crate::codegen::jitgen) fn emit_store_gvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::set_global_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
        );
        self.a64_frame_load(3, lfp, off); // x3 = val (from slot)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- @@cvar via runtime::get_class_var(vm, globals, name).
    pub(in crate::codegen::jitgen) fn emit_load_cvar(
        &mut self,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let f = runtime::get_class_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;                       // result in x0
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// rax <- dynamic (outer-frame) local. Walk `outer` outer-LFP links
    /// (LFP_OUTER == 0, so `[lfp]` is the next outer frame), then load the slot.
    /// Mirrors x86 `load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_load_dyn_var(&mut self, src: DynVar) -> bool {
        let lfp = GP::R14.a64().0;
        let off = src.reg.0 as u32 * 8 + LFP_SELF as u32;
        let rax = GP::Rax.a64().0;
        self.a64_get_outer(src.outer, lfp, rax);
        self.a64_frame_load(rax, rax, off);
        true
    }

    /// dynamic (outer-frame) local <- src. Symmetric to `emit_load_dyn_var`.
    pub(in crate::codegen::jitgen) fn emit_store_dyn_var(&mut self, dst: DynVar, src: GP) -> bool {
        let lfp = GP::R14.a64().0;
        let off = dst.reg.0 as u32 * 8 + LFP_SELF as u32;
        let s = src.a64().0;
        // Walk to the outer LFP in x9 and form the address in x10 — both pure
        // lowering scratch (x9..x15 never alias a GP value register, which map
        // only to x0..x8 / x20..x23), so `src` is never clobbered whatever
        // register it is.
        self.a64_get_outer(dst.outer, lfp, 9);
        self.a64_frame_store(s, 9, off);
        true
    }

    ///
    /// Emit the generational GC write barrier after a JIT inline store whose
    /// parent object is in `parent` and whose stored child value is in `child`.
    ///
    /// Fast path — a young parent (the common case), an already-remembered
    /// parent, or an immediate child — is one flag-bit test plus an
    /// immediate-tag test, with no call. The rare slow path saves the
    /// caller-saved registers the JIT may have live (the abstract scratch GPs
    /// `x0..x8` and the caller-saved FP regs `d0..d7`), so it is fully
    /// transparent to the surrounding code and needs no liveness information,
    /// then calls `jit_write_barrier`. aarch64 twin of x86
    /// `emit_write_barrier_rdi`. See `doc/gc.md`.
    ///
    pub(in crate::codegen::jitgen) fn emit_write_barrier(&mut self, parent: GP, child: GP) {
        let skip = self.jit.label();
        let p = parent.a64().0;
        let c = child.a64().0;
        monoasm_arm64!(&mut self.jit,
            // barrier armed?  (WB_PENDING = flag bit 6 = old & not remembered)
            ldrb w9, [x(p), #(RVALUE_OFFSET_FLAG as u32)];
            tbz x9, #(6), skip;        // WB_PENDING clear -> skip
            // child immediate?  (heap pointers have the low 3 bits clear)
            mov x9, (0b111);
            and x9, x(c), x9;
            cbnz x9, skip;             // immediate child -> skip
        );
        // Slow path: save the caller-saved regs the JIT may have live, pass the
        // parent in the C-ABI arg0 (x0), and call. `x(p)` is untouched by the
        // saves, so it still holds the parent when read into x0.
        let f = jit_write_barrier as *const () as u64;
        monoasm_arm64!(&mut self.jit,
            sub sp, sp, #(144);
            str x0, [sp, #(0)];
            str x1, [sp, #(8)];
            str x2, [sp, #(16)];
            str x3, [sp, #(24)];
            str x4, [sp, #(32)];
            str x5, [sp, #(40)];
            str x6, [sp, #(48)];
            str x7, [sp, #(56)];
            str x8, [sp, #(64)];
            str d0, [sp, #(72)];
            str d1, [sp, #(80)];
            str d2, [sp, #(88)];
            str d3, [sp, #(96)];
            str d4, [sp, #(104)];
            str d5, [sp, #(112)];
            str d6, [sp, #(120)];
            str d7, [sp, #(128)];
            str x30, [sp, #(136)];
            mov x0, x(p);              // parent -> arg0
            mov x9, (f);
            blr x9;
            ldr x30, [sp, #(136)];
            ldr d7, [sp, #(128)];
            ldr d6, [sp, #(120)];
            ldr d5, [sp, #(112)];
            ldr d4, [sp, #(104)];
            ldr d3, [sp, #(96)];
            ldr d2, [sp, #(88)];
            ldr d1, [sp, #(80)];
            ldr d0, [sp, #(72)];
            ldr x8, [sp, #(64)];
            ldr x7, [sp, #(56)];
            ldr x6, [sp, #(48)];
            ldr x5, [sp, #(40)];
            ldr x4, [sp, #(32)];
            ldr x3, [sp, #(24)];
            ldr x2, [sp, #(16)];
            ldr x1, [sp, #(8)];
            ldr x0, [sp, #(0)];
            add sp, sp, #(144);
        );
        self.jit.bind_label(skip);
    }

    /// Store `src` into a heap-spilled instance variable of the object in rdi
    /// (x4) — the non-self twin of emit_store_self_ivar_heap. The var-table may
    /// be too small (None / capa 0 / len <= idx), so the fast inline store is
    /// guarded by a bounds check that falls through to a cold
    /// `set_ivar(obj, ivarid, src)` runtime call (which grows the table). The
    /// live FP pool is saved around that call. aarch64 lays the cold path inline
    /// (no separate page). Bails on an out-of-range field offset.
    pub(in crate::codegen::jitgen) fn emit_store_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using_fpr: UsingFpr,
    ) -> bool {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let off = idx * 8;
        let rdi = GP::Rdi.a64().0; // object (&RValue)
        let s = src.a64().0;
        let generic = self.jit.label();
        let exit = self.jit.label();
        // var_table bounds check (None / capa 0 / len <= idx -> grow via runtime).
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];
            cbz x9, generic;
            ldr x10, [x9, #(MONOVEC_CAPA as u32)];
            cbz x10, generic;
            ldr x10, [x9, #(MONOVEC_LEN as u32)];
            cmp x10, #(idx);
        );
        self.jit.bcond_label(monoasm::Cond::Le, &generic); // len <= idx -> grow
        // fast path: write straight into the table slot.
        monoasm_arm64!(&mut self.jit, ldr x9, [x9, #(MONOVEC_PTR as u32)];);
        self.a64_field_store(s, 9, off);
        // Write barrier (rdi still holds the parent &RValue). The cold path
        // below goes through `set_ivar`, which already barriers, so it jumps
        // straight to `exit`.
        self.emit_write_barrier(GP::Rdi, src);
        monoasm_arm64!(&mut self.jit, b exit;);
        // cold path: set_ivar(obj, ivarid, src), preserving the FP pool. src (s)
        // and rdi survive emit_fpr_save (it only touches d-regs / sp) and are
        // read into the C-arg regs just before the call.
        let f = set_ivar as *const () as u64;
        monoasm_arm64!(&mut self.jit, generic:);
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x(rdi);            // base: &mut RValue
            mov x1, (ivar as u64);     // id: IvarId
            mov x2, x(s);              // val
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        monoasm_arm64!(&mut self.jit, exit:);
        true
    }

    /// Load a heap-spilled instance variable into the accumulator (x23). Unless
    /// loading from self, bounds-check the var-table (None / capa 0 / len <= idx
    /// -> nil); an unset (zero) slot also reads nil. Bails on an out-of-range
    /// field offset. `x9` is the scratch for the table/data pointer chain.
    pub(in crate::codegen::jitgen) fn emit_load_ivar_heap(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        self_: bool,
        dst: GP,
    ) -> bool {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let off = idx * 8;
        let rdi = GP::Rdi.a64().0;
        let dst = dst.a64().0;
        let nil = self.jit.label();
        let exit = self.jit.label();
        monoasm_arm64!(&mut self.jit,
            ldr x9, [x(rdi), #(RVALUE_OFFSET_VAR as u32)];   // var_table
        );
        if !self_ {
            monoasm_arm64!(&mut self.jit,
                cbz x9, nil;                                 // None -> nil
                ldr x10, [x9, #(MONOVEC_CAPA as u32)];
                cbz x10, nil;                                // capa 0 -> nil
                ldr x10, [x9, #(MONOVEC_LEN as u32)];
                cmp x10, #(idx);
            );
            self.jit.bcond_label(monoasm::Cond::Le, &nil);   // len <= idx -> nil
        }
        monoasm_arm64!(&mut self.jit, ldr x9, [x9, #(MONOVEC_PTR as u32)];); // data ptr
        self.a64_field_load(dst, 9, off);                    // value
        monoasm_arm64!(&mut self.jit,
            cbnz x(dst), exit;                               // set -> exit
        nil:
            mov x(dst), (NIL_VALUE);
        exit:
        );
        true
    }

    /// Alias a global var via runtime::alias_global_var(globals=x20, new, old).
    /// Bails when an xmm pool register is live.
    pub(in crate::codegen::jitgen) fn emit_alias_gvar(&mut self, new: IdentId, old: IdentId, using_fpr: UsingFpr) -> bool {
        let f = runtime::alias_global_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x20;                 // globals
            mov x1, (new.get() as u64);  // new IdentId
            mov x2, (old.get() as u64);  // old IdentId
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// Check class variable existence via runtime::check_class_var(vm, globals,
    /// name); the looked-up Value lands in x0. Bails when an xmm pool register
    /// is live (no xmm save around the C call yet).
    pub(in crate::codegen::jitgen) fn emit_check_cvar(
        &mut self,
        name: IdentId,
        using_fpr: UsingFpr,
    ) -> bool {
        let f = runtime::check_class_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }

    /// @@cvar <- src via runtime::set_class_var(vm, globals, name, val). The
    /// Option<Value> result (None == error) is checked by a following
    /// HandleError. Bails when an xmm pool register is live or the slot offset
    /// exceeds the 12-bit scaled load immediate.
    pub(in crate::codegen::jitgen) fn emit_store_cvar(
        &mut self,
        name: IdentId,
        src: SlotId,
        using_fpr: UsingFpr,
    ) -> bool {
        let lfp = GP::R14.a64().0; // x22
        let off = src.0 as u32 * 8 + LFP_SELF as u32;
        let f = runtime::set_class_var as *const () as u64;
        self.emit_fpr_save(using_fpr, false);
        monoasm_arm64!(&mut self.jit,
            mov x0, x19;                  // vm (Executor)
            mov x1, x20;                  // globals
            mov x2, (name.get() as u64); // name (IdentId)
        );
        self.a64_frame_load(3, lfp, off); // x3 = val (from slot)
        monoasm_arm64!(&mut self.jit,
            str x30, [sp, #-16]!;
            mov x9, (f);
            blr x9;
            ldr x30, [sp], #16;
        );
        self.emit_fpr_restore(using_fpr, false);
        true
    }
}
