use super::*;
use crate::codegen::jitgen::asmir::compile_shared::set_ivar;

impl Codegen {
    ///
    /// Load ivar embedded to RValue. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - r15: Value
    ///
    /// #### destroy
    /// - rdi
    ///
    pub(super) fn load_ivar_inline(&mut self, ivarid: IvarId) {
        let exit = self.jit.label();
        monoasm! {&mut self.jit,
            movq r15, [rdi + (RVALUE_OFFSET_KIND as i32 + (ivarid.get() as i32) * 8)];
            // We must check whether the ivar slot is None.
            testq r15, r15;
            jne  exit;
            movq r15, (NIL_VALUE);
        exit:
        }
    }

    ///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - r15: Value
    ///
    /// #### destroy
    /// - rdi, rsi, rdx
    ///
    pub(super) fn load_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool, self_: bool) {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let exit = self.jit.label();
        let nil = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
        }
        if !self_ {
            self.check_len(idx, &nil);
        }
        monoasm! { &mut self.jit,
            movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
            movq r15, [rdi + (idx as i32 * 8)];
            testq r15, r15;
            jne  exit;
        nil:
            movq r15, (NIL_VALUE);
        exit:
        }
    }
}

impl Codegen {
    ///
    /// Guard that the object in *rdi* is not frozen.
    ///
    /// Check bit 1 of the flag field at RVALUE_OFFSET_FLAG.
    /// If the object is frozen, call the runtime to set a FrozenError
    /// and jump to the error side exit.
    ///
    /// #### in
    /// - rdi: &RValue (also the Value, since lower bits are 0 for heap objects)
    ///
    /// #### destroy
    /// - rsi (only on frozen path)
    ///
    pub(super) fn guard_frozen(&mut self, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [rdi + (RVALUE_OFFSET_FLAG as i32)], (0b10);
            jnz  deopt;
        }
    }

    ///
    /// Store *src* in ivar embedded to RValue `rdi`. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    pub(super) fn store_ivar_object_inline(&mut self, src: GP, ivarid: IvarId) {
        monoasm!( &mut self.jit,
            movq [rdi + (RVALUE_OFFSET_KIND as i32 + (ivarid.get() as i32) * 8)], R(src as _);
        );
        self.emit_write_barrier_rdi(src);
    }

    ///
    /// Load slot `slot_index` of a `Struct` instance whose slot
    /// vector spilled to the **heap** (the class has more than
    /// `STRUCT_INLINE_SLOTS` members). Two movs.
    ///
    /// #### in
    /// - rdi: &RValue
    /// #### out
    /// - r15: Value
    /// #### destroy
    /// - rdi
    pub(super) fn load_struct_slot_heap(&mut self, slot_index: u16) {
        monoasm! {&mut self.jit,
            movq rdi, [rdi + (RVALUE_OFFSET_HEAP_PTR as i32)];
            movq r15, [rdi + ((slot_index as i32) * 8)];
        }
    }

    ///
    /// Store *src* into inline slot `slot_index` of `rdi`. Single mov.
    /// Caller must have emitted `GuardFrozen` already.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - src: Value to store
    /// #### out
    /// - rax: src (return value of the writer)
    pub(super) fn store_struct_slot_inline(&mut self, src: GP, slot_index: u16) {
        monoasm! {&mut self.jit,
            movq [rdi + ((slot_index as i32) * 8 + RVALUE_OFFSET_INLINE as i32)], R(src as _);
        }
        // Write barrier: rdi = the struct (parent), src = stored value.
        self.emit_write_barrier_rdi(src);
        monoasm! {&mut self.jit,
            movq rax, R(src as _);
        }
    }

    ///
    /// Store *src* into heap slot `slot_index` of `rdi`. Two movs.
    /// Caller must have emitted `GuardFrozen` already.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - src: Value to store
    /// #### out
    /// - rax: src
    /// #### destroy
    /// - rdi
    pub(super) fn store_struct_slot_heap(&mut self, src: GP, slot_index: u16) {
        // Write barrier before `rdi` is repointed at the heap buffer:
        // rdi = the struct (parent), src = stored value.
        self.emit_write_barrier_rdi(src);
        monoasm! {&mut self.jit,
            movq rdi, [rdi + (RVALUE_OFFSET_HEAP_PTR as i32)];
            movq [rdi + ((slot_index as i32) * 8)], R(src as _);
            movq rax, R(src as _);
        }
    }

    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    pub(super) fn store_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using: UsingXmm,
    ) {
        self.store_ivar_heap_inner(src, ivarid, is_object_ty, Some(using));
    }

    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - rdx
    ///
    pub(super) fn store_self_ivar_heap(&mut self, src: GP, ivarid: IvarId, is_object_ty: bool) {
        self.store_ivar_heap_inner(src, ivarid, is_object_ty, None);
    }

    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy (if using.is_some())
    /// - caller-save registers
    ///
    /// #### destroy (if using.is_none())
    /// - rdx
    fn store_ivar_heap_inner(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using: Option<UsingXmm>,
    ) {
        let exit = self.jit.label();
        let generic = if let Some(using) = using {
            Some((using, self.jit.label()))
        } else {
            None
        };
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        monoasm! { &mut self.jit,
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
        }
        if let Some((_, generic)) = &generic {
            self.check_len(idx, generic);
        }
        monoasm! { &mut self.jit,
            movq rdx, [rdx + (MONOVEC_PTR)]; // ptr
            movq [rdx + (idx as i32 * 8)], R(src as _);
        }
        // Fast-path store: emit the write barrier (rdi still holds the
        // parent &RValue). The generic path below goes through `set_ivar`,
        // which already barriers, so it jumps straight to `exit`.
        self.emit_write_barrier_rdi(src);
        monoasm! { &mut self.jit,
        exit:
        }

        if let Some((using, generic)) = generic {
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            generic:
                movl rsi, (ivar);
                movq rdx, R(src as _);
            );
            self.xmm_save(using);
            monoasm!( &mut self.jit,
                movq rax, (set_ivar);
                call rax;
            );
            self.xmm_restore(using);
            monoasm!( &mut self.jit,
                jmp  exit;
            );
            self.jit.select_page(0);
        }
    }

    ///
    /// Check whether the length of `ivar_table` is greater than `idx`.
    ///
    /// #### in
    /// - rdx: ivar_table
    ///
    fn check_len(&mut self, idx: u32, fail: &DestLabel) {
        monoasm! { &mut self.jit,
            // check var_table is not None
            testq rdx, rdx;
            jz   fail;
            // check capa is not 0
            cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
            jz   fail;
            // check len > idx
            cmpq [rdx + (MONOVEC_LEN)], (idx); // len
            jle  fail;
        }
    }
}

///
/// Generational GC write barrier slow path, called from JIT inline stores.
///
/// The inline fast path has already verified that `parent` is old, not yet
/// remembered, and that the stored child is a heap object; this records
/// `parent` in the remembered set. See `doc/generational_gc_plan.md`.
///
extern "C" fn jit_write_barrier(parent: *mut RValue) {
    // SAFETY: `parent` is the live `&RValue` the store wrote into. The
    // inline fast path has already checked it is `wb_pending` with a heap
    // child, so `write_barrier_bulk` records it in the remembered set.
    unsafe { (*parent).write_barrier_bulk() };
}

impl Codegen {
    ///
    /// Emit the generational GC write barrier after a JIT inline store
    /// whose parent object is in `rdi` and whose stored child value is in
    /// `child`.
    ///
    /// Fast path — a young parent (the common case), an already-remembered
    /// parent, or an immediate child — is three flag/immediate tests with
    /// no scratch register and no allocator access. The rare slow path
    /// saves *all* caller-saved registers (so it is fully transparent to
    /// the surrounding code, needing no liveness information) and calls
    /// `jit_write_barrier`. See `doc/generational_gc_plan.md`.
    ///
    pub(super) fn emit_write_barrier_rdi(&mut self, child: GP) {
        let skip = self.jit.label();
        monoasm! { &mut self.jit,
            // barrier armed?  (WB_PENDING = flag bit 6 = old & not remembered)
            // Young objects and already-remembered old objects both have it
            // clear, so the common case skips after this single test.
            testb [rdi + (RVALUE_OFFSET_FLAG as i32)], 0x40;
            jz   skip;
            // child immediate?  (heap pointers have the low 3 bits clear)
            testq R(child as _), 0b111;
            jnz  skip;
        }
        // Slow path: rdi already holds the parent (the call's argument).
        self.save_registers();
        monoasm! { &mut self.jit,
            movq [rsp + 176], rax;          // save rax (save_registers skips it)
            movq rax, (jit_write_barrier);
            call rax;
            movq rax, [rsp + 176];
        }
        self.restore_registers();
        monoasm! { &mut self.jit,
        skip:
        }
    }
}

impl Codegen {
    pub(super) fn load_dyn_var(&mut self, src: DynVar) {
        self.get_outer(src.outer);
        let offset = conv(src.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq rax, [rax - (offset)];
        );
    }

    pub(in crate::codegen::jitgen) fn load_dyn_var_specialized(&mut self, offset: usize, reg: SlotId) {
        monoasm!( &mut self.jit,
            movq rax, [rbp + ((offset - (BP_CFP + CFP_LFP) as usize - 8 - conv(reg) as usize))];
        );
    }

    pub(super) fn store_dyn_var(&mut self, dst: DynVar, src: GP) {
        self.get_outer(dst.outer);
        let offset = conv(dst.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq [rax - (offset)], R(src as _);
        );
    }

    pub(in crate::codegen::jitgen) fn store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
        monoasm!( &mut self.jit,
            movq [rbp + ((offset - (BP_CFP + CFP_LFP) as usize - 8 - conv(dst) as usize))], R(src as _);
        );
    }

    fn get_outer(&mut self, outer: usize) {
        monoasm!( &mut self.jit,
            movq rax, [r14];
        );
        for _ in 0..outer - 1 {
            monoasm!( &mut self.jit,
                movq rax, [rax];
            );
        }
    }
}

impl Codegen {
    pub(super) fn load_cvar(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::get_class_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn check_cvar(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::check_class_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn store_cvar(&mut self, name: IdentId, src: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::set_class_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn load_gvar(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::get_global_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn store_gvar(&mut self, name: IdentId, src: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::set_global_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn ivar_in_different_class() {
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.get, c.get]
        "##,
            r##"
            class S
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                def get
                    [@a, @b, @c, @d, @e, @f, @g, @h]
                end
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
            end
            
            "##,
        );
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.get, c.get]
        "##,
            r##"
            class S < Array
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                def get
                    [@a, @b, @c, @d, @e, @f, @g, @h]
                end
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
            end
            
            "##,
        );
    }
}
