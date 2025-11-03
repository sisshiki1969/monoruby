use super::*;

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
    /// Store *src* in ivar embedded to RValue `rdi`. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    pub(super) fn store_ivar_object_inline(&mut self, src: GP, ivarid: IvarId) {
        monoasm!( &mut self.jit,
            movq [rdi + (RVALUE_OFFSET_KIND as i32 + (ivarid.get() as i32) * 8)], R(src as _);
        );
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

extern "C" fn set_ivar(base: &mut RValue, id: IvarId, val: Value) {
    base.set_ivar_by_ivarid(id, val)
}

impl Codegen {
    pub(super) fn load_dyn_var(&mut self, src: DynVar) {
        self.get_outer(src.outer);
        let offset = conv(src.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq rax, [rax - (offset)];
        );
    }

    pub(super) fn store_dyn_var(&mut self, dst: DynVar, src: GP) {
        self.get_outer(dst.outer);
        let offset = conv(dst.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq [rax - (offset)], R(src as _);
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
            movq rcx, [r14 - (conv(src))];
            movq rax, (runtime::set_class_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn load_gvar(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, r12;
            movl rsi, (name.get());
            movq rax, (runtime::get_global_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn store_gvar(&mut self, name: IdentId, src: SlotId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, r12;
            movl rsi, (name.get());
            movq rdx, [r14 - (conv(src))];
            movq rax, (runtime::set_global_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
    }

    pub(super) fn load_svar(&mut self, id: u32, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (id);
            movq rax, (runtime::get_special_var);
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
