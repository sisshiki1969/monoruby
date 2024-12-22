use super::*;

impl Codegen {
    ///
    /// Load instance var *ivarid* of the object *rdi* into register *rax*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rsi
    ///
    pub(super) fn load_ivar(&mut self, ivarid: IvarId, is_object_ty: bool, min_len: usize) {
        if is_object_ty && ivarid.is_inline() {
            self.load_ivar_inline(ivarid);
        } else {
            self.load_ivar_heap(ivarid, is_object_ty, min_len);
        }
    }

    ///
    /// Load ivar embedded to RValue. (only for object type)
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi
    ///
    fn load_ivar_inline(&mut self, ivarid: IvarId) {
        monoasm! {&mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_KIND as i32 + (ivarid.get() as i32) * 8)];
            // We must check whether the ivar slot is None.
            movq rdi, (NIL_VALUE);
            testq rax, rax;
            cmoveqq rax, rdi;
        }
    }

    /*///
    /// Load instance var *ivarid* of the object *rdi* into register *rax*.
    ///
    /// ### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rsi, rdx
    ///
    pub(super) fn load_ivar_generic(&mut self, ivarid: IvarId) {
        if ivarid.is_inline() {
            let exit = self.jit.label();
            let not_object = self.jit.label();
            monoasm! { &mut self.jit,
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  not_object;
            }
            self.load_ivar_inline(ivarid);
            monoasm! { &mut self.jit,
            exit:
            }
            self.jit.select_page(1);
            monoasm! { &mut self.jit,
            not_object:
            }
            self.load_ivar_heap(ivarid, false);
            monoasm! { &mut self.jit,
                jmp  exit;
            }
            self.jit.select_page(0);
        } else {
            self.load_ivar_heap_generic(ivarid);
        }
    }*/

    /*///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rsi, rdx
    ///
    fn load_ivar_heap_generic(&mut self, ivar_id: IvarId) {
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movl rsi, (ivar_id.get());
            xorq rax, rax;
            movl rdx, (OBJECT_INLINE_IVAR);
            // we don't know ty of the receiver in a compile time.
            cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
            cmoveqq rax, rdx;
            subl rsi, rax;
            movq rax, (NIL_VALUE);
            // ensure var_table is not None
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rdx, rdx;
            jz   exit;
            // ensure capa is not 0
            cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
            jz   exit;
            cmpq [rdx + (MONOVEC_LEN)], rsi;
            movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
            // rax = if len > idx { rdi[idx] } else { nil }
            cmovgtq rax, [rdi + rsi * 8];
        exit:
        }
    }*/

    ///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rsi, rdx
    ///
    fn load_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool, min_len: usize) {
        let exit = self.jit.label();
        let ivar = ivarid.get() as i32;
        let len_flag = min_len > (ivarid.get() as usize);
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as i32
        } else {
            ivar
        };
        if len_flag {
            monoasm! { &mut self.jit,
                movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
                movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
                movq rax, [rdi + (idx * 8)];
            exit:
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (NIL_VALUE);
                movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
            }
            if min_len == 0 {
                monoasm! { &mut self.jit,
                    // ensure var_table is not None
                    testq rdx, rdx;
                    jz   exit;
                    // ensure capa is not 0
                    cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
                    jz   exit;
                }
            }
            monoasm! { &mut self.jit,
                cmpq [rdx + (MONOVEC_LEN)], (idx);  // len
                movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
                // rax = if len > idx { rdi[idx] } else { nil }
                cmovgtq rax, [rdi + (idx * 8)];
            exit:
            }
        }
    }
}

impl Codegen {
    ///
    /// Store the object *rax* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rax: Value
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    pub(super) fn store_ivar(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        min_len: usize,
        using_xmm: UsingXmm,
    ) {
        if is_object_ty && ivarid.is_inline() {
            self.store_ivar_inline(ivarid);
        } else {
            self.store_ivar_heap(ivarid, is_object_ty, min_len, using_xmm);
        }
    }

    ///
    /// Store ivar embedded to RValue. (only for object type)
    ///
    /// #### in
    /// - rax: Value
    /// - rdi: &RValue
    ///
    fn store_ivar_inline(&mut self, ivarid: IvarId) {
        monoasm!( &mut self.jit,
            movq [rdi + (RVALUE_OFFSET_KIND as i32 + (ivarid.get() as i32) * 8)], rax;
        );
    }

    /*///
    /// Store the object *rax* in an instance var *ivarid* of the object *rdi*.
    ///
    /// ### in
    /// - rdi: receiver: Value
    /// - rax: value: Value
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    pub(super) fn store_ivar_generic(&mut self, using_xmm: UsingXmm, ivarid: IvarId) {
        let exit = self.jit.label();
        let generic = self.jit.label();
        if ivarid.is_inline() {
            let idx = ivarid.get() as i32;
            let no_inline = self.jit.label();
            monoasm! { &mut self.jit,
                // we don't know ty of the receiver at a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  no_inline;
            }
            self.store_ivar_inline(ivarid);
            monoasm! { &mut self.jit,
            exit:
            }
            self.jit.select_page(1);
            monoasm! { &mut self.jit,
            no_inline:
            }
            self.store_ivar_heap_sub(idx, generic);
            monoasm! { &mut self.jit,
                jmp  exit;
            }
            self.jit.select_page(0);
        } else {
            monoasm! { &mut self.jit,
                movl rsi, (ivarid.get());
                xorq rcx, rcx;
                movl rdx, (OBJECT_INLINE_IVAR);
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                cmoveqq rcx, rdx;
                subl rsi, rcx;
                // ensure var_table is not None
                movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
                testq rdx, rdx;
                jz   generic;
                // ensure capa is not 0
                cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
                jz   generic;
                // ensure len > idx
                cmpq [rdx + (MONOVEC_LEN)], rsi;
                jle  generic;
                movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
                movq [rdi + rsi * 8], rax;
            exit:
            }
        }
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        generic:
            movl rsi, (ivarid.get());
            movq rdx, rax;
        );
        self.set_ivar(using_xmm);
        monoasm!(&mut self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }*/

    ///
    /// Store ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - rax: src: Value
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    fn store_ivar_heap(
        &mut self,
        ivarid: IvarId,
        is_object_ty: bool,
        min_len: usize,
        using: UsingXmm,
    ) {
        let exit = self.jit.label();
        let generic = self.jit.label();
        let ivar = ivarid.get() as i32;
        let len_flag = min_len > (ivarid.get() as usize);
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as i32
        } else {
            ivar
        };
        self.store_ivar_heap_sub(idx, generic, len_flag, min_len > 0);
        monoasm! { &mut self.jit,
        exit:
        }

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        generic:
            movl rsi, (ivar);
            movq rdx, rax;
        );
        self.set_ivar(using);
        monoasm!( &mut self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    fn store_ivar_heap_sub(&mut self, idx: i32, generic: DestLabel, len_flag: bool, nonzero: bool) {
        monoasm! { &mut self.jit,
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
        }
        if !len_flag {
            monoasm! { &mut self.jit,
                // check var_table is not None
                testq rdx, rdx;
                jz   generic;
                // check capa is not 0
                cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
                jz   generic;
            }
        }
        if !len_flag {
            monoasm! { &mut self.jit,
                // check len > idx
                cmpq [rdx + (MONOVEC_LEN)], (idx); // len
                jle  generic;
            }
        }
        monoasm! { &mut self.jit,
            movq rdx, [rdx + (MONOVEC_PTR)]; // ptr
            movq [rdx + (idx * 8)], rax;
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

impl Codegen {
    ///
    /// Set an instance variable.
    ///
    /// #### in
    ///
    /// - rdi: base: &mut RValue
    /// - rsi: IvarId
    /// - rdx: src: Value
    ///
    /// #### destroy
    ///
    /// - caller-save registers
    ///
    pub(in crate::compiler::jitgen::asmir) fn set_ivar(&mut self, using: UsingXmm) {
        self.xmm_save(using);
        monoasm!( &mut self.jit,
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(using);
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
