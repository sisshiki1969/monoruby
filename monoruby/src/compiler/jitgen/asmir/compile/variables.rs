use super::*;

impl Codegen {
    pub(super) fn load_ivar(
        &mut self,
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
    ) {
        if is_object_ty && is_self_cached {
            if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movq rax, [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)];
                    // We must check whether the ivar slot is None.
                    movq rdi, (NIL_VALUE);
                    testq rax, rax;
                    cmoveqq rax, rdi;
                );
            } else {
                self.load_ivar_heap(cached_ivarid, is_object_ty);
            }
        } else {
            // ctx.self_class != cached_class merely happens, but possible.
            self.get_instance_var(name, using_xmm)
        }
    }

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
    /// - rdi, rsi
    ///
    fn load_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool) {
        let exit = self.jit.label();
        let ivar = ivarid.get() as i32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as i32
        } else {
            ivar
        };
        monoasm!( &mut self.jit,
            movq rax, (NIL_VALUE);
            movq rsi, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rsi, rsi;
            jz   exit;
            movq rdi, [rsi + (MONOVEC_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rsi + (MONOVEC_LEN)]; // len
            cmpq rdi, (idx);
            movq rdi, [rsi + (MONOVEC_PTR)]; // ptr
            cmovgtq rax, [rdi + (idx * 8)];
        exit:
        );
    }

    fn get_instance_var(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rsi, (name.get());  // id: IdentId
            movq rdx, r12; // &mut Globals
            movq rax, (variables::get_instance_var);
            call rax;
        );
        self.xmm_restore(using_xmm);
    }
}

impl Codegen {
    pub(super) fn store_ivar(
        &mut self,
        name: IdentId,
        cached_ivarid: IvarId,
        is_object_ty: bool,
        is_self_cached: bool,
        using_xmm: UsingXmm,
        error: DestLabel,
    ) {
        let exit = self.jit.label();
        if is_self_cached {
            if is_object_ty && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movq [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)], rax;
                );
            } else {
                self.store_ivar_heap(cached_ivarid, is_object_ty, using_xmm);
            }
        } else {
            self.set_instance_ivar(name, using_xmm);
            self.handle_error(error);
        }
        self.jit.bind_label(exit);
    }

    ///
    /// Store ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - rax: src: Value
    ///
    /// #### destroy
    /// - rdi, rsi
    ///
    fn store_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool, using: UsingXmm) {
        let exit = self.jit.label();
        let generic = self.jit.label();
        let ivar = ivarid.get() as i32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as i32
        } else {
            ivar
        };
        monoasm! { &mut self.jit,
            movq rsi, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rsi, rsi;
            jz   generic;
            movq rdi, [rsi + (MONOVEC_CAPA)]; // capa
            testq rdi, rdi;
            jz   generic;
            movq rdi, [rsi + (MONOVEC_LEN)]; // len
            cmpq rdi, (idx);
            jle  generic;
        }
        monoasm! { &mut self.jit,
            movq rdi, [rsi + (MONOVEC_PTR)]; // ptr
            movq [rdi + (idx * 8)], rax;
        exit:
        }

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        generic:
            movq rdi, [r14 - (LFP_SELF)];
            movl rsi, (ivar);
            movq rdx, rax;
        );
        self.set_ivar(using);
        monoasm!( &mut self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    fn set_instance_ivar(&mut self, name: IdentId, using_xmm: UsingXmm) {
        self.xmm_save(using_xmm);
        monoasm!( &mut self.jit,
            movq rdx, rdi;  // base: Value
            movq rcx, (name.get());  // id: IdentId
            movq r8, rax;   // val: Value
            movq rdi, rbx; //&mut Executor
            movq rsi, r12; //&mut Globals
            movq rax, (variables::set_instance_var);
            call rax;
        );
        self.xmm_restore(using_xmm);
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

///
/// Get instance variable.
///
/// rax <= the value of instance variable. <Value>
///
extern "C" fn get_instance_var(base: Value, name: IdentId, globals: &mut Globals) -> Value {
    globals.get_ivar(base, name).unwrap_or_default()
}

///
/// Set instance variable.
///
/// rax <= Some(*val*). If error("can't modify frozen object") occured, returns None.
///
extern "C" fn set_instance_var(
    vm: &mut Executor,
    globals: &mut Globals,
    base: Value,
    name: IdentId,
    val: Value,
) -> Option<Value> {
    if let Err(err) = globals.set_ivar(base, name, val) {
        vm.set_error(err);
        return None;
    };
    Some(val)
}

#[cfg(test)]
mod test {
    use super::tests::*;

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
