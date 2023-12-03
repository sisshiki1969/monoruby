use super::*;

impl Codegen {
    pub(super) fn jit_load_ivar(
        &mut self,
        ctx: &mut BBContext,
        id: IdentId,
        ret: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        ctx.release(ret);
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        let xmm_using = ctx.get_xmm_using();
        let is_object_ty = ctx.self_value.ty() == Some(ObjKind::OBJECT);
        if is_object_ty && ctx.self_value.class() == cached_class {
            if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movq rax, [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)];
                );
                // We must check whether the ivar slot is None.
                monoasm!( &mut self.jit,
                    movq rdi, (NIL_VALUE);
                    testq rax, rax;
                    cmoveqq rax, rdi;
                );
            } else {
                self.load_ivar_heap(cached_ivarid, is_object_ty);
            }
        } else {
            // ctx.self_class != cached_class merely happens, but possible.
            self.xmm_save(xmm_using);
            monoasm!( &mut self.jit,
                movq rsi, (id.get());  // id: IdentId
                movq rdx, r12; // &mut Globals
                movq rax, (get_instance_var);
                call rax;
            );
            self.xmm_restore(xmm_using);
        }
        self.save_rax_to_acc(ctx, ret);
    }

    pub(super) fn jit_store_ivar(
        &mut self,
        ctx: &mut BBContext,
        id: IdentId,
        src: SlotId,
        pc: BcPc,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        let exit = self.jit.label();
        let using = ctx.get_xmm_using();
        self.fetch_to_rax(ctx, src);
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        if ctx.self_value.class() == cached_class {
            let is_object_ty = ctx.self_value.ty() == Some(ObjKind::OBJECT);
            if is_object_ty && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movq [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)], rax;
                );
            } else {
                self.store_ivar_heap(cached_ivarid, is_object_ty, using);
            }
        } else {
            self.xmm_save(using);
            monoasm!( &mut self.jit,
                movq rdx, rdi;  // base: Value
                movq rcx, (id.get());  // id: IdentId
                movq r8, rax;   // val: Value
                movq rdi, rbx; //&mut Executor
                movq rsi, r12; //&mut Globals
                movq rax, (set_instance_var);
                call rax;
            );
            self.xmm_restore(using);
            self.jit_handle_error(ctx, pc);
        }
        self.jit.bind_label(exit);
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
            movq rdi, [rsi + (IVAR_TABLE_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rsi + (IVAR_TABLE_LEN)]; // len
            cmpq rdi, (idx);
            movq rdi, [rsi + (IVAR_TABLE_PTR)]; // ptr
            cmovgtq rax, [rdi + (idx * 8)];
        exit:
        );
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
            movq rdi, [rsi + (IVAR_TABLE_CAPA)]; // capa
            testq rdi, rdi;
            jz   generic;
            movq rdi, [rsi + (IVAR_TABLE_LEN)]; // len
            cmpq rdi, (idx);
            jle  generic;
        }
        monoasm! { &mut self.jit,
            movq rdi, [rsi + (IVAR_TABLE_PTR)]; // ptr
            movq [rdi + (idx * 8)], rax;
        exit:
        }

        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        generic:
            movq rdi, [r14 - (LBP_SELF)];
            movl rsi, (ivar);
            movq rdx, rax;
        );
        self.set_ivar(using);
        monoasm!( &mut self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    pub(super) fn jit_load_gvar(&mut self, ctx: &mut BBContext, name: IdentId, dst: SlotId) {
        ctx.release(dst);
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, r12;
            movl rsi, (name.get());
            movq rax, (runtime::get_global_var);
            call rax;
        };
        self.xmm_restore(xmm_using);
        self.save_rax_to_acc(ctx, dst);
    }

    pub(super) fn jit_store_gvar(&mut self, ctx: &mut BBContext, name: IdentId, val: SlotId) {
        self.fetch_slots(ctx, &[val]);
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, r12;
            movl rsi, (name.get());
            movq rdx, [r14 - (conv(val))];
            movq rax, (runtime::set_global_var);
            call rax;
        };
        self.xmm_restore(xmm_using);
    }

    pub(super) fn jit_load_svar(&mut self, ctx: &mut BBContext, id: u32, dst: SlotId) {
        ctx.release(dst);
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movl rsi, r12;
            movl rdx, (id);
            movq rax, (runtime::get_special_var);
            call rax;
        };
        self.xmm_restore(xmm_using);
        self.save_rax_to_acc(ctx, dst);
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
