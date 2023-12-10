use super::*;

impl Codegen {
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
    pub(super) fn load_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool) {
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
    pub(super) fn store_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool, using: UsingXmm) {
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

    pub(in crate::compiler::jitgen) fn jit_load_gvar(
        &mut self,
        store: &Store,
        ctx: &mut BBContext,
        name: IdentId,
        dst: SlotId,
    ) {
        let mut ir = AsmIr::new();
        ctx.release(dst);
        ir.load_gvar(ctx, name);
        ir.rax2acc(ctx, dst);
        self.gen_code(store, ir);
    }

    pub(in crate::compiler::jitgen) fn jit_store_gvar(
        &mut self,
        store: &Store,
        ctx: &mut BBContext,
        name: IdentId,
        src: SlotId,
    ) {
        let mut ir = AsmIr::new();
        ctx.fetch_slots(&mut ir, &[src]);
        ir.store_gvar(ctx, name, src);
        self.gen_code(store, ir);
    }

    pub(in crate::compiler::jitgen) fn jit_load_svar(
        &mut self,
        ctx: &mut BBContext,
        id: u32,
        dst: SlotId,
    ) {
        ctx.release(dst);
        let using_xmm = ctx.get_using_xmm();
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movl rsi, r12;
            movl rdx, (id);
            movq rax, (runtime::get_special_var);
            call rax;
        };
        self.xmm_restore(using_xmm);
        self.save_rax_to_acc(ctx, dst);
    }
}

impl BBContext {
    pub(in crate::compiler::jitgen) fn jit_load_ivar(
        &mut self,
        ir: &mut AsmIr,
        name: IdentId,
        dst: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.release(dst);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = self.get_using_xmm();
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = self.self_value.class() == cached_class;
        ir.inst.push(AsmInst::LoadIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
        });
        ir.rax2acc(self, dst);
    }

    pub(in crate::compiler::jitgen) fn jit_store_ivar(
        &mut self,
        ir: &mut AsmIr,
        name: IdentId,
        src: SlotId,
        pc: BcPc,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        self.fetch_to_reg(ir, src, GP::Rax);
        ir.stack2reg(SlotId(0), GP::Rdi);
        let using_xmm = self.get_using_xmm();
        let error = ir.new_error(pc, self.get_write_back());
        let is_object_ty = self.self_value.ty() == Some(ObjKind::OBJECT);
        let is_self_cached = self.self_value.class() == cached_class;
        ir.inst.push(AsmInst::StoreIVar {
            name,
            cached_ivarid,
            is_object_ty,
            is_self_cached,
            using_xmm,
            error,
        });
    }
}

///
/// Get instance variable.
///
/// rax <= the value of instance variable. <Value>
///
pub(super) extern "C" fn get_instance_var(
    base: Value,
    name: IdentId,
    globals: &mut Globals,
) -> Value {
    globals.get_ivar(base, name).unwrap_or_default()
}

///
/// Set instance variable.
///
/// rax <= Some(*val*). If error("can't modify frozen object") occured, returns None.
///
pub(super) extern "C" fn set_instance_var(
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
