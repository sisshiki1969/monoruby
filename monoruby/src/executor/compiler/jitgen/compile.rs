use super::*;

impl Codegen {
    pub(super) fn jit_load_ivar(
        &mut self,
        ctx: &BBContext,
        id: IdentId,
        ret: SlotId,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        let xmm_using = ctx.get_xmm_using();
        if ctx.self_value.class() == cached_class {
            if ctx.self_value.ty() == Some(ObjKind::OBJECT) {
                if cached_ivarid.get() < OBJECT_INLINE_IVAR as u32 {
                    monoasm!( &mut self.jit,
                        movq rax, [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)];
                    );
                } else {
                    self.load_ivar_heap(cached_ivarid);
                }
                // We must check whether the ivar slot is None.
                monoasm!( &mut self.jit,
                    movq rdi, (NIL_VALUE);
                    testq rax, rax;
                    cmoveqq rax, rdi;
                );
            } else {
                monoasm!( &mut self.jit,
                    movl rsi, (cached_ivarid.get());
                );
                self.get_ivar(&xmm_using);
            }
        } else {
            // ctx.self_class != cached_class merely happens, but possible.
            self.xmm_save(&xmm_using);
            monoasm!( &mut self.jit,
                movq rsi, (id.get());  // id: IdentId
                movq rdx, r12; // &mut Globals
                movq rax, (runtime::get_instance_var);
                call rax;
            );
            self.xmm_restore(&xmm_using);
        }
        self.store_rax(ret);
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
    fn load_ivar_heap(&mut self, ivarid: IvarId) {
        let exit = self.jit.label();
        let idx = (ivarid.get() as usize - OBJECT_INLINE_IVAR) as i32;
        monoasm!( &mut self.jit,
            movq rax, (NIL_VALUE);
            movq rsi, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rsi, rsi;
            jz   exit;
            movq rdi, [rsi + (VEC_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rsi + (VEC_LEN)]; // len
            cmpq rdi, (idx);
            movq rdi, [rsi + (VEC_PTR)]; // ptr
            cmovgtq rax, [rdi + (idx * 8)];
        exit:
        );
    }

    pub(super) fn jit_store_ivar(
        &mut self,
        ctx: &BBContext,
        id: IdentId,
        src: SlotId,
        pc: BcPc,
        cached_class: ClassId,
        cached_ivarid: IvarId,
    ) {
        assert!(!cached_class.is_always_frozen());
        let exit = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        monoasm!( &mut self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        if ctx.self_value.class() == cached_class {
            if ctx.self_value.ty() == Some(ObjKind::OBJECT)
                && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32
            {
                self.load_rax(src);
                monoasm!( &mut self.jit,
                    movq [rdi + (RVALUE_OFFSET_KIND as i32 + (cached_ivarid.get() as i32) * 8)], rax;
                );
            } else {
                monoasm!( &mut self.jit,
                    movl rsi, (cached_ivarid.get());
                );
                self.set_ivar(src, &xmm_using);
            }
        } else {
            self.xmm_save(&xmm_using);
            monoasm!( &mut self.jit,
                movq rdx, rdi;  // base: Value
                movq rcx, (id.get());  // id: IdentId
                movq r8, [r14 - (conv(src))];   // val: Value
                movq rdi, rbx; //&mut Executor
                movq rsi, r12; //&mut Globals
                movq rax, (runtime::set_instance_var);
                call rax;
            );
            self.xmm_restore(&xmm_using);
            self.jit_handle_error(ctx, pc);
        }
        self.jit.bind_label(exit);
    }

    pub(super) fn jit_class_def(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        if superclass.is_zero() {
            monoasm! { &mut self.jit,
                xorq rcx, rcx;
            }
        } else {
            monoasm! { &mut self.jit,
                movq rcx, [r14 - (conv(superclass))];  // rcx <- superclass: Option<Value>
            }
        }
        if is_module {
            monoasm! { &mut self.jit,
                movl r8, 1; // r8 <- is_module
            }
        } else {
            monoasm! { &mut self.jit,
                xorq r8, r8;
            }
        }
        monoasm! { &mut self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value
        };
        self.jit_handle_error(ctx, pc);
        self.jit_class_def_sub(ctx, func_id, ret, pc);
        self.xmm_restore(&xmm_using);
    }

    pub(super) fn jit_singleton_class_def(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        base: SlotId,
        func_id: FuncId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))];  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_singleton_class);
            call rax;  // rax <- self: Value
        };
        self.jit_handle_error(ctx, pc);
        self.jit_class_def_sub(ctx, func_id, ret, pc);
        self.xmm_restore(&xmm_using);
    }

    fn jit_class_def_sub(&mut self, ctx: &BBContext, func_id: FuncId, ret: SlotId, pc: BcPc) {
        monoasm! { &mut self.jit,
            movq r15, rax; // r15 <- self
            movq rcx, rax; // rcx <- self
            movl rdx, (func_id.get());  // rdx <- func_id
            movq rdi, rbx;  // &mut Executor
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::get_classdef_data);
            call rax; // rax <- &FuncData

            movq r8, rax;
            movq rdi, [r8 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_SELF)], r15;
        }
        self.set_method_outer();
        monoasm! { &mut self.jit,
            movq r13 , [r8 + (FUNCDATA_PC)];
            movq rax, [r8 + (FUNCDATA_CODEPTR)];
            xorq rdx, rdx;
        }
        self.call_rax();
        if !ret.is_zero() {
            self.store_rax(ret);
        }
        // pop class context.
        monoasm! { &mut self.jit,
            movq r13, rax;
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::pop_class_context);
            call rax;
            movq rax, r13;
        }
        self.jit_handle_error(ctx, pc);
    }
}

impl Codegen {
    ///
    /// Get an instance variable.
    ///
    /// #### in
    ///
    /// - rdi: &RValue
    ///
    /// - rsi: IvarId
    ///
    /// #### out
    ///
    /// - rax: Value
    ///
    pub(super) fn get_ivar(&mut self, xmm_using: &[Xmm]) {
        self.xmm_save(xmm_using);
        monoasm!( &mut self.jit,
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(xmm_using);
    }

    ///
    /// Set an instance variable.
    ///
    /// #### in
    ///
    /// - rdi: &RValue
    ///
    /// - rsi: IvarId
    ///
    /// #### destroy
    ///
    /// - caller-save registers
    ///
    pub(super) fn set_ivar(&mut self, src: SlotId, xmm_using: &[Xmm]) {
        self.xmm_save(xmm_using);
        monoasm!( &mut self.jit,
            movq rdx, [r14 - (conv(src))];   // val: Value
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(xmm_using);
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn raise_method_recv_class() {
        run_test_error(
            r##"
          class A
            def w
              42
            end
          end
          class B
          end
          a = A.new
          res = []
          for i in 0..10
            if i == 8
              a = B.new
            end
            res << a.w
          end
          res
        "##,
        );
    }

    #[test]
    fn deopt_reader_recv_class() {
        run_test(
            r##"
            class A
                attr_accessor :w
            end
            class B
              def w
                100
              end
            end
            a = A.new
            a.w = 42
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              res << a.w
            end
            res
        "##,
        );
    }

    #[test]
    fn deopt_writer_recv_class() {
        run_test(
            r##"
            class A
              attr_accessor :w
            end
            class B
              attr_reader :w
              def w=(v)
                @w = v * 2
              end
            end
            a = A.new
            res = []
            for i in 0..10
              if i == 8
                a = B.new
              end
              a.w = 42
              res << a.w
            end
            res
        "##,
        );
    }

    #[test]
    fn deopt_reader_class_version() {
        run_test(
            r##"
        class A
          attr_accessor :w
        end
        a = A.new
        a.w = 42
        res = []
        for i in 0..10
          if i == 8
            class A
              def w
                99
              end
            end
          end
          res << a.w
        end
        res
        "##,
        );
    }

    #[test]
    fn deopt_writer_class_version() {
        run_test(
            r##"
        class A
          attr_accessor :w
        end
        a = A.new
        res = []
        for i in 0..10
          if i == 8
            class A
              def w=(v)
                @w = v * 2
              end
            end
          end
          a.w = 42
          res << a.w
        end
        res
        "##,
        );
    }

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
    }
}
