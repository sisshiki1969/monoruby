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
        let exit = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        monoasm!(self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        if ctx.self_value.class() == cached_class {
            if ctx.self_value.kind() == Some(ObjKind::OBJECT)
                && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32
            {
                monoasm!(self.jit,
                    movq rax, [rdi + (16 + (cached_ivarid.get() as i32) * 8)];
                );
            } else {
                self.get_ivar(cached_ivarid, &xmm_using);
            }
        } else {
            // ctx.self_class != cached_class merely happens, but possible.
            self.xmm_save(&xmm_using);
            monoasm!(self.jit,
                movq rsi, (id.get());  // id: IdentId
                movq rdx, r12; // &mut Globals
                movq rax, (runtime::get_instance_var);
                call rax;
            );
            self.xmm_restore(&xmm_using);
        }
        self.jit.bind_label(exit);
        self.store_rax(ret);
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
        monoasm!(self.jit,
            movq rdi, [r14 - (LBP_SELF)];  // base: Value
        );
        if ctx.self_value.class() == cached_class {
            if ctx.self_value.kind() == Some(ObjKind::OBJECT)
                && cached_ivarid.get() < OBJECT_INLINE_IVAR as u32
            {
                self.load_rax(src);
                monoasm!(self.jit,
                    movq [rdi + (16 + (cached_ivarid.get() as i32) * 8)], rax;
                );
            } else {
                self.set_ivar(src, cached_ivarid, &xmm_using);
            }
        } else {
            self.xmm_save(&xmm_using);
            monoasm!(self.jit,
                movq rsi, rdi;  // base: Value
                movq rdx, (id.get());  // id: IdentId
                movq rcx, [r14 - (conv(src))];   // val: Value
                movq rdi, r12; //&mut Globals
                movq rax, (runtime::set_instance_var);
                call rax;
            );
            self.xmm_restore(&xmm_using);
            self.handle_error(pc);
        }
        self.jit.bind_label(exit);
    }

    pub(super) fn jit_get_index(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
            let deopt = self.gen_side_deopt(pc, ctx);
            monoasm! { self.jit,
                movq rdi, [r14 - (conv(base))]; // base: Value
                movq rsi, [r14 - (conv(idx))]; // idx: Value
            };
            self.guard_class(ARRAY_CLASS, deopt);
            monoasm! { self.jit,
                testq rsi, 0b01;
                jeq deopt;
                sarq rsi, 1;
            };
            self.xmm_save(&xmm_using);
            monoasm! { self.jit,
                movq rax, (runtime::get_array_integer_index);
                call rax;
            };
            self.xmm_restore(&xmm_using);
        } else {
            self.xmm_save(&xmm_using);
            monoasm! { self.jit,
                movq rdi, rbx; // &mut Interp
                movq rsi, r12; // &mut Globals
                movq rdx, [r14 - (conv(base))]; // base: Value
                movq rcx, [r14 - (conv(idx))]; // idx: Value
                movq r8, (pc.get_u64() + 8);
                movq rax, (runtime::get_index);
                call rax;
            };
            self.xmm_restore(&xmm_using);
        }
        self.handle_error(pc);
        self.store_rax(ret);
    }

    pub(super) fn jit_index_assign(
        &mut self,
        ctx: &BBContext,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
            let deopt = self.gen_side_deopt(pc, ctx);
            monoasm! { self.jit,
                movq rdi, [r14 - (conv(base))]; // base: Value
                movq rsi, [r14 - (conv(idx))]; // idx: Value
            };
            self.guard_class(ARRAY_CLASS, deopt);
            monoasm! { self.jit,
                testq rsi, 0b01;
                jeq deopt;
                sarq rsi, 1;
            };
            self.xmm_save(&xmm_using);
            monoasm! { self.jit,
                movq rdx, r12;
                movq rcx, [r14 - (conv(src))];
                movq rax, (runtime::set_array_integer_index);
                call rax;
            };
            self.xmm_restore(&xmm_using);
        } else {
            self.xmm_save(&xmm_using);
            monoasm! { self.jit,
                movq rdx, [r14 - (conv(base))]; // base: Value
                movq rcx, [r14 - (conv(idx))]; // idx: Value
                movq r8, [r14 - (conv(src))];  // src: Value
                movq rdi, rbx; // &mut Interp
                movq rsi, r12; // &mut Globals
                movq r9, (pc.get_u64() + 8);
                movq rax, (runtime::set_index);
                call rax;
            };
            self.xmm_restore(&xmm_using);
        }
        self.handle_error(pc);
    }

    pub(super) fn jit_class_def(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        superclass: SlotId,
        name: IdentId,
        func_id: FuncId,
        is_module: bool,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        let jit_return = self.vm_return;
        if superclass.is_zero() {
            monoasm! { self.jit,
                xorq rcx, rcx;
            }
        } else {
            monoasm! { self.jit,
                movq rcx, [r14 - (conv(superclass))];  // rcx <- superclass: Option<Value>
            }
        }
        if is_module {
            monoasm! { self.jit,
                movl r8, 1; // r8 <- is_module
            }
        } else {
            monoasm! { self.jit,
                xorq r8, r8;
            }
        }
        monoasm! { self.jit,
            movl rdx, (name.get());  // rdx <- name
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (runtime::define_class);
            call rax;  // rax <- self: Value
            testq rax, rax; // rax: Option<Value>
            jeq  jit_return;
            movq r15, rax; // r15 <- self
            movl rsi, (func_id.get());  // rdx <- func_id
            //movq rdi, rbx;  // &mut Interp
            movq rdi, r12;  // &mut Globals
            movq rax, (runtime::get_func_data);
            call rax; // rax <- &FuncData
            //
            //       +-------------+
            // +0x08 |     pc      |
            //       +-------------+
            //  0x00 |   ret reg   | <- rsp
            //       +-------------+
            // -0x08 | return addr |
            //       +-------------+
            // -0x10 |   old rbp   |
            //       +-------------+
            // -0x18 |    meta     |
            //       +-------------+
            // -0x20 |    block    |
            //       +-------------+
            // -0x28 |     %0      |
            //       +-------------+
            // -0x30 | %1(1st arg) | <- rdx
            //       +-------------+
            //       |             |
            //
            movq r8, rax;
            movq rdi, [r8 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            movq [rsp - (16 + LBP_BLOCK)], 0;
            movq [rsp - (16 + LBP_SELF)], r15;
        }
        self.set_method_outer();
        monoasm! {self.jit,
            movq r13 , [r8 + (FUNCDATA_OFFSET_PC)];
            movq rax, [r8 + (FUNCDATA_OFFSET_CODEPTR)];
            xorq rdi, rdi;
        }
        self.call_rax();
        monoasm! {self.jit,
            testq rax, rax;
            jeq jit_return;
        };
        if !ret.is_zero() {
            self.store_rax(ret);
        }
        // pop class context.
        monoasm!(self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rax, (runtime::pop_class_context);
            call rax;
        );
        self.xmm_restore(&xmm_using);
    }
}

impl Codegen {
    fn get_ivar(&mut self, cached_ivarid: IvarId, xmm_using: &[Xmm]) {
        self.xmm_save(xmm_using);
        monoasm!(self.jit,
            movl rsi, (cached_ivarid.get());
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(xmm_using);
    }

    fn set_ivar(&mut self, src: SlotId, cached_ivarid: IvarId, xmm_using: &[Xmm]) {
        self.xmm_save(xmm_using);
        monoasm!(self.jit,
            movl rsi, (cached_ivarid.get());
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
}
