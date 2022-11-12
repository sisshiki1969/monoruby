use super::*;

#[derive(Debug)]
struct Cached {
    codeptr: CodePtr,
    meta: Meta,
    class_id: ClassId,
    version: u32,
    pc: BcPc,
}

impl Cached {
    fn new(pc: BcPc, codeptr: CodePtr) -> Self {
        let (class_id, version) = (pc - 1).class_version();
        Cached {
            codeptr,
            meta: (pc + 1).meta(),
            class_id,
            version,
            pc: (pc + 1).pc(),
        }
    }
}

impl Codegen {
    pub(super) fn gen_method_call(
        &mut self,
        fnstore: &FnStore,
        ctx: &mut BBContext,
        mut method_info: MethodInfo,
        pc: BcPc,
    ) {
        let MethodInfo {
            recv, args, len, ..
        } = method_info;
        match (pc - 1).op1() {
            BcOp::MethodCall(ret, name, ..) => {
                ctx.dealloc_xmm(ret);
                ctx.write_back_slot(self, recv);
                ctx.write_back_range(self, args, len);
                self.gen_call(fnstore, ctx, method_info, name, None, ret, pc);
            }
            BcOp::MethodCallBlock(ret, name, ..) => {
                ctx.dealloc_xmm(ret);
                // We must write back all registers since slots may be accessed from block.
                let wb = ctx.get_write_back();
                self.gen_write_back(wb);
                method_info.args = method_info.args + 1;
                self.gen_call(fnstore, ctx, method_info, name, Some(args), ret, pc);
            }
            BcOp::Yield(ret) => {
                ctx.dealloc_xmm(ret);
                ctx.write_back_range(self, args, len);
                self.gen_yield(ctx, method_info, ret, pc);
            }
            _ => unreachable!(),
        }
    }

    fn gen_call(
        &mut self,
        fnstore: &FnStore,
        ctx: &BBContext,
        method_info: MethodInfo,
        name: IdentId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
    ) {
        let MethodInfo {
            callee_codeptr,
            recv,
            ..
        } = method_info;
        if let Some(codeptr) = callee_codeptr {
            let cached = Cached::new(pc, codeptr);
            if recv.is_zero() && ctx.self_class != cached.class_id {
                self.gen_call_not_cached(ctx, method_info, name, block, ret, pc);
            } else {
                self.gen_call_cached(fnstore, ctx, method_info, block, ret, cached, pc);
            }
        } else {
            self.gen_call_not_cached(ctx, method_info, name, block, ret, pc);
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    fn gen_call_cached(
        &mut self,
        fnstore: &FnStore,
        ctx: &BBContext,
        method_info: MethodInfo,
        block: Option<SlotId>,
        ret: SlotId,
        cached: Cached,
        pc: BcPc,
    ) {
        let deopt = self.gen_side_writeback_deopt(pc - 1, ctx);
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(method_info.recv))];
        );
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !method_info.recv.is_zero() {
            self.guard_class(cached.class_id, deopt);
        }
        self.guard_version(cached.version, deopt);
        let func_id = cached.meta.func_id();
        match fnstore[func_id].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, method_info.len);
                if cached.class_id.is_always_frozen() {
                    if !ret.is_zero() {
                        monoasm!(self.jit,
                            movq rax, (NIL_VALUE);
                        );
                        self.store_rax(ret);
                    }
                } else {
                    self.attr_reader(ctx, ivar_name, ret);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, method_info.len);
                self.attr_writer(ctx, ivar_name, ret, method_info.args, pc);
            }
            FuncKind::Builtin { abs_address } => {
                self.native_call(ctx, method_info, func_id, ret, block, abs_address, pc);
            }
            FuncKind::ISeq(_) => {
                self.method_call_cached(ctx, method_info, ret, block, cached, pc);
            }
        };
    }

    ///
    /// generate JIT code for a method call which was not cached.
    ///
    fn gen_call_not_cached(
        &mut self,
        ctx: &BBContext,
        method_info: MethodInfo,
        name: IdentId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
    ) {
        let MethodInfo { recv, len, .. } = method_info;
        // set arguments to a callee stack.
        //
        //       +-------------+
        //  0x00 |             | <- rsp
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
        // -0x30 | %1(1st arg) |
        //       +-------------+
        //       |             |
        //
        // argument registers:
        //   rdi: args len
        //
        let method_resolved = self.jit.label();
        let patch_meta = self.jit.label();
        let patch_adr = self.jit.label();
        let patch_pc = self.jit.label();
        let slow_path = self.jit.label();
        let raise = self.jit.label();
        let cached_class_version = self.jit.const_i32(-1);
        let cached_recv_class = self.jit.const_i32(0);
        let global_class_version = self.class_version;
        let entry_find_method = self.entry_find_method;
        let entry_panic = self.entry_panic;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        // class guard
        // r15 <- recv's class
        if recv.is_zero() {
            // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
            monoasm!(self.jit,
                movl r15, (ctx.self_class.0);
            );
        } else {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(recv))];
                movq rax, (Value::get_class);
                call rax;
                movl r15, rax;  // r15: receiver class_id
            );
        }
        monoasm!(self.jit,
            cmpl r15, [rip + cached_recv_class];
            jne slow_path;
        );
        // version guard
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl [rip + cached_class_version], rax;
            jne slow_path;
        method_resolved:
        );

        self.push_frame(false);
        self.set_self_and_args(method_info, block);

        monoasm!(self.jit,
            // set meta.
            movq rax, qword 0;
        patch_meta:
            movq [rsp - (16 + OFFSET_META)], rax;

            movq r13, qword 0;
        patch_pc:
            // patch point
            call entry_panic;
        patch_adr:
        );

        self.pop_frame();
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            testq rax, rax;
            jeq raise;
        );
        if !ret.is_zero() {
            self.store_rax(ret);
        }

        // slow path
        // r15: recv's class
        self.jit.select_page(1);
        monoasm!(self.jit,
        slow_path:
            movq rsi, (u32::from(name)); // IdentId
            movq rdx, (len as usize); // args_len: usize
            movq rcx, [rbp - (conv(recv))]; // receiver: Value
            call entry_find_method;
            // absolute address was returned to rax.
            testq rax, rax;
            jeq raise;

            lea rdi, [rip + patch_meta];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_META)];
            movq [rdi], rcx;

            lea rdi, [rip + patch_pc];
            subq rdi, 8;
            movq rcx, [rax + (FUNCDATA_OFFSET_PC)];
            movq [rdi], rcx;

            movq rax, [rax + (FUNCDATA_OFFSET_CODEPTR)];
            lea rdi, [rip + patch_adr];
            // calculate a displacement to the function address.
            subq rax, rdi;
            // apply patch.
            movl [rdi - 4], rax;

            movl rax, [rip + global_class_version];
            movl [rip + cached_class_version], rax;
            movl [rip + cached_recv_class], r15;
            jmp method_resolved;
        );
        let entry_return = self.vm_return;
        // raise error.
        monoasm!(self.jit,
        raise:
            movq r13, ((pc + 2).get_u64());
            jmp entry_return;
        );
        self.jit.select_page(0);
    }

    fn attr_reader(&mut self, ctx: &BBContext, ivar_name: IdentId, ret: SlotId) {
        let exit = self.jit.label();
        let slow_path = self.jit.label();
        let no_inline = self.jit.label();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(-1);
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        monoasm!(self.jit,
            movl rsi, [rip + cached_ivarid];
            cmpl rsi, (-1);
            jeq  slow_path;
            xorq rax, rax;
            movw rax, [rdi + 2];    // ObjKind
            cmpq rax, (ObjKind::OBJECT);
            jne  no_inline;
            cmpl rsi, (OBJECT_INLINE_IVAR);
            jge no_inline;
            movq rax, [rdi + rsi * 8 + 16];
            jmp exit;
        no_inline:
        );
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit.bind_label(exit);
        if !ret.is_zero() {
            self.store_rax(ret);
        }

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, (ivar_name.get()); // IvarId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cached_class];
            lea  r8, [rip + cached_ivarid];
            movq rax, (get_instance_var_with_cache);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );
        self.jit.select_page(0);
    }

    fn attr_writer(
        &mut self,
        ctx: &BBContext,
        ivar_name: IdentId,
        ret: SlotId,
        args: SlotId,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        let slow_path = self.jit.label();
        let no_inline = self.jit.label();
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(-1);
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        monoasm!(self.jit,
            movl rsi, [rip + cached_ivarid];
            cmpl rsi, (-1);
            jeq  slow_path;
            xorq rax, rax;
            movw rax, [rdi + 2];    // ObjKind
            cmpq rax, (ObjKind::OBJECT);
            jne  no_inline;
            cmpl rsi, (OBJECT_INLINE_IVAR);
            jge no_inline;
            movq rax, [rbp - (conv(args))];  //val: Value
            movq [rdi + rsi * 8 + 16], rax;
            jmp exit;
        no_inline:
        );
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, [rbp - (conv(args))];  //val: Value
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.jit.bind_label(exit);
        if !ret.is_zero() {
            self.store_rax(ret);
        }

        self.jit.select_page(1);
        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, rdi;  // recv: Value
            movq rdx, (ivar_name.get()); // name: IdentId
            movq rcx, [rbp - (conv(args))];  //val: Value
            movq rdi, r12; //&mut Globals
            lea  r8, [rip + cached_class];
            lea  r9, [rip + cached_ivarid];
            movq rax, (set_instance_var_with_cache);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );
        self.jit.select_page(0);
    }

    fn native_call(
        &mut self,
        ctx: &BBContext,
        method_info: MethodInfo,
        func_id: FuncId,
        ret: SlotId,
        block: Option<SlotId>,
        abs_address: u64,
        pc: BcPc,
    ) {
        let MethodInfo { args, len, .. } = method_info;
        // set arguments to a callee stack.
        //
        //       +-------------+
        //  0x00 |             | <- rsp
        //       +-------------+
        // -0x08 | return addr |
        //       +-------------+
        // -0x10 |   old rbp   |
        //       +-------------+
        // -0x18 |    meta     |
        //       +-------------+
        // -0x20 |   block     |
        //       +-------------+
        // -0x28 |     %0      |
        //       +-------------+
        // -0x30 | %1(1st arg) |
        //       +-------------+
        //       |             |
        //
        // rdi: receiver
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        let xmm_using = ctx.get_xmm_using();
        let caller = self.jit.label();
        monoasm!(self.jit,
        caller:
            pushq rbp;
            movq rbp, rsp;
            //movq [rbp - (OFFSET_OUTER)], 0;
            movq rax, (Meta::native(func_id, len as _).0);
            movq [rbp - (OFFSET_META)], rax;
            movq [rbp - (OFFSET_BLOCK)], r9;
            subq rsp, ((OFFSET_SELF + 15) & !0xf);
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq rax, (abs_address);
            call rax;
            leave;
            ret;
        );
        self.jit.select_page(0);

        match block {
            Some(block) => {
                monoasm!(self.jit,
                    movq r9, [rbp - (conv(block))]; // block
                );
            }
            None => {
                monoasm!(self.jit,
                    movq r9, 0;
                );
            }
        }
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, rdi;  // self: Value
        );
        self.push_frame(false);
        monoasm!(self.jit,
            lea  rcx, [rbp - (conv(args))];  // args: *const Value
            movq r8, (len);
            call caller;
        );
        self.pop_frame();
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn method_call_cached(
        &mut self,
        ctx: &BBContext,
        method_info: MethodInfo,
        ret: SlotId,
        block: Option<SlotId>,
        cached: Cached,
        pc: BcPc,
    ) {
        // argument registers:
        //   rdi: args len
        //
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);

        self.push_frame(false);
        self.set_self_and_args(method_info, block);

        monoasm!(self.jit,
            // set meta.
            movq rax, qword (cached.meta.0);
            movq [rsp - (16 + OFFSET_META)], rax;

            movq r13, qword (cached.pc.get_u64());
        );
        let src_point = self.jit.get_current_address();
        monoasm!(self.jit,
            // patch point
            call (cached.codeptr - src_point - 5);
        );
        self.pop_frame();
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn gen_yield(&mut self, ctx: &BBContext, method_info: MethodInfo, ret: SlotId, pc: BcPc) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { self.jit,
            // rdx <- &FuncData
            movq rdi, r12;
            movq rsi, [rbp - (OFFSET_BLOCK)];
            movq rax, (vm_get_block_data);
            call rax;
            movq rdx, rax;
        }
        self.push_frame(true);
        monoasm! { self.jit,
            // rsi <- CodePtr
            movq rsi, [rdx + (FUNCDATA_OFFSET_CODEPTR)];
            // set meta
            movq rdi, [rdx + (FUNCDATA_OFFSET_META)];
            movq [rsp -(16 + OFFSET_META)], rdi;
            // set pc
            movq r13, [rdx + (FUNCDATA_OFFSET_PC)];
            // set self
            movq rax, [rbp - (OFFSET_SELF)];
            movq [rsp - (16 + OFFSET_SELF)], rax;
            // set block
            movq [rsp - (16 + OFFSET_BLOCK)], 0;
        };
        // set arguments
        self.set_args(method_info.args, method_info.len);
        monoasm! { self.jit,
            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rdi, (method_info.len);
            call rsi;
        };
        self.pop_frame();
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }
}

impl Codegen {
    fn guard_version(&mut self, cached_version: u32, side_exit: DestLabel) {
        let global_class_version = self.class_version;
        monoasm!(self.jit,
            cmpl [rip + global_class_version], (cached_version);
            jne side_exit;
        );
    }

    /// Set *self*, len, block, and arguments.
    ///
    /// out    : rdi <- len
    /// destroy: rax
    fn set_self_and_args(&mut self, method_info: MethodInfo, block: Option<SlotId>) {
        let MethodInfo {
            recv, args, len, ..
        } = method_info;
        // set self, len
        monoasm!(self.jit,
            movq rax, [rbp - (conv(recv))];
            movq [rsp - (16 + OFFSET_SELF)], rax;
            movq rdi, (len);
        );
        self.set_args(args, len);
        // set block
        match block {
            Some(block) => {
                monoasm!(self.jit,
                    movq rax, [rbp - (conv(block))];
                    movq [rsp - (16 + OFFSET_BLOCK)], rax;
                );
            }
            None => {
                monoasm!(self.jit,
                    movq [rsp - (16 + OFFSET_BLOCK)], 0;
                );
            }
        }
    }

    /// Set arguments.
    ///
    /// destroy: rax
    fn set_args(&mut self, args: SlotId, len: u16) {
        // set arguments
        for i in 0..len {
            let reg = args + i;
            monoasm!(self.jit,
                movq rax, [rbp - (conv(reg))];
                movq [rsp - (16 + OFFSET_ARG0 as i32 + i as i32 * 8)], rax;
            );
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn polymorphic() {
        tests::run_test(
            r##"
        class C
          attr_accessor :a
          def initialize
            @a=10
          end
          def f
            @a
          end
        end

        class C1 < C
          attr_accessor :a
          def initialize
            @a=20
          end
        end
                
        res = []
                
        a = [C1.new, C1.new, C1.new, C1.new, C.new, C.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        a = [C.new, C.new, C.new, C.new, C1.new, C1.new]
        for i in 0..a.length - 1
          res << a[i].f
        end
                
        res
        "##,
        );
    }

    #[test]
    fn yield_test() {
        tests::run_test(
            r##"
          def f(x,y)
            yield x,y
          end
          
          res = []
          for i in 0..10
            res << f(i,5) {|x,y| x+y}
            res << f(i,8) {|x,y| x+y}
          end
          res
        "##,
        );
    }

    #[test]
    fn iterator() {
        tests::run_test(
            r##"
        class Array
          def iich
            for i in 0...self.size
              yield(self[i])
            end
          end
        end

        a = []
        [2,5,7,10,2.2,7,9].iich do |x|
          a << x*2
        end
        a
        "##,
        );
    }
}
