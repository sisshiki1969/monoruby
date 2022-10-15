use super::*;

struct Cached {
    codeptr: CodePtr,
    meta: Meta,
    class_id: ClassId,
    version: u32,
    pc: BcPc,
}

impl Codegen {
    pub(super) fn gen_method_call(
        &mut self,
        globals: &Globals,
        ctx: &BBContext,
        name: IdentId,
        recv: SlotId,
        args: SlotId,
        len: u16,
        ret: SlotId,
        pc: BcPc,
        codeptr: Option<CodePtr>,
        class_id: ClassId,
        version: u32,
    ) {
        if let Some(codeptr) = codeptr {
            let meta = (pc + 1).meta();
            let callee_pc = (pc + 1).pc();
            let cached = Cached {
                codeptr,
                meta,
                class_id,
                version,
                pc: callee_pc,
            };
            self.gen_method_call_cached(globals, &ctx, recv, args, len, ret, cached, pc);
        } else {
            self.gen_method_call_not_cached(&ctx, recv, name, args, len, ret, pc);
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    fn gen_method_call_cached(
        &mut self,
        globals: &Globals,
        ctx: &BBContext,
        recv: SlotId,
        args: SlotId,
        len: u16,
        ret: SlotId,
        cached: Cached,
        pc: BcPc,
    ) {
        let deopt = self.gen_side_deopt_dest(pc - 1, &ctx);
        monoasm!(self.jit,
            movq rdi, [rbp - (conv(recv))];
        );
        if !recv.is_zero() {
            self.guard_class(cached.class_id, deopt);
        }
        self.guard_version(cached.version, deopt);
        let func_id = cached.meta.func_id();
        match globals.func[func_id].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, len);
                if cached.class_id.is_always_frozen() {
                    if !ret.is_zero() {
                        monoasm!(self.jit,
                            movq rax, (NIL_VALUE);
                        );
                        self.store_rax(ret);
                    }
                } else {
                    self.attr_reader(&ctx, ivar_name, ret);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, len);
                self.attr_writer(&ctx, ivar_name, ret, args, pc);
            }
            FuncKind::Builtin { abs_address } => {
                self.native_call(&ctx, ret, args, len, abs_address, pc);
            }
            FuncKind::Normal(_) => {
                self.method_call_cached(recv, ret, args, len, &ctx, cached, pc);
            }
        };
    }

    ///
    /// generate JIT code for a method call which was not cached.
    ///
    fn gen_method_call_not_cached(
        &mut self,
        ctx: &BBContext,
        recv: SlotId,
        name: IdentId,
        args: SlotId,
        len: u16,
        ret: SlotId,
        pc: BcPc,
    ) {
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
        // -0x20 |     %0      |
        //       +-------------+
        // -0x28 | %1(1st arg) |
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
        if !recv.is_zero() {
            monoasm!(self.jit,
                movq rdi, [rbp - (conv(recv))];
                movq rax, (Value::get_class);
                call rax;
                movl r15, rax;  // r15: receiver class_id
                cmpl r15, [rip + cached_recv_class];
                jne slow_path;
            );
        }
        monoasm!(self.jit,
            movl rax, [rip + global_class_version];
            cmpl [rip + cached_class_version], rax;
            jne slow_path;
        method_resolved:
        );

        self.set_self_and_args(recv, args, len);

        monoasm!(self.jit,
            // set meta.
            movq rax, qword (0);
        patch_meta:
            movq [rsp - (16 + OFFSET_META)], rax;

            movq r13, qword (0);
        patch_pc:
            movq rdi, (len);
            // patch point
            call entry_panic;
        patch_adr:
        );

        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            testq rax, rax;
            jeq raise;
        );
        if !ret.is_zero() {
            self.store_rax(ret);
        }

        self.jit.select_page(1);
        // call site stub code.
        monoasm!(self.jit,
        slow_path:
            movq rdx, (u32::from(name)); // IdentId
            movq rcx, (len as usize); // args_len: usize
            movq r8, [rbp - (conv(recv))]; // receiver: Value
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
        );
        if !recv.is_zero() {
            monoasm!(self.jit,
                movl [rip + cached_recv_class], r15;
            );
        }
        monoasm!(self.jit,
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
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(-1);
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        monoasm!(self.jit,
            movl rsi, [rip + cached_ivarid];
            cmpl rsi, (-1);
            jeq  slow_path;
        );
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );

        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, (ivar_name.get()); // IvarId
            movq rdx, r12; // &mut Globals
            lea  rcx, [rip + cached_class];
            lea  r8, [rip + cached_ivarid];
            movq rax, (vm_get_instance_var);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit.bind_label(exit);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
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
        let cached_class = self.jit.const_i32(0);
        let cached_ivarid = self.jit.const_i32(-1);
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        monoasm!(self.jit,
            movl rsi, [rip + cached_ivarid];
            cmpl rsi, (-1);
            jeq  slow_path;
        );
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, [rbp - (conv(args))];  //val: Value
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp exit;
        );

        self.jit.bind_label(slow_path);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rsi, rdi;  // recv: Value
            movq rdx, (ivar_name.get()); // name: IdentId
            movq rcx, [rbp - (conv(args))];  //val: Value
            movq rdi, r12; //&mut Globals
            lea  r8, [rip + cached_class];
            lea  r9, [rip + cached_ivarid];
            movq rax, (vm_set_instance_var);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        self.jit.bind_label(exit);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn native_call(
        &mut self,
        ctx: &BBContext,
        ret: SlotId,
        args: SlotId,
        len: u16,
        abs_address: u64,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, rdi;  // self: Value
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            lea  rcx, [rbp - (conv(args))];  // args: *const Value
            movq r8, (len);
            movq rax, (abs_address);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn method_call_cached(
        &mut self,
        recv: SlotId,
        ret: SlotId,
        args: SlotId,
        len: u16,
        ctx: &BBContext,
        cached: Cached,
        pc: BcPc,
    ) {
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
        // -0x20 |     %0      |
        //       +-------------+
        // -0x28 | %1(1st arg) |
        //       +-------------+
        //       |             |
        //
        // argument registers:
        //   rdi: args len
        //
        let method_resolved = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);

        self.jit.bind_label(method_resolved);

        self.set_self_and_args(recv, args, len);

        monoasm!(self.jit,
            // set meta.
            movq rax, qword (cached.meta.0);
            movq [rsp - (16 + OFFSET_META)], rax;

            movq r13, qword (cached.pc.get_u64());
            movq rdi, (len);
        );
        let src_point = self.jit.get_current_address();
        monoasm!(self.jit,
            // patch point
            call (cached.codeptr - src_point - 5);
        );
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
            movl rax, [rip + global_class_version];
            cmpl rax, (cached_version);
            jne side_exit;
        );
    }

    fn set_self_and_args(&mut self, recv: SlotId, args: SlotId, len: u16) {
        // set self
        monoasm!(self.jit,
            movq rax, [rbp - (conv(recv))];
            movq [rsp - (16 + OFFSET_SELF)], rax;
        );
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
