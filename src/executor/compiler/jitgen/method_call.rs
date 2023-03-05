use super::*;

impl Codegen {
    extern "C" fn cos(f: f64) -> f64 {
        f.cos()
    }

    extern "C" fn sin(f: f64) -> f64 {
        f.sin()
    }

    pub(super) fn gen_method_call(
        &mut self,
        fnstore: &FnStore,
        ctx: &mut BBContext,
        method_info: MethodInfo,
        ret: SlotId,
        callid: CallSiteId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            args, len, recv, ..
        } = method_info;
        self.write_back_slot(ctx, recv);
        self.write_back_range(ctx, args, len);
        ctx.dealloc_xmm(ret);
        // We must write back and unlink all local vars since they may be accessed by eval.
        self.gen_write_back_locals(ctx);
        self.gen_call(
            fnstore,
            ctx,
            method_info,
            callid,
            None,
            ret,
            pc + 1,
            has_splat,
        );
    }

    pub(super) fn gen_inlinable(
        &mut self,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        inline_id: &InlineMethod,
        ret: SlotId,
        pc: BcPc,
    ) {
        let MethodInfo { recv, args, .. } = method_info;
        let (class, version) = pc.class_version();
        let deopt = self.gen_side_deopt(pc, ctx);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        self.load_rdi(*recv);
        if !recv.is_zero() {
            self.guard_class(class, deopt);
        }
        self.guard_version(version, deopt);
        match inline_id {
            InlineMethod::IntegerTof => {
                let fret = ctx.xmm_write(ret);
                monoasm!(self.jit,
                    sarq  rdi, 1;
                    cvtsi2sdq xmm(fret.enc()), rdi;
                );
            }
            InlineMethod::MathSqrt => {
                let fsrc = self.xmm_read_assume_float(ctx, *args, pc);
                let fret = ctx.xmm_write(ret);
                monoasm!(self.jit,
                    sqrtsd xmm(fret.enc()), xmm(fsrc.enc());
                );
            }
            InlineMethod::MathCos => {
                let fsrc = self.xmm_read_assume_float(ctx, *args, pc);
                let fret = ctx.xmm_write(ret);
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    movq xmm0, xmm(fsrc.enc());
                    movq rax, (Self::cos as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    movq xmm(fret.enc()), xmm0;
                );
            }
            InlineMethod::MathSin => {
                let fsrc = self.xmm_read_assume_float(ctx, *args, pc);
                let fret = ctx.xmm_write(ret);
                let xmm_using = ctx.get_xmm_using();
                self.xmm_save(&xmm_using);
                monoasm!(self.jit,
                    movq xmm0, xmm(fsrc.enc());
                    movq rax, (Self::sin as u64);
                    call rax;
                );
                self.xmm_restore(&xmm_using);
                monoasm!(self.jit,
                    movq xmm(fret.enc()), xmm0;
                );
            }
        }
    }

    pub(super) fn gen_method_call_with_block(
        &mut self,
        fnstore: &FnStore,
        ctx: &mut BBContext,
        mut method_info: MethodInfo,
        ret: SlotId,
        callid: CallSiteId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            args, len, recv, ..
        } = method_info;
        self.write_back_slot(ctx, recv);
        self.write_back_range(ctx, args, len + 1);
        ctx.dealloc_xmm(ret);
        // We must write back and unlink all local vars since they may be accessed from block.
        self.gen_write_back_locals(ctx);
        method_info.args = args + 1;
        self.gen_call(
            fnstore,
            ctx,
            method_info,
            callid,
            Some(args),
            ret,
            pc + 1,
            has_splat,
        );
    }

    fn gen_call(
        &mut self,
        fnstore: &FnStore,
        ctx: &BBContext,
        method_info: MethodInfo,
        callid: CallSiteId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            func_data, recv, ..
        } = method_info;
        if func_data.is_some() {
            let cached = InlineCached::new(pc);
            if recv.is_zero() && ctx.self_value.class() != cached.class_id {
                self.gen_call_not_cached(ctx, method_info, callid, block, ret, pc, has_splat);
            } else {
                self.gen_call_cached(
                    fnstore,
                    ctx,
                    callid,
                    method_info,
                    block,
                    ret,
                    cached,
                    pc,
                    has_splat,
                );
            }
        } else {
            self.gen_call_not_cached(ctx, method_info, callid, block, ret, pc, has_splat);
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    fn gen_call_cached(
        &mut self,
        fnstore: &FnStore,
        ctx: &BBContext,
        callid: CallSiteId,
        method_info: MethodInfo,
        block: Option<SlotId>,
        ret: SlotId,
        cached: InlineCached,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            recv,
            len,
            func_data,
            ..
        } = method_info;
        let deopt = self.gen_side_deopt(pc - 1, ctx);
        self.load_rdi(recv);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_zero() {
            self.guard_class(cached.class_id, deopt);
        }
        self.guard_version(cached.version, deopt);
        let func_id = func_data.unwrap().meta.func_id();
        match fnstore[func_id].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, len);
                assert!(block.is_none());
                assert!(fnstore[callid].kw_args.is_empty());
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
                assert_eq!(1, len);
                assert!(block.is_none());
                assert!(fnstore[callid].kw_args.is_empty());
                self.attr_writer(ctx, ivar_name, ret, method_info.args, pc);
            }
            FuncKind::Builtin { abs_address } => {
                self.native_call(ctx, method_info, func_id, ret, block, abs_address, pc);
            }
            FuncKind::ISeq(_) => {
                self.method_call_cached(
                    ctx,
                    fnstore,
                    callid,
                    method_info,
                    ret,
                    block,
                    pc,
                    has_splat,
                );
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
        callid: CallSiteId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo { recv, len, .. } = method_info;
        // argument registers:
        //   rdi: args len
        //
        let method_resolved = self.jit.label();
        let slow_path = self.jit.label();
        let raise = self.jit.label();
        let global_class_version = self.class_version;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        // class guard
        // r15 <- recv's class
        if recv.is_zero() {
            // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
            monoasm!(self.jit,
                movl r15, (ctx.self_value.class().0);
            );
        } else {
            self.load_rdi(recv);
            monoasm!(self.jit,
                movq rax, (Value::get_class);
                call rax;
                movl r15, rax;  // r15: receiver class_id
            );
        }
        monoasm! {self.jit,
            movq r13, (pc.get_u64());
            // check inline cache
            cmpq [r13 + 8], 0;
            jeq  slow_path;
            // class guard
            cmpl r15, [r13 - 8];
            jne  slow_path;
            // version guard
            movl rax, [rip + global_class_version];
            cmpl [r13 - 4], rax;
            jne  slow_path;
        method_resolved:
        }

        self.set_method_outer();
        self.set_self_and_args(method_info, block, has_splat);

        monoasm!(self.jit,
            // set meta.
            movq rax, [r13 + 8];
            movq rax, [rax + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rax;
            movq rcx, rax;

            lea  r8, [rsp - (16 + LBP_SELF)];
            movq r9, rdi;
            subq rsp, 4096;
            movq rdi, r12; // &Globals
            movl rsi, (callid.get()); // CallSiteId
            lea  rdx, [r14 - (LBP_SELF)];
            movq rax, (runtime::vm_handle_arguments);
            call rax;
            movq rdi, rax;
            addq rsp, 4096;

            movq r13, [r13 + 8];
            // set codeptr
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];
            // set CallSiteId
            movl rcx, (callid.get());
        );
        self.call_rax();
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
            movq rdi, r12;
            movq rsi, (callid.get()); // CallSiteId
            movq rdx, (len as usize); // args_len: usize
            movq rcx, [r14 - (conv(recv))]; // receiver: Value
            movw r8, (recv.0);
            movq rax, (runtime::find_method);
            call rax;
            // absolute address was returned to rax.
            testq rax, rax;
            jeq raise;
            movq [r13 + 8], rax;

            movl rax, [rip + global_class_version];
            movl [r13 - 4], rax;
            movl [r13 - 8], r15;
            jmp method_resolved;
        );
        let entry_return = self.vm_return;
        // raise error.
        self.jit.bind_label(raise);
        monoasm!(self.jit,
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
            cmpw [rdi + 2], (ObjKind::OBJECT);
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
            cmpw [rdi + 2], (ObjKind::OBJECT);
            jne  no_inline;
            cmpl rsi, (OBJECT_INLINE_IVAR);
            jge no_inline;
            movq rax, [r14 - (conv(args))];  //val: Value
            movq [rdi + rsi * 8 + 16], rax;
            jmp exit;
        no_inline:
        );
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, [r14 - (conv(args))];  //val: Value
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
            movq rcx, [r14 - (conv(args))];  //val: Value
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
            movq rax, (Meta::native(func_id, len as _).get());
            movq [r14 - (LBP_META)], rax;
            movq [r14 - (LBP_BLOCK)], r9;
            movq [r14 - (LBP_SELF)], rdx;
            subq rsp, ((LBP_SELF + 15) & !0xf);
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
                    movq r9, [r14 - (conv(block))]; // block
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
            movq [rsp - (16 + LBP_SELF)], rcx;
        );
        self.set_method_outer();
        monoasm!(self.jit,
            lea  rcx, [r14 - (conv(args))];  // args: *const Value
            movq r8, (len);
        );
        self.call_dest(caller);
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn method_call_cached(
        &mut self,
        ctx: &BBContext,
        fnstore: &FnStore,
        callid: CallSiteId,
        method_info: MethodInfo,
        ret: SlotId,
        block: Option<SlotId>,
        pc: BcPc,
        has_splat: bool,
    ) {
        let func_data = method_info.func_data.unwrap();
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        self.execute_gc();
        self.set_method_outer();
        self.set_self_and_args(method_info, block, has_splat);
        // argument registers:
        //   rdi: args len
        let callee_func_id = func_data.meta.func_id();
        match &fnstore[callee_func_id].kind {
            FuncKind::ISeq(info) => {
                if !info.is_block_style
                    && info.pos_num() == info.req_num()
                    && info.key_num() == 0
                    && info.block_param_name().is_none()
                {
                    // We must check args_num == req_num
                    let reg_num = info.total_reg_num() - 1;
                    let arguments_num = info.args.args_names.len();
                    let len = reg_num - arguments_num;
                    let temp_start = info.total_reg_num() - len;
                    if len != 0 {
                        monoasm!(self.jit,
                            movq rax, (NIL_VALUE);
                        );
                        for ofs in temp_start..temp_start + len {
                            monoasm!(self.jit,
                                movq [rsp - ((16 + LBP_SELF + ofs as i64 * 8) as i32)], rax;
                            );
                        }
                    }
                } else {
                    monoasm!(self.jit,
                        lea  r8, [rsp - (16 + LBP_SELF)];
                        movq r9, rdi;
                        subq rsp, 4096;
                        movq rdi, r12; // &Globals
                        movl rsi, (callid.get()); // CallSiteId
                        lea  rdx, [r14 - (LBP_SELF)];
                        movl rcx, (func_data.meta.func_id().get());
                        movq rax, (runtime::vm_handle_arguments);
                        call rax;
                        movq rdi, rax;
                        addq rsp, 4096;
                    );
                }
            }
            _ => {}
        }
        monoasm!(self.jit,
            // set meta.
            movq rax, (func_data.meta.get());
            movq [rsp - (16 + LBP_META)], rax;
            // set pc.
            movq r13, (func_data.pc.get_u64());
        );
        self.call_codeptr(func_data.codeptr.unwrap());
        self.xmm_restore(&xmm_using);
        self.handle_error(pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &BBContext,
        args: SlotId,
        len: u16,
        ret: SlotId,
        callid: CallSiteId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        let no_block = self.no_block;
        monoasm! { self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
            // rax <- outer_cfp, rdx <- &FuncData
            testq rax, rax;
            jz  no_block;
        }

        self.set_block_self_outer();
        monoasm! { self.jit,
            // r13 <- &FuncData
            movq r13, rdx;
            // set meta
            movq rdi, [r13 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
        };
        // set arguments
        self.jit_set_arguments(args, len, true);
        monoasm! { self.jit,
            lea  r8, [rsp - (16 + LBP_SELF)];
            movq r9, rdi;
            movl rcx, [rsp - (16 + LBP_META)];
            subq rsp, 4096;
            movq rdi, r12; // &Globals
            movl rsi, (callid.get()); // CallSiteId
            lea  rdx, [r14 - (LBP_SELF)];
            movq rax, (runtime::vm_handle_arguments);
            call rax;
            movq rdi, rax;
            addq rsp, 4096;
            // argument registers:
            //   rdi: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];
        };
        self.call_rax();
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
    /// ### out
    /// - rdi <- the number of arguments
    ///
    /// ### destroy
    /// - caller save registers
    fn set_self_and_args(
        &mut self,
        method_info: MethodInfo,
        block: Option<SlotId>,
        has_splat: bool,
    ) {
        let MethodInfo {
            recv, args, len, ..
        } = method_info;
        // set self, len
        self.load_rax(recv);
        monoasm!(self.jit,
            movq [rsp - (16 + LBP_SELF)], rax;
        );
        self.jit_set_arguments(args, len, has_splat);
        // set block
        if let Some(block) = block {
            self.load_rax(block);
            monoasm!(self.jit,
                movq [rsp - (16 + LBP_BLOCK)], rax;
            );
        } else {
            monoasm!(self.jit,
                movq [rsp - (16 + LBP_BLOCK)], 0;
            );
        }
    }

    /// Set arguments.
    ///
    /// ### out
    ///
    /// - rdi: the number of arguments
    ///
    /// ### save
    ///
    /// - rsi
    ///
    /// ### destroy
    ///
    /// - caller save registers
    fn jit_set_arguments(&mut self, args: SlotId, len: u16, has_splat: bool) {
        monoasm!(self.jit,
            movq rdi, (len);
        );
        // set arguments
        if len != 0 {
            let splat = self.splat;
            if has_splat {
                monoasm!(self.jit,
                    lea r8, [rsp - (16 + LBP_ARG0)];
                );
                for i in 0..len {
                    let next = self.jit.label();
                    let reg = args + i;
                    self.load_rax(reg);
                    let no_splat = self.jit.label();
                    monoasm! {self.jit,
                        testq rax, 0b111;
                        jne  no_splat;
                        cmpw [rax + 2], (ObjKind::SPLAT);
                        jne  no_splat;
                        call splat;
                        jmp next;
                    no_splat:
                    }
                    monoasm! {self.jit,
                        movq [r8], rax;
                        subq r8, 8;
                    next:
                    }
                }
            } else {
                for i in 0..len {
                    let reg = args + i;
                    self.load_rax(reg);
                    monoasm! {self.jit,
                        movq [rsp - ((16 + LBP_ARG0 + 8 * (i as i64)) as i32)], rax;
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn polymorphic() {
        tests::run_test_with_prelude(
            r##"
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
