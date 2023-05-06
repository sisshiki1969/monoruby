use super::*;

impl Codegen {
    extern "C" fn cos(f: f64) -> f64 {
        f.cos()
    }

    extern "C" fn sin(f: f64) -> f64 {
        f.sin()
    }

    fn integer_tof(
        gen: &mut Codegen,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        ret: SlotId,
        pc: BcPc,
        deopt: DestLabel,
    ) {
        let MethodInfo { recv, .. } = method_info;
        gen.load_rdi(*recv);
        if !recv.is_zero() {
            gen.guard_class(pc.class_version().0, deopt);
        }
        let fret = ctx.xmm_write(ret);
        monoasm!(gen.jit,
            sarq  rdi, 1;
            cvtsi2sdq xmm(fret.enc()), rdi;
        );
    }

    fn math_sqrt(
        gen: &mut Codegen,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        ret: SlotId,
        pc: BcPc,
        deopt: DestLabel,
    ) {
        let MethodInfo { recv, args, .. } = method_info;
        gen.load_rdi(*recv);
        if !recv.is_zero() {
            gen.guard_class(pc.class_version().0, deopt);
        }
        let fsrc = gen.xmm_read_assume_float(ctx, *args, pc);
        let fret = ctx.xmm_write(ret);
        monoasm!(gen.jit,
            sqrtsd xmm(fret.enc()), xmm(fsrc.enc());
        );
    }

    fn math_cos(
        gen: &mut Codegen,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        ret: SlotId,
        pc: BcPc,
        deopt: DestLabel,
    ) {
        let MethodInfo { recv, args, .. } = method_info;
        gen.load_rdi(*recv);
        if !recv.is_zero() {
            gen.guard_class(pc.class_version().0, deopt);
        }
        let fsrc = gen.xmm_read_assume_float(ctx, *args, pc);
        let fret = ctx.xmm_write(ret);
        let xmm_using = ctx.get_xmm_using();
        gen.xmm_save(&xmm_using);
        monoasm!(gen.jit,
            movq xmm0, xmm(fsrc.enc());
            movq rax, (Self::cos);
            call rax;
        );
        gen.xmm_restore(&xmm_using);
        monoasm!(gen.jit,
            movq xmm(fret.enc()), xmm0;
        );
    }

    fn math_sin(
        gen: &mut Codegen,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        ret: SlotId,
        pc: BcPc,
        deopt: DestLabel,
    ) {
        let MethodInfo { recv, args, .. } = method_info;
        gen.load_rdi(*recv);
        if !recv.is_zero() {
            gen.guard_class(pc.class_version().0, deopt);
        }
        let fsrc = gen.xmm_read_assume_float(ctx, *args, pc);
        let fret = ctx.xmm_write(ret);
        let xmm_using = ctx.get_xmm_using();
        gen.xmm_save(&xmm_using);
        monoasm!(gen.jit,
            movq xmm0, xmm(fsrc.enc());
            movq rax, (Self::sin);
            call rax;
        );
        gen.xmm_restore(&xmm_using);
        monoasm!(gen.jit,
            movq xmm(fret.enc()), xmm0;
        );
    }

    fn object_nil(
        gen: &mut Codegen,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        ret: SlotId,
        _pc: BcPc,
        _deopt: DestLabel,
    ) {
        let MethodInfo { recv, .. } = method_info;
        gen.load_rdi(*recv);
        ctx.dealloc_xmm(ret);
        let l1 = gen.jit.label();
        monoasm!(gen.jit,
            movq rax, (FALSE_VALUE);
            cmpq rdi, (NIL_VALUE);
            jne  l1;
            movq rax, (TRUE_VALUE);
        l1:
        );
        gen.store_rax(ret);
    }

    pub(super) fn gen_inlinable(
        &mut self,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        inline_id: &InlineMethod,
        ret: SlotId,
        pc: BcPc,
    ) {
        let (_, version) = pc.class_version();
        let deopt = self.gen_side_deopt(pc, ctx);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        self.guard_version(version, deopt);
        match inline_id {
            InlineMethod::IntegerTof => {
                Self::integer_tof(self, ctx, method_info, ret, pc, deopt);
            }
            InlineMethod::MathSqrt => {
                Self::math_sqrt(self, ctx, method_info, ret, pc, deopt);
            }
            InlineMethod::MathCos => {
                Self::math_cos(self, ctx, method_info, ret, pc, deopt);
            }
            InlineMethod::MathSin => {
                Self::math_sin(self, ctx, method_info, ret, pc, deopt);
            }
            InlineMethod::ObjectNil => {
                Self::object_nil(self, ctx, method_info, ret, pc, deopt);
            }
        }
    }

    pub(super) fn gen_call(
        &mut self,
        fnstore: &FnStore,
        classstore: &ClassStore,
        ctx: &BBContext,
        info: MethodInfo,
        callid: CallSiteId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            func_data, recv, ..
        } = info;
        if func_data.is_some() {
            let cached = InlineCached::new(pc);
            if recv.is_zero() && ctx.self_value.class() != cached.class_id {
                self.gen_call_not_cached(fnstore, ctx, info, callid, block, ret, pc, has_splat);
            } else {
                self.gen_call_cached(
                    fnstore, classstore, ctx, callid, info, block, ret, cached, pc, has_splat,
                );
            }
        } else {
            unreachable!();
            //self.gen_call_not_cached(ctx, info, callid, block, ret, pc, has_splat);
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    fn gen_call_cached(
        &mut self,
        fnstore: &FnStore,
        classstore: &ClassStore,
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
        let mut self_in_rdi_flag = false;
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_zero() {
            self.load_rdi(recv);
            self_in_rdi_flag = true;
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
                    if !self_in_rdi_flag {
                        self.load_rdi(recv);
                    }
                    let ivar_id = classstore[cached.class_id].get_ivarid(ivar_name);
                    self.attr_reader(ctx, ivar_name, ivar_id, ret);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, len);
                assert!(block.is_none());
                assert!(fnstore[callid].kw_args.is_empty());
                if !self_in_rdi_flag {
                    self.load_rdi(recv);
                }
                let ivar_id = classstore[cached.class_id].get_ivarid(ivar_name);
                self.attr_writer(ctx, ivar_name, ivar_id, ret, method_info.args, pc);
            }
            FuncKind::Builtin { abs_address } => {
                if !self_in_rdi_flag {
                    self.load_rdi(recv);
                }
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
        fnstore: &FnStore,
        ctx: &BBContext,
        method_info: MethodInfo,
        callid: CallSiteId,
        block: Option<SlotId>,
        ret: SlotId,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo { recv, .. } = method_info;
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
        self.set_self_and_args(method_info, block, has_splat, &fnstore[callid]);

        monoasm! {self.jit,
            // set meta.
            movq r15, [r13 + 8];    // &FuncData
            movq rax, [r15 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rax;
        }

        monoasm! { self.jit,
            movq rsi, [r15 + (FUNCDATA_OFFSET_PC)];
        }
        self.block_arg_expand();

        monoasm! {self.jit,
            movq rcx, r15;
            movl rdx, (callid.get()); // CallSiteId
        }
        self.handle_arguments();
        monoasm!(self.jit,
            testq rax, rax;
            jeq raise;
        );

        monoasm! {self.jit,
            movq rdx, rdi;
            // set codeptr
            movq rax, [r15 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r15 + (FUNCDATA_OFFSET_PC)];
        }
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
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, (callid.get()); // CallSiteId
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
        let raise = self.entry_raise;
        // raise error.
        monoasm!(self.jit,
        raise:
            movq r13, ((pc + 2).get_u64());
            jmp raise;
        );
        self.jit.select_page(0);
    }

    fn attr_reader(
        &mut self,
        ctx: &BBContext,
        ivar_name: IdentId,
        ivar_id: Option<IvarId>,
        ret: SlotId,
    ) {
        let exit = self.jit.label();
        let no_inline = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        if let Some(ivar_id) = ivar_id {
            monoasm!(self.jit,
                movl rsi, (ivar_id.get());
                cmpw [rdi + 2], (ObjKind::OBJECT);
                jne  no_inline;
                cmpl rsi, (OBJECT_INLINE_IVAR);
                jge  no_inline;
                movq rax, [rdi + rsi * 8 + 16];
            exit:
            );
            if !ret.is_zero() {
                self.store_rax(ret);
            }
        } else {
            let slow_path = self.jit.label();
            let cache = self.jit.const_i64(-1);
            monoasm!(self.jit,
                lea  rax, [rip + cache];    // cache.ivarid
                movl rsi, [rax + 4];
                cmpl rsi, (-1);
                jeq  slow_path;
                cmpw [rdi + 2], (ObjKind::OBJECT);
                jne  no_inline;
                cmpl rsi, (OBJECT_INLINE_IVAR);
                jge no_inline;
                movq rax, [rdi + rsi * 8 + 16];
            exit:
            );
            if !ret.is_zero() {
                self.store_rax(ret);
            }

            self.jit.select_page(1);
            self.jit.bind_label(slow_path);
            self.xmm_save(&xmm_using);
            monoasm!(self.jit,
                movq rsi, (ivar_name.get()); // IvarId
                movq rdx, r12; // &mut Globals
                lea  rcx, [rip + cache];
                movq rax, (get_instance_var_with_cache);
                call rax;
            );
            self.xmm_restore(&xmm_using);
            monoasm!(self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }

        self.jit.select_page(1);
        self.jit.bind_label(no_inline);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rax, (RValue::get_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        monoasm!(self.jit,
            jmp  exit;
        );
        self.jit.select_page(0);
    }

    fn attr_writer(
        &mut self,
        ctx: &BBContext,
        ivar_name: IdentId,
        ivar_id: Option<IvarId>,
        ret: SlotId,
        args: SlotId,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        let no_inline = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        if let Some(ivar_id) = ivar_id {
            monoasm!(self.jit,
                movl rsi, (ivar_id.get());
                cmpw [rdi + 2], (ObjKind::OBJECT);
                jne  no_inline;
                cmpl rsi, (OBJECT_INLINE_IVAR);
                jge no_inline;
                movq rax, [r14 - (conv(args))];  //val: Value
                movq [rdi + rsi * 8 + 16], rax;
                exit:
            );
            if !ret.is_zero() {
                self.store_rax(ret);
            }
        } else {
            let slow_path = self.jit.label();
            let cache = self.jit.const_i64(-1);
            monoasm!(self.jit,
                lea  rax, [rip + cache];    // cache.ivarid
                movl rsi, [rax + 4];
                cmpl rsi, (-1);
                jeq  slow_path;
                cmpw [rdi + 2], (ObjKind::OBJECT);
                jne  no_inline;
                cmpl rsi, (OBJECT_INLINE_IVAR);
                jge no_inline;
                movq rax, [r14 - (conv(args))];  //val: Value
                movq [rdi + rsi * 8 + 16], rax;
            exit:
            );
            if !ret.is_zero() {
                self.store_rax(ret);
            }

            self.jit.select_page(1);
            self.jit.bind_label(slow_path);
            self.xmm_save(&xmm_using);
            monoasm!(self.jit,
                movq rdx, rdi;  // recv: Value
                movq rcx, (ivar_name.get()); // name: IdentId
                movq r8, [r14 - (conv(args))];  //val: Value
                movq rdi, rbx; //&mut Executor
                movq rsi, r12; //&mut Globals
                lea  r9, [rip + cache];
                movq rax, (set_instance_var_with_cache);
                call rax;
            );
            self.xmm_restore(&xmm_using);
            monoasm!(self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }

        self.jit.select_page(1);
        self.jit.bind_label(no_inline);
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rdx, [r14 - (conv(args))];  //val: Value
            movq rax, (RValue::set_ivar);
            call rax;
        );
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
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
        // rdi : receiver(self)
        let MethodInfo { args, len, .. } = method_info;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!(self.jit,
            movq rax, (Meta::native(func_id, 0 /* for GC */ as _).get());
            movq [rsp - (16 + LBP_META)], rax;
            movq [rsp - (16 + LBP_SELF)], rdi;  // self: Value
            movq [rsp - (16 + LBP_OUTER)], 0;
        );
        match block {
            Some(block) => {
                monoasm!(self.jit,
                    movq rax, [r14 - (conv(block))];
                    movq [rsp - (16 + LBP_BLOCK)], rax;
                );
            }
            None => {
                monoasm!(self.jit,
                    movq [rsp - (16 + LBP_BLOCK)], 0;
                );
            }
        }
        monoasm!(self.jit,
            lea  rcx, [r14 - (conv(args))];  // args: *const Value
        );
        let stack_offset = (LBP_SELF + 31) & !0xf;
        let entry = self.jit.label();
        self.push_frame();
        monoasm!(self.jit,
            // set lfp
            lea  rdx, [rsp - 16];
            movq [rsp - (16 + BP_LFP)], rdx;
            call entry;
        );
        self.pop_frame();
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
        self.jit.select_page(1);
        monoasm!(self.jit,
        entry:
            pushq rbp;
            subq rsp, (stack_offset);
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq r8, (len); // len
            movq r9, (abs_address);
            movq rax, (crate::executor::compiler::wrapper::wrapper);
            call rax;
            addq rsp, (stack_offset);
            popq rbp;
            ret;
        );
        self.jit.select_page(0);
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
        self.set_self_and_args(method_info, block, has_splat, &fnstore[callid]);
        // argument registers:
        //   rdi: args len
        let callee_func_id = func_data.meta.func_id();
        match &fnstore[callee_func_id].kind {
            FuncKind::ISeq(info) => {
                let callsite = &fnstore[callid];
                if info.is_block_style && info.reqopt_num() > 1 && callsite.arg_num == 1 {
                    self.single_arg_expand();
                }
                if info.pos_num() == info.req_num() {
                    if info.key_num() != 0 {
                        let CallSiteInfo {
                            kw_pos, kw_args, ..
                        } = callsite;
                        let callee_kw_pos = info.args.pos_num + 1;
                        for (callee, param_name) in info.args.keyword_names.iter().enumerate() {
                            let callee_ofs = (callee_kw_pos as i64 + callee as i64) * 8 + LBP_SELF;
                            match kw_args.get(param_name) {
                                Some(caller) => {
                                    let caller_ofs =
                                        (kw_pos.0 as i64 + *caller as i64) * 8 + LBP_SELF;
                                    monoasm! {self.jit,
                                        movq  rax, [r14 - (caller_ofs)];
                                        movq  [rsp - (16 + callee_ofs)], rax;
                                    }
                                }
                                None => {
                                    monoasm! {self.jit,
                                        movq  [rsp - (16 + callee_ofs)], 0;
                                    }
                                }
                            }
                        }
                        // TODO: We must care about a rest keyword paramter.
                    }
                    if !callsite.hash_splat_pos.is_empty() {
                        monoasm! {self.jit,
                            lea  rcx, [rsp - (16 + LBP_SELF)];
                            subq rsp, 4088;
                            pushq rdi;
                            movq rdi, rbx;
                            movq rsi, r12;
                            movl rdx, (callid.get());
                            movl r8, (callee_func_id.get());
                            movq rax, (runtime::jit_handle_hash_splat);
                            call rax;
                            popq rdi;
                            addq rsp, 4088;
                        }
                    }
                } else {
                    monoasm! {self.jit,
                        movq rcx, (func_data as *const _ as u64);
                        movl rdx, (callid.get());
                    }
                    self.handle_arguments();
                    self.jit_handle_error(ctx, pc);
                }
            }
            _ => {}
        }
        monoasm!(self.jit,
            movq rdx, rdi;
            // set meta.
            movq rax, (func_data.meta.get());
            movq [rsp - (16 + LBP_META)], rax;
            // set pc.
            movq r13, (func_data.pc().get_u64());
        );
        self.call_codeptr(func_data.codeptr.unwrap());
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &BBContext,
        fnstore: &FnStore,
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
        self.jit_set_arguments(args, len, true, &fnstore[callid]);

        monoasm! { self.jit,
            movq rsi, [r13 + (FUNCDATA_OFFSET_PC)];
        }
        self.block_arg_expand();

        monoasm! { self.jit,
            movq rcx, r13;
            movl rdx, (callid.get()); // CallSiteId
        }
        self.handle_arguments();
        self.jit_handle_error(ctx, pc);
        monoasm! { self.jit,
            // argument registers:
            //   rdx: args len
            //
            // global registers:
            //   rbx: &mut Interp
            //   r12: &mut Globals
            //   r13: pc
            //
            movq rdx, rdi;
            movq rax, [r13 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_OFFSET_PC)];
        };
        self.call_rax();
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
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
        callsite: &CallSiteInfo,
    ) {
        let MethodInfo {
            recv, args, len, ..
        } = method_info;
        // set self, len
        self.load_rax(recv);
        monoasm!(self.jit,
            movq [rsp - (16 + LBP_SELF)], rax;
        );
        self.jit_set_arguments(args, len, has_splat, callsite);
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
    /// ### destroy
    ///
    /// - caller save registers
    /// - r15
    ///
    fn jit_set_arguments(
        &mut self,
        args: SlotId,
        len: u16,
        has_splat: bool,
        callsite: &CallSiteInfo,
    ) {
        // set arguments
        if has_splat {
            monoasm!(self.jit,
                lea r15, [rsp - (16 + LBP_ARG0)];
                movq r8, (len);
                subq rsp, 1024;
            );
            for i in 0..len {
                let reg = args + i;
                if callsite.splat_pos.contains(&(i as usize)) {
                    self.load_rdi(reg);
                    monoasm! {self.jit,
                        movq rsi, r15;
                        movq rax, (expand_splat);
                        subq rsp, 8;
                        pushq r8;
                        call rax;
                        popq r8;
                        addq rsp, 8;
                        lea  r8, [r8 + rax * 1 - 1];
                        shlq rax, 3;
                        subq r15, rax;
                    }
                } else {
                    self.load_rax(reg);
                    monoasm! {self.jit,
                        movq [r15], rax;
                        subq r15, 8;
                    }
                }
            }
            monoasm!(self.jit,
                addq rsp, 1024;
                movq rdi, r8;
            );
        } else {
            for i in 0..len {
                let reg = args + i;
                self.load_rax(reg);
                monoasm! {self.jit,
                    movq [rsp - ((16 + LBP_ARG0 + 8 * (i as i64)) as i32)], rax;
                }
            }
            monoasm!(self.jit,
                movq rdi, (len);
            );
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
