use super::*;

impl Codegen {
    pub(super) fn gen_inlinable(
        &mut self,
        ctx: &mut BBContext,
        method_info: &MethodInfo,
        inline_gen: InlineGen,
        pc: BcPc,
    ) {
        let (_, version) = pc.class_version();
        let deopt = self.gen_side_deopt(pc, ctx);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        self.guard_version(version, deopt);
        inline_gen(self, ctx, method_info, pc, deopt);
    }

    pub(super) fn gen_call(
        &mut self,
        store: &Store,
        ctx: &BBContext,
        info: MethodInfo,
        callid: CallSiteId,
        block: Option<SlotId>,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            func_data, recv, ..
        } = info;
        if func_data.is_some() {
            let cached = InlineCached::new(pc);
            if recv.is_zero() && ctx.self_value.class() != cached.class_id {
                self.gen_call_not_cached(store, ctx, info, callid, block, pc, has_splat);
            } else {
                self.gen_call_cached(store, ctx, callid, info, block, cached, pc, has_splat);
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
        store: &Store,
        ctx: &BBContext,
        callid: CallSiteId,
        method_info: MethodInfo,
        block: Option<SlotId>,
        cached: InlineCached,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo {
            recv,
            len,
            func_data,
            ret,
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
        match store[func_id].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, len);
                assert!(block.is_none());
                assert!(store[callid].kw_args.is_empty());
                assert!(store[callid].block_func_id.is_none());
                if cached.class_id.is_always_frozen() {
                    if !ret.is_zero() {
                        monoasm!( &mut self.jit,
                            movq rax, (NIL_VALUE);
                        );
                        self.store_rax(ret);
                    }
                } else {
                    if !self_in_rdi_flag {
                        self.load_rdi(recv);
                    }
                    let ivar_id = store[cached.class_id].get_ivarid(ivar_name);
                    self.attr_reader(ctx, ivar_name, ivar_id, ret);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, len);
                assert!(block.is_none());
                assert!(store[callid].kw_args.is_empty());
                assert!(store[callid].block_func_id.is_none());
                if !self_in_rdi_flag {
                    self.load_rdi(recv);
                }
                let ivar_id = store[cached.class_id].get_ivarid(ivar_name);
                self.attr_writer(ctx, ivar_name, ivar_id, ret, method_info.args, pc);
            }
            FuncKind::Builtin { abs_address } => {
                if !self_in_rdi_flag {
                    self.load_rdi(recv);
                }
                self.native_call(
                    ctx,
                    method_info,
                    func_id,
                    store[callid].block_func_id,
                    block,
                    abs_address,
                    pc,
                );
            }
            FuncKind::ISeq(_) => {
                self.method_call_cached(
                    ctx,
                    store,
                    callid,
                    method_info,
                    block,
                    pc,
                    has_splat,
                    cached.class_id,
                );
            }
        };
    }

    ///
    /// generate JIT code for a method call which was not cached.
    ///
    fn gen_call_not_cached(
        &mut self,
        store: &Store,
        ctx: &BBContext,
        method_info: MethodInfo,
        callid: CallSiteId,
        block: Option<SlotId>,
        pc: BcPc,
        has_splat: bool,
    ) {
        let MethodInfo { recv, ret, .. } = method_info;
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
            monoasm!( &mut self.jit,
                movl r15, (ctx.self_value.class().0);
            );
        } else {
            self.load_rdi(recv);
            let get_class = self.get_class;
            monoasm!( &mut self.jit,
                call get_class;
                movl r15, rax;  // r15: receiver class_id
            );
        }
        monoasm! { &mut self.jit,
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
        self.set_self_and_args(method_info, block, has_splat, &store[callid]);

        monoasm! { &mut self.jit,
            // set meta.
            movq r15, [r13 + 8];    // &FuncData
            movq rax, [r15 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rax;
        }

        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_OFFSET_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
            movq rcx, r15;
            movl rdx, (callid.get()); // CallSiteId
        }
        self.handle_arguments();
        monoasm!( &mut self.jit,
            testq rax, rax;
            jeq raise;
        );

        monoasm! { &mut self.jit,
            movq rdx, rdi;
            // set codeptr
            movq rax, [r15 + (FUNCDATA_OFFSET_CODEPTR)];
            // set pc
            movq r13, [r15 + (FUNCDATA_OFFSET_PC)];
        }
        self.call_rax();
        self.xmm_restore(&xmm_using);
        monoasm!( &mut self.jit,
            testq rax, rax;
            jeq raise;
        );
        if !ret.is_zero() {
            self.store_rax(ret);
        }

        // slow path
        // r15: recv's class
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
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
        monoasm!( &mut self.jit,
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
            if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                    cmpw [rdi + (RVALUE_OFFSET_KIND)], (ObjKind::OBJECT);
                    jne  no_inline;
                    movq rax, [rdi + rsi * 8 + 16];
                    testq rax,rax;
                    jnz  exit;
                    movq rax, (NIL_VALUE);
                exit:
                );
                self.jit.select_page(1);
                self.jit.bind_label(no_inline);
                self.get_ivar(&xmm_using);
                monoasm!( &mut self.jit,
                    jmp  exit;
                );
                self.jit.select_page(0);
            } else {
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                );
                self.get_ivar(&xmm_using);
            }
        } else {
            let slow_path = self.jit.label();
            let cache = self.jit.const_i64(-1);
            monoasm!( &mut self.jit,
                lea  rax, [rip + cache];
                movl rsi, [rax + 4];
                cmpl rsi, (-1);
                jeq  slow_path;
            );
            self.get_ivar(&xmm_using);
            self.jit.bind_label(exit);

            self.jit.select_page(1);
            self.jit.bind_label(slow_path);
            self.xmm_save(&xmm_using);
            monoasm!( &mut self.jit,
                movq rsi, (ivar_name.get()); // IvarId
                movq rdx, r12; // &mut Globals
                lea  rcx, [rip + cache];
                movq rax, (get_instance_var_with_cache);
                call rax;
            );
            self.xmm_restore(&xmm_using);
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
        if !ret.is_zero() {
            self.store_rax(ret);
        }
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
            if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                    cmpw [rdi + (RVALUE_OFFSET_KIND)], (ObjKind::OBJECT);
                    jne  no_inline;
                    movq rax, [r14 - (conv(args))];  //val: Value
                    movq [rdi + rsi * 8 + 16], rax;
                exit:
                );
                self.jit.select_page(1);
                self.jit.bind_label(no_inline);
                self.set_ivar(args, &xmm_using);
                self.jit_handle_error(ctx, pc);
                monoasm!( &mut self.jit,
                    jmp exit;
                );
                self.jit.select_page(0);
            } else {
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                );
                self.set_ivar(args, &xmm_using);
                self.jit_handle_error(ctx, pc);
            }
            if !ret.is_zero() {
                self.store_rax(ret);
            }
        } else {
            let slow_path = self.jit.label();
            let cache = self.jit.const_i64(-1);
            monoasm!( &mut self.jit,
                lea  rax, [rip + cache];    // cache.ivarid
                movl rsi, [rax + 4];
                cmpl rsi, (-1);
                jeq  slow_path;
                cmpw [rdi + (RVALUE_OFFSET_KIND)], (ObjKind::OBJECT);
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
            monoasm!( &mut self.jit,
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
            monoasm!( &mut self.jit,
                jmp exit;
            );

            self.jit.bind_label(no_inline);
            self.set_ivar(args, &xmm_using);
            self.jit_handle_error(ctx, pc);
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
    }

    fn native_call(
        &mut self,
        ctx: &BBContext,
        method_info: MethodInfo,
        func_id: FuncId,
        block_func_id: Option<FuncId>,
        block: Option<SlotId>,
        abs_address: u64,
        pc: BcPc,
    ) {
        // rdi : receiver(self)
        let MethodInfo { args, len, ret, .. } = method_info;
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm!( &mut self.jit,
            movq rax, (Meta::native(func_id, 0 /* for GC */ as _).get());
            movq [rsp - (16 + LBP_META)], rax;
            movq [rsp - (16 + LBP_SELF)], rdi;  // self: Value
            movq [rsp - (16 + LBP_OUTER)], 0;
        );
        if let Some(block_func_id) = block_func_id {
            let bh = BlockHandler::from(block_func_id);
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], (bh.0.get());
            );
        } else {
            match block {
                Some(block) => {
                    monoasm!( &mut self.jit,
                        movq rax, [r14 - (conv(block))];
                        movq [rsp - (16 + LBP_BLOCK)], rax;
                    );
                }
                None => {
                    monoasm!( &mut self.jit,
                        movq [rsp - (16 + LBP_BLOCK)], 0;
                    );
                }
            }
        }
        monoasm!( &mut self.jit,
            lea  rcx, [r14 - (conv(args))];  // args: *const Value
        );
        let stack_offset = (LBP_SELF + 31) & !0xf;
        let return_address = self.jit.label();
        self.push_frame();
        monoasm!( &mut self.jit,
            // set lfp
            lea  rdx, [rsp - 16];
            movq [rsp - (16 + BP_LFP)], rdx;
            movq rdi, rbx;  // &mut Interp
            movq rsi, r12;  // &mut Globals
            movq r8, (len); // len
            lea  rax, [rip + return_address];
            pushq rax;
            pushq rbp;
            subq rsp, (stack_offset);
            movq rax, (abs_address);
            call rax;
        return_address:
            addq rsp, (stack_offset + 16);
        );
        self.pop_frame();
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    fn method_call_cached(
        &mut self,
        ctx: &BBContext,
        store: &Store,
        callid: CallSiteId,
        method_info: MethodInfo,
        block: Option<SlotId>,
        pc: BcPc,
        has_splat: bool,
        recv_classid: ClassId,
    ) {
        let func_data = method_info.func_data.unwrap();
        let xmm_using = ctx.get_xmm_using();
        let ret = method_info.ret;
        self.xmm_save(&xmm_using);
        self.execute_gc();
        self.set_method_outer();
        self.set_self_and_args(method_info, block, has_splat, &store[callid]);
        // argument registers:
        //   rdi: args len
        let callee_func_id = func_data.meta.func_id();
        match &store[callee_func_id].kind {
            FuncKind::ISeq(info) => {
                let callsite = &store[callid];
                if info.is_block_style && info.reqopt_num() > 1 && callsite.pos_num == 1 {
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
                                    monoasm! { &mut self.jit,
                                        movq  rax, [r14 - (caller_ofs)];
                                        movq  [rsp - (16 + callee_ofs)], rax;
                                    }
                                }
                                None => {
                                    monoasm! { &mut self.jit,
                                        movq  [rsp - (16 + callee_ofs)], 0;
                                    }
                                }
                            }
                        }
                        // TODO: We must care about a rest keyword paramter.
                    }
                    if !callsite.hash_splat_pos.is_empty() {
                        monoasm! { &mut self.jit,
                            lea  rcx, [rsp - (16 + LBP_SELF)];
                            subq rsp, 1016;
                            pushq rdi;
                            movq rdi, rbx;
                            movq rsi, r12;
                            movl rdx, (callid.get());
                            movl r8, (callee_func_id.get());
                            movq rax, (runtime::jit_handle_hash_splat);
                            call rax;
                            popq rdi;
                            addq rsp, 1016;
                        }
                    }
                } else {
                    monoasm! { &mut self.jit,
                        movq rcx, (func_data as *const _ as u64);
                        movl rdx, (callid.get());
                    }
                    self.handle_arguments();
                    self.jit_handle_error(ctx, pc);
                }
            }
            _ => {}
        }
        monoasm!( &mut self.jit,
            // set meta.
            movq rax, (func_data.meta.get());
            movq [rsp - (16 + LBP_META)], rax;
            // set pc.
            movq r13, (func_data.pc().get_u64());
        );
        match store[callee_func_id].get_jit_code(recv_classid) {
            Some(dest) => self.call_label(dest),
            None => self.call_codeptr(func_data.codeptr.unwrap()),
        };
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        if !ret.is_zero() {
            self.store_rax(ret);
        }
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &BBContext,
        store: &Store,
        args: SlotId,
        len: u16,
        ret: SlotId,
        callid: CallSiteId,
        pc: BcPc,
    ) {
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        let no_block = self.no_block;
        monoasm! { &mut self.jit,
            subq rsp, 32;
            lea  rdi, [rsp];
            movq rsi, [rbx];
            movq rdx, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
            lea  rdx, [rax + 8];
            movq rax, [rax];
            // rax <- outer_cfp, rdx <- &FuncData
            testq rax, rax;
            jz  no_block;
        }

        self.set_block_self_outer();
        monoasm! { &mut self.jit,
            // r13 <- &FuncData
            movq r13, rdx;
            // set meta
            movq rdi, [r13 + (FUNCDATA_OFFSET_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
        };
        // set arguments
        self.jit_set_arguments(args, len, true, &store[callid]);

        monoasm! { &mut self.jit,
            movq rsi, [r13 + (FUNCDATA_OFFSET_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
            movq rcx, r13;
            movl rdx, (callid.get()); // CallSiteId
        }
        self.handle_arguments();
        self.jit_handle_error(ctx, pc);
        monoasm! { &mut self.jit,
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
        monoasm! { &mut self.jit,
            addq rsp, 32;
        }
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
        let cont = self.jit.label();
        monoasm!( &mut self.jit,
            cmpl [rip + global_class_version], (cached_version);
            je   cont;
            movq rdi, (Value::symbol(IdentId::get_id("__version_guard")).get());
            jmp  side_exit;
        cont:
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
        monoasm!( &mut self.jit,
            movq [rsp - (16 + LBP_SELF)], rax;
        );
        self.jit_set_arguments(args, len, has_splat, callsite);
        // set block
        if let Some(func_id) = callsite.block_func_id {
            let bh = BlockHandler::from(func_id);
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], (bh.0.get());
            );
        } else if let Some(block) = block {
            self.load_rax(block);
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], rax;
            );
        } else {
            monoasm!( &mut self.jit,
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
            monoasm!( &mut self.jit,
                lea r15, [rsp - (16 + LBP_ARG0)];
                movq r8, (len);
                subq rsp, 1024;
            );
            for i in 0..len {
                let reg = args + i;
                if callsite.splat_pos.contains(&(i as usize)) {
                    self.load_rdi(reg);
                    monoasm! { &mut self.jit,
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
                    monoasm! { &mut self.jit,
                        movq [r15], rax;
                        subq r15, 8;
                    }
                }
            }
            monoasm!( &mut self.jit,
                addq rsp, 1024;
                movq rdi, r8;
            );
        } else {
            for i in 0..len {
                let reg = args + i;
                self.load_rax(reg);
                monoasm! { &mut self.jit,
                    movq [rsp - ((16 + LBP_ARG0 + 8 * (i as i64)) as i32)], rax;
                }
            }
            monoasm!( &mut self.jit,
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

    #[test]
    fn attr_accessor() {
        tests::run_test_with_prelude(
            r##"
            x = [C.new, B.new, A.new]
            res = []
            for e in x
                e.a += 1000.0
                e.b += 1000.0
                e.c += 1000.0
                res << e.a
                res << e.b
                res << e.c
            end
            res
            "##,
            r##"
            class C
              def initialize
                @a = 1
                @b = 2
                @c = 3
              end
              attr_accessor :a, :b, :c
            end
            class B < C
              def initialize
                @b = 10
                @c = 20
                @a = 30
              end
              attr_accessor :a, :b, :c
            end
            class A < B
              def initialize
                @c = 100
                @a = 200
                @b = 300
              end
              attr_accessor :a, :b, :c
            end
        "##,
        );
    }
}
