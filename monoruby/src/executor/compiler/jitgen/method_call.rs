use super::*;

// ~~~text
// MethodCall
//  0   2   4   6    8  10  12  14
// +---+---+---+---++---+---+---+---+
// |callid |ret| op|| class |version|
// +---+---+---+---++---+---+---+---+
// 16  18  20  22   24  26  28  30
// +---+---+---+---++---+---+---+---+
// |len|arg|rcv| op||  fid  |       |
// +---+---+---+---++---+---+---+---+
// ~~~

const CALLSITE_ID: usize = 0;
//const CACHED_VERSION: usize = 12;
//const RECV_REG: usize = 20;
//const CACHED_FUNCID: usize = 24;

impl Codegen {
    pub(super) fn gen_inlinable(
        &mut self,
        ctx: &mut BBContext,
        callsite: &CallSiteInfo,
        inline_gen: InlineGen,
        pc: BcPc,
    ) {
        self.writeback_acc(ctx);
        let deopt = self.gen_side_deopt(pc, ctx);
        self.guard_version(pc, ctx, deopt);
        inline_gen(self, ctx, callsite, pc, deopt);
    }

    pub(super) fn gen_call(
        &mut self,
        store: &Store,
        ctx: &mut BBContext,
        fid: FuncId,
        callid: CallSiteId,
        pc: BcPc,
    ) {
        let CallSiteInfo { recv, ret, .. } = store[callid];
        self.fetch_slots(ctx, &[recv]);
        self.fetch_callargs(ctx, &store[callid]);
        ctx.release(ret);
        if store[callid].recv.is_zero() && ctx.self_value.class() != pc.cached_class() {
            // the cache is invalid because the receiver class is not matched.
            self.gen_call_not_cached(ctx, &store[callid], pc);
        } else {
            self.gen_call_cached(store, ctx, callid, fid, pc);
        }
    }

    ///
    /// generate JIT code for a method call which was cached.
    ///
    fn gen_call_cached(
        &mut self,
        store: &Store,
        ctx: &mut BBContext,
        callid: CallSiteId,
        fid: FuncId,
        pc: BcPc,
    ) {
        let CallSiteInfo {
            recv,
            args,
            len,
            ret,
            ..
        } = store[callid];
        let cached_class = pc.cached_class();
        let deopt = self.gen_side_deopt(pc, ctx);
        // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
        // Thus, we can omit a class guard.
        if !recv.is_zero() {
            self.load_rdi(recv);
            self.guard_class(cached_class, deopt);
        }
        self.guard_version(pc, ctx, deopt);
        match store[fid].kind {
            FuncKind::AttrReader { ivar_name } => {
                assert_eq!(0, len);
                assert!(store[callid].kw_len() == 0);
                assert!(store[callid].block_fid.is_none());
                assert!(store[callid].block_arg.is_none());
                if cached_class.is_always_frozen() {
                    if let Some(ret) = ret {
                        monoasm!( &mut self.jit,
                            movq rax, (NIL_VALUE);
                        );
                        self.store_rax(ret);
                    }
                } else {
                    self.load_rdi(recv);
                    let ivar_id = store[cached_class].get_ivarid(ivar_name);
                    self.attr_reader(ctx, ivar_name, ivar_id, ret);
                }
            }
            FuncKind::AttrWriter { ivar_name } => {
                assert_eq!(1, len);
                assert!(store[callid].kw_len() == 0);
                assert!(store[callid].block_fid.is_none());
                assert!(store[callid].block_arg.is_none());
                self.load_rdi(recv);
                let ivar_id = store[cached_class].get_ivarid(ivar_name);
                self.attr_writer(ctx, ivar_name, ivar_id, ret, args, pc);
            }
            FuncKind::Builtin { .. } => {
                self.method_call_cached(ctx, store, callid, fid, pc, cached_class, true);
            }
            FuncKind::ISeq(_) => {
                self.method_call_cached(ctx, store, callid, fid, pc, cached_class, false);
            }
        };
    }

    ///
    /// generate JIT code for a method call which was not cached.
    ///
    fn gen_call_not_cached(&mut self, ctx: &mut BBContext, callsite: &CallSiteInfo, pc: BcPc) {
        let has_splat = callsite.has_splat();
        let CallSiteInfo { id, recv, ret, .. } = *callsite;
        // argument registers:
        //   rdi: args len
        //
        let method_resolved = self.jit.label();
        let slow_path = self.jit.label();
        let raise = self.jit.label();
        let global_class_version = self.class_version;
        let xmm_using = ctx.get_xmm_using();
        self.writeback_acc(ctx);
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
            movq r13, ((pc + 1).get_u64());
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
        self.set_self_and_args(has_splat, callsite);

        monoasm! { &mut self.jit,
        movq rdx, [r13 + 8];    // FuncId
        }
        self.get_func_data();
        monoasm! { &mut self.jit,
            movq r15, rdx;
            // set meta.
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rax;
        }

        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
            movl rdx, (id.get()); // CallSiteId
        }
        self.handle_arguments();
        monoasm!( &mut self.jit,
            testq rax, rax;
            jeq raise;
        );

        monoasm! { &mut self.jit,
            movq rdx, rdi;
            // set codeptr
            movq rax, [r15 + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
        }
        self.call_rax();
        self.xmm_restore(&xmm_using);
        monoasm!( &mut self.jit,
            testq rax, rax;
            jeq raise;
        );
        if let Some(ret) = ret {
            self.store_rax(ret);
        }

        // slow path
        // r15: recv's class
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, (id.get()); // CallSiteId
            movq rax, (runtime::find_method);
            call rax;
            // FuncId was returned to rax.
            testq rax, rax;
            jeq raise;
            movl [r13 + 8], rax;

            movl rax, [rip + global_class_version];
            movl [r13 - 4], rax;
            movl [r13 - 8], r15;
            jmp method_resolved;
        );
        let raise = self.entry_raise;
        // raise error.
        monoasm!( &mut self.jit,
        raise:
            movq r13, ((pc + 1).get_u64());
            jmp raise;
        );
        self.jit.select_page(0);
    }

    fn attr_reader(
        &mut self,
        ctx: &mut BBContext,
        ivar_name: IdentId,
        ivar_id: Option<IvarId>,
        dst: Option<SlotId>,
    ) {
        let exit = self.jit.label();
        // rdi: base: Value
        if let Some(ivar_id) = ivar_id {
            if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
                let not_object = self.jit.label();
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                    // we don't know ty of the receiver in a compile time.
                    cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                    jne  not_object;
                    movq rax, [rdi + rsi * 8 + (RVALUE_OFFSET_KIND)];
                    movq rdi, (NIL_VALUE);
                    testq rax,rax;
                    cmoveqq rax, rdi;
                exit:
                );
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                    not_object:
                );
                self.load_ivar_heap_index();
                monoasm!( &mut self.jit,
                    jmp  exit;
                );
                self.jit.select_page(0);
            } else {
                monoasm!( &mut self.jit,
                    movl rsi, (ivar_id.get());
                    xorq rax, rax;
                    movl rdx, (OBJECT_INLINE_IVAR);
                    // we don't know ty of the receiver in a compile time.
                    cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                    cmoveqq rax, rdx;
                    subl rsi, rax;
                );
                self.load_ivar_heap_index();
            }
        } else {
            let xmm_using = ctx.get_xmm_using();
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
        self.save_rax_to_acc(ctx, dst);
    }

    ///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    /// - rsi: index
    ///
    /// #### out
    /// - rax: Value
    ///
    /// #### destroy
    /// - rdi, rdx
    ///
    fn load_ivar_heap_index(&mut self) {
        let exit = self.jit.label();
        monoasm!( &mut self.jit,
            movq rax, (NIL_VALUE);
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
            testq rdx, rdx;
            jz   exit;
            movq rdi, [rdx + (IVAR_TABLE_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rdx + (IVAR_TABLE_LEN)]; // len
            cmpq rdi, rsi;
            movq rdi, [rdx + (IVAR_TABLE_PTR)]; // ptr
            cmovgtq rax, [rdi + rsi * 8];
        exit:
        );
    }

    fn attr_writer(
        &mut self,
        ctx: &mut BBContext,
        ivar_name: IdentId,
        ivar_id: Option<IvarId>,
        dst: Option<SlotId>,
        args: SlotId,
        pc: BcPc,
    ) {
        let exit = self.jit.label();
        let no_inline = self.jit.label();
        let xmm_using = ctx.get_xmm_using();
        // rdi: base: Value
        if let Some(ivar_id) = ivar_id {
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
                movq rdx, [r14 - (conv(args))];
            );
            if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
                monoasm!( &mut self.jit,
                    // we don't know ty of the receiver in a compile time.
                    cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                    jne  no_inline;
                    movq [rdi + rsi * 8 + (RVALUE_OFFSET_KIND)], rdx;
                exit:
                );
                self.jit.select_page(1);
                self.jit.bind_label(no_inline);
                self.set_ivar(&xmm_using);
                self.jit_handle_error(ctx, pc);
                monoasm!( &mut self.jit,
                    jmp exit;
                );
                self.jit.select_page(0);
            } else {
                self.set_ivar(&xmm_using);
                self.jit_handle_error(ctx, pc);
            }
        } else {
            let slow_path = self.jit.label();
            let cache = self.jit.const_i64(-1);
            monoasm!( &mut self.jit,
                lea  rax, [rip + cache];    // cache.ivarid
                movl rsi, [rax + 4];
                cmpl rsi, (-1);
                jeq  slow_path;
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  no_inline;
                cmpl rsi, (OBJECT_INLINE_IVAR);
                jge no_inline;
                movq rax, [r14 - (conv(args))];  //val: Value
                movq [rdi + rsi * 8 + (RVALUE_OFFSET_KIND)], rax;
            exit:
            );

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
            monoasm!( &mut self.jit,
                movq rdx, [r14 - (conv(args))];   // val: Value
            );
            self.set_ivar(&xmm_using);
            self.jit_handle_error(ctx, pc);
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        }
        self.save_rax_to_acc(ctx, dst);
    }

    fn method_call_cached(
        &mut self,
        ctx: &mut BBContext,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        pc: BcPc,
        recv_classid: ClassId,
        native: bool,
    ) {
        let has_splat = store[callid].has_splat();
        let xmm_using = ctx.get_xmm_using();
        let dst = store[callid].ret;
        self.writeback_acc(ctx);
        self.xmm_save(&xmm_using);
        self.execute_gc();
        self.set_method_outer();
        self.set_self_and_args(has_splat, &store[callid]);
        let func_data = &store[callee_fid].data;
        monoasm! { &mut self.jit,
            // set meta.
            movq rax, (func_data.meta().get());
            movq [rsp - (16 + LBP_META)], rax;
        }
        // argument registers:
        //   rdi: args len
        match &store[callee_fid].kind {
            FuncKind::ISeq(info) => {
                let callsite = &store[callid];
                if info.is_block_style && info.reqopt_num() > 1 && callsite.pos_num == 1 {
                    self.single_arg_expand();
                }
                if info.pos_num() == info.req_num()
                    && !(info.key_num() == 0 && callsite.kw_len() != 0)
                {
                    // no optional param, no rest param.
                    if info.key_num() != 0 {
                        let CallSiteInfo {
                            kw_pos, kw_args, ..
                        } = callsite;
                        let callee_kw_pos = info.pos_num() + 1;
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
                            subq rsp, 4088;
                            pushq rdi;
                            movq rdi, rbx;
                            movq rsi, r12;
                            movl rdx, (callid.get());
                            movl r8, (callee_fid.get());
                            movq rax, (runtime::jit_handle_hash_splat);
                            call rax;
                            popq rdi;
                            addq rsp, 4088;
                        }
                    }
                } else {
                    monoasm! { &mut self.jit,
                        movl rdx, (callid.get());
                    }
                    self.handle_arguments();
                    self.jit_handle_error(ctx, pc);
                }
            }
            _ => {}
        }
        if native {
            monoasm! { &mut self.jit,
                movq rdx, rdi;
            }
            self.call_codeptr(func_data.codeptr().unwrap());
        } else {
            monoasm! { &mut self.jit,
                // set pc.
                movq r13, (func_data.pc().get_u64());
            }
            match store[callee_fid].get_jit_code(recv_classid) {
                Some(dest) => self.call_label(dest),
                None => self.call_codeptr(func_data.codeptr().unwrap()),
            };
        }

        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, dst);
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &mut BBContext,
        store: &Store,
        args: SlotId,
        len: u16,
        dst: Option<SlotId>,
        callid: CallSiteId,
        pc: BcPc,
    ) {
        self.fetch_callargs(ctx, &store[callid]);
        ctx.release(dst);
        self.writeback_acc(ctx);
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(&xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
        self.jit_handle_error(ctx, pc);
        monoasm! { &mut self.jit,
            lea  rdx, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_FUNCDATA))];
            movq rax, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_OUTER))];
            // rax <- outer_cfp, rdx <- &FuncData
        }

        self.set_block_self_outer();
        monoasm! { &mut self.jit,
            // r13 <- &FuncData
            movq r13, rdx;
            // set meta
            movq rdi, [r13 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rdi;
            // set block
            movq [rsp - (16 + LBP_BLOCK)], 0;
        };
        // set arguments
        self.jit_set_arguments(args, len, true, &store[callid]);

        monoasm! { &mut self.jit,
            movq rsi, [r13 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
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
            movq rax, [r13 + (FUNCDATA_CODEPTR)];
            // set pc
            movq r13, [r13 + (FUNCDATA_PC)];
        };
        self.call_rax();
        self.xmm_restore(&xmm_using);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, dst);
    }
}

impl Codegen {
    ///
    /// Version guard fro JIT.
    ///
    /// Check the cached class version, and jump to `side_exit` if the version is changed.
    ///
    /// ### destroy
    /// - caller save registers
    /// - r13
    ///
    fn guard_version(&mut self, pc: BcPc, ctx: &BBContext, side_exit: DestLabel) {
        assert_eq!(0, self.jit.get_page());
        let global_version = self.class_version;
        let unmatch = self.jit.label();
        let exit = self.jit.label();
        let deopt = self.jit.label();
        let cached_version = self.jit.const_i32(pc.cached_version() as i32);
        let cached_fid = self.jit.const_i32(pc.cached_fid().get() as i32);
        monoasm! { &mut self.jit,
            movl rax, [rip + cached_version];
            cmpl [rip + global_version], rax;
            jne  unmatch;
        exit:
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        unmatch:
            movq r13, (pc.as_ptr());
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 + (CALLSITE_ID)];  // CallSiteId
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<FuncId>
            movl rax, rax;
        }
        self.jit_handle_error(ctx, pc);
        monoasm! { &mut self.jit,
            cmpl [rip + cached_fid], rax;
            jne  deopt;
            movl rax, [rip + global_version];
            //movl [r13 + (CACHED_VERSION)], rax;
            movl [rip + cached_version], rax;
            jmp  exit;
        deopt:
            movq rdi, (Value::symbol(IdentId::get_id("__version_guard")).id());
            jmp  side_exit;
        }
        self.jit.select_page(0);
    }

    /// Set *self*, len, block, and arguments.
    ///
    /// ### out
    /// - rdi <- the number of arguments
    ///
    /// ### destroy
    /// - caller save registers
    fn set_self_and_args(&mut self, has_splat: bool, callsite: &CallSiteInfo) {
        let CallSiteInfo {
            args,
            pos_num,
            recv,
            ..
        } = *callsite;
        // set self, len
        self.load_rax(recv);
        monoasm!( &mut self.jit,
            movq [rsp - (16 + LBP_SELF)], rax;
        );
        if let Some(func_id) = callsite.block_fid {
            let bh = BlockHandler::from(func_id);
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], (bh.0.id());
            );
        } else if let Some(block) = callsite.block_arg {
            self.load_rax(block);
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], 0;
            );
        }
        self.jit_set_arguments(args, pos_num as u16, has_splat, callsite);
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
    ///
    fn jit_set_arguments(
        &mut self,
        args: SlotId,
        pos_num: u16,
        has_splat: bool,
        callsite: &CallSiteInfo,
    ) {
        // set arguments
        if has_splat {
            monoasm!( &mut self.jit,
                lea r8, [rsp - (16 + LBP_ARG0)];
                subq rsp, 4088;
                pushq r15;
                movq r15, r8;
                movq r8, (pos_num);
            );
            for i in 0..pos_num {
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
                popq r15;
                addq rsp, 4088;
                movq rdi, r8;
            );
        } else {
            for i in 0..pos_num {
                let reg = args + i;
                self.load_rax(reg);
                monoasm! { &mut self.jit,
                    movq [rsp - ((16 + LBP_ARG0 + 8 * (i as i64)) as i32)], rax;
                }
            }
            monoasm!( &mut self.jit,
                movq rdi, (pos_num);
            );
        }
    }
}

#[cfg(test)]
mod test {
    use crate::tests::*;
    #[test]
    fn polymorphic() {
        run_test_with_prelude(
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
        run_test(
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
        run_test(
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
        run_test_with_prelude(
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

    #[test]
    fn jit_attr_reader() {
        run_test_with_prelude(
            r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
            r###"
        class C
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
        );
        run_test_with_prelude(
            r###"
        x = C.new
        [x.a, x.b, x.c, x.d, x.e, x.f, x.g, x.h]
        "###,
            r###"
        class C < Array
          attr_reader :a, :b, :c, :d, :e, :f, :g, :h
          def initialize
            @a = 1
            @b = 2
            @c = 3
            @d = 4
            @e = 5
            @f = 6
            @g = 7
            @h = 8
          end
        end
        "###,
        );
    }

    #[test]
    fn deopt_method_recv_class() {
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
        run_test_once(
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
    fn attr_reader_in_different_class() {
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
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
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
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
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
        );
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.a, s.b, s.c, s.d, s.e, s.f, s.g, s.h, c.a, c.b, c.c, c.d, c.e, c.f, c.g, c.h]
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
                attr_reader :a, :b, :c, :d, :e, :f, :g, :h
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
                attr_reader :a, :b, :c, :c, :e, :f, :g, :h
            end
            
            "##,
        );
    }
}
