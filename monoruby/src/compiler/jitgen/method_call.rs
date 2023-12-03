use super::*;

// ~~~text
/// MethodCall
///  0   2   4   6    8  10  12  14
/// +---+---+---+---++---+---+---+---+
/// |callid |ret| op||pos|arg|rcv| - |
/// +---+---+---+---++---+---+---+---+
/// 16  18  20  22   24  26  28  30
/// +---+---+---+---++---+---+---+---+
/// |  fid  |   | op|| class |version|
/// +---+---+---+---++---+---+---+---+
// ~~~

const CALLSITE_ID: usize = 0;
const CACHED_CLASS: usize = 24;
const CACHED_VERSION: usize = 28;
const CACHED_FUNCID: usize = 16;

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
        let CallSiteInfo { ret, .. } = store[callid];
        let mut ir = AsmIr::new();
        ctx.fetch_callargs(&mut ir, &store[callid]);
        self.gen_code(ir);
        ctx.release(ret);
        if store[callid].recv.is_zero() && Some(ctx.self_value.class()) != pc.cached_class1() {
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
        let cached_class = pc.cached_class1().unwrap();
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
                assert!(store[callid].kw_num() == 0);
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
                assert!(store[callid].kw_num() == 0);
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
        let CallSiteInfo { id, recv, ret, .. } = *callsite;
        // argument registers:
        //   rdi: args len
        //
        let resolved = self.jit.label();
        let slow_path = self.jit.label();
        let global_class_version = self.class_version;
        let xmm_using = ctx.get_xmm_using();
        self.writeback_acc(ctx);
        self.xmm_save(xmm_using);
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
                movl r15, rax;  // r15: receiver's ClassId
            );
        }
        monoasm! { &mut self.jit,
            movq r13, (pc.get_u64());
            // check inline cache
            cmpl [r13 + (CACHED_FUNCID)], 0;
            jeq  slow_path;
            // class guard
            cmpl r15, [r13 + (CACHED_CLASS)];
            jne  slow_path;
            // version guard
            movl rax, [rip + global_class_version];
            cmpl [r13 + (CACHED_VERSION)], rax;
            jne  slow_path;
        resolved:
            movl rdx, [r13 + (CACHED_FUNCID)];    // FuncId
        }
        self.get_func_data();

        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + 8];
            pushq rax;
            // set outer
            movq rax, 0;
            pushq rax;
            // set meta.
            pushq [r15 + (FUNCDATA_META)];
        };
        // set block
        self.push_block(callsite);
        // set self
        monoasm!( &mut self.jit,
            pushq [r14 - (conv(callsite.recv))];
            addq  rsp, 64;
        );
        self.jit_set_arguments(callsite);

        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
            movl rdx, (id.get()); // CallSiteId
        }
        self.handle_arguments();
        self.jit_handle_error(ctx, pc);

        self.call_funcdata();

        self.xmm_restore(xmm_using);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, ret);

        // slow path
        // r15: receiver's ClassId
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, (id.get()); // CallSiteId
            movq rax, (runtime::find_method);
            call rax;
        }
        self.jit_handle_error(ctx, pc);
        monoasm! { &mut self.jit,
            // FuncId was returned to rax.
            movl [r13 + (CACHED_FUNCID)], rax;

            movl rax, [rip + global_class_version];
            movl [r13 + (CACHED_VERSION)], rax;
            movl [r13 + (CACHED_CLASS)], r15;
            jmp resolved;
        }
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
            self.get_ivar(xmm_using);
            self.jit.bind_label(exit);

            self.jit.select_page(1);
            self.jit.bind_label(slow_path);
            self.xmm_save(xmm_using);
            monoasm!( &mut self.jit,
                movq rsi, (ivar_name.get()); // IvarId
                movq rdx, r12; // &mut Globals
                lea  rcx, [rip + cache];
                movq rax, (get_instance_var_with_cache);
                call rax;
            );
            self.xmm_restore(xmm_using);
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
                self.set_ivar(xmm_using);
                self.jit_handle_error(ctx, pc);
                monoasm!( &mut self.jit,
                    jmp exit;
                );
                self.jit.select_page(0);
            } else {
                self.set_ivar(xmm_using);
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
            self.xmm_save(xmm_using);
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
            self.xmm_restore(xmm_using);
            monoasm!( &mut self.jit,
                jmp exit;
            );

            self.jit.bind_label(no_inline);
            monoasm!( &mut self.jit,
                movq rdx, [r14 - (conv(args))];   // val: Value
            );
            self.set_ivar(xmm_using);
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
        let xmm_using = ctx.get_xmm_using();
        let ret = store[callid].ret;
        self.writeback_acc(ctx);
        self.xmm_save(xmm_using);
        self.execute_gc();
        let callsite = &store[callid];
        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + 8];
            pushq rax;
            // set outer
            movq  rax, 0;
            pushq rax;
            // set meta.
            movq rax, (store[callee_fid].data.meta().get());
            pushq rax;
        }
        // set block
        self.push_block(callsite);
        // set self
        monoasm!( &mut self.jit,
            pushq [r14 - (conv(callsite.recv))];
            addq  rsp, 64;
        );
        self.jit_set_arguments(callsite);

        let func_data = &store[callee_fid].data;
        //   rdi: args len
        match &store[callee_fid].kind {
            FuncKind::ISeq(info) => {
                let callsite = &store[callid];
                if info.is_block_style() && info.reqopt_num() > 1 && callsite.pos_num == 1 {
                    self.single_arg_expand();
                }
                if info.optional_num() == 0 && !(info.key_num() == 0 && callsite.kw_num() != 0) {
                    // no optional param, no rest param.
                    if info.key_num() != 0 {
                        self.handle_keyword_args(callsite, info)
                        // TODO: We must care about a rest keyword paramter.
                    }
                    if !callsite.hash_splat_pos.is_empty() {
                        self.handle_hash_splat(callid, callee_fid)
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
        } else {
            monoasm! { &mut self.jit,
                // set pc.
                movq r13, (func_data.pc().get_u64());
            }
        }

        monoasm!( &mut self.jit,
            // push cfp
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rsi;
            // set lfp
            lea  r14, [rsp - 16];
            movq [r14 - (BP_LFP)], r14;
        );

        if native {
            let codeptr = func_data.codeptr().unwrap();
            self.call_codeptr(codeptr);
        } else {
            match store[callee_fid].get_jit_code(recv_classid) {
                Some(dest) => {
                    monoasm! { &mut self.jit,
                        call dest;
                    }
                }
                None => {
                    let codeptr = func_data.codeptr().unwrap();
                    self.call_codeptr(codeptr);
                }
            };
        }
        self.pop_frame();

        self.xmm_restore(xmm_using);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, ret);
    }

    pub(super) fn gen_yield(
        &mut self,
        ctx: &mut BBContext,
        store: &Store,
        callid: CallSiteId,
        pc: BcPc,
    ) {
        let CallSiteInfo { ret, .. } = store[callid];
        let mut ir = AsmIr::new();
        ctx.fetch_callargs(&mut ir, &store[callid]);
        self.gen_code(ir);
        ctx.release(ret);
        self.writeback_acc(ctx);
        let xmm_using = ctx.get_xmm_using();
        self.xmm_save(xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::get_yield_data);
            call rax;
        }
        self.jit_handle_error(ctx, pc);
        monoasm! { &mut self.jit,
            lea  r15, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_FUNCDATA))];
            movq rdi, [rax + ((RVALUE_OFFSET_KIND as i64 + PROCINNER_OUTER))];
            // rdi <- outer_cfp, r15 <- &FuncData
        }

        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + 8];
            pushq rax;
            // set outer
            lea  rax, [rdi - (LBP_OUTER)];
            pushq rax;
            // set meta
            pushq [r15 + (FUNCDATA_META)];
            // set block
            movq rax, 0;
            pushq rax;
            // set self
            pushq [rdi - (LBP_SELF)];
            addq  rsp, 64;
        };
        // set arguments
        self.jit_set_arguments(&store[callid]);

        monoasm! { &mut self.jit,
            movq rsi, [r15 + (FUNCDATA_PC)];
        }
        self.block_arg_expand();

        monoasm! { &mut self.jit,
            movl rdx, (callid.get()); // CallSiteId
        }
        self.handle_arguments();
        self.jit_handle_error(ctx, pc);

        self.call_funcdata();

        self.xmm_restore(xmm_using);
        self.jit_handle_error(ctx, pc);
        self.save_rax_to_acc(ctx, ret);
    }

    fn push_block(&mut self, callsite: &CallSiteInfo) {
        if let Some(func_id) = callsite.block_fid {
            let bh = BlockHandler::from(func_id);
            monoasm!( &mut self.jit,
                movq rax, (bh.0.id());
                pushq rax;
            );
        } else if let Some(block) = callsite.block_arg {
            monoasm!( &mut self.jit,
                pushq [r14 - (conv(block))];
            );
        } else {
            monoasm!( &mut self.jit,
                movq rax, 0;
                pushq rax;
            );
        }
    }

    fn call_codeptr(&mut self, codeptr: CodePtr) {
        let src_point = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            call (codeptr - src_point - 5);
        }
    }

    fn call_funcdata(&mut self) {
        monoasm! { &mut self.jit,
            movq rdx, rdi;
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            // push cfp
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rsi;
            // set lfp
            lea  r14, [rsp - 16];
            movq [r14 - (BP_LFP)], r14;
            call [r15 + (FUNCDATA_CODEPTR)];
        }
        self.pop_frame();
    }

    fn handle_keyword_args(&mut self, callsite: &CallSiteInfo, info: &ISeqInfo) {
        let CallSiteInfo {
            kw_pos, kw_args, ..
        } = callsite;
        let mut callee_ofs = (info.pos_num() as i64 + 1) * 8 + LBP_SELF;
        for param_name in &info.args.kw_names {
            match kw_args.get(param_name) {
                Some(caller) => {
                    let caller_ofs = (kw_pos.0 as i64 + *caller as i64) * 8 + LBP_SELF;
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
            callee_ofs += 8;
        }
    }

    fn handle_hash_splat(&mut self, callid: CallSiteId, callee_fid: FuncId) {
        monoasm! { &mut self.jit,
            lea  rcx, [rsp - (16 + LBP_SELF)];
            subq rsp, 4088;
            pushq rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());
            movl r8, (callee_fid.get());
            movq rax, (jit_handle_hash_splat);
            call rax;
            popq rdi;
            addq rsp, 4088;
        }
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
        let cached_version = self.jit.const_i32((pc + 1).cached_version() as i32);
        let cached_fid = if let Some(fid) = pc.cached_fid() {
            fid.get()
        } else {
            0
        };
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
            cmpl rax, (cached_fid);
            jne  deopt;
            movl rax, [rip + global_version];
            movl [rip + cached_version], rax;
            jmp  exit;
        deopt:
            movq rdi, (Value::symbol(IdentId::get_id("__version_guard")).id());
            jmp  side_exit;
        }
        self.jit.select_page(0);
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
    fn jit_set_arguments(&mut self, callsite: &CallSiteInfo) {
        let CallSiteInfo { args, pos_num, .. } = *callsite;
        let pos_num = pos_num as u16;
        // set arguments
        if callsite.has_splat() {
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

extern "C" fn jit_handle_hash_splat(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    callee_reg: *mut Option<Value>,
    callee_func_id: FuncId,
) {
    let callsite = &globals.store[callid];
    let CallSiteInfo { hash_splat_pos, .. } = callsite;
    let info = globals.store[callee_func_id].as_ruby_func();
    let callee_kw_pos = info.pos_num() + 1;
    for (id, param_name) in info.args.kw_names.iter().enumerate() {
        for hash in hash_splat_pos {
            unsafe {
                let h = vm.register(hash.0 as usize).unwrap();
                // We must check whether h is a hash.
                if let Some(v) = h.as_hash().get(Value::symbol(*param_name)) {
                    *callee_reg.sub(callee_kw_pos + id) = Some(v);
                }
            }
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
