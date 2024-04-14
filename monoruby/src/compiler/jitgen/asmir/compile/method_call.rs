use super::*;

mod attr_accessor;
mod object_send;
mod r#yield;

// ~~~text
// MethodCall
//  0   2   4   6    8  10  12  14
// +---+---+---+---++---+---+---+---+
// |callid |ret| op||  fid  |   -   |
// +---+---+---+---++---+---+---+---+
// InlineCache
// 16  18  20  22   24  26  28  30
// +---+---+---+---++---+---+---+---+
// |pos|arg|rcv| op|| class |version|
// +---+---+---+---++---+---+---+---+
// ~~~
impl Codegen {
    /*///
    /// generate JIT code for a method call which was not cached.
    ///
    pub(super) fn send_not_cached(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        self_class: ClassId,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: DestLabel,
    ) {
        let callsite = &store[callid];
        // argument registers:
        //   rdi: args len
        //
        let resolved = self.jit.label();
        let slow_path = self.jit.label();
        let global_class_version = self.class_version;

        self.xmm_save(using_xmm);
        // r15 <- recv's class
        if callsite.recv.is_self() {
            // If recv is *self*, a recv's class is guaranteed to be ctx.self_class.
            monoasm!( &mut self.jit,
                movl r15, (self_class.u32());
            );
        } else {
            self.load_rdi(callsite.recv);
            let get_class = self.get_class;
            monoasm!( &mut self.jit,
                call get_class;
                movl r15, rax;  // r15: receiver's ClassId
            );
        }
        monoasm! { &mut self.jit,
            movq r13, (pc.u64());
            // check inline cache
            cmpl [r13 + (BC_OFFSET_CACHED_FUNCID)], 0;
            jeq  slow_path;
            // class guard
            cmpl r15, [r13 + (BC_OFFSET_CACHED_CLASS)];
            jne  slow_path;
            // version guard
            movl rax, [rip + global_class_version];
            cmpl [r13 + (BC_OFFSET_CACHED_VERSION)], rax;
            jne  slow_path;
        resolved:
            movl rdx, [r13 + (BC_OFFSET_CACHED_FUNCID)];    // FuncId
        }
        self.get_func_data();
        // r15 <- &FuncData

        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + 8];
            pushq rax;
            // set outer
            xorq rax, rax;
            pushq rax;
            // set meta.
            pushq [r15 + (FUNCDATA_META)];
        };
        // set block
        self.push_block(callsite.block_fid, callsite.block_arg);
        // set self
        monoasm!( &mut self.jit,
            pushq [r14 - (conv(callsite.recv))];
            addq  rsp, 64;
        );

        self.generic_call(callid, callsite.args, error);
        self.xmm_restore(using_xmm);
        self.handle_error(error);

        // slow path
        // r15: receiver's ClassId
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        slow_path:
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, (callid.get()); // CallSiteId
            movq rax, (runtime::find_method);
            call rax;
        }
        self.handle_error(error);
        monoasm! { &mut self.jit,
            // FuncId was returned to rax.
            movl [r13 + (BC_OFFSET_CACHED_FUNCID)], rax;

            movl rax, [rip + global_class_version];
            movl [r13 + (BC_OFFSET_CACHED_VERSION)], rax;
            movl [r13 + (BC_OFFSET_CACHED_CLASS)], r15;
            jmp resolved;
        }
        self.jit.select_page(0);
    }*/

    ///
    /// ### in
    /// rdi: numer of args.
    ///
    pub(super) fn send_cached(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        fid: FuncId,
        recv_class: ClassId,
        using_xmm: UsingXmm,
        error: DestLabel,
    ) {
        let caller = &store[callid];
        let callee = &store[fid];
        let (meta, codeptr, pc) = callee.get_data();
        self.setup_method_frame(meta, caller);
        self.copy_keyword_args(caller, callee);
        if callee.kw_rest().is_some() || !caller.hash_splat_pos.is_empty() {
            let offset = (16 + (LBP_ARG0 as usize) + 8 * callee.total_args() + 8) / 16 * 16;
            self.handle_hash_splat_kw_rest(callid, meta, offset, error);
        }

        self.call_fn(store, fid, recv_class, pc, codeptr);

        self.xmm_restore(using_xmm);
        self.handle_error(error);
    }

    ///
    /// Set up a callee frame for methods.
    ///
    /// ### in
    /// - r13: receiver
    ///
    /// ### destroy
    /// - rax
    ///
    fn setup_method_frame(&mut self, meta: Meta, callsite: &CallSiteInfo) {
        monoasm! { &mut self.jit,
            subq rsp, 32;
            // set outer
            xorq rax, rax;
            pushq rax;
            // set meta.
            movq rax, (meta.get());
            pushq rax;
        }
        // set block
        self.push_block(callsite.block_fid, callsite.block_arg);
        // set self
        monoasm! { &mut self.jit,
            pushq r13;
            addq rsp, 64;
        }
    }

    ///
    /// Push block.
    ///
    /// ### destroy
    /// - rax
    ///
    fn push_block(&mut self, block_fid: Option<FuncId>, block_arg: Option<SlotId>) {
        if let Some(func_id) = block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm!( &mut self.jit,
                movq rax, (bh.id());
                pushq rax;
            );
        } else if let Some(block) = block_arg {
            monoasm!( &mut self.jit,
                pushq [r14 - (conv(block))];
            );
        } else {
            monoasm!( &mut self.jit,
                xorq rax, rax;
                pushq rax;
            );
        }
    }

    fn call_fn(
        &mut self,
        store: &Store,
        fid: FuncId,
        recv_class: ClassId,
        pc: Option<BcPc>,
        codeptr: CodePtr,
    ) {
        self.set_lfp();
        self.push_frame();

        if let FuncKind::Builtin { .. } = store[fid].kind {
            self.call_codeptr(codeptr);
        } else {
            match store[fid].get_jit_code(recv_class) {
                Some(dest) => {
                    monoasm! { &mut self.jit,
                        call dest;
                    }
                }
                None => {
                    // set pc.
                    monoasm! { &mut self.jit,
                        movq r13, (pc.unwrap().u64());
                    }
                    self.call_codeptr(codeptr);
                }
            };
        }
        self.pop_frame();
    }

    fn call_codeptr(&mut self, codeptr: CodePtr) {
        let src_point = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            call (codeptr - src_point - 5);
        }
    }

    ///
    /// ### in
    /// - r15: &FuncData
    ///
    fn call_funcdata(&mut self) {
        monoasm! { &mut self.jit,
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            // push cfp
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rsi;
        }
        self.set_lfp();
        monoasm! { &mut self.jit,
            call [r15 + (FUNCDATA_CODEPTR)];
        }
        self.pop_frame();
    }

    ///
    /// Handle keyword arguments
    ///
    /// ### destroy
    /// - rax
    ///
    fn copy_keyword_args(&mut self, caller: &CallSiteInfo, callee: &FuncInfo) {
        let CallSiteInfo {
            kw_pos, kw_args, ..
        } = caller;
        let mut callee_ofs = (callee.pos_num() as i64 + 1) * 8 + LBP_SELF;
        for param_name in callee.kw_names() {
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

    fn handle_hash_splat_kw_rest(
        &mut self,
        callid: CallSiteId,
        meta: Meta,
        offset: usize,
        error: DestLabel,
    ) {
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movl rdx, (callid.get());
            movq rcx, (meta.get());
            lea  r8, [rsp - 16];   // callee_lfp
            subq rsp, (offset);
            movq rax, (jit_handle_hash_splat_kw_rest);
            call rax;
            addq rsp, (offset);
        }
        self.handle_error(error);
    }

    fn generic_call(&mut self, callid: CallSiteId, args: SlotId, error: DestLabel) {
        monoasm! { &mut self.jit,
            movl r8, (callid.get()); // CallSiteId
            lea  rdx, [r14 - (conv(args))];
        }
        self.generic_handle_arguments(runtime::jit_handle_arguments_no_block);
        self.handle_error(error);
        self.call_funcdata();
    }
}

///
/// Handle hash splat arguments and a keyword rest parameter.
///
extern "C" fn jit_handle_hash_splat_kw_rest(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    meta: Meta,
    callee_lfp: Lfp,
) -> Option<Value> {
    let caller_lfp = vm.cfp().lfp();
    match runtime::jit_hash_splat_kw_rest(globals, callid, callee_lfp, caller_lfp, meta) {
        Ok(_) => Some(Value::nil()),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}
