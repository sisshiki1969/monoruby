use super::*;

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
    ///
    /// Set req, opt and rest arguments.
    ///
    /// ### out
    /// - rax: Some(Value)
    /// - rdi: the number of arguments
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn jit_set_arguments(
        &mut self,
        callid: CallSiteId,
        args: SlotId,
        offset: usize,
        meta: Meta,
    ) {
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());
            lea  rcx, [r14 - (conv(args))];
            lea  r8, [rsp - (RSP_LOCAL_FRAME)];   // callee_lfp
            movq r9, (meta.get());
            subq rsp, (offset);
            movq rax, (crate::runtime::jit_generic_set_arguments);
            call rax;
            addq rsp, (offset);
        }
    }

    /*///
    /// generate JIT code for a method call which was not cached.
    ///
    pub(super) fn send_not_cached(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        self_class: ClassId,
        pc: BytecodePtr,
        error: DestLabel,
    ) -> CodePtr {
        let callsite = &store[callid];
        let resolved = self.jit.label();
        let slow_path = self.jit.label();
        let global_class_version = self.class_version;

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
            movq r13, (pc.as_ptr());
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
            lea   rax, [rsp + (24 - RSP_LOCAL_FRAME)];
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

        let return_addr = self.generic_call(callid, callsite.args, error);

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
        self.handle_error(&error);
        monoasm! { &mut self.jit,
            // FuncId was returned to rax.
            movl [r13 + (BC_OFFSET_CACHED_FUNCID)], rax;

            movl rax, [rip + global_class_version];
            movl [r13 + (BC_OFFSET_CACHED_VERSION)], rax;
            movl [r13 + (BC_OFFSET_CACHED_CLASS)], r15;
            jmp resolved;
        }
        self.jit.select_page(0);

        return_addr
    }*/

    ///
    /// ### in
    /// - r13: receiver: Value.
    ///
    pub(super) fn gen_send(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        error: &DestLabel,
    ) -> CodePtr {
        let caller = &store[callid];
        let callee = &store[callee_fid];
        let (meta, codeptr, pc) = callee.get_data();
        self.setup_method_frame(meta, caller);
        self.setup_keyword_args(callid, caller, callee, error);
        self.do_call(store, callee, codeptr, recv_class, pc)
    }

    ///
    /// ### in
    /// rdi: numer of args.
    ///
    pub(super) fn gen_send_specialized(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        entry_label: DestLabel,
        patch_point: Option<DestLabel>,
        error: &DestLabel,
    ) -> CodePtr {
        let caller = &store[callid];
        let callee = &store[callee_fid];
        let meta = callee.meta();
        self.setup_method_frame(meta, caller);
        self.setup_keyword_args(callid, caller, callee, error);
        self.do_specialized_call(entry_label, patch_point)
    }

    ///
    /// ### in
    /// rdi: numer of args.
    ///
    pub(super) fn gen_binop_cached(
        &mut self,
        store: &Store,
        callee_fid: FuncId,
        recv_class: ClassId,
    ) -> CodePtr {
        let callee = &store[callee_fid];
        let (meta, codeptr, pc) = callee.get_data();
        monoasm! { &mut self.jit,
            subq rsp, 32;
            // set outer
            xorq rax, rax;
            pushq rax;
            // set meta.
            movq rax, (meta.get());
            pushq rax;
            // set block
            xorq rax, rax;
            pushq rax;
            // set self
            pushq r13;
            addq rsp, 64;
        }
        self.do_call(store, callee, codeptr, recv_class, pc)
    }

    pub(super) fn gen_yield(
        &mut self,
        callid: CallSiteId,
        using_xmm: UsingXmm,
        error: &DestLabel,
    ) -> CodePtr {
        self.xmm_save(using_xmm);
        self.get_proc_data();
        self.handle_error(&error);
        // rax <- outer, rdx <- FuncId
        monoasm! { &mut self.jit,
            movq rdi, rax;
        }
        // rdi <- outer, rdx <- FuncId
        self.get_func_data();
        // rdi <- outer, r15 <- &FuncData

        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + (24 - RSP_LOCAL_FRAME)];
            pushq rax;
            // set outer
            pushq rdi;
            // set meta
            pushq [r15 + (FUNCDATA_META)];
            // set block
            xorq rax, rax;
            pushq rax;
            // set self
            pushq [rdi - (LFP_SELF)];
            addq  rsp, 64;
        };

        let return_addr = self.generic_call(callid, &error);
        self.xmm_restore(using_xmm);
        self.handle_error(&error);
        return_addr
    }

    pub(super) fn gen_yield_specialized(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        block_iseq: ISeqId,
        block_entry: DestLabel,
        error: &DestLabel,
    ) -> CodePtr {
        let caller = &store[callid];
        let block_fid = store[block_iseq].func_id();
        let callee = &store[block_fid];
        let meta = callee.meta();
        self.setup_yield_frame(meta);
        self.setup_keyword_args(callid, caller, callee, error);
        self.do_specialized_call(block_entry, None)
    }

    ///
    /// Set up a callee method frame for send.
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
        if let Some(func_id) = callsite.block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm!( &mut self.jit,
                movq rax, (bh.id());
                pushq rax;
            );
        } else if let Some(block) = callsite.block_arg {
            monoasm!( &mut self.jit,
                movq rax, [r14 - (conv(block))];
                pushq rax;
            );
        } else {
            monoasm!( &mut self.jit,
                xorq rax, rax;
                pushq rax;
            );
        }
        // set self
        monoasm! { &mut self.jit,
            pushq r13;
            addq rsp, 64;
        }
    }

    ///
    /// Set up a callee block frame for yield.
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    fn setup_yield_frame(&mut self, meta: Meta) {
        monoasm! { &mut self.jit,
            movq rdi, [rbx + (EXECUTOR_CFP)];
            movq rdi, [rdi];
            movq rdi, [rdi - (CFP_LFP)];
            // rdi <- outer LFP
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + (24 - RSP_LOCAL_FRAME)];
            pushq rax;
            // set outer
            pushq rdi;
            // set meta
            movq  rax, (meta.get());
            pushq rax;
            // set block
            xorq rax, rax;
            pushq rax;
            // set self
            pushq [rdi - (LFP_SELF)];
            addq  rsp, 64;
        };
    }

    ///
    /// Set up keyword arguments for callee.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn setup_keyword_args(
        &mut self,
        callid: CallSiteId,
        caller: &CallSiteInfo,
        callee: &FuncInfo,
        error: &DestLabel,
    ) {
        let meta = callee.meta();
        self.copy_keyword_args(caller, callee);
        if callee.kw_rest().is_some() || !caller.hash_splat_pos.is_empty() {
            let offset = callee.get_offset();
            self.handle_hash_splat_kw_rest(callid, meta, offset, error);
        }
    }

    fn do_call(
        &mut self,
        store: &Store,
        callee: &FuncInfo,
        codeptr: CodePtr,
        recv_class: ClassId,
        pc: Option<BytecodePtrBase>,
    ) -> CodePtr {
        self.set_lfp();
        self.push_frame();

        if let Some(iseq) = callee.is_iseq() {
            match store[iseq].get_jit_code(recv_class) {
                Some(dest) => {
                    monoasm! { &mut self.jit,
                        call dest;  // CALL_SITE
                    }
                }
                None => {
                    // set pc.
                    monoasm! { &mut self.jit,
                        movq r13, (pc.unwrap().as_ptr());
                    }
                    self.call_codeptr(codeptr);
                }
            };
        } else {
            self.call_codeptr(codeptr)
        }
        let return_addr = self.jit.get_current_address();

        self.pop_frame();
        return_addr
    }

    fn do_specialized_call(&mut self, entry: DestLabel, patch_point: Option<DestLabel>) -> CodePtr {
        self.set_lfp();
        self.push_frame();

        if let Some(patch) = patch_point {
            self.jit.bind_label(patch);
        }
        monoasm! { &mut self.jit,
            call entry;    // CALL_SITE
        }
        let return_addr = self.jit.get_current_address();

        self.pop_frame();
        return_addr
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

    fn call_codeptr(&mut self, codeptr: CodePtr) {
        let src_point = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            call (codeptr - src_point - 5); // CALL_SITE
        }
    }

    ///
    /// ### in
    /// - r15: &FuncData
    ///
    fn call_funcdata(&mut self) -> CodePtr {
        monoasm! { &mut self.jit,
            // push cfp
            lea  rsi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rsi;
        }
        self.set_lfp();
        monoasm! { &mut self.jit,
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
        }
        let return_addr = self.jit.get_current_address();
        self.pop_frame();
        return_addr
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
        let mut callee_ofs = (callee.pos_num() as i32 + 1) * 8 + LFP_SELF;
        for param_name in callee.kw_names() {
            match kw_args.get(param_name) {
                Some(caller) => {
                    let caller_ofs = (kw_pos.0 as i32 + *caller as i32) * 8 + LFP_SELF;
                    monoasm! { &mut self.jit,
                        movq  rax, [r14 - (caller_ofs)];
                        movq  [rsp - (RSP_LOCAL_FRAME + callee_ofs)], rax;
                    }
                }
                None => {
                    monoasm! { &mut self.jit,
                        movq  [rsp - (RSP_LOCAL_FRAME + callee_ofs)], 0;
                    }
                }
            }
            callee_ofs += 8;
        }
    }

    ///
    /// Handle hash splat arguments and a keyword rest parameter.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn handle_hash_splat_kw_rest(
        &mut self,
        callid: CallSiteId,
        meta: Meta,
        offset: usize,
        error: &DestLabel,
    ) {
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movl rdx, (callid.get());
            movq rcx, (meta.get());
            lea  r8, [rsp - (RSP_LOCAL_FRAME)];   // callee_lfp
            subq rsp, (offset);
            movq rax, (jit_handle_hash_splat_kw_rest);
            call rax;
            addq rsp, (offset);
        }
        self.handle_error(error);
    }

    ///
    /// Invoke method.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### out
    /// - rax: return value
    ///
    fn generic_call(&mut self, callid: CallSiteId, error: &DestLabel) -> CodePtr {
        monoasm! { &mut self.jit,
            movl r8, (callid.get()); // CallSiteId
        }
        self.generic_handle_arguments(runtime::jit_handle_arguments_no_block);
        self.handle_error(error);
        self.call_funcdata()
    }
}

///
/// Handle hash splat arguments and a keyword rest parameter.
///
/// ### in
/// - rdi: &mut Executor
/// - rsi: &mut Globals
/// - rdx: CallSiteId
/// - rcx: Meta
/// - r8: callee_lfp
///
/// ### out
/// - rax: Option<Value>
///
/// ### destroy
/// - caller save registers
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

const CACHE_SIZE: usize = 4;
const CACHE_METHOD: usize = std::mem::offset_of!(CacheEntry, method);
const CACHE_FID: usize = std::mem::offset_of!(CacheEntry, fid);
const CACHE_COUNTER: usize = std::mem::offset_of!(CacheEntry, counter);

#[repr(C)]
struct CacheEntry {
    method: Option<IdentId>,
    fid: Option<FuncId>,
    counter: usize,
}

impl std::fmt::Debug for CacheEntry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(method) = self.method {
            write!(
                f,
                "method: {:?}, fid: {:?}, counter: {}",
                method, self.fid, self.counter
            )
        } else {
            write!(f, "method: None")
        }
    }
}

#[repr(C)]
#[derive(Debug)]
struct Cache([CacheEntry; CACHE_SIZE]);

extern "C" fn expect_string(
    vm: &mut Executor, // rdi
    v: Value,          // rcx
) -> Option<IdentId> {
    v.expect_symbol_or_string()
        .map_err(|err| vm.set_error(err))
        .ok()
}

extern "C" fn no_method_name(vm: &mut Executor) -> Option<Value> {
    let err = MonorubyErr::argumenterr("no method name given.");
    vm.set_error(err);
    None
}

extern "C" fn wrong_number_of_arg(
    vm: &mut Executor,
    expected: usize,
    given: usize,
) -> Option<Value> {
    let err = MonorubyErr::wrong_number_of_arg(expected, given);
    vm.set_error(err);
    None
}

extern "C" fn find(
    vm: &mut Executor,
    globals: &mut Globals,
    recv: Value,
    func_name: IdentId,
) -> Option<FuncId> {
    globals
        .find_method(recv, func_name, true)
        .map_err(|err| vm.set_error(err))
        .ok()
}

impl Codegen {
    pub(crate) fn object_send_inline(
        &mut self,
        callid: CallSiteId,
        store: &Store,
        using_xmm: UsingXmm,
        error: &DestLabel,
        no_splat: bool,
    ) {
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            block_fid,
            block_arg,
            ..
        } = store[callid];
        let cache = self.jit.data(std::mem::size_of::<Cache>());

        self.check_version_with_cache(&cache);

        self.xmm_save(using_xmm);
        if no_splat {
            if pos_num < 1 {
                monoasm! { &mut self.jit,
                    movq rdi, rbx;
                    movq rax, (no_method_name);
                    call rax;
                    jmp error;
                }
            } else {
                monoasm! { &mut self.jit,
                    movq rcx, [r14 - (conv(args))];
                }
                self.object_send_inner(recv, cache, &error);
            }
        } else {
            self.object_send_splat_arg0(args, &error);
            self.object_send_inner(recv, cache, &error);
        }
        self.handle_error(&error);
        monoasm! { &mut self.jit,
            // FuncId was returned to rax.
            movl rdx, rax;
        }
        self.get_func_data();
        // r15 <- &FuncData

        monoasm! { &mut self.jit,
            subq  rsp, 16;
            // set prev_cfp
            pushq [rbx + (EXECUTOR_CFP)];
            // set lfp
            lea   rax, [rsp + (24 - RSP_LOCAL_FRAME)];
            pushq rax;
            // set outer
            xorq rax, rax;
            pushq rax;
            // set meta.
            pushq [r15 + (FUNCDATA_META)];
        };
        // set block
        self.push_block(block_fid, block_arg);
        // set self
        monoasm!( &mut self.jit,
            pushq [r14 - (conv(recv))];
            addq  rsp, 64;
        );

        if no_splat {
            self.object_send_handle_arguments(args, pos_num, callid, &error);
        } else {
            monoasm! { &mut self.jit,
                movl r8, (callid.get()); // CallSiteId
            }
            self.generic_handle_arguments(runtime::jit_handle_arguments_no_block_for_send_splat);
            self.handle_error(&error);
        }
        self.call_funcdata();
        self.xmm_restore(using_xmm);
        self.handle_error(&error);
    }

    fn check_version_with_cache(&mut self, cache: &DestLabel) {
        let cached_version = self.jit.data_i32(-1);
        let global_version = self.class_version_label();
        let clear_cache = self.jit.label();
        let exit = self.jit.label();
        monoasm! {&mut self.jit,
            movl rax, [rip + global_version];
            cmpl rax, [rip + cached_version];
            jne  clear_cache;
        exit:
        }

        self.jit.select_page(1);
        monoasm! {&mut self.jit,
        clear_cache:
            movl [rip + cached_version], rax;
            xorq rax, rax;
            lea  rdi, [rip + cache];
            movq [rdi], rax;
            movq [rdi + 16], rax;
            movq [rdi + 32], rax;
            movq [rdi + 48], rax;
            jmp exit;
        }
        self.jit.select_page(0);
    }

    ///
    /// ### out
    /// - rcx: arg0
    ///
    fn object_send_splat_arg0(&mut self, args: SlotId, error: &DestLabel) {
        let exit = self.jit.label();
        let heap = self.jit.label();
        monoasm! { &mut self.jit,
            movq rcx, [r14 - (conv(args))];
            testq rcx, 0b111;
            jnz  exit;
            cmpw [rcx + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  exit;
            movq rax, [rcx + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            movq rcx, [rcx + (RVALUE_OFFSET_INLINE)];
            testq rcx, rcx;
            jne  exit;
            movq rdi, rbx;
            movq rax, (no_method_name);
            call rax;
            jmp error;
        heap:
            movq rcx, [rcx + (RVALUE_OFFSET_HEAP_PTR)];
            movq rcx, [rcx];
        exit:
        }
    }

    fn object_send_handle_arguments(
        &mut self,
        args: SlotId,
        pos_num: usize,
        callid: CallSiteId,
        error: &DestLabel,
    ) {
        let loop0 = self.jit.label();
        let not_simple = self.jit.label();
        let arg_error = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            movzxb rax, [r15 + ((FUNCDATA_META + META_KIND) as i32)];
            testq rax, 0b1_0000;
            jz   not_simple;
            movzxw rax, [r15 + (FUNCDATA_MIN)];
            cmpw  rax, (pos_num - 1);
            jne  arg_error;
            lea  rdi, [r14 - (conv(args + 1usize))];
            lea  rdx, [rsp - (RSP_LOCAL_FRAME + LFP_ARG0)];
            movq r8, (pos_num);
            // src: rdi, dst: rdx
        loop0:
            subq r8, 1;
            jz  exit;
            movq rax, [rdi];
            movq [rdx], rax;
            subq rdi, 8;
            subq rdx, 8;
            jmp  loop0;
        exit:
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        arg_error:
            movq rdi, rbx;
            movq rsi, (pos_num - 1);
            movq rdx, rax;
            movq rax, (wrong_number_of_arg);
            call rax;
            jmp  error;
        not_simple:
            movl r8, (callid.get()); // CallSiteId
        }
        self.generic_handle_arguments(runtime::jit_handle_arguments_no_block_for_send);
        self.handle_error(&error);
        monoasm! { &mut self.jit,
            jmp exit;
        }
        self.jit.select_page(0);
    }

    ///
    /// ### in
    /// - rcx: arg0
    ///
    /// ### out
    /// - rax: Some(FuncId)
    ///
    fn object_send_inner(&mut self, recv: SlotId, cache: DestLabel, error: &DestLabel) {
        let not_symbol = self.jit.label();
        let l1 = self.jit.label();
        let found = self.jit.label();
        let not_found = self.jit.label();
        let loop0 = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            cmpb rcx, (TAG_SYMBOL);
            jne  not_symbol;
            shrq rcx, 32;
        l1:
        // rdi: &Cache
        // rcx: IdentId
        // rdx: min &Cache
            lea  rdi, [rip + cache];
            movq rdx, rdi;
            movl rax, [rdi + (CACHE_METHOD)];
            testq rax, rax;
            jz   not_found;
            cmpl rcx, rax;
            jeq  found;
            movq r8, (CACHE_SIZE - 1);

        loop0:
            movq rax, [rdi + (CACHE_COUNTER)];
            cmpq rax, [rdx + (CACHE_COUNTER)];
            cmovltq rdx, rdi;
            addq rdi, 16;
            movl rax, [rdi + (CACHE_METHOD)];
            testq rax, rax;
            jz   not_found;
            cmpl rcx, rax;
            jeq  found;
            subq r8, 1;
            jz   not_found;
            jmp  loop0;
        found:
        // rdi: cur &mut Cache
        // rdx: min &mut Cache
            movl rax, [rdi + (CACHE_FID)];
            addq [rdi + (CACHE_COUNTER)], 1;
            movq rsi, [rdi + (CACHE_COUNTER)];
            cmpq rsi, [rdx + (CACHE_COUNTER)];
            jle  exit;
        // swap cur, min,
            movq rsi, [rdi];
            xchgq rsi, [rdx];
            movq [rdi], rsi;
            movq rsi, [rdi + 8];
            xchgq rsi, [rdx + 8];
            movq [rdi + 8], rsi;
        exit:
        }
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        // rdi: &Cache
        // rdx: min &Cache
        not_found:
            pushq rdi;
            pushq rcx;
            movq rdi, rbx;
            movq rsi, r12;
            movq rdx, [r14 - (conv(recv))];
            lea  r8, [rip + cache];
            movq rax, (find);
            call rax;
            popq rcx;
            popq rdi;
            // rax: Option<FuncId>
            movl rax, rax;
            testq rax, rax;
            jz   exit;
            movl [rdi + (CACHE_FID)], rax;
            movl [rdi + (CACHE_METHOD)], rcx;
            movq [rdi + (CACHE_COUNTER)], 1;
            jmp  exit;
        not_symbol:
            movq rdi, rbx;
            movq rsi, rcx;
            movq rax, (expect_string);
            call rax;
            movl rax, rax;
        }
        self.handle_error(&error);
        monoasm! { &mut self.jit,
            movq rcx, rax;
            jmp  l1;
        }
        self.jit.select_page(0);
    }
}
