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
            lea  r8, [rsp - 16];   // callee_lfp
            movq r9, (meta.get());
            subq rsp, (offset);
            movq rax, (crate::runtime::jit_generic_set_arguments);
            call rax;
            addq rsp, (offset);
        }
    }
}

impl Codegen {
    ///
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
    }

    ///
    /// Attribute reader
    ///
    /// ### in
    /// rdi: receiver: Value
    ///
    pub(super) fn attr_reader(&mut self, ivar_id: IvarId) {
        let exit = self.jit.label();
        if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
            let not_object = self.jit.label();
            monoasm!( &mut self.jit,
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  not_object;
                movq rax, [rdi + (ivar_id.get() as i32 * 8 + RVALUE_OFFSET_KIND)];
                movq rdi, (NIL_VALUE);
                testq rax,rax;
                cmoveqq rax, rdi;
            exit:
            );
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            not_object:
                movl rsi, (ivar_id.get());
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
            movq rdi, [rdx + (MONOVEC_CAPA)]; // capa
            testq rdi, rdi;
            jz   exit;
            movq rdi, [rdx + (MONOVEC_LEN)]; // len
            cmpq rdi, rsi;
            movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
            cmovgtq rax, [rdi + rsi * 8];
        exit:
        );
    }

    ///
    /// Attribute writer
    ///
    /// ### in
    /// rdi: receiver: Value
    /// rdx: value: Value
    ///
    pub(super) fn attr_writer(&mut self, using_xmm: UsingXmm, error: DestLabel, ivar_id: IvarId) {
        let exit = self.jit.label();
        let no_inline = self.jit.label();
        if ivar_id.get() < OBJECT_INLINE_IVAR as u32 {
            monoasm!( &mut self.jit,
                // we don't know ty of the receiver in a compile time.
                cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::OBJECT);
                jne  no_inline;
                movq [rdi + (ivar_id.get() as i32 * 8 + RVALUE_OFFSET_KIND)], rdx;
            exit:
            );
            self.jit.select_page(1);
            self.jit.bind_label(no_inline);
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
            );
            self.set_ivar(using_xmm);
            self.handle_error(error);
            monoasm!( &mut self.jit,
                jmp exit;
            );
            self.jit.select_page(0);
        } else {
            monoasm!( &mut self.jit,
                movl rsi, (ivar_id.get());
            );
            self.set_ivar(using_xmm);
            self.handle_error(error);
        }
    }

    ///
    /// ### in
    /// rdi: numer of args.
    ///
    pub(super) fn send_cached(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        callee_fid: FuncId,
        recv_class: ClassId,
        native: bool,
        offset: usize,
        using_xmm: UsingXmm,
        error: DestLabel,
    ) {
        let caller = &store[callid];
        let callee = &store[callee_fid];
        let (meta, codeptr, pc) = callee.get_data();
        self.setup_frame(meta, caller);
        self.copy_keyword_args(caller, callee);
        if callee.kw_rest().is_some() || !caller.hash_splat_pos.is_empty() {
            self.handle_hash_splat_kw_rest(callid, meta, offset, error);
        }

        self.set_lfp();
        self.push_frame();

        if native {
            self.call_codeptr(codeptr);
        } else {
            match store[callee_fid].get_jit_code(recv_class) {
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

        self.xmm_restore(using_xmm);
        self.handle_error(error);
    }

    ///
    /// Set up a callee frame
    ///
    /// ### in
    /// - r13: receiver
    ///
    /// ### destroy
    /// - rax
    ///
    fn setup_frame(&mut self, meta: Meta, callsite: &CallSiteInfo) {
        monoasm! { &mut self.jit,
            subq rsp, 32;
            // set prev_cfp
            //movq rax, [rbx + (EXECUTOR_CFP)];
            //pushq rax;
            //movq [rsp - (16 + BP_PREV_CFP)], rax;
            // set lfp
            //lea   rax, [rsp - 16];
            //pushq rax;
            //movq [rsp - (16 + BP_LFP)], rax;
            // set outer
            xorq rax, rax;
            pushq rax;
            //movq [rsp - (16 + LBP_OUTER)], 0;
            // set meta.
            movq rax, (meta.get());
            pushq rax;
            //movq [rsp - (16 + LBP_META)], rax;
        }
        // set block
        if let Some(func_id) = callsite.block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm!( &mut self.jit,
                movq rax, (bh.id());
                pushq rax;
                //movq [rsp - (16 + LBP_BLOCK)], rax;
            );
        } else if let Some(block) = callsite.block_arg {
            monoasm!( &mut self.jit,
                movq rax, [r14 - (conv(block))];
                pushq rax;
                //movq [rsp - (16 + LBP_BLOCK)], rax;
            );
        } else {
            monoasm!( &mut self.jit,
                xorq rax, rax;
                pushq rax;
                //movq [rsp - (16 + LBP_BLOCK)], 0;
            );
        }
        // set self
        monoasm! { &mut self.jit,
            //movq [rsp - (16 + LBP_SELF)], r13;
            pushq r13;
            addq rsp, 64;
        }
    }

    pub(super) fn gen_yield(
        &mut self,
        store: &Store,
        callid: CallSiteId,
        using_xmm: UsingXmm,
        error: DestLabel,
    ) {
        let callsite = &store[callid];
        self.xmm_save(using_xmm);
        self.get_proc_data();
        self.handle_error(error);
        // rax <- outer, rdx <- FuncId
        monoasm! { &mut self.jit,
            movq rdi, rax;
        }
        self.get_func_data();
        // rdi <- outer, r15 <- &FuncData

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
            xorq rax, rax;
            pushq rax;
            // set self
            pushq [rdi - (LBP_SELF)];
            addq  rsp, 64;
        };

        self.generic_call(callid, callsite.args, error);
        self.xmm_restore(using_xmm);
        self.handle_error(error);
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
    match v.expect_symbol_or_string() {
        Ok(sym) => Some(sym),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
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
    //cache: &Cache,
) -> Option<FuncId> {
    //eprintln!("{:#?}", cache);
    match globals.find_method(recv, func_name, true) {
        Ok(fid) => Some(fid),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

impl Codegen {
    pub(crate) fn object_send_inline(
        &mut self,
        callid: CallSiteId,
        recv: SlotId,
        args: SlotId,
        pos_num: usize,
        block_fid: Option<FuncId>,
        block_arg: Option<SlotId>,
        using_xmm: UsingXmm,
        error: DestLabel,
        no_splat: bool,
    ) {
        let cache = self.jit.data(std::mem::size_of::<Cache>());

        self.check_version(cache);

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
                self.object_send_inner(recv, cache, error);
            }
        } else {
            self.object_send_splat_arg0(args, error);
            self.object_send_inner(recv, cache, error);
        }
        self.handle_error(error);
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
            lea   rax, [rsp + 8];
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
            self.object_send_handle_arguments(args, pos_num, callid, error);
        } else {
            monoasm! { &mut self.jit,
                movl r8, (callid.get()); // CallSiteId
                lea  rdx, [r14 - (conv(args))];
            }
            self.generic_handle_arguments(runtime::jit_handle_arguments_no_block_for_send_splat);
            self.handle_error(error);
        }
        self.call_funcdata();
        self.xmm_restore(using_xmm);
        self.handle_error(error);
    }

    fn check_version(&mut self, cache: DestLabel) {
        let cached_version = self.jit.data_i32(-1);
        let global_version = self.class_version;
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
    fn object_send_splat_arg0(&mut self, args: SlotId, error: DestLabel) {
        let exit = self.jit.label();
        let heap = self.jit.label();
        monoasm! { &mut self.jit,
            movq rcx, [r14 - (conv(args))];
            testq rcx, 0b111;
            jnz  exit;
            cmpw [rcx + (RVALUE_OFFSET_TY)], (ObjKind::ARRAY);
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
        error: DestLabel,
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
            lea  rdx, [rsp - (16 + LBP_ARG0)];
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
            lea  rdx, [r14 - (conv(args))];
            movl r8, (callid.get()); // CallSiteId
        }
        self.generic_handle_arguments(runtime::jit_handle_arguments_no_block_for_send);
        self.handle_error(error);
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
    fn object_send_inner(&mut self, recv: SlotId, cache: DestLabel, error: DestLabel) {
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
            jnz  loop0;

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
        not_symbol:
            movq rdi, rbx;
            movq rsi, rcx;
            movq rax, (expect_string);
            call rax;
            movl rax, rax;
        }
        self.handle_error(error);
        monoasm! { &mut self.jit,
            movq rcx, rax;
            jmp  l1;
        }
        self.jit.select_page(0);
    }
}
