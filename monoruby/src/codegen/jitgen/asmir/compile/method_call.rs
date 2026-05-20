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
            pushq [rbp - (rbp_local(callsite.recv))];
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
    /// Set up a callee block frame for yield.
    ///
    /// ### out
    /// - rax: return value
    ///
    /// ### destroy
    /// - caller save registers
    /// - r15
    ///
    pub(super) fn gen_yield(&mut self, callid: CallSiteId, error: &DestLabel) -> CodePtr {
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
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], rdi;
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rax;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
            movq rax, [rdi - (LFP_SELF)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
        };

        monoasm! { &mut self.jit,
            movl r8, (callid.get()); // CallSiteId
        }
        self.generic_handle_arguments(runtime::jit_handle_arguments_no_block);
        self.handle_error(error);
        self.call_funcdata()
    }

    ///
    /// Set up a callee method frame for send.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn setup_method_frame(
        &mut self,
        store: &Store,
        meta: Meta,
        callid: CallSiteId,
        outer_lfp: Option<Lfp>,
    ) {
        let callsite = &store[callid];
        if let Some(outer_lfp) = outer_lfp {
            monoasm! { &mut self.jit,
                movq rax, (outer_lfp.as_ptr());
                movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], rax;
            }
        } else {
            monoasm! { &mut self.jit,
                movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], 0;
            }
        }
        monoasm! { &mut self.jit,
            movq rax, (meta.get());
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rax;
            // SVAR / CME slots — zero-fill so the GC mark walker
            // never dereferences uninitialised stack memory. Lazy
            // `$~` allocation rewrites SVAR on first MatchData; CME
            // stays zero pending its own migration.
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
        }
        self.set_block(callsite.block_fid, callsite.block_arg);
        monoasm! { &mut self.jit,
        }
    }

    ///
    /// Set up a callee block frame for yield.
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    pub(super) fn setup_yield_frame(&mut self, meta: Meta, outer: usize) {
        let outer = outer - 1;
        monoasm! { &mut self.jit,
            movq rdi, [rbx + (EXECUTOR_CFP)];
        }
        for _ in 0..outer {
            monoasm! { &mut self.jit,
                movq rdi, [rdi];
            }
        }
        monoasm! { &mut self.jit,
            movq rdi, [rdi - (CFP_LFP)];
            // rdi <- outer LFP
            movq rax, [rbx + (EXECUTOR_CFP)];
            movq [rsp - (RSP_CFP)], rax;
            // set lfp
            lea   rax, [rsp + (24 - RSP_LOCAL_FRAME)];
            movq [rsp - (RSP_CFP + CFP_LFP)], rax;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], rdi;
            movq  rax, (meta.get());
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rax;
            // Yield builds a block frame — SVAR/CME are unused by
            // block-style callees (they resolve via outer chain),
            // but zero them so the GC mark walker stays sound.
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
            movq rax, [rdi - (LFP_SELF)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
        };
    }

    pub(super) fn do_call(
        &mut self,
        store: &Store,
        callee_fid: FuncId,
        recv_class: ClassId,
        call_site_bc_ptr: BytecodePtr,
    ) -> CodePtr {
        let callee = &store[callee_fid];
        let (_, codeptr, pc) = callee.get_data();
        self.set_lfp();
        self.push_frame();

        if let Some(iseq) = callee.is_iseq() {
            match store[iseq].get_jit_entry(recv_class) {
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
            // builtin: always set rcx to call site bc ptr
            let call_site_ptr_val = call_site_bc_ptr.as_ptr() as u64;
            monoasm! { &mut self.jit,
                movq rcx, (call_site_ptr_val);
            }
            self.call_codeptr(codeptr)
        }
        let return_addr = self.jit.get_current_address();

        self.pop_frame();
        return_addr
    }

    pub(super) fn do_specialized_call(
        &mut self,
        entry: DestLabel,
        patch_point: Option<DestLabel>,
    ) -> CodePtr {
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
    /// Set block.
    ///
    /// ### destroy
    /// - rax
    ///
    fn set_block(&mut self, block_fid: Option<FuncId>, block_arg: Option<SlotId>) {
        if let Some(func_id) = block_fid {
            let bh = BlockHandler::from_caller(func_id);
            monoasm!( &mut self.jit,
                movq rax, (bh.id());
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], rax;
            );
        } else if let Some(block) = block_arg {
            monoasm!( &mut self.jit,
                movq rax, [rbp - (rbp_local(block))];
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
            );
        }
    }

    fn call_codeptr(&mut self, codeptr: CodePtr) {
        let src_point = self.jit.get_current_address();
        monoasm! { &mut self.jit,
            call (codeptr - src_point - 5); // CALL_SITE
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
    vm: &mut Executor,     // rdi
    globals: &mut Globals, // rsi
    v: Value,              // rdx
) -> Option<IdentId> {
    v.expect_symbol_or_string(globals)
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
    vm.find_method(globals, recv, func_name, true)
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
                    movq rcx, [rbp - (rbp_local(args))];
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
            movq [rsp - (RSP_LOCAL_FRAME + LFP_OUTER)], 0;
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rax;
            // Zero SVAR/CME — method-introducing frame's lazy `$~`
            // container is allocated on first MatchData write.
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SVAR)], 0;
            movq [rsp - (RSP_LOCAL_FRAME + LFP_CME)], 0;
        };
        self.set_block(block_fid, block_arg);
        monoasm!( &mut self.jit,
            movq rax, [rbp - (rbp_local(recv))];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
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
            movq rcx, [rbp - (rbp_local(args))];
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
            lea  rdi, [rbp - (rbp_local(args + 1usize))];
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
    /// Specialized argument setup for a forwarding call `g(x.., ...)`.
    ///
    /// `f`'s `...` rest is already an `Array` at slot `args + lead_num`,
    /// with `lead_num` ordinary leading args at `args .. args+lead_num`.
    /// If, at runtime, that value is an `Array` whose length equals
    /// `expected_len` (= `g`'s required arity − `lead_num`, a
    /// compile-time constant) and the forwarded kw-rest is nil, copy the
    /// leading args and then the array elements straight into the callee
    /// frame with no `Array` re-parse. Every guard runs *before* any
    /// callee-frame write, so a miss is a clean jump to the generic
    /// `jit_generic_set_arguments` path with nothing to roll back.
    ///
    /// ### out
    /// - rax: NIL_VALUE on success (non-zero), None(0) for error.
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn jit_set_arguments_forwarded(
        &mut self,
        callid: CallSiteId,
        fid: FuncId,
        offset: usize,
        args: SlotId,
        lead_num: usize,
        expected_len: usize,
        recv: SlotId,
        kwrest_guard: Option<SlotId>,
        deferred_src: Option<(SlotId, u16)>,
    ) {
        if let Some((src, _len)) = deferred_src {
            // D1: the `...` rest `Array` was deferred. `recv` and the
            // `lead_num` leading args are `f`'s own slots (`f`'s rbp).
            // The forwarded positionals come from the *caller* frame:
            // `f` established its own rbp (`init_func`), so the caller's
            // rbp is the value `f` saved at `[rbp]`; the gate guarantees
            // the caller is exactly one (outermost) level up, so the
            // source lives at `[caller_rbp - rbp_local(src + j)]`.
            // Exact arity and a nil forwarded `**kwrest` are gate
            // invariants, hence no length/kw guard and no fallback.
            monoasm! { &mut self.jit,
                movq rax, [rbp - (rbp_local(recv))];
                movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
            }
            for i in 0..lead_num {
                monoasm! { &mut self.jit,
                    movq rax, [rbp - (rbp_local(args + i))];
                    movq [rsp - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32)], rax;
                }
            }
            monoasm! { &mut self.jit,
                movq rcx, [rbp];
            }
            for j in 0..expected_len {
                monoasm! { &mut self.jit,
                    movq rax, [rcx - (rbp_local(src + j))];
                    movq [rsp - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * (lead_num + j)) as i32)], rax;
                }
            }
            monoasm! { &mut self.jit,
                movq rax, (NIL_VALUE);
            }
            return;
        }
        let fallback = self.jit.label();
        let heap = self.jit.label();
        let got = self.jit.label();
        let loop0 = self.jit.label();
        let done = self.jit.label();
        let exit = self.jit.label();
        let rest_slot = args + lead_num;
        monoasm! { &mut self.jit,
            // set self
            movq rax, [rbp - (rbp_local(recv))];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
        }
        // copy `lead_num` ordinary leading args (unrolled; lead_num is a
        // small compile-time constant). They were spilled to their frame
        // slots by `write_back_recv_and_callargs`.
        for i in 0..lead_num {
            monoasm! { &mut self.jit,
                movq rax, [rbp - (rbp_local(args + i))];
                movq [rsp - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * i) as i32)], rax;
            }
        }
        monoasm! { &mut self.jit,
            // load forwarded `...` rest array
            movq rcx, [rbp - (rbp_local(rest_slot))];
            testq rcx, 0b111;
            jnz  fallback;
            cmpw [rcx + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  fallback;
            // rax = len, rsi = element base (inline vs heap)
            movq rax, [rcx + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            lea  rsi, [rcx + (RVALUE_OFFSET_INLINE)];
            jmp  got;
        heap:
            movq rax, [rcx + (RVALUE_OFFSET_HEAP_LEN)];
            movq rsi, [rcx + (RVALUE_OFFSET_HEAP_PTR)];
        got:
            // speculative length guard (expected_len baked as immediate)
            cmpq rax, (expected_len);
            jne  fallback;
        }
        if let Some(kw) = kwrest_guard {
            monoasm! { &mut self.jit,
                // only fast-path when no keywords are actually forwarded
                cmpq [rbp - (rbp_local(kw))], (NIL_VALUE);
                jne  fallback;
            }
        }
        monoasm! { &mut self.jit,
            // two-pointer copy of the `...` array into callee slots
            // [lead_num ..]: src ascends, dst (callee LFP) descends
            lea  rdx, [rsp - (RSP_LOCAL_FRAME + LFP_ARG0 + (8 * lead_num) as i32)];
            movq r8, (expected_len);
            testq r8, r8;
            jz   done;
        loop0:
            movq rax, [rsi];
            movq [rdx], rax;
            addq rsi, 8;
            subq rdx, 8;
            subq r8, 1;
            jnz  loop0;
        done:
            movq rax, (NIL_VALUE);   // success sentinel for handle_error
            jmp  exit;
        }
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        fallback:
        }
        self.jit_set_arguments(callid, fid, offset);
        monoasm! { &mut self.jit,
            jmp  exit;
        }
        self.jit.select_page(0);
        monoasm! { &mut self.jit,
        exit:
        }
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
            movq rdx, [rbp - (rbp_local(recv))];
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
            movq rsi, r12;
            movq rdx, rcx;
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
