use super::*;

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
            //lea   rax, [rsp + 8];
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
        self.handle_error(error);
        monoasm! { &mut self.jit,
            movq rcx, rax;
            jmp  l1;
        }
        self.jit.select_page(0);
    }
}

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
