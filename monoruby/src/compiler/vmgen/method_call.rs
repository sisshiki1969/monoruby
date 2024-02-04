use super::*;

const CALLSITE_ID: i64 = 0 - 16;
const CACHED_CLASS: i64 = 24 - 16;
const CACHED_VERSION: i64 = 28 - 16;
const CACHED_FUNCID: i64 = 8 - 16;
const RET_REG: i64 = 4 - 16;
const OPCODE_SUB: i64 = 7 - 16;
const POS_NUM: i64 = 16 - 16;
const ARG_REG: i64 = 18 - 16;
const RECV_REG: i64 = 20 - 16;

impl Codegen {
    ///
    /// Call Method
    ///
    /// ~~~text
    /// MethodCall
    /// 0   2   4   6    8   10  12  14
    /// +---+---+---+---++---+---+---+---+
    /// |callid |ret| op||  fid  |   -   |
    /// +---+---+---+---++---+---+---+---+
    /// InlineCache
    /// 16  18  20  22   24  26  28  30
    /// +---+---+---+---++---+---+---+---+
    /// |pos|arg|rcv| op|| class |version|
    /// +---+---+---+---++---+---+---+---+
    ///
    /// operands
    /// ret:  return register
    /// callid:   call site id
    /// rcv:  receiver register
    /// arg:  the start of argument registers
    /// len:  the number of argument registers
    ///
    /// inline method cache
    /// class:    a class of the receiver
    /// version:  class version
    /// fid:      FuncId
    /// ~~~
    pub(super) fn vm_call(&mut self, has_splat: bool) -> CodePtr {
        let label = self.jit.get_current_address();
        let exec = self.jit.label();
        let slow_path1 = self.jit.label();
        let slow_path2 = self.jit.label();
        let class_version = self.class_version;
        let get_class = self.get_class;
        self.execute_gc(None);
        monoasm! { &mut self.jit,
            pushq r13;
            subq  rsp, 8;
            // set self (= receiver)
            movzxw rdi, [r13 + (RECV_REG)];
        };
        // rdi: receiver: Value
        self.vm_get_slot_value(GP::Rdi);
        monoasm! { &mut self.jit,
            movq [rsp - (16 + LBP_SELF)], rdi;
            call get_class;
            movl r15, rax;
            // r15: class of receiver: ClassId
            cmpl r15, [r13 + (CACHED_CLASS)];
            jne  slow_path1;
            movl rdi, [r13 + (CACHED_VERSION)];
            cmpl rdi, [rip + class_version];
            jne  slow_path2;
        exec:
            movl rdx, [r13 + (CACHED_FUNCID)];
        };
        self.get_func_data();
        self.set_method_outer();
        self.call(has_splat);
        self.fetch_and_dispatch();

        self.slow_path(exec, slow_path1, slow_path2);

        label
    }

    /// Yield
    ///
    /// ~~~text
    /// Yield
    /// 0   2   4   6    8   10  12  14
    /// +---+---+---+---++---+---+---+---+
    /// |callid |ret| op||  fid  |   -   |
    /// +---+---+---+---++---+---+---+---+
    /// InlineCache
    /// 16  18  20  22   24  26  28  30
    /// +---+---+---+---++---+---+---+---+
    /// |pos|arg|rcv| op|| class |version|
    /// +---+---+---+---++---+---+---+---+
    /// ~~~
    pub(super) fn vm_yield(&mut self) -> CodePtr {
        let label = self.jit.get_current_address();
        self.get_proc_data();
        // rax: outer, rdx: FuncId
        self.vm_handle_error();
        self.get_func_data();
        // rax: outer, r15: &FuncData
        monoasm! { &mut self.jit,
            pushq r13; // push pc
            subq rsp, 8;
        };
        self.set_block_self_outer();
        self.call(true);
        self.fetch_and_dispatch();
        label
    }

    ///
    /// Call
    ///
    /// ### in
    /// - rdi: args len
    /// - r13: pc
    /// - r15: &FuncData
    ///
    fn call(&mut self, _: bool) {
        monoasm! { &mut self.jit,
            // rsi <- callee LFP
            lea  rsi, [rsp - 16];
            // rdx <- len
            movsxw rdx, [r13 + (POS_NUM)]; // rdi <- pos_num
            movl r8, [r13 + (CALLSITE_ID)]; // CallSiteId
            // set meta
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rax;
            movzxw r9, [r13 + (ARG_REG)]; // rcx <- %args
        }
        self.vm_get_slot_addr(GP::R9); // rdi <- *args
        monoasm! { &mut self.jit,
            // TODO: this possibly cause problem.
            subq rsp, 4096;
            movq rcx, r12;
            movq rdi, rbx;
            movq rax, (vm_expand_splat);
            call rax;
            addq rsp, 4096;
        };
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            sarq rax, 1;
            movq rdx, rax;
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];    // r13: BcPc
            // push cfp
            movq rdi, [rbx + (EXECUTOR_CFP)];
            lea  rsi, [rsp - (16 + BP_PREV_CFP)];
            movq [rsi], rdi;
            movq [rbx + (EXECUTOR_CFP)], rsi;
            // set lfp
            lea  r14, [rsp - 16];
            movq [r14 - (BP_LFP)], r14;
            call [r15 + (FUNCDATA_CODEPTR)];
        }
        self.pop_frame();
        monoasm! { &mut self.jit,
            addq rsp, 8;
            popq r13;   // pop pc
            movzxw r15, [r13 + (RET_REG)];  // r15 <- :1
            addq r13, 16;
        };
        self.vm_handle_error();
        self.vm_store_r15_if_nonzero();
    }

    ///
    /// Generate slow path.
    ///
    /// When the receiver class is cached **and** the receiver class is different from the cached class,
    /// opcode_sub is set to 1.
    ///
    /// ### in
    /// - r15: ClassId of receiver
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn slow_path(&mut self, exec: DestLabel, slow_path1: DestLabel, slow_path2: DestLabel) {
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
            // receiver mismatch
        slow_path1:
            movl rax, [r13 + (CACHED_FUNCID)];
            testq rax, rax;
            // if receiver class was not cached, go to slow_path2.
            je   slow_path2;
            // if the receiver class was different from cached class
            movb [r13 + (OPCODE_SUB)], 1;
            // version mismatch
        slow_path2:
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, [r13 + (CALLSITE_ID)];  // CallSiteId
            movq rax, (runtime::vm_find_method);
            call rax;   // rax <- Option<FuncId>
        );
        self.vm_handle_error();
        self.save_cache(exec);
        self.jit.select_page(0);
    }

    ///
    /// Save inline method cache.
    ///
    /// ### in
    /// - rax: FuncId
    /// - r15: ClassId of receiver
    ///
    fn save_cache(&mut self, exec: DestLabel) {
        let class_version = self.class_version;
        monoasm!( &mut self.jit,
            movl [r13 + (CACHED_FUNCID)], rax;    // FuncId
            movl [r13 + (CACHED_CLASS)], r15;    // ClassId of receiver
            movl rdi, [rip + class_version];
            movl [r13 + (CACHED_VERSION)], rdi;    // class_version
            jmp exec;
        );
    }
}

extern "C" fn vm_expand_splat(
    vm: &mut Executor,
    callee_lfp: LFP,
    len: usize,
    globals: &mut Globals,
    callid: CallSiteId,
    src: *const Value,
) -> Option<Value> {
    let mut dst_len = 0;
    unsafe {
        let mut dst = callee_lfp.register_ptr(1) as _;
        let splat_pos = &globals.store[callid].splat_pos;
        for i in 0..len {
            let v = *src.sub(i);
            if splat_pos.contains(&i) {
                let ofs = expand_splat_inner(v, dst);
                dst_len += ofs;
                dst = dst.sub(ofs);
            } else {
                *dst = v;
                dst = dst.sub(1);
                dst_len += 1;
            }
        }
        let meta = callee_lfp.meta();
        if dst_len == 1
            && meta.is_block_style()
            && !meta.is_native()
            && globals.store[meta.func_id()]
                .as_ruby_func()
                .args
                .reqopt_num()
                > 1
        {
            dst = dst.add(1);
            let v = *dst;
            if let Some(ary) = v.is_array() {
                let len = ary.len();
                for i in 0..len {
                    *dst.sub(i) = ary[i];
                }
                dst_len = len
            }
        }
    }
    handle_arguments(vm, globals, callid, dst_len, callee_lfp)?;
    Some(Value::integer(dst_len as i64))
}

fn expand_splat_inner(src: Value, dst: *mut Value) -> usize {
    if let Some(ary) = src.is_array() {
        let len = ary.len();
        for i in 0..len {
            unsafe { *dst.sub(i) = ary[i] };
        }
        len
    } else if let Some(_range) = src.is_range() {
        unimplemented!()
    } else if let Some(_hash) = src.is_hash() {
        unimplemented!()
    } else {
        unsafe { *dst = src };
        1
    }
}

fn handle_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    callid: CallSiteId,
    arg_num: usize,
    callee_lfp: LFP,
) -> Option<Value> {
    let meta = *callee_lfp.meta();
    vm.handle_arguments(globals, callid, arg_num, callee_lfp, meta)?;

    let CallSiteInfo {
        block_fid,
        block_arg,
        ..
    } = globals.store[callid];

    let bh = if let Some(block_fid) = block_fid {
        let bh = BlockHandler::from(block_fid);
        Some(bh)
    } else if let Some(block_arg) = block_arg {
        unsafe { Some(BlockHandler(vm.get_slot(block_arg).unwrap())) }
    } else {
        None
    };
    callee_lfp.set_block(bh);
    Some(Value::nil())
}
