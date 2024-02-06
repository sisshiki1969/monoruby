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
    /// - r13: pc
    /// - r15: &FuncData
    ///
    fn call(&mut self, _: bool) {
        monoasm! { &mut self.jit,
            // rcx <- callee LFP
            lea  rcx, [rsp - 16];
            // rdx <- len
            movzxw rdx, [r13 + (POS_NUM)]; // rdi <- pos_num
            movl r8, [r13 + (CALLSITE_ID)]; // CallSiteId
            // set meta
            movq rax, [r15 + (FUNCDATA_META)];
            movq [rsp -(16 + LBP_META)], rax;
            movzxw r9, [r13 + (ARG_REG)]; // r9 <- %args
        }
        self.vm_get_slot_addr(GP::R9); // r9 <- *args
        let l1 = self.jit.label();
        let l2 = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdi, [r15 + (FUNCDATA_PC)];
            testq rdi, rdi;
            jeq  l1;
            movzxw rdi, [rdi + (INIT_METHOD_OFS + 16)];
            shlq rdi, 4;
            addq rdi, 16;
            jmp  l2;
        l1:
            movq rdi, rdx;
            // TODO: We must support rest argument in native methods.
            addq rdi, (LBP_ARG0 / 8 + 64 + 1);
            andq rdi, (-2);
            shlq rdi, 3;
        l2:
            subq rsp, rdi;
            subq rsp, 8;
            pushq rdi;
            movq rsi, r12;
            movq rdi, rbx;
            movq rax, (vm_handle_arguments);
            call rax;
            // rax <- arg_num: Value
            popq rdi;
            addq rsp, 8;
            addq rsp, rdi;
        };
        self.vm_handle_error();
        monoasm! { &mut self.jit,
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];    // r13: BcPc
            sarq rax, 1;
            movq rdx, rax;
        }
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
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

extern "C" fn vm_handle_arguments(
    vm: &mut Executor,
    globals: &mut Globals,
    len: usize,
    callee_lfp: LFP,
    callid: CallSiteId,
    src: *const Value,
) -> Option<Value> {
    fn push(
        arg_num: &mut usize,
        rest: &mut Vec<Value>,
        max_pos: usize,
        dst: *mut Value,
        v: Value,
        no_push: bool,
    ) {
        if *arg_num >= max_pos {
            if !no_push {
                rest.push(v);
            }
        } else {
            unsafe { *dst.sub(*arg_num) = v };
            *arg_num += 1;
        }
    }

    let mut arg_num = 0;
    let meta = *callee_lfp.meta();
    let is_block_style = meta.is_block_style();
    let callee_func_id = meta.func_id();
    let callee_info = &globals[callee_func_id];
    let max_pos = callee_info.max_positional_args();
    let no_push = callee_info.ignore_excess_positional_args();
    let mut rest = vec![];
    let splat_pos = &globals.store[callid].splat_pos;
    unsafe {
        let dst = callee_lfp.register_ptr(1) as *mut Value;
        for i in 0..len {
            let v = *src.sub(i);
            if splat_pos.contains(&i) {
                if let Some(ary) = v.is_array() {
                    for v in ary.iter() {
                        push(&mut arg_num, &mut rest, max_pos, dst, *v, no_push);
                    }
                } else if let Some(_range) = v.is_range() {
                    unimplemented!()
                } else if let Some(_hash) = v.is_hash() {
                    unimplemented!()
                } else {
                    push(&mut arg_num, &mut rest, max_pos, dst, v, no_push);
                };
            } else {
                push(&mut arg_num, &mut rest, max_pos, dst, v, no_push);
            }
        }
        // single array argument expansion for blocks
        if arg_num == 1 && is_block_style && max_pos > 1 {
            let v = *dst;
            if let Some(ary) = v.is_array() {
                arg_num = 0;
                for v in ary.iter() {
                    push(&mut arg_num, &mut rest, max_pos, dst, *v, no_push);
                }
            }
        }
    }

    match &callee_info.kind {
        FuncKind::ISeq(info) => {
            let caller = &globals.store[callid];
            if info.no_keyword() && caller.kw_num() != 0 {
                // handle excessive keyword arguments
                let mut h = IndexMap::default();
                for (k, id) in caller.kw_args.iter() {
                    let v = unsafe { vm.get_slot(caller.kw_pos + *id as u16).unwrap() };
                    h.insert(HashKey(Value::symbol(*k)), v);
                }
                let ex: Value = Value::hash(h);
                vm.handle_positional2(&info, arg_num, callee_lfp, Some(ex), rest)?;
            } else {
                vm.handle_positional2(&info, arg_num, callee_lfp, None, rest)?;
                vm.handle_keyword(&info, caller, callee_lfp);
            }
        }
        _ => {} // no keyword param and rest param for native func, attr_accessor, etc.
    }

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

    Some(Value::integer(arg_num as i64))
}
