use super::*;

impl Codegen {
    pub(super) fn gen_invoker(&mut self) {
        self.method_invoker = self.method_invoker();
        self.method_invoker2 = self.method_invoker2();
        self.block_invoker = self.block_invoker();
        self.block_invoker_with_self = self.block_invoker_with_self();
        self.binding_invoker = self.binding_invoker();
        self.fiber_invoker = self.fiber_invoker();
        self.fiber_invoker_with_self = self.fiber_invoker_with_self();
        self.resume_fiber = self.resume_fiber();
        self.yield_fiber = self.yield_fiber();
    }

    fn method_invoker(&mut self) -> MethodInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: FuncId
        // rcx: receiver: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        // r11: Option<BlockHandler>
        let error_exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq r11, [rsp + 8];
        }
        self.invoker_prologue();
        self.invoker_frame_setup(false, true);
        self.invoker_prep();
        self.invoker_args(error_exit);
        self.invoker_call();
        self.invoker_epilogue(error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "method-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn method_invoker2(&mut self) -> MethodInvoker2 {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: FuncId
        // rcx: receiver: Value
        // r8:  args: Arg
        // r9:  len: usize
        // r11: Option<BlockHandler>
        let error_exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq r11, [rsp + 8];
        }
        self.invoker_prologue();
        self.invoker_frame_setup(false, true);
        self.invoker_prep2();
        self.invoker_args(error_exit);
        self.invoker_call();
        self.invoker_epilogue(error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "method-invoker2");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn block_invoker(&mut self) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockData
        // rcx: <dummy>
        // r8:  *args: *const Value
        // r9:  len: usize
        let error_exit = self.jit.label();
        self.invoker_prologue();
        self.invoker_frame_setup(true, false);
        self.invoker_prep();
        self.invoker_args(error_exit);
        self.invoker_call();
        self.invoker_epilogue(error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "block-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn block_invoker_with_self(&mut self) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockData
        // rcx: self: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        let error_exit = self.jit.label();
        self.invoker_prologue();
        self.invoker_frame_setup(true, true);
        self.invoker_prep();
        self.invoker_args(error_exit);
        self.invoker_call();
        self.invoker_epilogue(error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "block-invoker-with-self");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn binding_invoker(&mut self) -> BindingInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: Lfp
        let error_exit = self.jit.label();
        self.invoker_prologue();
        monoasm! { &mut self.jit,
            // set lfp
            movq r14, rdx;
            movq [rsp - (RSP_CFP + CFP_LFP)], r14;
            // set FuncId
            movl rdx, [r14 - (LFP_META)];
        };
        self.get_func_data();
        self.push_frame();
        monoasm! { &mut self.jit,
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
        self.invoker_epilogue(error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "binding-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn fiber_invoker(&mut self) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockkData
        // rcx:
        // r8:  *args: *const Value
        // r9:  len: usize
        // [rsp + 8]: *mut Executor
        let error_exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq r10, [rsp + 8];
        };
        self.push_callee_save();
        monoasm! { &mut self.jit,
            movq [rdi + (EXECUTOR_RSP_SAVE)], rsp; // [vm.rsp_save] <- rsp
            movq rsp, [r10 + (EXECUTOR_RSP_SAVE)]; // rsp <- [child_vm.rsp_save]
            movq [r10 + (EXECUTOR_PARENT_FIBER)], rdi; // [child_vm.parent_fiber] <- vm
            movq rbx, r10;
            movq r12, rsi;
        }
        self.invoker_frame_setup(true, false);
        self.invoker_prep();
        self.invoker_args(error_exit);
        self.invoker_call();
        monoasm! { &mut self.jit,
            movq [rbx + (EXECUTOR_RSP_SAVE)], (-1); // [vm.rsp_save] <- -1 (terminated)
            movq rbx, [rbx + (EXECUTOR_PARENT_FIBER)]; // rbx <- [vm.parent_fiber]
            movq rsp, [rbx + (EXECUTOR_RSP_SAVE)]; // rsp <- [parent.rsp_save]
        error_exit:
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(pair, "fiber-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockkData
        // rcx: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        // [rsp + 8]: *mut Executor
        let error_exit = self.jit.label();
        monoasm! { &mut self.jit,
            movq r10, [rsp + 8];
        };
        self.push_callee_save();
        monoasm! { &mut self.jit,
            movq [rdi + (EXECUTOR_RSP_SAVE)], rsp; // [vm.rsp_save] <- rsp
            movq rsp, [r10 + (EXECUTOR_RSP_SAVE)]; // rsp <- [child_vm.rsp_save]
            movq [r10 + (EXECUTOR_PARENT_FIBER)], rdi; // [child_vm.parent_fiber] <- vm
            movq rbx, r10;
            movq r12, rsi;
        }
        self.invoker_frame_setup(true, true);
        self.invoker_prep();
        self.invoker_args(error_exit);
        self.invoker_call();
        monoasm! { &mut self.jit,
            movq [rbx + (EXECUTOR_RSP_SAVE)], (-1); // [vm.rsp_save] <- -1 (terminated)
            movq rbx, [rbx + (EXECUTOR_PARENT_FIBER)]; // rbx <- [vm.parent_fiber]
            movq rsp, [rbx + (EXECUTOR_RSP_SAVE)]; // rsp <- [parent.rsp_save]
        error_exit:
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(pair, "fiber-invoker-with-self");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn resume_fiber(
        &mut self,
    ) -> extern "C" fn(*mut Executor, &mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        self.push_callee_save();
        monoasm! { &mut self.jit,
            movq [rdi + (EXECUTOR_RSP_SAVE)], rsp; // [vm.rsp_save] <- rsp
            movq rsp, [rsi + (EXECUTOR_RSP_SAVE)]; // rsp <- [child_vm.rsp_save]
            movq [rsi + (EXECUTOR_PARENT_FIBER)], rdi; // [child_vm.parent_fiber] <- vm
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            movq rax, rdx;
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(pair, "resume-fiber");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        self.push_callee_save();
        monoasm! { &mut self.jit,
            movq [rdi + (EXECUTOR_RSP_SAVE)], rsp; // [vm.rsp_save] <- rsp
            movq rdi, [rdi + (EXECUTOR_PARENT_FIBER)]; // rdi <- [vm.parent_fiber]
            movq rsp, [rdi + (EXECUTOR_RSP_SAVE)]; // rsp <- [parent.rsp_save]
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            movq rax, rsi;
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(pair, "yield-fiber");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }
}

impl Codegen {
    fn invoker_prologue(&mut self) {
        // rdi: &mut Interp
        // rsi: &mut Globals
        monoasm! { &mut self.jit,
            pushq rbx;
            pushq r12;
            pushq r13;
            pushq r14;
            pushq r15;
            movq rbx, rdi;
            movq r12, rsi;
        }
    }

    ///
    /// Frame preparation.
    ///
    /// ### in
    /// - rcx: `self` (if *specify_self* is true)
    /// - rdx: FuncId (if *invoke_block* is false) or &BlockData (if *invoke_block* is true)
    /// - r11: BlockHandler (if *invoke_block* is false)
    ///
    /// ### out
    /// - r15: &FuncData
    /// - rsi: Meta
    /// - rcx: self
    ///
    fn invoker_frame_setup(&mut self, invoke_block: bool, specify_self: bool) {
        if invoke_block {
            monoasm! { &mut self.jit,
                // set block
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], 0;
                movq rax, [rdx + (PROCINNER_OUTER)];        // rax <- outer_lfp
                movl rdx, [rdx + (PROCINNER_FUNCID)];    // rdx <- FuncId
            };
            self.get_func_data();
            // r15: &FuncData
            self.set_block_outer();
            if !specify_self {
                monoasm! { &mut self.jit,
                    // set self
                    movq  rcx, [rax - (LFP_SELF)];
                };
            }
        } else {
            self.get_func_data();
            monoasm! { &mut self.jit,
                // set block
                movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], r11;
            };
            self.set_method_outer()
        }
        monoasm! { &mut self.jit,
            // set self
            movq [rsp - (RSP_LOCAL_FRAME + LFP_SELF)], rcx;
            // set meta
            movq rsi, [r15 + (FUNCDATA_META)];
            movq [rsp - (RSP_LOCAL_FRAME + LFP_META)], rsi;
        };
    }

    ///
    /// Arguments preparation.
    ///
    /// ### in
    /// - r8: *args
    /// - r9: len
    ///
    /// ### destroy
    /// - rax, rdi, r9, r10
    ///
    fn invoker_prep(&mut self) {
        let loop_exit = self.jit.label();
        let loop_ = self.jit.label();
        monoasm! { &mut self.jit,
            // r8 : *args
            // r9 : len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            movq r10, r9;
            negq r9;
        loop_:
            movq rax, [r8 + r10 * 8 - 8];
            movq [rsp + r9 * 8 - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
            subq r10, 1;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    ///
    /// Arguments preparation.
    ///
    /// ### in
    /// - r8: *args
    /// - r9: len
    ///
    /// ### destroy
    /// - rax, rdi, r9
    ///
    fn invoker_prep2(&mut self) {
        let loop_exit = self.jit.label();
        let loop_ = self.jit.label();
        monoasm! { &mut self.jit,
            // set block
            movq [rsp - (RSP_LOCAL_FRAME + LFP_BLOCK)], r11;
            // r8 <- *args
            // r9 <- len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            negq r9;
        loop_:
            movq rax, [r8 + r9 * 8 + 8];
            movq [rsp + r9 * 8 - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    ///
    /// Handle arguments.
    ///
    /// ### in
    /// - rdi: arg_num
    /// - rsi: Meta
    /// - r15: &FuncData
    /// - r8:  args: *const Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn invoker_args(&mut self, error_exit: DestLabel) {
        // In invoker call, CallSiteInfo is not available.
        // All invoker callsites have no splat arguments, no keyword arguments, and no hash splat arguments (thus, no extra positional arguments).
        // So several conditions are met, we can optimize this.
        // the conditions are:
        // - the callee is_simple (no optional, no rest, no keyword, no keyword rest, no block arguments)
        // - req == pos_num
        // - thus, no single argument expansion
        let generic = self.jit.label();
        let exit = self.jit.label();
        //self.guard_simple_call(GP::Rsi, GP::Rdi, GP::R8, exit, generic);
        monoasm! { &mut self.jit,
            // check if the callee is_simple
            shrq rsi, 56;
            testq rsi, 0b1_0000;
            jz  generic;
            // check if req == pos_num
            cmpw rdi, [r15 + (FUNCDATA_MIN)];
            jeq exit;
        generic:
            lea  rdx, [rsp - (RSP_LOCAL_FRAME)]; // callee lfp: Lfp
            subq rsp, 4096;
            movq rcx, rdi; // arg_num
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (handle_invoker_arguments);
            call rax;
            addq rsp, 4096;
            testq rax, rax;
            jz  error_exit;
        }
        self.jit.bind_label(exit);
    }

    ///
    /// Invoke the function.
    ///
    /// ### in
    /// - r15: &FuncData
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn invoker_call(&mut self) {
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];    // CALL_SITE
            movq rdi, [rsp - (RSP_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    fn invoker_epilogue(&mut self, error_exit: DestLabel) {
        monoasm! { &mut self.jit,
        error_exit:
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };
    }
}

extern "C" fn handle_invoker_arguments(
    vm: &mut Executor,
    globals: &Globals,
    callee_lfp: Lfp,
    mut arg_num: usize,
) -> Option<Value> {
    let callee_fid = callee_lfp.meta().func_id();
    let info = &globals.store[callee_fid];
    // expand array for block
    if info.single_arg_expand() && arg_num == 1 {
        arg_num = expand_array_for_block(info, arg_num, callee_lfp);
    }

    // required + optional + rest
    if let Err(err) = super::runtime::handle_positional(info, arg_num, callee_lfp) {
        vm.set_error(err);
        return None;
    };

    // keyword
    let params = info.kw_names();
    let callee_kw_pos = info.pos_num() + 1;
    for (id, _) in params.iter().enumerate() {
        unsafe {
            *callee_lfp.register_ptr(callee_kw_pos + id) = Some(Value::nil());
        }
    }

    Some(Value::nil())
}

/// deconstruct array for block
fn expand_array_for_block(info: &FuncInfo, arg_num: usize, callee_lfp: Lfp) -> usize {
    let req_num = info.req_num();
    let v = callee_lfp.register(1).unwrap();
    if let Some(src) = v.try_array_ty() {
        let ptr = callee_lfp.register_ptr(1);
        return block_expand_array(src, ptr as _, req_num);
    }
    arg_num
}

fn block_expand_array(src: Array, dst: *mut Value, min_len: usize) -> usize {
    let len = src.len();
    for i in 0..len {
        unsafe { *dst.sub(i) = src[i] }
    }
    for i in 0..len {
        unsafe { *dst.sub(i) = src[i] }
    }
    if min_len <= len {
        len
    } else {
        unsafe {
            std::slice::from_raw_parts_mut(dst.sub(len), min_len - len - 1).fill(Value::nil());
        }
        min_len
    }
}
