use crate::compiler::runtime::{PROCDATA_FUNCID, PROCDATA_OUTER};

use super::*;

impl JitModule {
    pub(super) fn method_invoker(&mut self) -> MethodInvoker {
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
        self.invoker_prologue(&error_exit);
        self.invoker_frame_setup(false, true);
        self.invoker_args_setup(&error_exit, true);
        self.call_invoker();
        self.invoker_epilogue(&error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "method-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    pub(super) fn method_invoker2(&mut self) -> MethodInvoker2 {
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
        self.invoker_prologue(&error_exit);
        self.invoker_frame_setup(false, true);
        self.invoker_args_setup(&error_exit, false);
        self.call_invoker();
        self.invoker_epilogue(&error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "method-invoker2");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    pub(super) fn block_invoker(&mut self) -> BlockInvoker {
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
        self.invoker_prologue(&error_exit);
        self.invoker_frame_setup(true, false);
        self.invoker_args_setup(&error_exit, true);
        self.call_invoker();
        self.invoker_epilogue(&error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "block-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    pub(super) fn block_invoker_with_self(&mut self) -> BlockInvoker {
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
        self.invoker_prologue(&error_exit);
        self.invoker_frame_setup(true, true);
        self.invoker_args_setup(&error_exit, true);
        self.call_invoker();
        self.invoker_epilogue(&error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "block-invoker-with-self");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    pub(super) fn binding_invoker(&mut self) -> BindingInvoker {
        let codeptr = self.jit.get_current_address();

        #[cfg(feature = "perf")]
        let pair = self.get_address_pair();

        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: Lfp
        let error_exit = self.jit.label();
        self.invoker_prologue(&error_exit);
        monoasm! { &mut self.jit,
            // set lfp
            movq r14, rdx;
            // set FuncId
            movl rdx, [r14 - (LFP_META)];
        };
        self.get_func_data();
        self.call_invoker_with_binding();
        self.invoker_epilogue(&error_exit);

        #[cfg(feature = "perf")]
        self.perf_info(pair, "binding-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    pub(super) fn fiber_invoker(&mut self) -> FiberInvoker {
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
        self.invoker_args_setup(&error_exit, true);
        self.call_invoker();
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

    pub(super) fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
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
        self.invoker_args_setup(&error_exit, true);
        self.call_invoker();
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

    pub(super) fn resume_fiber(
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

    pub(super) fn yield_fiber(&mut self) -> extern "C" fn(*mut Executor, Value) -> Option<Value> {
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

impl JitModule {
    ///
    /// Get *ClassId* of the *Value*.
    ///
    /// #### in
    /// - rdi: Value
    ///
    /// #### out
    /// - rax: ClassId
    ///
    pub(super) fn get_class(&mut self) -> DestLabel {
        let label = self.label();
        let l1 = self.label();
        let err = self.label();
        let fixnum = self.label();
        let flonum = self.label();
        let symbol = self.label();
        let nil = self.label();
        let true_ = self.label();
        let false_ = self.label();
        monoasm!(&mut self.jit,
        label:
            testq rdi, 0b001;
            jnz   fixnum;
            testq rdi, 0b010;
            jnz   flonum;
            testq rdi, 0b111;
            jnz   l1;
            testq rdi, rdi;
            jz    err;
            movl  rax, [rdi + (RVALUE_OFFSET_CLASS)];
            ret;
        l1:
            cmpb  rdi, (TAG_SYMBOL);
            je    symbol;
            cmpq  rdi, (NIL_VALUE);
            je    nil;
            cmpq  rdi, (TRUE_VALUE);
            je    true_;
            cmpq  rdi, (FALSE_VALUE);
            je    false_;
        err:
            movq  rax, (illegal_classid);  // rdi: Value
            call  rax;
            // no return
            ret;
        fixnum:
            movl  rax, (INTEGER_CLASS.u32());
            ret;
        flonum:
            movl  rax, (FLOAT_CLASS.u32());
            ret;
        symbol:
            movl  rax, (SYMBOL_CLASS.u32());
            ret;
        nil:
            movl  rax, (NIL_CLASS.u32());
            ret;
        true_:
            movl  rax, (TRUE_CLASS.u32());
            ret;
        false_:
            movl  rax, (FALSE_CLASS.u32());
            ret;
        );
        label
    }

    /*pub(super) fn entry_panic(&mut self) -> DestLabel {
        let label = self.label();
        monoasm! {&mut self.jit,
        label:
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::_dump_stacktrace);
            call rax;
            movq rdi, rbx;
            movq rsi, r12;
            movq rax, (runtime::panic);
            jmp rax;
            leave;
            ret;
        }
        label
    }*/
}

extern "C" fn illegal_classid(v: Value) {
    panic!("illegal Value for get_class(): {:016x}", v.id());
}

impl JitModule {
    fn invoker_prologue(&mut self, _error: &DestLabel) {
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
                movq rax, [rdx + (PROCDATA_OUTER)];        // rax <- outer_lfp
                movl rdx, [rdx + (PROCDATA_FUNCID)];    // rdx <- FuncId
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
    /// Handle arguments.
    ///
    /// In invoker call, CallSiteInfo is not available.
    /// All invoker callsites have no splat arguments, no keyword arguments, and no hash splat arguments (thus, no extra positional arguments).
    /// So conditions below are met, we can optimize this.
    /// - the callee is_simple (no optional, no post, no rest, no keyword, no keyword rest, no block arguments)
    /// - req == pos_num
    /// - thus, no single argument expansion
    ///
    /// ### in
    /// - rdi: arg_num
    /// - rsi: Meta
    /// - r15: &FuncData
    /// - r8: args: *const Value
    /// - r9: len: usize
    /// - r15: &FuncData
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn invoker_args_setup(&mut self, error_exit: &DestLabel, upward: bool) {
        let generic = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            // check if the callee is_simple
            shrq rsi, 56;
            testq rsi, 0b1_0000;
            jz  generic;
            // check if req == pos_num
            cmpw r9, [r15 + (FUNCDATA_MIN)];
            jne generic;
        }

        let loop_ = self.jit.label();
        monoasm! { &mut self.jit,
            // r8 : *args
            // r9 : len
            testq r9, r9;
            jeq  exit;
        };
        if upward {
            monoasm! { &mut self.jit,
                movq rdi, r9;
                negq r9;
            loop_:
                movq rax, [r8 + rdi * 8 - 8];
                movq [rsp + r9 * 8 - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
                subq rdi, 1;
                addq r9, 1;
                jne  loop_;
                jmp exit;
            };
        } else {
            monoasm! { &mut self.jit,
                negq r9;
            loop_:
                movq rax, [r8 + r9 * 8 + 8];
                movq [rsp + r9 * 8 - (RSP_LOCAL_FRAME + LFP_SELF)], rax;
                addq r9, 1;
                jne  loop_;
                jmp exit;
            };
        }

        monoasm! { &mut self.jit,
        generic:
            lea  rdx, [rsp - (RSP_LOCAL_FRAME)]; // callee lfp: Lfp
        }
        self.push_stack_offset();
        monoasm! { &mut self.jit,
            movq rcx, r9; // arg_num
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (if upward {handle_invoker_arguments} else {handle_invoker_arguments2});
            call rax;
        }
        self.pop_stack_offset();
        monoasm! { &mut self.jit,
            testq rax, rax;
            jz  error_exit;
        }
        self.jit.bind_label(exit);
    }

    fn invoker_epilogue(&mut self, error_exit: &DestLabel) {
        monoasm! { &mut self.jit,
        error_exit:
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        }
    }
}

extern "C" fn handle_invoker_arguments(
    vm: &mut Executor,
    globals: &Globals,
    callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
) -> Option<Value> {
    invoker_arguments_inner(vm, globals, callee_lfp, arg_num, args, true)
}

extern "C" fn handle_invoker_arguments2(
    vm: &mut Executor,
    globals: &Globals,
    callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
) -> Option<Value> {
    invoker_arguments_inner(vm, globals, callee_lfp, arg_num, args, false)
}

fn invoker_arguments_inner(
    vm: &mut Executor,
    globals: &Globals,
    callee_lfp: Lfp,
    arg_num: usize,
    args: *const Value,
    upward: bool,
) -> Option<Value> {
    let callee_fid = callee_lfp.func_id();
    let info = &globals.store[callee_fid];

    // required + optional + post + rest
    if let Err(err) = super::runtime::positional_invoker(info, callee_lfp, args, arg_num, upward) {
        vm.set_error(err);
        return None;
    };

    // keyword
    let params = info.kw_names();
    let callee_kw_pos = info.kw_reg_pos();
    for (id, _) in params.iter().enumerate() {
        unsafe {
            *callee_lfp.register_ptr(callee_kw_pos + id) = Some(Value::nil());
        }
    }

    Some(Value::nil())
}
