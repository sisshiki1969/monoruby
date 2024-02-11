use super::*;

impl Codegen {
    pub(super) fn gen_invoker(&mut self) {
        self.method_invoker = self.method_invoker();
        self.method_invoker2 = self.method_invoker2();
        self.block_invoker = self.block_invoker();
        self.block_invoker_with_self = self.block_invoker_with_self();
        self.fiber_invoker = self.fiber_invoker();
        self.fiber_invoker_with_self = self.fiber_invoker_with_self();
    }

    fn method_invoker(&mut self) -> MethodInvoker {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: FuncId
        // rcx: receiver: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        // r11: Option<BlockHandler>
        monoasm! { &mut self.jit,
            movq r11, [rsp + 8];
        }
        self.invoker_prologue();
        self.invoker_frame_setup(false, true);
        self.invoker_prep();
        self.invoker_call();
        self.invoker_epilogue();

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "method-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn method_invoker2(&mut self) -> MethodInvoker2 {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: FuncId
        // rcx: receiver: Value
        // r8:  args: Arg
        // r9:  len: usize
        // r11: Option<BlockHandler>
        monoasm! { &mut self.jit,
            movq r11, [rsp + 8];
        }
        self.invoker_prologue();
        self.invoker_frame_setup(false, true);
        self.invoker_prep2();
        self.invoker_call();
        self.invoker_epilogue();

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "method-invoker2");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn block_invoker(&mut self) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockData
        // rcx: <dummy>
        // r8:  *args: *const Value
        // r9:  len: usize
        self.invoker_prologue();
        self.invoker_frame_setup(true, false);
        self.invoker_prep();
        self.invoker_call();
        self.invoker_epilogue();

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "block-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn block_invoker_with_self(&mut self) -> BlockInvoker {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockData
        // rcx: self: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        self.invoker_prologue();
        self.invoker_frame_setup(true, true);
        self.invoker_prep();
        self.invoker_call();
        self.invoker_epilogue();

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "block-invoker-with-self");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn fiber_invoker(&mut self) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockkData
        // rcx:
        // r8:  *args: *const Value
        // r9:  len: usize
        // [rsp + 8]: *mut Executor
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
        self.invoker_call();
        monoasm! { &mut self.jit,
            movq [rbx + (EXECUTOR_RSP_SAVE)], (-1); // [vm.rsp_save] <- -1 (terminated)
            movq rbx, [rbx + (EXECUTOR_PARENT_FIBER)]; // rbx <- [vm.parent_fiber]
            movq rsp, [rbx + (EXECUTOR_RSP_SAVE)]; // rsp <- [parent.rsp_save]
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "fiber-invoker");

        unsafe { std::mem::transmute(codeptr.as_ptr()) }
    }

    fn fiber_invoker_with_self(&mut self) -> FiberInvoker {
        let codeptr = self.jit.get_current_address();
        // rdi: &mut Executor
        // rsi: &mut Globals
        // rdx: &BlockkData
        // rcx: Value
        // r8:  *args: *const Value
        // r9:  len: usize
        // [rsp + 8]: *mut Executor
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
        self.invoker_call();
        monoasm! { &mut self.jit,
            movq [rbx + (EXECUTOR_RSP_SAVE)], (-1); // [vm.rsp_save] <- -1 (terminated)
            movq rbx, [rbx + (EXECUTOR_PARENT_FIBER)]; // rbx <- [vm.parent_fiber]
            movq rsp, [rbx + (EXECUTOR_RSP_SAVE)]; // rsp <- [parent.rsp_save]
        }
        self.pop_callee_save();
        monoasm! { &mut self.jit,
            ret;
        };

        #[cfg(feature = "perf")]
        self.perf_info(codeptr, "fiber-invoker-with-self");

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
    ///
    /// ### out
    /// - r15: &FuncData
    ///
    fn invoker_frame_setup(&mut self, invoke_block: bool, specify_self: bool) {
        if invoke_block {
            monoasm! { &mut self.jit,
                movq [rsp - (16 + LBP_BLOCK)], 0;
                movq rax, [rdx + (PROCINNER_OUTER)];        // rax <- outer_lfp
                movl rdx, [rdx + (PROCINNER_FUNCID)];    // rdx <- FuncId
            };
            self.get_func_data();
            // r15: &FuncData
            self.set_block_outer();
            if !specify_self {
                monoasm! { &mut self.jit,
                    // set self
                    movq  rcx, [rax - (LBP_SELF)];
                };
            }
        } else {
            self.get_func_data();
            monoasm! { &mut self.jit,
                // set block
                movq [rsp - (16 + LBP_BLOCK)], r11;
            };
            self.set_method_outer()
        }
        monoasm! { &mut self.jit,
            // set self
            movq [rsp - (16 + LBP_SELF)], rcx;
            // set meta
            movq rdi, [r15 + (FUNCDATA_META)];
            movq [rsp - (16 + LBP_META)], rdi;
        };
    }

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
            movq [rsp + r9 * 8 - (16 + LBP_SELF)], rax;
            subq r10, 1;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    fn invoker_prep2(&mut self) {
        let loop_exit = self.jit.label();
        let loop_ = self.jit.label();
        monoasm! { &mut self.jit,
            // set block
            movq [rsp - (16 + LBP_BLOCK)], r11;
            // r8 <- *args
            // r9 <- len
            movq rdi, r9;
            testq r9, r9;
            jeq  loop_exit;
            negq r9;
        loop_:
            movq rax, [r8 + r9 * 8 + 8];
            movq [rsp + r9 * 8 - (16 + LBP_SELF)], rax;
            addq r9, 1;
            jne  loop_;
        loop_exit:
        };
    }

    ///
    ///
    /// ### in
    /// - rdi: arg_num
    /// - r15: &FuncData
    ///
    fn invoker_call(&mut self) {
        monoasm! { &mut self.jit,
            lea  rdx, [rsp - 16];
            subq rsp, 4096;
            movq rcx, rdi; // arg_num
            movq rdi, rbx; // &mut Executor
            movq rsi, r12; // &mut Globals
            movq rax, (handle_invoker_arguments);
            call rax;
            // set arg len
            movq rdx, rax;
            addq rsp, 4096;
        }
        self.push_frame();
        self.set_lfp();
        monoasm! { &mut self.jit,
            // r15 : &FuncData
            // set pc
            movq r13, [r15 + (FUNCDATA_PC)];
            call [r15 + (FUNCDATA_CODEPTR)];
            movq rdi, [rsp - (16 + BP_PREV_CFP)];
            movq [rbx + (EXECUTOR_CFP)], rdi;
        };
    }

    fn invoker_epilogue(&mut self) {
        monoasm! { &mut self.jit,
            popq r15;
            popq r14;
            popq r13;
            popq r12;
            popq rbx;
            ret;
        };
    }
}
