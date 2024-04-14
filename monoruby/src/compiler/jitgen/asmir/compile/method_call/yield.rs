use super::*;

impl Codegen {
    pub(in crate::compiler::jitgen) fn gen_yield_not_cached(
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
        self.setup_block_frame();

        self.generic_call(callid, callsite.args, error);
        self.xmm_restore(using_xmm);
        self.handle_error(error);
    }

    ///
    /// Set up a callee frame for blocks.
    ///
    /// ### in
    /// - rdi: outer
    /// - r15: &FuncData
    ///
    /// ### destroy
    /// - rax
    ///
    fn setup_block_frame(&mut self) {
        monoasm! { &mut self.jit,
            subq  rsp, 32;
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
    }
}
