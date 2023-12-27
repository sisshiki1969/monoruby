use super::*;

impl AsmIr {
    pub(in crate::compiler::jitgen) fn index(
        &mut self,
        bb: &mut BBContext,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        if pc.classid1() == Some(ARRAY_CLASS) && pc.is_integer2() {
            self.gen_array_index(bb, dst, base, idx, pc);
        } else {
            self.fetch_slots(bb, &[base, idx]);
            bb.link_stack(dst);
            self.generic_index(bb, base, idx, pc);
        }
        self.rax2acc(bb, dst);
    }

    pub(in crate::compiler::jitgen) fn index_assign(
        &mut self,
        bb: &mut BBContext,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        if pc.classid1() == Some(ARRAY_CLASS) && pc.is_integer2() {
            self.writeback_acc(bb);
            self.fetch_to_reg(bb, base, GP::Rdi);
            self.fetch_to_reg(bb, src, GP::R15);
            self.gen_array_index_assign(bb, idx, pc);
        } else {
            self.fetch_slots(bb, &[base, idx, src]);
            self.generic_index_assign(bb, pc, base, idx, src);
        }
    }

    fn gen_array_index(
        &mut self,
        bb: &mut BBContext,
        dst: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        self.fetch_to_reg(bb, base, GP::Rdi);

        let deopt = self.new_deopt(bb, pc);
        if let Some(idx) = bb.is_u16_literal(idx) {
            self.inst.push(AsmInst::ArrayU16Index { idx, deopt });
        } else {
            let error = self.new_error(bb, pc);
            let using_xmm = bb.get_using_xmm();
            self.fetch_to_reg(bb, idx, GP::Rsi);
            self.inst.push(AsmInst::ArrayIndex {
                pc,
                using_xmm,
                error,
                deopt,
            });
        }
        bb.link_stack(dst);
    }

    fn gen_array_index_assign(&mut self, bb: &mut BBContext, idx: SlotId, pc: BcPc) {
        if let Some(idx) = bb.is_u16_literal(idx) {
            self.array_u16_index_assign(bb, pc, idx);
        } else {
            self.fetch_to_reg(bb, idx, GP::Rsi);
            self.array_index_assign(bb, pc);
        }
    }
}

impl Codegen {
    pub(super) fn generic_index(&mut self, using: UsingXmm, base: SlotId, idx: SlotId, pc: BcPc) {
        self.xmm_save(using);
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, (pc.get_u64() + 8);
            movq rax, (runtime::get_index);
            call rax;
        }
        self.xmm_restore(using);
    }

    pub(super) fn gen_array_u16_index(&mut self, idx: u16, deopt: DestLabel) {
        let out_range = self.jit.label();
        monoasm! { &mut self.jit,
            movl rsi, (idx);
        }
        self.guard_rdi_array(deopt);
        self.array_index(out_range);
    }

    pub(super) fn gen_array_index(
        &mut self,
        pc: BcPc,
        using_xmm: UsingXmm,
        error: DestLabel,
        deopt: DestLabel,
    ) {
        let out_range = self.jit.label();
        let checked = self.jit.label();
        let exit = self.jit.label();
        let generic = self.jit.label();
        // in this point, *base* is loaded to rdi and *idx* is loaded to rsi.
        self.guard_rdi_array(generic);
        self.array_bound_check(deopt);
        monoasm! { &mut self.jit,
            jge  checked;
        }
        self.get_array_length();
        monoasm! { &mut self.jit,
            addq rsi, rax;
            js   out_range;
        checked:
        }
        self.array_index(out_range);
        self.jit.bind_label(exit);

        self.jit.select_page(1);
        self.jit.bind_label(generic);
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rdi; // base: Value
            movq rcx, rsi; // idx: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq r8, (pc.get_u64() + 8);
            movq rax, (runtime::get_index);
            call rax;
        }
        self.xmm_restore(using_xmm);
        self.handle_error(error);
        monoasm! { &mut self.jit,
            jmp  exit;
        }
        self.jit.select_page(0);
    }

    pub(super) fn generic_index_assign(
        &mut self,
        using: UsingXmm,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        pc: BcPc,
    ) {
        self.xmm_save(using);
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, [r14 - (conv(src))];  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq r9, (pc.get_u64() + 8);
            movq rax, (runtime::set_index);
            call rax;
        };
        self.xmm_restore(using);
    }

    pub(super) fn gen_array_u16_index_assign(
        &mut self,
        using_xmm: UsingXmm,
        idx: u16,
        deopt: DestLabel,
    ) {
        let generic = self.jit.label();
        monoasm! { &mut self.jit,
            movl rsi, (idx);
        }
        self.guard_rdi_array(deopt);
        self.array_index_assign(using_xmm, generic);
    }

    pub(super) fn gen_array_index_assign(&mut self, using_xmm: UsingXmm, deopt: DestLabel) {
        let generic = self.jit.label();
        self.array_bound_check(deopt);
        monoasm! { &mut self.jit,
            jlt generic;
        };
        self.guard_rdi_array(deopt);
        self.array_index_assign(using_xmm, generic);
    }
}

impl Codegen {
    ///
    /// ### in
    /// - rdi: base Value
    /// - rsi: index non-negative i64
    ///
    /// ### out
    /// - rax: result Value
    ///
    fn array_index(&mut self, out_range: DestLabel) {
        let exit = self.jit.label();
        let heap = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            // inline
            // rsi must be a positive integer.
            cmpq rax, rsi;
            // upper bound check
            jle  out_range;
            movq rax, [rdi + rsi * 8 + (RVALUE_OFFSET_INLINE)];
        exit:
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        heap:
            // heap
            // rsi must be a positive integer.
            movq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            cmpq rax, rsi;
            // upper bound check
            jle  out_range;
            movq rdi, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            movq rax, [rdi + rsi * 8];
            jmp exit;
        out_range:
            movl rax, (NIL_VALUE);
            jmp  exit;
        }
        self.jit.select_page(0);
    }

    fn array_index_assign(&mut self, using_xmm: UsingXmm, generic: DestLabel) {
        let exit = self.jit.label();
        let heap = self.jit.label();
        let store = self.jit.label();

        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jgt  heap;
            // inline
            // rsi must be a positive integer.
            cmpq rax, rsi;
            // upper bound check
            jle  generic;
            lea  rdi, [rdi + rsi * 8 + (RVALUE_OFFSET_INLINE)];
        store:
            movq [rdi], r15;
        exit:
        };

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        heap:
            movq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            // upper range check
            cmpq rax, rsi;
            jle generic;
            movq rdi, [rdi + (RVALUE_OFFSET_HEAP_PTR)];
            lea  rdi, [rdi + rsi * 8];
            jmp  store;
        };
        self.jit.bind_label(generic);
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq rdx, rbx;
            movq rcx, r12;
            movq r8, r15;
            movq rax, (set_array_integer_index);
            call rax;
        };
        self.xmm_restore(using_xmm);
        monoasm! { &mut self.jit,
            jmp  exit;
        };
        self.jit.select_page(0);
    }

    ///
    /// Get array length.
    ///
    /// ### in
    /// - rdi: ptr to Array.
    ///
    /// ### out
    /// - rax: the length.
    ///
    fn get_array_length(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
        }
    }

    ///
    /// Check whether `rdi` is Array. If not, go *side_exit*.
    ///
    /// #### out
    /// - rdi: ptr to Array.
    ///
    fn guard_rdi_array(&mut self, side_exit: DestLabel) {
        self.guard_class_rdi(ARRAY_CLASS, side_exit);
        monoasm! { &mut self.jit,
            cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::ARRAY);
            jne  side_exit;
        }
    }

    fn array_bound_check(&mut self, deopt: DestLabel) {
        monoasm! { &mut self.jit,
            xchgq rdi, rsi;
            testq rdi, 0b01;
            jeq deopt;
            xchgq rdi, rsi;
            sarq rsi, 1;
            // lower bound check
            cmpq rsi, 0;
        }
    }
}

extern "C" fn set_array_integer_index(
    mut base: Value,
    index: i64,
    vm: &mut Executor,
    _globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    match base.as_array_mut().set_index(index, src) {
        Ok(val) => Some(val),
        Err(err) => {
            vm.set_error(err);
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn array_index() {
        run_test(
            r##"
            a = [0,1,2,3,4,5]
            res = []
            -10.step(10,1) do |x| res << a[x] end
            res
        "##,
        );
        run_test(
            r##"
            a = [0,1,2,3,4,5]
            [a[-100000],a[-7],a[-6],a[-5],a[-4],a[-3],a[-2],a[-1],a[0],a[1],a[2],a[3],a[4],a[5],a[6],a[7],a[8],a[9],a[100000]]
        "##,
        );
    }

    #[test]
    fn array_index_assign() {
        run_test(
            r##"
            a = [nil, nil, nil, nil, nil]
            0.step(10,1) do |x| a[x] = x end
            a
        "##,
        );
        run_test(
            r##"
            a = [nil, nil, nil, nil, nil]
            -5.step(-1,1) do |x| a[x] = x end
            a
        "##,
        );
        run_test(
            r##"
            a = [0,1,2,3,4,5]
            a[-6] = 10
            a[-5] = 9
            a[-4] = 8
            a[-3] = 7
            a[-2] = 6
            a[-1] = 5
            r = a
            a[0] = 20
            a[1] = 21
            a[2] = 22
            a[3] = 23
            a[4] = 24
            a[5] = 25
            a[6] = 26
            [a,r]
        "##,
        );
        run_test("a = []; a[100] = 42; a");
        run_test("a = []; a[100000] = 42; a");
        run_test_error("a = [1,2]; a[-3] = 42");
        run_test_error("a = [1,2]; a[-100000] = 42");
    }
}
