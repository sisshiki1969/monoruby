use super::*;

impl Codegen {
    /*///
    /// Generic index operation.
    ///
    /// Execute `base`[[`idx`]] and store the result to *rax*.
    ///
    /// ### out
    /// - rax: result Option<Value>
    ///
    /// ### destroy
    ///
    /// - caller save registers
    ///
    pub(super) fn generic_index(
        &mut self,
        using: UsingXmm,
        base: SlotId,
        idx: SlotId,
        pc: BytecodePtr,
    ) {
        self.xmm_save(using);
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, (pc.as_ptr() as usize + 8);
            movq rax, (runtime::get_index);
            call rax;
        }
        self.xmm_restore(using);
    }*/

    ///
    /// Array index operation with u16 index `idx``.
    ///
    /// Execute *rdi*[[`idx`]] and store the result to *rax*.
    ///
    /// ### in
    /// - rdi: base Array
    ///
    /// ### out
    /// - rax: result Value
    ///
    pub(super) fn gen_array_u16_index(&mut self, idx: u16) {
        let out_range = self.jit.label();
        monoasm! { &mut self.jit,
            movl rsi, (idx);
        }
        self.array_index(&out_range);
    }

    ///
    /// Array index operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    ///
    /// ### out
    /// - rax: result Value
    ///
    pub(super) fn gen_array_index(&mut self) {
        let generic = self.jit.label();
        let checked = self.jit.label();
        let negative = self.jit.label();
        monoasm! { &mut self.jit,
            sarq  rsi, 1;
            testq rsi, rsi;
            js  negative;
        checked:
        }
        self.array_index(&generic);

        self.jit.select_page(1);
        self.jit.bind_label(negative);
        self.get_array_length();
        monoasm! { &mut self.jit,
            addq rsi, rax;
            jns  checked;
            jmp  generic;
        }
        self.jit.select_page(0);
    }

    ///
    /// Generic index assign operation.
    ///
    /// `base`[[`idx`]] = `src`
    ///
    /// ### out
    /// - rax: result Value
    ///    
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn generic_index_assign(
        &mut self,
        using: UsingXmm,
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        pc: BytecodePtr,
    ) {
        self.xmm_save(using);
        monoasm! { &mut self.jit,
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, [r14 - (conv(src))];  // src: Value
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq r9, (pc.as_ptr() as usize + 8);
            movq rax, (runtime::set_index);
            call rax;
        };
        self.xmm_restore(using);
    }

    ///
    /// Array index assign operation with u16 index `idx`.
    ///
    /// ### in
    /// - rdi: base: Array
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn gen_array_u16_index_assign(
        &mut self,
        using_xmm: UsingXmm,
        error: &DestLabel,
        idx: u16,
    ) {
        let generic = self.jit.label();
        monoasm! { &mut self.jit,
            movl rsi, (idx);
        }
        self.array_index_assign(using_xmm, &generic, error);
    }

    ///
    /// Aray index assign operation.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index Fixnum
    /// - rdx: Value
    ///    
    /// ### destroy
    /// - caller save registers
    ///
    pub(super) fn gen_array_index_assign(&mut self, using_xmm: UsingXmm, error: &DestLabel) {
        let generic = self.jit.label();
        let checked = self.jit.label();
        let negative = self.jit.label();
        monoasm! { &mut self.jit,
            sarq  rsi, 1;
            testq rsi, rsi;
            js   negative;
        checked:
        };
        self.array_index_assign(using_xmm, &generic, error);

        self.jit.select_page(1);
        self.jit.bind_label(negative);
        self.get_array_length();
        monoasm! { &mut self.jit,
            addq rsi, rax;
            jns  checked;
            jmp  generic;
        }
        self.jit.select_page(0);
    }
}

impl Codegen {
    ///
    /// Array index operation with non-negative i64 index.
    ///
    /// ### in
    /// - rdi: base Array
    /// - rsi: index non-negative i64
    ///
    /// ### out
    /// - rax: result Value
    ///
    fn array_index(&mut self, out_range: &DestLabel) {
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

    ///
    /// Array index assign operation with non-negative i64 index.
    ///
    /// ### in
    /// - rdi: base: Array
    /// - rsi: index non-negative i64
    /// - rdx: result Value
    ///
    /// ### destroy
    /// - caller save registers
    ///
    fn array_index_assign(&mut self, using_xmm: UsingXmm, generic: &DestLabel, error: &DestLabel) {
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
            jle  generic;
            movq [rdi + rsi * 8 + (RVALUE_OFFSET_INLINE)], rdx;
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
            movq [rdi + rsi * 8], rdx;
            jmp  exit;
        };
        self.jit.bind_label(generic.clone());
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            movq r8, rdx;
            movq rdx, rbx;
            movq rcx, r12;
            movq rax, (set_array_integer_index);
            call rax;
        };
        self.xmm_restore(using_xmm);
        self.handle_error(&error);
        monoasm! { &mut self.jit,
            jmp  exit;
        };
        self.jit.select_page(0);
    }

    ///
    /// Get array length.
    ///
    /// ### in
    /// - rdi: Array.
    ///
    /// ### out
    /// - rdi: Array.
    /// - rax: length of the Array.
    ///
    pub(crate) fn get_array_length(&mut self) {
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
        }
    }
}

extern "C" fn set_array_integer_index(
    base: Value,
    index: i64,
    vm: &mut Executor,
    _globals: &mut Globals,
    src: Value,
) -> Option<Value> {
    base.as_array()
        .set_index(index, src)
        .map_err(|err| vm.set_error(err))
        .ok()
}

#[cfg(test)]
mod test {
    use crate::tests::*;

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
