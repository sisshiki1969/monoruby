use super::*;

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
    pub(crate) fn array_index(&mut self, out_range: &DestLabel) {
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
    /// - caller save registers except xmm's
    ///
    pub(crate) fn array_index_assign(
        &mut self,
        using_xmm: UsingXmm,
        generic: &DestLabel,
        error: &DestLabel,
    ) {
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
