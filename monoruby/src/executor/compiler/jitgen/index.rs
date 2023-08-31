use super::*;

impl Codegen {
    pub(super) fn jit_get_index(
        &mut self,
        ctx: &mut BBContext,
        ret: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
            // flag for small positive integer
            let spi = ctx.is_u16_literal(idx);
            if spi.is_none() {
                self.fetch_slots(ctx, &[base, idx]);
            } else {
                self.fetch_slots(ctx, &[base]);
            }
            ctx.unlink_xmm(ret);
            let exit = self.jit.label();
            let out_range = self.jit.label();
            let heap = self.jit.label();
            let side_exit = self.gen_side_deopt(pc, ctx);
            self.load_guard_base(base, side_exit);

            if let Some(i) = spi {
                monoasm! { &mut self.jit,
                    movl rsi, (i);
                }
            } else {
                let exit = self.jit.label();
                self.load_rsi(idx);
                monoasm! { &mut self.jit,
                    testq rsi, 0b01;
                    jeq side_exit;
                    sarq rsi, 1;
                    // lower bound check
                    cmpq rsi, 0;
                    jge  exit;
                }
                self.get_array_length();
                monoasm! { &mut self.jit,
                    addq rsi, rax;
                    js   out_range;
                }
                monoasm! { &mut self.jit,
                exit:
                }
            }
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
        } else {
            self.fetch_slots(ctx, &[base, idx]);
            ctx.unlink_xmm(ret);
            let xmm_using = ctx.get_xmm_using();
            self.generic_index(&xmm_using, base, idx, pc);
        }
        self.jit_handle_error(ctx, pc);
        self.store_rax(ret);
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
        //let inline = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            //jle  inline;
            cmovgtq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
            //movq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
        //inline:
        }
    }

    pub(super) fn jit_index_assign(
        &mut self,
        ctx: &mut BBContext,
        src: SlotId,
        base: SlotId,
        idx: SlotId,
        pc: BcPc,
    ) {
        if pc.classid1() == ARRAY_CLASS && pc.classid2() == INTEGER_CLASS {
            // flag for small positive integer
            let spi = ctx.is_u16_literal(idx);
            if spi.is_none() {
                self.fetch_slots(ctx, &[base, idx, src]);
            } else {
                self.fetch_slots(ctx, &[base, src]);
            }
            let xmm_using = ctx.get_xmm_using();
            let store = self.jit.label();
            let exit = self.jit.label();
            let heap = self.jit.label();
            let generic = self.jit.label();
            let side_exit = self.gen_side_deopt(pc, ctx);
            self.load_guard_base(base, side_exit);

            if let Some(i) = spi {
                monoasm! { &mut self.jit,
                    movl rsi, (i);
                }
            } else {
                self.load_rsi(idx);
                monoasm! { &mut self.jit,
                    testq rsi, 0b01;
                    jeq side_exit;
                    sarq rsi, 1;
                    // lower range check
                    cmpq rsi, 0;
                    jlt generic;
                }
            }
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
                movq rax, [r14 - (conv(src))];
                movq [rdi], rax;
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
            self.xmm_save(&xmm_using);
            monoasm! { &mut self.jit,
                movq rdx, rbx;
                movq rcx, r12;
                movq r8, [r14 - (conv(src))];
                movq rax, (runtime::set_array_integer_index);
                call rax;
            };
            self.xmm_restore(&xmm_using);
            monoasm! { &mut self.jit,
                jmp  exit;
            };
            self.jit.select_page(0);
        } else {
            self.fetch_slots(ctx, &[base, idx, src]);
            let xmm_using = ctx.get_xmm_using();
            self.generic_index_assign(&xmm_using, base, idx, src, pc);
        }
        self.jit_handle_error(ctx, pc);
    }

    fn load_guard_base(&mut self, base: SlotId, side_exit: DestLabel) {
        self.load_rdi(base);
        self.guard_class(ARRAY_CLASS, side_exit);
        monoasm! { &mut self.jit,
            cmpw [rdi + (RVALUE_OFFSET_TY)], (ObjKind::ARRAY);
            jne  side_exit;
        }
    }

    fn generic_index(&mut self, using: &[Xmm], base: SlotId, idx: SlotId, pc: BcPc) {
        self.xmm_save(&using);
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, (pc.get_u64() + 8);
            movq rax, (runtime::get_index);
            call rax;
        }
        self.xmm_restore(&using);
    }

    fn generic_index_assign(
        &mut self,
        using: &[Xmm],
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        pc: BcPc,
    ) {
        self.xmm_save(&using);
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
        self.xmm_restore(&using);
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
