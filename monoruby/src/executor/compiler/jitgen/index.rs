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
            let spi = ctx.try_positive_i16_literal(idx);
            if spi.is_none() {
                self.fetch_slots(ctx, &[base, idx]);
            } else {
                self.fetch_slots(ctx, &[base]);
            }
            ctx.dealloc_xmm(ret);
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
            ctx.dealloc_xmm(ret);
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
        let inline = self.jit.label();
        monoasm! { &mut self.jit,
            movq rax, [rdi + (RVALUE_OFFSET_ARY_CAPA)];
            cmpq rax, (ARRAY_INLINE_CAPA);
            jle  inline;
            movq rax, [rdi + (RVALUE_OFFSET_HEAP_LEN)];
        inline:
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
            let spi = ctx.try_positive_i16_literal(idx);
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

    fn generic_index(&mut self, xmm_using: &[Xmm], base: SlotId, idx: SlotId, pc: BcPc) {
        self.xmm_save(&xmm_using);
        monoasm! { &mut self.jit,
            movq rdi, rbx; // &mut Interp
            movq rsi, r12; // &mut Globals
            movq rdx, [r14 - (conv(base))]; // base: Value
            movq rcx, [r14 - (conv(idx))]; // idx: Value
            movq r8, (pc.get_u64() + 8);
            movq rax, (runtime::get_index);
            call rax;
        }
        self.xmm_restore(&xmm_using);
    }

    fn generic_index_assign(
        &mut self,
        xmm_using: &[Xmm],
        base: SlotId,
        idx: SlotId,
        src: SlotId,
        pc: BcPc,
    ) {
        self.xmm_save(&xmm_using);
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
        self.xmm_restore(&xmm_using);
    }
}
