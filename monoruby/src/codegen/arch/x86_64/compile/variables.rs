use super::*;
use crate::codegen::jitgen::asmir::compile_shared::set_ivar;

impl Codegen {
    ///
    /// Load ivar on `var_table`.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### out
    /// - dst: Value
    ///
    /// #### destroy
    /// - rdi, rsi, rdx
    ///
    pub(super) fn load_ivar_heap(&mut self, ivarid: IvarId, is_object_ty: bool, self_: bool, dst: GP) {
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        let exit = self.jit.label();
        let nil = self.jit.label();
        monoasm! { &mut self.jit,
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
        }
        if !self_ {
            self.check_len(idx, &nil);
        }
        monoasm! { &mut self.jit,
            movq rdi, [rdx + (MONOVEC_PTR)]; // ptr
            movq R(dst as _), [rdi + (idx as i32 * 8)];
            testq R(dst as _), R(dst as _);
            jne  exit;
        nil:
            movq R(dst as _), (NIL_VALUE);
        exit:
        }
    }
}

impl Codegen {
    ///
    /// Guard that the object in *rdi* is not frozen.
    ///
    /// Check bit 1 of the flag field at RVALUE_OFFSET_FLAG.
    /// If the object is frozen, call the runtime to set a FrozenError
    /// and jump to the error side exit.
    ///
    /// #### in
    /// - rdi: &RValue (also the Value, since lower bits are 0 for heap objects)
    ///
    /// #### destroy
    /// - rsi (only on frozen path)
    ///
    pub(super) fn guard_frozen(&mut self, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            testb [rdi + (RVALUE_OFFSET_FLAG as i32)], (0b10);
            jnz  deopt;
        }
    }

    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy
    /// - caller-save registers
    ///
    pub(super) fn store_ivar_heap(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using: UsingFpr,
        wb: bool,
    ) {
        self.store_ivar_heap_inner(src, ivarid, is_object_ty, Some(using), wb);
    }

    ///
    /// Store *src* in an instance var *ivarid* of the object *rdi*.
    ///
    /// #### in
    /// - rdi: &RValue
    ///
    /// #### destroy (if using.is_some())
    /// - caller-save registers
    ///
    /// #### destroy (if using.is_none())
    /// - rdx
    fn store_ivar_heap_inner(
        &mut self,
        src: GP,
        ivarid: IvarId,
        is_object_ty: bool,
        using: Option<UsingFpr>,
        wb: bool,
    ) {
        let exit = self.jit.label();
        let generic = if let Some(using) = using {
            Some((using, self.jit.label()))
        } else {
            None
        };
        let ivar = ivarid.get() as u32;
        let idx = if is_object_ty {
            ivar - OBJECT_INLINE_IVAR as u32
        } else {
            ivar
        };
        monoasm! { &mut self.jit,
            movq rdx, [rdi + (RVALUE_OFFSET_VAR as i32)];
        }
        if let Some((_, generic)) = &generic {
            self.check_len(idx, generic);
        }
        monoasm! { &mut self.jit,
            movq rdx, [rdx + (MONOVEC_PTR)]; // ptr
            movq [rdx + (idx as i32 * 8)], R(src as _);
        }
        // Fast-path store: emit the write barrier (rdi still holds the
        // parent &RValue) unless the stored value is provably immediate.
        // The generic path below goes through `set_ivar`, which already
        // barriers, so it jumps straight to `exit`.
        if wb {
            self.emit_write_barrier_rdi(src);
        }
        monoasm! { &mut self.jit,
        exit:
        }

        if let Some((using, generic)) = generic {
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            generic:
                movl rsi, (ivar);
                movq rdx, R(src as _);
            );
            self.fpr_save(using);
            monoasm!( &mut self.jit,
                movq rax, (set_ivar);
                call rax;
            );
            self.fpr_restore(using);
            monoasm!( &mut self.jit,
                jmp  exit;
            );
            self.jit.select_page(0);
        }
    }

    ///
    /// Check whether the length of `ivar_table` is greater than `idx`.
    ///
    /// #### in
    /// - rdx: ivar_table
    ///
    fn check_len(&mut self, idx: u32, fail: &DestLabel) {
        monoasm! { &mut self.jit,
            // check var_table is not None
            testq rdx, rdx;
            jz   fail;
            // check capa is not 0
            cmpq [rdx + (MONOVEC_CAPA)], 0; // capa
            jz   fail;
            // check len > idx
            cmpq [rdx + (MONOVEC_LEN)], (idx); // len
            jle  fail;
        }
    }
}

impl Codegen {
    ///
    /// Emit the generational GC write barrier after a JIT inline store
    /// whose parent object is in `rdi` and whose stored child value is in
    /// `child`.
    ///
    /// Fast path — a young parent (the common case), an already-remembered
    /// parent, or an immediate child — is three flag/immediate tests with
    /// no scratch register and no allocator access. The rare slow path is a
    /// single call into the shared `JitModule::write_barrier` stub, which
    /// saves *all* caller-saved registers (so it is fully transparent to
    /// the surrounding code, needing no liveness information) and calls
    /// `jit_module::jit_write_barrier`. See `doc/gc.md`.
    ///
    pub(super) fn emit_write_barrier_rdi(&mut self, child: GP) {
        let skip = self.jit.label();
        let wb = self.write_barrier.clone();
        monoasm! { &mut self.jit,
            // barrier armed?  (WB_ARMED = flag bit 6 = old & not remembered)
            // Young objects and already-remembered old objects both have it
            // clear, so the common case skips after this single test.
            testb [rdi + (RVALUE_OFFSET_FLAG as i32)], 0x40;
            jz   skip;
            // child immediate?  (heap pointers have the low 3 bits clear)
            testq R(child as _), 0b111;
            jnz  skip;
            // Slow path: rdi already holds the parent (the stub's argument).
            call wb;
        skip:
        }
    }

    ///
    /// The inline half of an ivar store's type check (`LInst::IvarTyCheck`):
    /// test *src* against the slot's type state *expect*, and on a mismatch
    /// call the shared `ivar_ty_observe` stub (object in rdi, the ivar id
    /// and the value passed on the stack) to widen it. Transparent: every
    /// register survives; flags do not.
    ///
    pub(super) fn emit_ivar_ty_check(
        &mut self,
        src: GP,
        ivarid: IvarId,
        expect: crate::ivar_ty::IvarTy,
        state: u64,
    ) {
        let ok = self.jit.label();
        let cold = self.jit.label();
        let r = src as u64;
        if expect.nil() {
            monoasm! { &mut self.jit,
                cmpq R(r), (NIL_VALUE);
                jeq  ok;
            }
        }
        match expect.mono_class() {
            None => {
                monoasm! { &mut self.jit,
                    jmp cold;
                }
            }
            Some(INTEGER_CLASS) => {
                monoasm! { &mut self.jit,
                    testq R(r), 0b001;
                    jz   cold;
                }
            }
            Some(FLOAT_CLASS) => {
                monoasm! { &mut self.jit,
                    testq R(r), 0b001;
                    jnz  cold;
                    testq R(r), 0b010;
                    jnz  ok;
                    testq R(r), 0b111;
                    jnz  cold;
                    cmpl [R(r) + 4], (FLOAT_CLASS.u32());
                    jne  cold;
                }
            }
            Some(BOOL_CLASS) => {
                monoasm! { &mut self.jit,
                    cmpq R(r), (TRUE_VALUE);
                    jeq  ok;
                    cmpq R(r), (FALSE_VALUE);
                    jne  cold;
                }
            }
            Some(SYMBOL_CLASS) => {
                monoasm! { &mut self.jit,
                    cmpb R(r), (TAG_SYMBOL);
                    jne  cold;
                }
            }
            Some(class) => {
                monoasm! { &mut self.jit,
                    testq R(r), 0b111;
                    jnz  cold;
                    cmpl [R(r) + 4], (class.u32());
                    jne  cold;
                }
            }
        }
        let stub = self.ivar_ty_observe.clone();
        let id = ivarid.get() as i32;
        // A scratch register other than *src*, saved around its use (`pop`
        // leaves the flags alone).
        let t = if src == GP::Rax { GP::Rdi } else { GP::Rax } as u64;
        let slow = |jit: &mut monoasm::JitMemory| {
            let call = jit.label();
            let covered = jit.label();
            monoasm! { jit,
            cold:
                // The state may have widened since this was compiled.
                pushq R(t);
                movq R(t), (state);
                movq R(t), [R(t)];
                cmpq R(t), (crate::ivar_ty::IvarTy::TOP.get() as i32);
                jeq  covered;
                cmpq R(r), (NIL_VALUE);
                jne  call;
                testq R(t), (crate::ivar_ty::IvarTy::NIL_BIT as i32);
                jne  covered;
            call:
                popq R(t);
                lea  rsp, [rsp - 16];
                movq [rsp + 8], R(r);
                movq [rsp], (id);
                call stub;
                lea  rsp, [rsp + 16];
                jmp  ok;
            covered:
                popq R(t);
                jmp  ok;
            }
        };
        if self.jit.get_page() == 0 {
            self.jit.select_page(1);
            slow(&mut self.jit);
            self.jit.select_page(0);
        } else {
            let skip = self.jit.label();
            monoasm! { &mut self.jit,
                jmp skip;
            }
            slow(&mut self.jit);
            self.jit.bind_label(skip);
        }
        self.jit.bind_label(ok);
    }

    ///
    /// `LInst::IvarUnset`: if *reg* (a raw ivar slot) is 0, record nil into
    /// the slot's type state through the `ivar_ty_observe` stub and
    /// deoptimize.
    ///
    pub(super) fn emit_ivar_unset(
        &mut self,
        reg: GP,
        ivarid: IvarId,
        self_obj: bool,
        deopt: &DestLabel,
    ) {
        let cold = self.jit.label();
        let r = reg as u64;
        monoasm! { &mut self.jit,
            testq R(r), R(r);
            jz   cold;
        }
        let stub = self.ivar_ty_observe.clone();
        let id = ivarid.get() as i32;
        let deopt = deopt.clone();
        let page = self.jit.get_page();
        // Only made when it is bound: an unbound label stays on the
        // assembler's label list for good, and every finalize walks it.
        let skip = (page != 0).then(|| self.jit.label());
        if let Some(skip) = &skip {
            let skip = skip.clone();
            monoasm! { &mut self.jit,
                jmp skip;
            }
        } else {
            self.jit.select_page(1);
        }
        self.jit.bind_label(cold);
        if self_obj {
            self.encode_linst(LInst::Load {
                dst: GP::Rdi.into(),
                mem: LMem::Slot(SlotId::self_()),
            });
        }
        monoasm! { &mut self.jit,
            lea  rsp, [rsp - 16];
            movq [rsp + 8], (NIL_VALUE);
            movq [rsp], (id);
            call stub;
            lea  rsp, [rsp + 16];
            jmp  deopt;
        }
        match skip {
            Some(skip) => self.jit.bind_label(skip),
            None => self.jit.select_page(0),
        }
    }

    ///
    /// The bulk variant of [`Self::emit_write_barrier_rdi`], for an inline
    /// store that wrote *several* children (an array slice copy). Checking
    /// each one is not worth it, so an armed parent is remembered regardless
    /// of what was stored — the same safe over-approximation
    /// `RValue::write_barrier_bulk` makes. Parent in rdi.
    ///
    pub(super) fn emit_write_barrier_bulk_rdi(&mut self) {
        let skip = self.jit.label();
        let wb = self.write_barrier.clone();
        monoasm! { &mut self.jit,
            testb [rdi + (RVALUE_OFFSET_FLAG as i32)], 0x40;
            jz   skip;
            call wb;
        skip:
        }
    }
}

impl Codegen {
    pub(super) fn load_dyn_var(&mut self, src: DynVar) {
        self.get_outer(src.outer);
        let offset = conv(src.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq rax, [rax - (offset)];
        );
    }

    pub(in crate::codegen::jitgen) fn load_dyn_var_specialized(&mut self, offset: usize, reg: SlotId) {
        monoasm!( &mut self.jit,
            movq rax, [rbp + ((offset - (BP_CFP + CFP_LFP) as usize - 8 - conv(reg) as usize))];
        );
    }

    pub(super) fn store_dyn_var(&mut self, dst: DynVar, src: GP) {
        self.get_outer(dst.outer);
        let offset = conv(dst.reg) - LFP_OUTER;
        monoasm!( &mut self.jit,
            movq [rax - (offset)], R(src as _);
        );
    }

    pub(in crate::codegen::jitgen) fn store_dyn_var_specialized(&mut self, offset: usize, dst: SlotId, src: GP) {
        monoasm!( &mut self.jit,
            movq [rbp + ((offset - (BP_CFP + CFP_LFP) as usize - 8 - conv(dst) as usize))], R(src as _);
        );
    }

    fn get_outer(&mut self, outer: usize) {
        monoasm!( &mut self.jit,
            movq rax, [r14];
        );
        for _ in 0..outer - 1 {
            monoasm!( &mut self.jit,
                movq rax, [rax];
            );
        }
    }
}

impl Codegen {
    pub(super) fn load_cvar(&mut self, name: IdentId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::get_class_var);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }

    pub(super) fn check_cvar(&mut self, name: IdentId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::check_class_var);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }

    pub(super) fn store_cvar(&mut self, name: IdentId, src: SlotId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::set_class_var);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }

    pub(super) fn load_gvar(&mut self, name: IdentId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rax, (runtime::get_global_var);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }

    pub(super) fn store_gvar(&mut self, name: IdentId, src: SlotId, using_fpr: UsingFpr) {
        self.fpr_save(using_fpr);
        monoasm! { &mut self.jit,
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (name.get());
            movq rcx, [rbp - (rbp_local(src))];
            movq rax, (runtime::set_global_var);
            call rax;
        };
        self.fpr_restore(using_fpr);
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn ivar_in_different_class() {
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.get, c.get]
        "##,
            r##"
            class S
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                def get
                    [@a, @b, @c, @d, @e, @f, @g, @h]
                end
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
            end
            
            "##,
        );
        run_test_with_prelude(
            r##"
            s = S.new
            c = C.new
            [s.get, c.get]
        "##,
            r##"
            class S < Array
                def initialize
                    @a = 10
                    @b = 20
                    @c = 30
                    @d = 40
                    @e = 50
                    @f = 60
                    @g = 70
                    @h = 80
                end
                def get
                    [@a, @b, @c, @d, @e, @f, @g, @h]
                end
            end

            class C < S
                def initialize
                    @h = 8
                    @g = 7
                    @f = 6
                    @e = 5
                    @d = 4
                    @c = 3
                    @b = 2
                    @a = 1
                end
            end
            
            "##,
        );
    }
}
