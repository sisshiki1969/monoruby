use super::*;

impl Codegen {
    /// Compare the global class version against this unit's snapshot.
    ///
    /// The snapshot side is an *inline imm32*, not the unit's snapshot
    /// word: `movl rax, imm32` has a fixed 5-byte encoding (unlike a
    /// `cmp m32, imm` whose imm8 short form would make the patch site
    /// variable-length), so a successful salvage re-stamps the version
    /// by patching the 4 bytes before the label bound here (see
    /// `Codegen::set_class_version`). This removes the per-unit data
    /// load — one scattered cache line per compilation unit — from
    /// every guard on the hot path; only the shared, always-hot global
    /// word is read. The word (`cached_version`) still exists as the
    /// salvage records' key and for the aarch64 twin, which reads it.
    ///
    /// Every emitted compare registers its patch site here — this is
    /// the single choke point, so a guard shape cannot forget to (the
    /// #1157 failure mode: an unpatchable snapshot immediate silently
    /// turns every salvage into a permanent per-call deopt).
    fn check_version(&mut self, _cached_version: DestLabel, fail: &DestLabel) {
        let global_version = self.class_version_label();
        let version = self.class_version();
        let patch_site = self.jit.label();
        monoasm! { &mut self.jit,
            movl rax, (version);
        }
        self.jit.bind_label(patch_site.clone());
        self.unit_version_patch_sites.push(patch_site);
        monoasm! { &mut self.jit,
            cmpl rax, [rip + global_version];
            jne  fail;
        }
    }

    pub(super) fn version_guard_fail(&mut self, deopt: &DestLabel) {
        monoasm! { &mut self.jit,
            movq rdi, (Value::symbol_from_str("__version_guard").id());
            jmp  deopt;
        }
    }
    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, recompile immediately, and jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_class_version(
        &mut self,
        cached_version: DestLabel,
        position: Option<BytecodePtr>,
        with_recovery: bool,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        self.check_version(cached_version, &fail);
        let with_recovery = if with_recovery {
            let label = self.jit.label();
            self.jit.bind_label(label.clone());
            Some(label)
        } else {
            None
        };

        self.jit.select_page(1);
        self.gen_recompile(
            position,
            fail,
            RecompileReason::ClassVersionGuardFailed,
            with_recovery,
        );
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }

    pub(super) fn guard_class_version_specialized(
        &mut self,
        cached_version: DestLabel,
        idx: usize,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        self.check_version(cached_version, &fail);

        self.jit.select_page(1);
        self.gen_recompile_specialized(idx, fail, RecompileReason::ClassVersionGuardFailed);
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }

    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    pub(super) fn guard_class(&mut self, reg: GP, class_id: ClassId, fail: &DestLabel) {
        let fail = if reg != GP::Rdi {
            let label = self.jit.label();
            if self.jit.get_page() == 0 {
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                label:
                    movq rdi, R(reg as _);
                    jmp fail;
                );
                self.jit.select_page(0);
            } else {
                let label = self.jit.label();
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    jmp exit;
                label:
                    movq rdi, R(reg as _);
                    jmp fail;
                exit:
                );
            }
            label
        } else {
            fail.clone()
        };
        match class_id {
            INTEGER_CLASS => {
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jz fail;
                );
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jnz fail;
                    testq R(reg as _), 0b010;
                    jnz exit;
                );
                self.guard_rvalue(reg, FLOAT_CLASS, &fail);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (NIL_VALUE);
                    jnz fail;
                );
            }
            SYMBOL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpb R(reg as _), (TAG_SYMBOL);
                    jnz fail;
                );
            }
            TRUE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (TRUE_VALUE);
                    jnz fail;
                );
            }
            FALSE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (FALSE_VALUE);
                    jnz fail;
                );
            }
            BOOL_CLASS => {
                // TRUE_VALUE (0x1c) and FALSE_VALUE (0x14) differ only in
                // bit 3, so OR'ing bit 3 in collapses both to TRUE_VALUE.
                // No other tagged value lands at 0x1c after the OR. Use
                // rax as scratch so the source register is preserved for
                // the downstream consumer.
                monoasm!( &mut self.jit,
                    movq rax, R(reg as _);
                    orq rax, 8;
                    cmpq rax, (TRUE_VALUE);
                    jnz fail;
                );
            }
            _ => self.guard_rvalue(reg, class_id, &fail),
        }
        //if reg != GP::Rdi {
        //    monoasm!( &mut self.jit,
        //        xchgq R(reg as _), rdi;
        //    );
        //}
    }

    ///
    /// Route a class-guard miss through the `profile` recorder, then on to
    /// *deopt*.
    ///
    /// The dispatch stub's entry guard has always fed
    /// `jit class guard failed stats` (`class_guard_stub` jumps to
    /// `jit_class_guard_fail`), but an in-body guard just took its side exit,
    /// so the table only ever showed entry misses. That made it actively
    /// misleading: a site deopting millions of times on a receiver-class
    /// check read as "no class guard ever failed here". Now both are counted,
    /// against the class of the value that actually failed.
    ///
    /// The recorder is a C call sitting *before* the side exit's write-back,
    /// which still reads the register file, so every caller-saved register is
    /// preserved across it — `rax` included (`save_registers` deliberately
    /// skips it, and the write-back may hold a value there), and `rdi` above
    /// all, since the deopt reads it back as the reason value.
    ///
    /// Emitted on the cold page, and only under `profile`.
    ///
    #[cfg(feature = "profile")]
    pub(super) fn class_guard_fail_recorder(&mut self, deopt: &DestLabel) -> DestLabel {
        let entry = self.jit.label();
        let deopt = deopt.clone();
        let inline = self.jit.get_page() != 0;
        let skip = self.jit.label();
        if inline {
            monoasm!( &mut self.jit, jmp skip; );
        } else {
            self.jit.select_page(1);
        }
        monoasm!( &mut self.jit,
        entry:
            subq rsp, 16;
            movq [rsp], rax;
        );
        self.save_registers();
        monoasm!( &mut self.jit,
            movq rdx, rdi;      // the value that failed the guard
            movq rdi, rbx;      // &mut Executor
            movq rsi, r12;      // &mut Globals
            movq rax, (guard_fail);
            subq rsp, 4088;
            call rax;
            addq rsp, 4088;
        );
        self.restore_registers();
        monoasm!( &mut self.jit,
            movq rax, [rsp];
            addq rsp, 16;
            jmp deopt;
        );
        if inline {
            self.jit.bind_label(skip);
        } else {
            self.jit.select_page(0);
        }
        entry
    }

    ///
    /// [`Self::guard_class`] for a guard whose miss is a real side exit, as
    /// opposed to a dispatch arm's miss (`LInst::BrClassNe`). Identical code
    /// except that under `profile` the miss is recorded first — see
    /// [`Self::class_guard_fail_recorder`].
    ///
    pub(crate) fn guard_class_deopt(&mut self, reg: GP, class_id: ClassId, deopt: &DestLabel) {
        #[cfg(feature = "profile")]
        {
            let recorder = self.class_guard_fail_recorder(deopt);
            self.guard_class(reg, class_id, &recorder);
        }
        #[cfg(not(feature = "profile"))]
        {
            self.guard_class(reg, class_id, deopt);
        }
    }

    ///
    /// Class guard used in JIT dispatch stub.
    ///
    /// if *reg* is Bignum, always dispatched to VM entry.
    ///
    pub(crate) fn guard_class2(&mut self, reg: GP, class_id: ClassId, fail: &DestLabel) {
        let vm_entry = self.vm_entry();
        let fail = if reg != GP::Rdi {
            let label = self.jit.label();
            if self.jit.get_page() == 0 {
                self.jit.select_page(1);
                monoasm!( &mut self.jit,
                label:
                    movq rdi, R(reg as _);
                    jmp fail;
                );
                self.jit.select_page(0);
            } else {
                let label = self.jit.label();
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    jmp exit;
                label:
                    movq rdi, R(reg as _);
                    jmp fail;
                exit:
                );
            }
            label
        } else {
            fail.clone()
        };
        match class_id {
            INTEGER_CLASS => {
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jnz exit;
                );
                self.guard_rvalue(reg, INTEGER_CLASS, &fail);
                monoasm!( &mut self.jit,
                    jmp vm_entry;
                );
                self.jit.bind_label(exit);
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jnz fail;
                    testq R(reg as _), 0b010;
                    jnz exit;
                );
                self.guard_rvalue(reg, FLOAT_CLASS, &fail);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (NIL_VALUE);
                    jnz fail;
                );
            }
            SYMBOL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpb R(reg as _), (TAG_SYMBOL);
                    jnz fail;
                );
            }
            TRUE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (TRUE_VALUE);
                    jnz fail;
                );
            }
            FALSE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (FALSE_VALUE);
                    jnz fail;
                );
            }
            BOOL_CLASS => {
                // TRUE_VALUE (0x1c) and FALSE_VALUE (0x14) differ only in
                // bit 3, so OR'ing bit 3 in collapses both to TRUE_VALUE.
                // No other tagged value lands at 0x1c after the OR. Use
                // rax as scratch so the source register is preserved for
                // the downstream consumer.
                monoasm!( &mut self.jit,
                    movq rax, R(reg as _);
                    orq rax, 8;
                    cmpq rax, (TRUE_VALUE);
                    jnz fail;
                );
            }
            _ => self.guard_rvalue(reg, class_id, &fail),
        }
        //if reg != GP::Rdi {
        //    monoasm!( &mut self.jit,
        //        xchgq R(reg as _), rdi;
        //    );
        //}
    }

    pub(super) fn guard_array_ty(&mut self, r: GP, deopt: &DestLabel) {
        let label = self.set_rdi_for_deopt(r, deopt);
        monoasm! { &mut self.jit,
            testq R(r as _), 0b111;
            jnz  label;
            cmpb [R(r as _) + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  label;
        }
    }

    pub(super) fn guard_capture(&mut self, deopt: &DestLabel) {
        let captured = self.jit.label();
        self.jit.branch_if_captured(&captured);
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        captured:
            movq rdi, (Value::symbol_from_str("__capture_guard").id());
            jmp deopt;
        }
        self.jit.select_page(0);
    }

    fn set_rdi_for_deopt(&mut self, r: GP, deopt: &DestLabel) -> DestLabel {
        if r != GP::Rdi {
            assert_eq!(0, self.jit.get_page());
            self.jit.select_page(1);
            let label = self.jit.label();
            monoasm! { &mut self.jit,
            label:
                movq rdi, R(r as _);
                jmp deopt;
            }
            self.jit.select_page(0);
            label
        } else {
            deopt.clone()
        }
    }

    ///
    /// Float guard and unboxing.
    ///
    /// Unbox a Float Value and return f64.
    ///
    /// If the input Value was not Float, go to *deopt*.
    ///
    /// ### in
    ///
    /// - R(*reg*): Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi
    ///
    pub(super) fn float_to_f64(&mut self, reg: GP, dst: u64, deopt: &DestLabel) {
        let l1 = self.jit.label();
        monoasm!( &mut self.jit,
            testq R(reg as _), 0b001;
            jnz l1;
        );
        self.float_val_to_f64(reg, dst, deopt);
        // The fixnum bailout goes to the cold page when we are emitting hot
        // code; when this conversion itself sits in cold (page-1) code — a
        // block whose receiver stayed polymorphic, e.g. behind a
        // nil-tolerant guard — it is emitted in line instead, jumped over
        // by the fall-through (same dual shape as `guard_class`'s
        // fail-wrapper).
        if self.jit.get_page() == 0 {
            self.jit.select_page(1);
            monoasm!( &mut self.jit,
            l1:
                movq rdi, R(reg as _);
                jmp deopt;
            );
            self.jit.select_page(0);
        } else {
            let skip = self.jit.label();
            monoasm!( &mut self.jit,
                jmp skip;
            l1:
                movq rdi, R(reg as _);
                jmp deopt;
            skip:
            );
        }
    }

    /*///
    /// Convert Value to f64.
    ///
    /// go to *deopt* if *reg* was neither Float nor Fixnum(i63).
    ///
    /// ### in
    ///
    /// - R(*reg*): Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rax, rdi, R(*reg*)
    ///
    pub(super) fn numeric_val_to_f64(&mut self, reg: GP, xmm: VirtFPReg, deopt: &DestLabel) {
        let integer = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq R(reg as _), 0b001;
            jnz integer;
        }
        self.float_val_to_f64(reg, xmm, deopt);
        monoasm! {&mut self.jit,
            jmp  exit;
        integer:
        };
        self.integer_val_to_f64(reg, xmm);
        self.jit.bind_label(exit);
    }*/

    ///
    /// Copy the value(f64) of Float to *xmm*.
    ///
    /// If the input Value was not Float, go to *side_exit*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    /// ### Safety
    /// - if *reg* is Fixnum, cause UB.
    ///
    fn float_val_to_f64(&mut self, reg: GP, dst: u64, side_exit: &DestLabel) {
        let heap = self.jit.label();
        let exit = self.jit.label();
        let r = reg as _;
        monoasm! { &mut self.jit,
            testq R(r), 0b010;
            jz    heap;
            xorps xmm(dst), xmm(dst);
        }
        if reg == GP::Rax {
            monoasm! { &mut self.jit,
                movq rdi, (FLOAT_ZERO);
                cmpq R(r), rdi;
                // in the case of 0.0
                je exit;
            }
        } else {
            monoasm! { &mut self.jit,
                movq rax, (FLOAT_ZERO);
                cmpq R(r), rax;
                // in the case of 0.0
                je exit;
                movq rax, R(r);
            }
        }
        monoasm! { &mut self.jit,
            movq rdi, R(r);
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(dst), rdi;
        exit:
        }

        // Heap-Float load: on the cold page when emitting hot code, in line
        // (jumped over by the fall-through) when this conversion itself is
        // already in cold page-1 code — same dual shape as `float_to_f64`.
        if self.jit.get_page() == 0 {
            self.jit.select_page(1);
            self.jit.bind_label(heap);
            self.guard_rvalue(reg, FLOAT_CLASS, side_exit);
            monoasm! {&mut self.jit,
                movq xmm(dst), [R(r) + (RVALUE_OFFSET_KIND)];
                jmp  exit;
            }
            self.jit.select_page(0);
        } else {
            let skip = self.jit.label();
            monoasm! {&mut self.jit,
                jmp  skip;
            }
            self.jit.bind_label(heap);
            self.guard_rvalue(reg, FLOAT_CLASS, side_exit);
            monoasm! {&mut self.jit,
                movq xmm(dst), [R(r) + (RVALUE_OFFSET_KIND)];
            }
            self.jit.bind_label(skip);
        }
    }

    ///
    /// Class guard for RValue.
    ///
    /// If the class of *reg* was not matched *class_id*, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): RValue
    ///
    pub(super) fn guard_rvalue(&mut self, reg: GP, class_id: ClassId, deopt: &DestLabel) {
        monoasm!( &mut self.jit,
            testq R(reg as _), 0b111;
            jnz deopt;
            cmpl [R(reg as _) + 4], (class_id.u32());
            jne deopt;
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn guard_class() {
        let mut r#gen = Codegen::new();
        let side_exit = r#gen.entry_panic.clone();

        for (class, value) in [
            (INTEGER_CLASS, Value::integer(-2558)),
            (INTEGER_CLASS, Value::integer(i32::MAX as i64)),
            (INTEGER_CLASS, Value::integer(i32::MIN as i64)),
            (FLOAT_CLASS, Value::float(1.44e-17)),
            (FLOAT_CLASS, Value::float(0.0)),
            (FLOAT_CLASS, Value::float(f64::MAX)),
            (FLOAT_CLASS, Value::float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
            (SYMBOL_CLASS, Value::symbol_from_str("Ruby")),
            (TRUE_CLASS, Value::bool(true)),
            (FALSE_CLASS, Value::bool(false)),
        ] {
            let entry_point = r#gen.jit.get_current_address();
            r#gen.guard_class(GP::Rdi, class, &side_exit);
            monoasm!( &mut r#gen.jit,
                xorq rax, rax;
                ret;
            );
            r#gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
    }

    #[test]
    fn unbox_float() {
        let mut r#gen = Codegen::new();
        let side_exit = r#gen.entry_panic.clone();
        let entry_point = r#gen.jit.get_current_address();
        let x = 2;
        r#gen.float_to_f64(GP::Rdi, x, &side_exit);
        monoasm!( &mut r#gen.jit,
            movq xmm0, xmm(x);
            ret;
        );
        r#gen.jit.finalize();

        for expected in [
            1.44e-17,
            16857.555,
            0.0,
            -52182.84922374,
            f64::MAX,
            f64::MIN,
            f64::NAN,
            1.0 / 0.0,
            -1.0 / 0.0,
        ] {
            let func: fn(Value) -> f64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            let actual = func(Value::float(expected));
            if expected.is_nan() {
                assert!(actual.is_nan())
            } else {
                assert_eq!(expected, actual);
            }
        }
    }

    /*#[test]
    fn unbox_integer_float() {
        let mut gen = Codegen::new(false);
        let side_exit = gen.entry_panic();
        let entry_point = gen.jit.get_current_address();
        let x = VirtFPReg(0);
        gen.numeric_val_to_f64(GP::Rdi, x, &side_exit);
        monoasm!( &mut gen.jit,
            movq xmm0, xmm(x.enc());
            ret;
        );
        gen.jit.finalize();

        for expected in [
            1.44e-17,
            16857.555,
            0.0,
            -52182.84922374,
            f64::MAX,
            f64::MIN,
            f64::NAN,
            f64::INFINITY,
            f64::NEG_INFINITY,
            1.0 / 0.0,
            -1.0 / 0.0,
        ] {
            let func: fn(Value) -> f64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            let actual = func(Value::float(expected));
            if expected.is_nan() {
                assert!(actual.is_nan())
            } else {
                assert_eq!(expected, actual);
            }
        }

        for (expected, i) in [
            (5555555555.0, 5555555555),
            (100.0, 100),
            (0.0, 0),
            (-4444444444.0, -4444444444),
        ] {
            let func: fn(Value) -> f64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            let actual = func(Value::integer(i));
            assert_eq!(expected, actual);
        }
    }*/
}
