use super::*;

impl Codegen {
    fn check_version(&mut self, cached_version: u32, fail: &DestLabel) {
        let global_version = self.class_version_label();
        monoasm! { &mut self.jit,
            movl rax, [rip + global_version];
            cmpl rax, (cached_version);
            jne  fail;
        }
    }

    fn version_guard_fail(&mut self, deopt: &DestLabel) {
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
        cached_version: u32,
        position: Option<BytecodePtr>,
        with_recovery: bool,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        self.check_version(cached_version, &fail);

        self.jit.select_page(1);
        self.exec_recompile(position, fail, RecompileReason::ClassVersionGuardFailed);
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }

    pub(super) fn guard_class_version_specialized(
        &mut self,
        cached_version: u32,
        idx: usize,
        deopt: &DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let fail = self.jit.label();
        self.check_version(cached_version, &fail);

        self.jit.select_page(1);
        self.exec_specialized_recompile(idx, fail, RecompileReason::ClassVersionGuardFailed);
        self.version_guard_fail(deopt);
        self.jit.select_page(0);
    }

    fn dec_counter(
        &mut self,
        dec: &DestLabel,
        recompile: &DestLabel,
        deopt: &DestLabel,
        count: i32,
    ) {
        let counter = self.jit.data_i32(count);
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            cmpl [rip + counter], 0;
            jlt deopt;
            jeq recompile;
        dec:
            subl [rip + counter], 1;
            jmp deopt;
        );
    }

    pub(super) fn recompile_and_deopt(
        &mut self,
        position: Option<BytecodePtr>,
        deopt: &DestLabel,
        reason: RecompileReason,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();

        self.dec_counter(&dec, &recompile, deopt, COUNT_DEOPT_RECOMPILE);

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.exec_recompile(position, recompile, reason);
        monoasm!( &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        );
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt");
    }

    pub(super) fn recompile_and_deopt_specialized(
        &mut self,
        deopt: &DestLabel,
        idx: usize,
        reason: RecompileReason,
    ) {
        let recompile = self.jit.label();
        let dec = self.jit.label();

        self.dec_counter(&dec, &recompile, deopt, COUNT_DEOPT_RECOMPILE_SPECIALIZED);

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.exec_specialized_recompile(idx, recompile, reason);
        monoasm! { &mut self.jit,
            xorq rdi, rdi;
            jmp dec;
        }
        self.jit.select_page(0);
        #[cfg(feature = "jit-debug")]
        eprintln!(" => deopt_specialized");
    }

    ///
    /// Execute recompilation of the loop / method.
    ///
    /// ### in
    /// - r12: &mut Globals
    /// - r14: Lfp
    ///
    /// ### destroy
    /// - rax
    ///
    fn exec_recompile(
        &mut self,
        position: Option<BytecodePtr>,
        label: DestLabel,
        reason: RecompileReason,
    ) {
        self.jit.bind_label(label);
        self.jit.save_registers();
        monoasm!( &mut self.jit,
            movq rdi, r12;
            movq rsi, r14;
        );
        if let Some(pc) = position {
            monoasm!( &mut self.jit,
                movq rdx, (pc.as_ptr());
                movl rcx, (reason as u32);
                movq rax, (exec_jit_recompile_partial);
                call rax;
            );
        } else {
            monoasm!( &mut self.jit,
                movl rdx, (reason as u32);
                movq rax, (exec_jit_recompile_method);
                call rax;
            );
        }
        self.jit.restore_registers();
    }

    fn exec_specialized_recompile(
        &mut self,
        idx: usize,
        label: DestLabel,
        reason: RecompileReason,
    ) {
        self.jit.bind_label(label);
        self.jit.save_registers();
        monoasm!( &mut self.jit,
            movq rdi, r12;
            movq rsi, (idx);
            movl rdx, (reason as u32);
            movq rax, (exec_jit_specialized_recompile);
            call rax;
        );
        self.jit.restore_registers();
    }

    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - rdi: Value
    ///
    pub(crate) fn guard_class_rdi(&mut self, class_id: ClassId, deopt: &DestLabel) {
        self.guard_class(GP::Rdi, class_id, deopt)
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
            cmpw [R(r as _) + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  label;
        }
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
    pub(super) fn float_to_f64(&mut self, reg: GP, xmm: Xmm, deopt: &DestLabel) {
        let l1 = self.jit.label();
        monoasm!( &mut self.jit,
            testq R(reg as _), 0b001;
            jnz l1;
        );
        self.float_val_to_f64(reg, xmm, deopt);
        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        monoasm!( &mut self.jit,
        l1:
            movq rdi, R(reg as _);
            jmp deopt;
        );
        self.jit.select_page(0);
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
    pub(super) fn numeric_val_to_f64(&mut self, reg: GP, xmm: Xmm, deopt: &DestLabel) {
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
    fn float_val_to_f64(&mut self, reg: GP, xmm: Xmm, side_exit: &DestLabel) {
        let heap = self.jit.label();
        let exit = self.jit.label();
        let r = reg as _;
        let dst = xmm.enc();
        monoasm! { &mut self.jit,
            testq R(r), 0b010;
            jz    heap;
            xorps xmm(dst), xmm(dst);
            movq rax, (FLOAT_ZERO);
            cmpq R(r), rax;
            // in the case of 0.0
            je exit;
            movq rax, R(r);
            sarq rax, 63;
            addq rax, 2;
            andq R(r), (-4);
            orq R(r), rax;
            rolq R(r), 61;
            movq xmm(dst), R(r);
        exit:
        }

        assert_eq!(0, self.jit.get_page());
        self.jit.select_page(1);
        self.jit.bind_label(heap);
        self.guard_rvalue(reg, FLOAT_CLASS, side_exit);
        monoasm! {&mut self.jit,
            movq xmm(xmm.enc()), [R(r) + (RVALUE_OFFSET_KIND)];
            jmp  exit;
        }
        self.jit.select_page(0);
    }

    ///
    /// Class guard for RValue.
    ///
    /// If the class of *reg* was not matched *class_id*, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
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
        let mut gen = Codegen::new();
        let side_exit = gen.entry_panic.clone();

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
            let entry_point = gen.jit.get_current_address();
            gen.guard_class_rdi(class, &side_exit);
            monoasm!( &mut gen.jit,
                xorq rax, rax;
                ret;
            );
            gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
    }

    #[test]
    fn unbox_float() {
        let mut gen = Codegen::new();
        let side_exit = gen.entry_panic.clone();
        let entry_point = gen.jit.get_current_address();
        let x = Xmm(0);
        gen.float_to_f64(GP::Rdi, x, &side_exit);
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
        let x = Xmm(0);
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
