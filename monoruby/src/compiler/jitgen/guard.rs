use super::*;

impl Codegen {
    /*///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version, and if the version is changed, call `find_method` and
    /// compare obtained FuncId and cached FuncId.
    /// If different, jump to `deopt`.
    /// If identical, update the cached version and go on.
    ///
    /// ### in
    /// - rdi: receiver: Value
    ///
    /// ### out
    /// - rdi: receiver: Value
    ///
    /// ### destroy
    /// - caller save registers except rdi
    /// - stack
    ///
    pub(super) fn guard_class_version_with_recovery(
        &mut self,
        cached_fid: FuncId,
        cached_version: u32,
        callid: CallSiteId,
        using_xmm: UsingXmm,
        deopt: DestLabel,
        error: DestLabel,
    ) {
        assert_eq!(0, self.jit.get_page());
        let global_version = self.class_version;
        let unmatch = self.jit.label();
        let exit = self.jit.label();
        let fail = self.jit.label();
        let cached_version = self.jit.data_i32(cached_version as i32);
        monoasm! { &mut self.jit,
            movl rax, [rip + cached_version];
            cmpl [rip + global_version], rax;
            jne  unmatch;
        exit:
        }

        self.jit.select_page(1);
        self.jit.bind_label(unmatch);
        self.xmm_save(using_xmm);
        monoasm! { &mut self.jit,
            pushq rdi;
            pushq r13;
            movq rcx, rdi;
            movq rdi, rbx;
            movq rsi, r12;
            movl rdx, (callid.get());  // CallSiteId
            movq rax, (runtime::find_method);
            call rax;   // rax <- Option<FuncId>
            popq r13;
            popq rdi;
            movl rax, rax;
        }
        self.xmm_restore(using_xmm);
        self.handle_error(error);
        monoasm! { &mut self.jit,
            cmpl rax, (cached_fid.get());
            jne  fail;
            movl rax, [rip + global_version];
            movl [rip + cached_version], rax;
            jmp  exit;
        fail:
            movq rdi, (Value::symbol_from_str("__version_guard").id());
            jmp  deopt;
        }
        self.jit.select_page(0);
    }*/

    ///
    /// Class version guard for JIT.
    ///
    /// Check the cached class version.
    /// If different, jump to `deopt`.
    ///
    /// ### destroy
    /// - rax
    ///
    pub(super) fn guard_class_version(&mut self, cached_version: u32, deopt: DestLabel) {
        assert_eq!(0, self.jit.get_page());
        let global_version = self.class_version;
        let fail = self.jit.label();
        let cached_version = self.jit.data_i32(cached_version as i32);
        monoasm! { &mut self.jit,
            movl rax, [rip + cached_version];
            cmpl [rip + global_version], rax;
            jne  fail;
        }

        self.jit.select_page(1);
        monoasm! { &mut self.jit,
        fail:
            movq rdi, (Value::symbol_from_str("__version_guard").id());
            jmp  deopt;
        }
        self.jit.select_page(0);
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
    pub(crate) fn guard_class_rdi(&mut self, class_id: ClassId, deopt: DestLabel) {
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
    pub(super) fn guard_class(&mut self, reg: GP, class_id: ClassId, fail: DestLabel) {
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
            fail
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
                self.guard_rvalue(reg, FLOAT_CLASS, fail);
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
            _ => self.guard_rvalue(reg, class_id, fail),
        }
        //if reg != GP::Rdi {
        //    monoasm!( &mut self.jit,
        //        xchgq R(reg as _), rdi;
        //    );
        //}
    }

    pub(super) fn guard_array_ty(&mut self, r: GP, deopt: DestLabel) {
        let label = self.set_rdi_for_deopt(r, deopt);
        monoasm! { &mut self.jit,
            testq R(r as _), 0b111;
            jnz  label;
            cmpw [R(r as _) + (RVALUE_OFFSET_TY)], (ObjTy::ARRAY.get());
            jne  label;
        }
    }

    fn set_rdi_for_deopt(&mut self, r: GP, deopt: DestLabel) -> DestLabel {
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
            deopt
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
    pub(super) fn float_to_f64(&mut self, reg: GP, xmm: Xmm, deopt: DestLabel) {
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

    ///
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
    pub(super) fn numeric_val_to_f64(&mut self, reg: GP, xmm: Xmm, deopt: DestLabel) {
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
    }

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
    fn float_val_to_f64(&mut self, reg: GP, xmm: Xmm, side_exit: DestLabel) {
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
    fn guard_rvalue(&mut self, reg: GP, class_id: ClassId, deopt: DestLabel) {
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
        let mut gen = Codegen::new(false);
        let side_exit = gen.entry_panic;

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
            gen.guard_class_rdi(class, side_exit);
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
        let mut gen = Codegen::new(false);
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        let x = Xmm(0);
        gen.float_to_f64(GP::Rdi, x, side_exit);
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

    #[test]
    fn unbox_integer_float() {
        let mut gen = Codegen::new(false);
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        let x = Xmm(0);
        gen.numeric_val_to_f64(GP::Rdi, x, side_exit);
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
    }
}
