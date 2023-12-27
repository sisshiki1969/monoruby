use super::*;

impl Codegen {
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
    pub(super) fn guard_class(&mut self, reg: GP, class_id: ClassId, deopt: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jz deopt;
                );
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    testq R(reg as _), 0b001;
                    jnz deopt;
                    testq R(reg as _), 0b010;
                    jnz exit;
                );
                self.guard_rvalue(reg, FLOAT_CLASS, deopt);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (NIL_VALUE);
                    jnz deopt;
                );
            }
            SYMBOL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpb R(reg as _), (TAG_SYMBOL);
                    jnz deopt;
                );
            }
            TRUE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (TRUE_VALUE);
                    jnz deopt;
                );
            }
            FALSE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq R(reg as _), (FALSE_VALUE);
                    jnz deopt;
                );
            }
            _ => self.guard_rvalue(reg, class_id, deopt),
        }
    }

    ///
    /// Float guard.
    ///
    /// Generate type guard for Float.
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    pub(super) fn guard_float(&mut self, reg: GP, deopt: DestLabel) {
        self.guard_class(reg, FLOAT_CLASS, deopt)
    }

    ///
    /// Fixnum guard.
    ///
    /// Generate type guard for Fixnum(i63).
    /// If the type was not matched, go to *deopt*.
    ///
    /// ### in
    /// - R(*reg*): Value
    ///
    pub(super) fn guard_fixnum(&mut self, reg: GP, deopt: DestLabel) {
        self.guard_class(reg, INTEGER_CLASS, deopt)
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
    /// - rdi, rax
    ///
    pub(super) fn float_to_f64(&mut self, reg: GP, xmm: u64, deopt: DestLabel) {
        monoasm!( &mut self.jit,
            testq R(reg as _), 0b001;
            jnz deopt;
        );
        self.float_val_to_f64(reg, xmm, deopt);
    }

    ///
    /// Convert the Value to f64.
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
    /// ### registers destroyed
    ///
    /// - rdi, rax
    ///
    pub(super) fn numeric_val_to_f64(&mut self, reg: GP, xmm: u64, deopt: DestLabel) {
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
            sarq R(reg as _), 1;
            cvtsi2sdq xmm(xmm), R(reg as _);
        exit:
        };
    }

    ///
    /// Copy the value(f64) of Float to *xmm*.
    ///
    /// ### in
    /// - R(*reg*): Value (must be a flonum or heap-allocated Float)
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    fn float_val_to_f64(&mut self, reg: GP, xmm: u64, side_exit: DestLabel) {
        let flonum = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq R(reg as _), 0b010;
            jnz flonum;
        }
        self.guard_rvalue(reg, FLOAT_CLASS, side_exit);
        let flonum_to_f64 = self.flonum_to_f64;
        monoasm! {&mut self.jit,
            movq xmm(xmm), [R(reg as _) + (RVALUE_OFFSET_KIND)];
            jmp  exit;
        flonum:
        }
        if reg != GP::Rdi {
            monoasm! {&mut self.jit,
                movq rdi, R(reg as _);
            }
        }
        monoasm! {&mut self.jit,
            call flonum_to_f64;
            movq xmm(xmm), xmm0;
        exit:
        }
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
mod test {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false, Value::object(OBJECT_CLASS));
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
            (SYMBOL_CLASS, Value::symbol(IdentId::get_id("Ruby"))),
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
        let mut gen = Codegen::new(false, Value::object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        gen.float_to_f64(GP::Rdi, 0, side_exit);
        monoasm!( &mut gen.jit,
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
        let mut gen = Codegen::new(false, Value::object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        gen.numeric_val_to_f64(GP::Rdi, 0, side_exit);
        monoasm!( &mut gen.jit,
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
