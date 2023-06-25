use super::*;

impl Codegen {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    /// - rdi: Value
    ///
    pub(in crate::executor) fn guard_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                monoasm!( &mut self.jit,
                    testq rdi, 0b001;
                    jz side_exit;
                );
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!( &mut self.jit,
                    testq rdi, 0b001;
                    jnz side_exit;
                    testq rdi, 0b010;
                    jnz exit;
                );
                self.guard_rvalue(FLOAT_CLASS, side_exit);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq rdi, (NIL_VALUE);
                    jnz side_exit;
                );
            }
            SYMBOL_CLASS => {
                monoasm!( &mut self.jit,
                    cmpb rdi, (TAG_SYMBOL);
                    jnz side_exit;
                );
            }
            TRUE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq rdi, (TRUE_VALUE);
                    jnz side_exit;
                );
            }
            FALSE_CLASS => {
                monoasm!( &mut self.jit,
                    cmpq rdi, (FALSE_VALUE);
                    jnz side_exit;
                );
            }
            _ => self.guard_rvalue(class_id, side_exit),
        }
    }

    ///
    /// Float guard.
    ///
    /// Generate type guard for Float.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    /// - rdi: Value
    ///
    pub(super) fn guard_float(&mut self, side_exit: DestLabel) {
        self.guard_class(FLOAT_CLASS, side_exit)
    }

    ///
    /// Float guard and unboxing.
    ///
    /// Unbox a Float Value and return f64.
    ///
    /// If the input Value was not Float, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    ///
    /// - rdi, rax
    ///
    pub(super) fn unbox_float(&mut self, xmm: u64, side_exit: DestLabel) {
        monoasm!( &mut self.jit,
            testq rdi, 0b001;
            jnz side_exit;
        );
        self.float_to_f64(xmm, side_exit);
    }

    ///
    /// Convert the Value to f64.
    ///
    /// side-exit if neither Float nor Integer.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm(*xmm*)
    ///
    /// ### registers destroyed
    ///
    /// - rdi, rax
    ///
    pub(super) fn unbox_integer_float_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        let integer = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq rdi, 0b001;
            jnz integer;
        }
        self.float_to_f64(xmm, side_exit);
        monoasm! {&mut self.jit,
            jmp  exit;
        integer:
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        };
        self.jit.bind_label(exit);
    }

    ///
    /// Copy the value(f64) of Float to *xmm*.
    ///
    /// ### in
    /// - rdi: Value (must be a flonum or heap-allocated Float)
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - rax, rdi
    ///
    fn float_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        let flonum = self.jit.label();
        let exit = self.jit.label();
        monoasm! { &mut self.jit,
            testq rdi, 0b010;
            jnz flonum;
        }
        self.guard_rvalue(FLOAT_CLASS, side_exit);
        let flonum_to_f64 = self.flonum_to_f64;
        monoasm! {&mut self.jit,
            movq xmm(xmm), [rdi + 16];
            jmp  exit;
        flonum:
            call flonum_to_f64;
            movq xmm(xmm), xmm0;
        exit:
        }
    }

    ///
    /// Class guard for RValue.
    ///
    /// ### in
    /// - rdi: Value
    ///
    fn guard_rvalue(&mut self, class_id: ClassId, side_exit: DestLabel) {
        monoasm!( &mut self.jit,
            testq rdi, 0b111;
            jnz side_exit;
            cmpl [rdi + 4], (class_id.0);
            jne side_exit;
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn guard_class() {
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
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
            gen.guard_class(class, side_exit);
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
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        gen.unbox_float(0, side_exit);
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
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        gen.unbox_integer_float_to_f64(0, side_exit);
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
