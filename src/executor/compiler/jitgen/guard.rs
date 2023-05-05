use super::*;

impl Codegen {
    ///
    /// Type guard.
    ///
    /// Generate type guard for *class_id*.
    /// If the type was not matched, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - rdi: Value
    ///
    /// ### registers destroyed
    ///
    /// - rax
    ///
    pub(super) fn guard_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        match class_id {
            INTEGER_CLASS => {
                //let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jz side_exit;
                );
                //self.guard_unpacked_class(class_id, side_exit);
                //self.jit.bind_label(exit);
            }
            FLOAT_CLASS => {
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz side_exit;
                    testq rdi, 0b010;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
            }
            NIL_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (NIL_VALUE);
                    jnz side_exit;
                );
            }
            SYMBOL_CLASS => {
                monoasm!(self.jit,
                    cmpb rdi, (TAG_SYMBOL);
                    jnz side_exit;
                );
            }
            TRUE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (TRUE_VALUE);
                    jnz side_exit;
                );
            }
            FALSE_CLASS => {
                monoasm!(self.jit,
                    cmpq rdi, (FALSE_VALUE);
                    jnz side_exit;
                );
            }
            _ => self.guard_unpacked_class(class_id, side_exit),
        }
    }

    fn guard_unpacked_class(&mut self, class_id: ClassId, side_exit: DestLabel) {
        monoasm!(self.jit,
            testq rdi, 0b111;
            jnz side_exit;
            cmpl [rdi + 4], (class_id.0);
            jne side_exit;
        )
    }

    ///
    /// Float guard and unboxing.
    ///
    /// Unbox a Float Value and return f64.
    /// If the input Value was not Float, deoptimize and go to *side_exit*.
    ///
    /// ### in
    ///
    /// - rdi: Value
    ///
    /// ### out
    ///
    /// - xmm0: f64
    ///
    /// ### registers destroyed
    ///
    /// - rdi, rax
    ///
    fn unbox_float(&mut self, xmm: u64, side_exit: DestLabel) -> DestLabel {
        let entry = self.jit.label();
        let flonum = self.jit.label();
        let exit = self.jit.label();
        monoasm!(self.jit,
        entry:
            testq rdi, 0b001;
            jnz side_exit;
            testq rdi, 0b010;
            jnz flonum;
        );
        self.guard_unpacked_class(FLOAT_CLASS, side_exit);
        monoasm! {&mut self.jit,
            movq xmm(xmm), [rdi + 16];
            jmp  exit;
        flonum:
            xorps xmm(xmm), xmm(xmm);
            movq rax, (FLOAT_ZERO);
            cmpq rdi, rax;
            // in the case of 0.0
            je exit;
            movq rax, rdi;
            sarq rax, 63;
            addq rax, 2;
            andq rdi, (-4);
            orq rdi, rax;
            rolq rdi, 61;
            movq xmm(xmm), rdi;
        exit:
        }
        entry
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
            (INTEGER_CLASS, Value::new_integer(-2558)),
            (INTEGER_CLASS, Value::new_integer(i32::MAX as i64)),
            (INTEGER_CLASS, Value::new_integer(i32::MIN as i64)),
            (FLOAT_CLASS, Value::new_float(1.44e-17)),
            (FLOAT_CLASS, Value::new_float(0.0)),
            (FLOAT_CLASS, Value::new_float(f64::MAX)),
            (FLOAT_CLASS, Value::new_float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
            (SYMBOL_CLASS, Value::new_symbol(IdentId::get_id("Ruby"))),
            (TRUE_CLASS, Value::bool(true)),
            (FALSE_CLASS, Value::bool(false)),
        ] {
            let entry_point = gen.jit.get_current_address();
            gen.guard_class(class, side_exit);
            monoasm!(gen.jit,
                xorq rax, rax;
                ret;
            );
            gen.jit.finalize();

            let func: fn(Value) -> u64 = unsafe { std::mem::transmute(entry_point.as_ptr()) };
            assert_eq!(0, func(value));
        }
    }

    #[test]
    fn guard_float() {
        let mut gen = Codegen::new(false, Value::new_object(OBJECT_CLASS));
        let side_exit = gen.entry_panic;
        let entry_point = gen.jit.get_current_address();
        gen.unbox_float(0, side_exit);
        monoasm!(gen.jit,
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
            let actual = func(Value::new_float(expected));
            if expected.is_nan() {
                assert!(actual.is_nan())
            } else {
                assert_eq!(expected, actual);
            }
        }
    }
}
