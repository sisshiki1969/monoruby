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
                let exit = self.jit.label();
                monoasm!(self.jit,
                    testq rdi, 0b001;
                    jnz exit;
                );
                self.guard_unpacked_class(class_id, side_exit);
                self.jit.bind_label(exit);
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
            (INTEGER_CLASS, Value::new_integer(i64::MAX)),
            (INTEGER_CLASS, Value::new_integer(i64::MIN)),
            (FLOAT_CLASS, Value::new_float(1.44e-17)),
            (FLOAT_CLASS, Value::new_float(0.0)),
            (FLOAT_CLASS, Value::new_float(f64::MAX)),
            (FLOAT_CLASS, Value::new_float(f64::MIN)),
            (NIL_CLASS, Value::nil()),
            (
                SYMBOL_CLASS,
                Value::new_symbol(IdentId::get_ident_id("Ruby")),
            ),
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
}
