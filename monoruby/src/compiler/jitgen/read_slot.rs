use super::*;

impl Codegen {
    ///
    /// Fetch *reg* and store in a corresponding stack slot.
    ///
    fn fetch_slot(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_slot()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                );
                self.store_rax(reg);
                ctx[reg] = LinkMode::Both(freg);
            }
            LinkMode::Literal(v) => {
                self.literal_to_stack(reg, v);
                ctx[reg] = LinkMode::Stack;
            }
            LinkMode::R15 => {
                self.store_r15(reg);
                ctx.release(reg);
            }
            LinkMode::Both(_) | LinkMode::Stack => {}
        }
    }

    ///
    /// Fetch *reg*s and store in corresponding stack slots.
    ///
    pub(crate) fn fetch_slots(&mut self, ctx: &mut BBContext, reg: &[SlotId]) {
        reg.iter().for_each(|r| self.fetch_slot(ctx, *r));
    }

    pub(super) fn fetch_binary(&mut self, ctx: &mut BBContext, mode: &OpMode) {
        match mode {
            OpMode::RR(lhs, rhs) => {
                self.fetch_slots(ctx, &[*lhs, *rhs]);
            }
            OpMode::RI(r, _) | OpMode::IR(_, r) => {
                self.fetch_slots(ctx, &[*r]);
            }
        }
    }

    ///
    /// Fetch from *args* to *args* + *len* - 1 and store in corresponding stack slots.
    ///
    pub(crate) fn fetch_range(&mut self, ctx: &mut BBContext, args: SlotId, len: u16) {
        for reg in args.0..args.0 + len {
            self.fetch_slot(ctx, SlotId::new(reg))
        }
    }

    pub(crate) fn fetch_callargs(&mut self, ctx: &mut BBContext, callsite: &CallSiteInfo) {
        let CallSiteInfo { args, len, .. } = callsite;
        self.fetch_range(ctx, *args, *len as u16);
    }

    ///
    /// Fetch *arg* and store in *rax*.
    ///
    pub(super) fn fetch_to_rax(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_rax()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                );
                self.store_rax(reg);
                ctx[reg] = LinkMode::Both(freg);
            }
            LinkMode::Literal(v) => {
                monoasm!(&mut self.jit,
                    movq rax, (v.id());
                );
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.load_rax(reg);
            }
            LinkMode::R15 => {
                monoasm! {&mut self.jit,
                    movq rax, r15;
                }
            }
        }
    }

    ///
    /// Fetch *arg* and store in *rdi*.
    ///
    pub(super) fn fetch_to_rdi(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_rdi()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                    movq rdi, rax;
                );
                self.store_rax(reg);
                ctx[reg] = LinkMode::Both(freg);
            }
            LinkMode::Literal(v) => {
                monoasm!(&mut self.jit,
                    movq rdi, (v.id());
                );
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.load_rdi(reg);
            }
            LinkMode::R15 => {
                monoasm!(&mut self.jit,
                    movq rdi, r15;
                );
            }
        }
    }

    ///
    /// Fetch *arg* and store in *rsi*.
    ///
    pub(super) fn fetch_to_rsi(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_rax()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(freg) => {
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                    movq rsi, rax;
                );
                self.store_rax(reg);
                ctx[reg] = LinkMode::Both(freg);
            }
            LinkMode::Literal(v) => {
                monoasm!(&mut self.jit,
                    movq rsi, (v.id());
                );
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.load_rsi(reg);
            }
            LinkMode::R15 => {
                monoasm!(&mut self.jit,
                    movq rsi, r15;
                );
            }
        }
    }

    ///
    /// Fetch *arg* and store in *r15*.
    ///
    pub(super) fn fetch_to_r15(&mut self, ctx: &mut BBContext, reg: SlotId) {
        if reg >= ctx.sp {
            eprintln!("warning: {:?} >= {:?} in fetch_to_rax()", reg, ctx.sp);
            panic!();
        };
        match ctx[reg] {
            LinkMode::Xmm(freg) => {
                self.writeback_acc(ctx);
                let f64_to_val = self.f64_to_val;
                monoasm!( &mut self.jit,
                    movq xmm0, xmm(freg.enc());
                    call f64_to_val;
                    movq r15, rax;
                );
                self.store_rax(reg);
                ctx[reg] = LinkMode::Both(freg);
            }
            LinkMode::Literal(v) => {
                self.writeback_acc(ctx);
                monoasm!(&mut self.jit,
                    movq r15, (v.id());
                );
            }
            LinkMode::Both(_) | LinkMode::Stack => {
                self.writeback_acc(ctx);
                self.load_r15(reg);
            }
            LinkMode::R15 => {}
        }
    }

    ///
    /// Fetch *lhs* and *rhs* as f64, and store in xmm registers.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    pub(super) fn fetch_float_binary(
        &mut self,
        ctx: &mut BBContext,
        lhs: SlotId,
        rhs: SlotId,
        pc: BcPc,
    ) -> (Xmm, Xmm) {
        if lhs != rhs {
            (
                self.fetch_float_assume(ctx, lhs, pc.classid1(), pc),
                self.fetch_float_assume(ctx, rhs, pc.classid2(), pc),
            )
        } else {
            let lhs = self.fetch_float_assume(ctx, lhs, pc.classid1(), pc);
            (lhs, lhs)
        }
    }

    ///
    /// Fetch *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    ///
    pub(super) fn fetch_float_assume_float(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> Xmm {
        match ctx[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => freg,
            LinkMode::Stack => {
                let freg = ctx.link_new_both(reg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.unbox_float(freg.enc(), side_exit);
                freg
            }
            LinkMode::R15 => {
                let freg = ctx.link_new_both(reg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                monoasm! {&mut self.jit,
                    movq rdi, r15;
                }
                self.unbox_float(freg.enc(), side_exit);
                freg
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    let freg = ctx.link_new_xmm(reg);
                    let f = self.jit.const_f64(f);
                    monoasm! {&mut self.jit,
                        movq xmm(freg.enc()), [rip + f];
                    }
                    freg
                } else if let Some(i) = v.try_fixnum() {
                    let freg = ctx.link_new_both(reg);
                    let f = self.jit.const_f64(i as f64);
                    monoasm! {&mut self.jit,
                        movq [r14 - (conv(reg))], (Value::integer(i).id());
                        movq xmm(freg.enc()), [rip + f];
                    }
                    freg
                } else {
                    unreachable!()
                }
            }
        }
    }

    pub(crate) fn fetch_float_assume_float_enc(
        &mut self,
        ctx: &mut BBContext,
        reg: SlotId,
        pc: BcPc,
    ) -> u64 {
        self.fetch_float_assume_float(ctx, reg, pc).enc()
    }
}

impl Codegen {
    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi, rax
    ///
    fn fetch_float_assume(
        &mut self,
        ctx: &mut BBContext,
        rhs: SlotId,
        class: ClassId,
        pc: BcPc,
    ) -> Xmm {
        match class {
            INTEGER_CLASS => self.fetch_float_assume_integer(ctx, rhs, pc),
            FLOAT_CLASS => self.fetch_float_assume_float(ctx, rhs, pc),
            _ => unreachable!(),
        }
    }

    ///
    /// Read from a slot *reg* as f64, and store in xmm register.
    ///
    /// ### destroy
    /// - rdi
    ///
    fn fetch_float_assume_integer(&mut self, ctx: &mut BBContext, reg: SlotId, pc: BcPc) -> Xmm {
        match ctx[reg] {
            LinkMode::Both(freg) | LinkMode::Xmm(freg) => freg,
            LinkMode::Stack => {
                let freg = ctx.link_new_both(reg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                self.load_rdi(reg);
                self.integer_to_f64(freg.enc(), side_exit);
                freg
            }
            LinkMode::R15 => {
                let freg = ctx.link_new_both(reg);
                let side_exit = self.gen_side_deopt(pc, ctx);
                monoasm! {&mut self.jit,
                    movq rdi, r15;
                }
                self.integer_to_f64(freg.enc(), side_exit);
                freg
            }
            LinkMode::Literal(v) => {
                if let Some(f) = v.try_float() {
                    let freg = ctx.link_new_xmm(reg);
                    let f = self.jit.const_f64(f);
                    monoasm! {&mut self.jit,
                        movq xmm(freg.enc()), [rip + f];
                    }
                    freg
                } else if let Some(i) = v.try_fixnum() {
                    let freg = ctx.link_new_both(reg);
                    let f = self.jit.const_f64(i as f64);
                    monoasm! {&mut self.jit,
                        movq [r14 - (conv(reg))], (Value::integer(i).id());
                        movq xmm(freg.enc()), [rip + f];
                    }
                    freg
                } else {
                    unreachable!()
                }
            }
        }
    }

    ///
    /// Assume the Value is Integer, and convert to f64.
    ///
    /// side-exit if not Integer.
    ///
    /// ### in
    /// - rdi: Value
    ///
    /// ### out
    /// - xmm(*xmm*)
    ///
    /// ### destroy
    /// - none
    fn integer_to_f64(&mut self, xmm: u64, side_exit: DestLabel) {
        monoasm!(&mut self.jit,
            testq rdi, 0b01;
            jz side_exit;
            sarq rdi, 1;
            cvtsi2sdq xmm(xmm), rdi;
        );
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn float_test() {
        let gen = Codegen::new(false, Value::nil());

        let from_f64_entry = gen.jit.get_label_address(gen.f64_to_val);
        let from_f64: fn(f64) -> Value = unsafe { std::mem::transmute(from_f64_entry.as_ptr()) };

        for lhs in [
            0.0,
            4.2,
            35354354354.2135365,
            -3535354345111.5696876565435432,
            f64::MAX,
            f64::MAX / 10.0,
            f64::MIN * 10.0,
            f64::NAN,
        ] {
            let v = from_f64(lhs);
            let rhs = match v.unpack() {
                RV::Float(f) => f,
                _ => panic!(),
            };
            if lhs.is_nan() {
                assert!(rhs.is_nan());
            } else {
                assert_eq!(lhs, rhs);
            }
        }
    }

    #[test]
    fn float_test2() {
        let mut gen = Codegen::new(false, Value::nil());

        let panic = gen.entry_panic;
        let assume_int_to_f64 = gen.jit.label();
        monoasm!(&mut gen.jit,
        assume_int_to_f64:
            pushq rbp;
        );
        gen.integer_to_f64(0, panic);
        monoasm!(&mut gen.jit,
            popq rbp;
            ret;
        );
        gen.jit.finalize();
        let int_to_f64_entry = gen.jit.get_label_address(assume_int_to_f64);

        let int_to_f64: fn(Value) -> f64 =
            unsafe { std::mem::transmute(int_to_f64_entry.as_ptr()) };
        assert_eq!(143.0, int_to_f64(Value::integer(143)));
        assert_eq!(14354813558.0, int_to_f64(Value::integer(14354813558)));
        assert_eq!(-143.0, int_to_f64(Value::integer(-143)));
    }
}
