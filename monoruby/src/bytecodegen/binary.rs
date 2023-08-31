use super::*;
use ::paste::paste;

impl BytecodeGen {
    /// Generate bytecode Ir for binary operations.
    pub(super) fn gen_binop(
        &mut self,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        match op {
            BinOp::Shr => return self.gen_binop_method(IdentId::_SHR, lhs, rhs, use_mode, loc),
            BinOp::Shl => return self.gen_binop_method(IdentId::_SHL, lhs, rhs, use_mode, loc),
            BinOp::Match => {
                return self.gen_binop_method(IdentId::get_id("=~"), lhs, rhs, use_mode, loc)
            }
            _ => {}
        };
        let use_mode2 = match use_mode {
            UseMode2::Store(r) => UseMode2::Store(r),
            _ => UseMode2::Push,
        };
        match op {
            BinOp::Add => self.gen_add(use_mode2, lhs, rhs, loc),
            BinOp::Sub => self.gen_sub(use_mode2, lhs, rhs, loc),
            BinOp::Mul => self.gen_mul(use_mode2, lhs, rhs, loc),
            BinOp::Div => self.gen_div(use_mode2, lhs, rhs, loc),
            BinOp::Rem => self.gen_rem(use_mode2, lhs, rhs, loc),
            BinOp::Exp => self.gen_exp(use_mode2, lhs, rhs, loc),
            BinOp::BitOr => self.gen_bitor(use_mode2, lhs, rhs, loc),
            BinOp::BitAnd => self.gen_bitand(use_mode2, lhs, rhs, loc),
            BinOp::BitXor => self.gen_bitxor(use_mode2, lhs, rhs, loc),
            BinOp::LAnd => self.gen_land(use_mode2, lhs, rhs),
            BinOp::LOr => self.gen_lor(use_mode2, lhs, rhs),
            BinOp::Cmp(kind) => self.gen_cmp(use_mode2, kind, lhs, rhs, false, loc),
            _ => unreachable!(),
        }?;
        match use_mode {
            UseMode2::NotUse => {
                self.pop();
            }
            UseMode2::Ret => {
                self.emit_ret(None);
            }
            _ => {}
        };
        Ok(())
    }

    pub(super) fn gen_binop_method(
        &mut self,
        method: IdentId,
        lhs: Node,
        rhs: Node,
        use_mode: UseMode2,
        loc: Loc,
    ) -> Result<()> {
        let old = self.temp;
        let lhs = self.gen_expr_reg(lhs)?;
        let rhs = self.gen_expr_reg(rhs)?;
        self.temp = old;
        let ret = match use_mode {
            UseMode2::Push | UseMode2::Ret => Some(self.push().into()),
            UseMode2::Store(dst) => Some(dst),
            UseMode2::NotUse => None,
        };
        self.emit_binary_op(method, lhs, rhs, ret, loc);
        if use_mode == UseMode2::Ret {
            self.emit_ret(None);
        }
        Ok(())
    }

    ///
    /// Generate TEQ.
    ///
    /// `temp` is not moved.
    ///
    pub(super) fn gen_teq_condbr(
        &mut self,
        lhs: Node,
        rhs: BcReg,
        cont_pos: Label,
        jmp_if_true: bool,
    ) -> Result<()> {
        let loc = lhs.loc;
        let old = self.temp;
        let lhs = self.push_expr(lhs)?.into();
        self.emit(
            BcIr::Cmp(CmpKind::TEq, lhs, BinopMode::RR(lhs, rhs), true),
            loc,
        );
        self.temp = old;
        self.emit_condbr(lhs, cont_pos, jmp_if_true, true);
        Ok(())
    }

    pub(super) fn gen_opt_condbr(
        &mut self,
        jmp_if_true: bool,
        cond: Node,
        else_pos: Label,
    ) -> Result<()> {
        if let NodeKind::BinOp(BinOp::Cmp(kind), box lhs, box rhs) = cond.kind {
            let loc = cond.loc;
            let cond = self.sp().into();
            if kind == CmpKind::Cmp {
                self.gen_cmp(UseMode2::Push, kind, lhs, rhs, false, loc)?; // +1
                self.pop();
                self.emit_condbr(cond, else_pos, jmp_if_true, false);
            } else {
                self.gen_cmp(UseMode2::Push, kind, lhs, rhs, true, loc)?;
                self.pop();
                self.emit_condbr(cond, else_pos, jmp_if_true, true);
            }
        } else if let NodeKind::BinOp(BinOp::LAnd, box lhs, box rhs) = cond.kind {
            if jmp_if_true {
                self.gen_opt_lor_condbr(jmp_if_true, lhs, rhs, else_pos)?;
            } else {
                self.gen_opt_land_condbr(jmp_if_true, lhs, rhs, else_pos)?;
            }
        } else if let NodeKind::BinOp(BinOp::LOr, box lhs, box rhs) = cond.kind {
            if jmp_if_true {
                self.gen_opt_land_condbr(jmp_if_true, lhs, rhs, else_pos)?;
            } else {
                self.gen_opt_lor_condbr(jmp_if_true, lhs, rhs, else_pos)?;
            }
        } else {
            let old = self.temp;
            let cond = self.gen_expr_reg(cond)?;
            self.temp = old;
            self.emit_condbr(cond, else_pos, jmp_if_true, false);
        }
        Ok(())
    }
}

macro_rules! gen_ri_ops {
  (($op:ident, $inst:ident)) => {
      paste! {
          fn [<gen_ $op>](
              &mut self,
              use_mode: UseMode2,
              lhs: Node,
              rhs: Node,
              loc: Loc,
          ) -> Result<BcReg> {
              let mode = self.gen_mode(lhs, rhs)?;
              let dst = match use_mode {
                  UseMode2::Push => self.push().into(),
                  UseMode2::Store(dst) => dst,
                  _ => unreachable!(),
              };
              self.emit(BcIr::BinOp(BinOpK::$inst, dst, mode), loc);
              Ok(dst)
          }
      }
  };
  (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
      gen_ri_ops!(($op1, $inst1));
      gen_ri_ops!($(($op2, $inst2)),+);
  };
}

impl BytecodeGen {
    ///
    /// Generate operand mode.
    ///
    /// ### argument
    /// - `lhs`: left hand side expression.
    /// - `rhs`: right hand side expression.
    ///
    /// ### returns
    /// - `BinopMode`: operand mode.
    ///
    /// ### note
    /// `temp` is not moved.
    ///
    fn gen_mode(&mut self, lhs: Node, rhs: Node) -> Result<BinopMode> {
        let old = self.temp;
        let mode = if let Some(i) = is_smi(&rhs) {
            let lhs = self.gen_expr_reg(lhs)?;
            BinopMode::RI(lhs, i)
        } else if let Some(i) = is_smi(&lhs) {
            let rhs = self.gen_expr_reg(rhs)?;
            BinopMode::IR(i, rhs)
        } else {
            let lhs = self.gen_expr_reg(lhs)?;
            let rhs = self.gen_expr_reg(rhs)?;
            BinopMode::RR(lhs, rhs)
        };
        self.temp = old;
        Ok(mode)
    }

    gen_ri_ops!(
        (add, Add),
        (sub, Sub),
        (mul, Mul),
        (div, Div),
        (bitor, BitOr),
        (bitand, BitAnd),
        (bitxor, BitXor),
        (exp, Exp),
        (rem, Rem)
    );

    ///
    /// Generate BcIr::Cmp.
    ///
    /// - Evaluate *lhs* and *rhs*.
    /// - If *dst* is Some, store the result to *dst*. (`temp` is not moved)
    /// - If *dst* is None, push the result. (`temp` is moved to +1)
    ///
    /// ### Parameters
    /// - `dst`: destination register. if None, push the result.
    /// - `kind`: kind of comparison.
    /// - `lhs`: left hand side expression.
    /// - `rhs`: right hand side expression.
    /// - `optimizable`: if true, the result is used for the next conditional branch instruction.
    /// - `loc`: location of the expression.
    ///
    fn gen_cmp(
        &mut self,
        use_mode: UseMode2,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<BcReg> {
        let old = self.temp;
        let mode = if let Some(i) = is_smi(&rhs) {
            let lhs = self.gen_expr_reg(lhs)?;
            BinopMode::RI(lhs, i)
        } else {
            let lhs = self.gen_expr_reg(lhs)?;
            let rhs = self.gen_expr_reg(rhs)?;
            BinopMode::RR(lhs, rhs)
        };
        self.temp = old;
        let dst = match use_mode {
            UseMode2::Push => self.push().into(),
            UseMode2::Store(dst) => dst,
            _ => unreachable!(),
        };
        self.emit(BcIr::Cmp(kind, dst, mode, optimizable), loc);
        Ok(dst)
    }

    fn gen_land(&mut self, use_mode: UseMode2, lhs: Node, rhs: Node) -> Result<BcReg> {
        let exit_pos = self.new_label();
        // Support "a &&= 100"
        if let NodeKind::MulAssign(lhs, _) = &rhs.kind {
            self.is_assign_local(&lhs[0]);
        }
        let dst = match use_mode {
            UseMode2::Store(dst) => dst,
            UseMode2::Push => self.push().into(),
            _ => unreachable!(),
        };
        self.gen_store_expr(dst, lhs)?;
        self.emit_condbr(dst, exit_pos, false, false);
        self.gen_store_expr(dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_lor(&mut self, use_mode: UseMode2, lhs: Node, rhs: Node) -> Result<BcReg> {
        let exit_pos = self.new_label();
        // Support "a ||= 100"
        if let NodeKind::MulAssign(lhs, _) = &rhs.kind {
            self.is_assign_local(&lhs[0]);
        }
        match use_mode {
            UseMode2::Push => {
                let dst = self.push().into();
                self.gen_store_expr(dst, lhs)?;
                self.emit_condbr(dst, exit_pos, true, false);
                self.gen_store_expr(dst, rhs)?;
                self.apply_label(exit_pos);
                Ok(dst)
            }
            UseMode2::Store(dst) => {
                let tmp = self.push().into();
                self.gen_store_expr(tmp, lhs)?;
                self.emit_condbr(tmp, exit_pos, true, false);
                self.gen_store_expr(tmp, rhs)?;
                self.apply_label(exit_pos);
                self.pop();
                self.emit_mov(dst, tmp);
                Ok(dst)
            }
            _ => unreachable!(),
        }
    }

    fn gen_opt_land_condbr(
        &mut self,
        jmp_if_true: bool,
        lhs: Node,
        rhs: Node,
        else_pos: Label,
    ) -> Result<()> {
        self.gen_opt_condbr(jmp_if_true, lhs, else_pos)?;
        self.gen_opt_condbr(jmp_if_true, rhs, else_pos)
    }

    fn gen_opt_lor_condbr(
        &mut self,
        jmp_if_true: bool,
        lhs: Node,
        rhs: Node,
        else_pos: Label,
    ) -> Result<()> {
        let cont_pos = self.new_label();
        self.gen_opt_condbr(!jmp_if_true, lhs, cont_pos)?;
        self.gen_opt_condbr(jmp_if_true, rhs, else_pos)?;
        self.apply_label(cont_pos);
        Ok(())
    }
}
