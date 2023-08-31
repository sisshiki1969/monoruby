use super::*;
use ::paste::paste;

impl BytecodeGen {
    /// Generate bytecode Ir for binary operations.
    pub(super) fn gen_binop(
        &mut self,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<BcReg> {
        match op {
            BinOp::Add => self.gen_add(dst, lhs, rhs, loc),
            BinOp::Sub => self.gen_sub(dst, lhs, rhs, loc),
            BinOp::Mul => self.gen_mul(dst, lhs, rhs, loc),
            BinOp::Div => self.gen_div(dst, lhs, rhs, loc),
            BinOp::Rem => self.gen_rem(dst, lhs, rhs, loc),
            BinOp::Exp => self.gen_exp(dst, lhs, rhs, loc),
            BinOp::BitOr => self.gen_bitor(dst, lhs, rhs, loc),
            BinOp::BitAnd => self.gen_bitand(dst, lhs, rhs, loc),
            BinOp::BitXor => self.gen_bitxor(dst, lhs, rhs, loc),
            BinOp::Shr => self.gen_binop_method(IdentId::_SHR, dst, lhs, rhs, loc),
            BinOp::Shl => self.gen_binop_method(IdentId::_SHL, dst, lhs, rhs, loc),
            BinOp::LAnd => self.gen_land(dst, lhs, rhs),
            BinOp::LOr => self.gen_lor(dst, lhs, rhs),
            BinOp::Match => self.gen_match(dst, lhs, rhs),
            BinOp::Cmp(kind) => self.gen_cmp(dst, kind, lhs, rhs, false, loc),
        }
    }

    fn gen_binop_method(
        &mut self,
        method: IdentId,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
        loc: Loc,
    ) -> Result<BcReg> {
        let old = self.temp;
        let lhs = self.gen_expr_reg(lhs)?;
        let rhs = self.gen_expr_reg(rhs)?;
        self.temp = old;
        let ret = if let Some(ret) = dst {
            ret
        } else {
            self.push().into()
        };
        self.emit_binary_op(method, lhs, rhs, Some(ret), loc);
        Ok(ret)
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
                self.gen_cmp(None, kind, lhs, rhs, false, loc)?; // +1
                self.pop();
                self.emit_condbr(cond, else_pos, jmp_if_true, false);
            } else {
                self.gen_cmp(None, kind, lhs, rhs, true, loc)?;
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
              dst: Option<BcReg>,
              lhs: Node,
              rhs: Node,
              loc: Loc,
          ) -> Result<BcReg> {
              let mode = self.gen_mode(lhs, rhs)?;
              let dst = match dst {
                  None => self.push().into(),
                  Some(local) => local,
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
        dst: Option<BcReg>,
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
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local,
        };
        self.emit(BcIr::Cmp(kind, dst, mode, optimizable), loc);
        Ok(dst)
    }

    fn gen_land(&mut self, dst: Option<BcReg>, lhs: Node, rhs: Node) -> Result<BcReg> {
        let exit_pos = self.new_label();
        // Support "a &&= 100"
        if let NodeKind::MulAssign(lhs, _) = &rhs.kind {
            self.is_assign_local(&lhs[0]);
        }
        let dst = match dst {
            None => self.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(dst, lhs)?;
        self.emit_condbr(dst, exit_pos, false, false);
        self.gen_store_expr(dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_lor(&mut self, dst: Option<BcReg>, lhs: Node, rhs: Node) -> Result<BcReg> {
        let exit_pos = self.new_label();
        // Support "a ||= 100"
        if let NodeKind::MulAssign(lhs, _) = &rhs.kind {
            self.is_assign_local(&lhs[0]);
        }
        match dst {
            None => {
                let dst = self.push().into();
                self.gen_store_expr(dst, lhs)?;
                self.emit_condbr(dst, exit_pos, true, false);
                self.gen_store_expr(dst, rhs)?;
                self.apply_label(exit_pos);
                Ok(dst)
            }
            Some(dst) => {
                let tmp = self.push().into();
                self.gen_store_expr(tmp, lhs)?;
                self.emit_condbr(tmp, exit_pos, true, false);
                self.gen_store_expr(tmp, rhs)?;
                self.apply_label(exit_pos);
                self.pop();
                self.emit_mov(dst, tmp);
                Ok(dst)
            }
        }
    }

    fn gen_match(&mut self, dst: Option<BcReg>, lhs: Node, rhs: Node) -> Result<BcReg> {
        let loc = lhs.loc().merge(rhs.loc());
        let ret = if let Some(ret) = dst {
            ret
        } else {
            self.sp().into()
        };
        let method = IdentId::get_id("=~");
        self.gen_method_call(
            method,
            Some(lhs),
            ArgList::from_args(vec![rhs]),
            dst,
            UseMode::Push,
            loc,
        )?;
        Ok(ret)
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
