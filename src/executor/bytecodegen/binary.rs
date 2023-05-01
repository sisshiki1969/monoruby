use super::*;

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
            BinOp::Shr => self.gen_shr(dst, lhs, rhs, loc),
            BinOp::Shl => self.gen_shl(dst, lhs, rhs, loc),
            BinOp::LAnd => self.gen_land(dst, lhs, rhs),
            BinOp::LOr => self.gen_lor(dst, lhs, rhs),
            BinOp::Match => self.gen_match(dst, lhs, rhs),
            BinOp::Cmp(kind) => self.gen_cmp(dst, kind, lhs, rhs, false, loc),
        }
    }
}

macro_rules! gen_ops {
  (($op:ident, $inst:ident)) => {
      paste! {
          fn [<gen_ $op>](
              &mut self,
              dst: Option<BcReg>,
              lhs: Node,
              rhs: Node,
              loc: Loc,
          ) -> Result<BcReg> {
              let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
              self.emit(BcIr::BinOp(BinOpK::$inst, dst, BinopMode::RR(lhs, rhs)), loc);
              Ok(dst)
          }
      }
  };
  (($op1:ident, $inst1:ident), $(($op2:ident, $inst2:ident)),+) => {
      gen_ops!(($op1, $inst1));
      gen_ops!($(($op2, $inst2)),+);
  };
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
              let (dst, mode) = if let Some(i) = is_smi(&rhs) {
                  let (dst, lhs) = self.gen_singular(dst, lhs)?;
                  (dst, BinopMode::RI(lhs, i))
              } else if let Some(i) = is_smi(&lhs) {
                  let (dst, rhs) = self.gen_singular(dst, rhs)?;
                  (dst, BinopMode::IR(i, rhs))
              } else {
                  let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
                  (dst, BinopMode::RR(lhs, rhs))
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
    gen_ri_ops!(
        (add, Add),
        (sub, Sub),
        (mul, Mul),
        (div, Div),
        (bitor, BitOr),
        (bitand, BitAnd),
        (bitxor, BitXor),
        (shr, Shr),
        (shl, Shl),
        (exp, Exp)
    );
    gen_ops!((rem, Rem));

    fn gen_singular(&mut self, dst: Option<BcReg>, lhs: Node) -> Result<(BcReg, BcReg)> {
        let lhs = self.gen_temp_expr(lhs)?;
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs))
    }

    fn gen_binary(
        &mut self,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<(BcReg, BcReg, BcReg)> {
        let (lhs, rhs) = self.gen_binary_temp_expr(lhs, rhs)?;
        let dst = match dst {
            None => self.push().into(),
            Some(local) => local,
        };
        Ok((dst, lhs, rhs))
    }

    fn gen_cmp(
        &mut self,
        dst: Option<BcReg>,
        kind: CmpKind,
        lhs: Node,
        rhs: Node,
        optimizable: bool,
        loc: Loc,
    ) -> Result<BcReg> {
        let (dst, mode) = if let Some(i) = is_smi(&rhs) {
            let (dst, lhs) = self.gen_singular(dst, lhs)?;
            (dst, BinopMode::RI(lhs, i))
        } else {
            let (dst, lhs, rhs) = self.gen_binary(dst, lhs, rhs)?;
            (dst, BinopMode::RR(lhs, rhs))
        };
        self.emit(BcIr::Cmp(kind, dst, mode, optimizable), loc);
        Ok(dst)
    }

    fn gen_land(&mut self, dst: Option<BcReg>, lhs: Node, rhs: Node) -> Result<BcReg> {
        let exit_pos = self.new_label();
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
        let dst = match dst {
            None => self.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(dst, lhs)?;
        self.emit_condbr(dst, exit_pos, true, false);
        self.gen_store_expr(dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_match(&mut self, dst: Option<BcReg>, lhs: Node, rhs: Node) -> Result<BcReg> {
        let loc = lhs.loc().merge(rhs.loc());
        let ret = if let Some(ret) = dst {
            ret
        } else {
            self.next_reg().into()
        };
        let method = IdentId::get_id("=~");
        self.gen_method_call(
            method,
            Some(lhs),
            ArgList::from_args(vec![rhs]),
            dst,
            UseMode::Use,
            loc,
        )?;
        Ok(ret)
    }

    pub(super) fn gen_teq_condbr(
        &mut self,
        lhs: Node,
        rhs: BcReg,
        cont_pos: Label,
        jmp_if_true: bool,
    ) -> Result<()> {
        let loc = lhs.loc;
        let lhs = self.gen_temp_expr(lhs)?;
        self.emit(
            BcIr::Cmp(CmpKind::TEq, lhs, BinopMode::RR(lhs, rhs), true),
            loc,
        );
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
            let cond = self.next_reg().into();
            self.gen_cmp(None, kind, lhs, rhs, true, loc)?;
            self.pop();
            self.emit_condbr(cond, else_pos, jmp_if_true, true);
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
            let cond = self.gen_temp_expr(cond)?;
            self.emit_condbr(cond, else_pos, jmp_if_true, false);
        }
        Ok(())
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
