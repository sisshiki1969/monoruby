use super::*;

impl IrContext {
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

impl IrContext {
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
}
