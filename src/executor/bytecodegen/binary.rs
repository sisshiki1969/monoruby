use super::*;

impl IrContext {
    /// Generate bytecode Ir for binary operations.
    pub(super) fn gen_binop(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        op: BinOp,
        lhs: Node,
        rhs: Node,
        dst: Option<BcReg>,
        loc: Loc,
    ) -> Result<BcReg> {
        match op {
            BinOp::Add => self.gen_add(ctx, info, dst, lhs, rhs, loc),
            BinOp::Sub => self.gen_sub(ctx, info, dst, lhs, rhs, loc),
            BinOp::Mul => self.gen_mul(ctx, info, dst, lhs, rhs, loc),
            BinOp::Div => self.gen_div(ctx, info, dst, lhs, rhs, loc),
            BinOp::Rem => self.gen_rem(ctx, info, dst, lhs, rhs, loc),
            BinOp::Exp => self.gen_exp(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitOr => self.gen_bitor(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitAnd => self.gen_bitand(ctx, info, dst, lhs, rhs, loc),
            BinOp::BitXor => self.gen_bitxor(ctx, info, dst, lhs, rhs, loc),
            BinOp::Shr => self.gen_shr(ctx, info, dst, lhs, rhs, loc),
            BinOp::Shl => self.gen_shl(ctx, info, dst, lhs, rhs, loc),
            BinOp::LAnd => self.gen_land(ctx, info, dst, lhs, rhs),
            BinOp::LOr => self.gen_lor(ctx, info, dst, lhs, rhs),
            BinOp::Cmp(kind) => self.gen_cmp(ctx, info, dst, kind, lhs, rhs, false, loc),
            _ => Err(MonorubyErr::unsupported_operator(
                op,
                loc,
                info.sourceinfo.clone(),
            )),
        }
    }
}

macro_rules! gen_ops {
  (($op:ident, $inst:ident)) => {
      paste! {
          fn [<gen_ $op>](
              &mut self,
              ctx: &mut FnStore,
              info: &mut ISeqInfo,
              dst: Option<BcReg>,
              lhs: Node,
              rhs: Node,
              loc: Loc,
          ) -> Result<BcReg> {
              let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
              self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
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
              ctx: &mut FnStore,
              info: &mut ISeqInfo,
              dst: Option<BcReg>,
              lhs: Node,
              rhs: Node,
              loc: Loc,
          ) -> Result<BcReg> {
              if let Some(i) = is_smi(&rhs) {
                  let (dst, lhs) = self.gen_singular(ctx, info, dst, lhs)?;
                  self.push(BcIr::BinOpRi(BinOpK::$inst, dst, lhs, i), loc);
                  Ok(dst)
              } else if let Some(i) = is_smi(&lhs) {
                  let (dst, rhs) = self.gen_singular(ctx, info, dst, rhs)?;
                  self.push(BcIr::BinOpIr(BinOpK::$inst, dst, i, rhs), loc);
                  Ok(dst)
              } else {
                  let (dst, lhs, rhs) = self.gen_binary(ctx, info, dst, lhs, rhs)?;
                  self.push(BcIr::BinOp(BinOpK::$inst, dst, lhs, rhs), loc);
                  Ok(dst)
              }
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

    fn gen_land(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<BcReg> {
        let exit_pos = self.new_label();
        let dst = match dst {
            None => info.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(ctx, info, dst, lhs)?;
        self.gen_condnotbr(dst, exit_pos, false);
        self.gen_store_expr(ctx, info, dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }

    fn gen_lor(
        &mut self,
        ctx: &mut FnStore,
        info: &mut ISeqInfo,
        dst: Option<BcReg>,
        lhs: Node,
        rhs: Node,
    ) -> Result<BcReg> {
        let exit_pos = self.new_label();
        let dst = match dst {
            None => info.push().into(),
            Some(reg) => reg,
        };
        self.gen_store_expr(ctx, info, dst, lhs)?;
        self.gen_condbr(dst, exit_pos, false);
        self.gen_store_expr(ctx, info, dst, rhs)?;
        self.apply_label(exit_pos);
        Ok(dst)
    }
}
