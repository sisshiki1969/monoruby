use super::*;

///
/// Type Ir.
///
#[derive(PartialEq)]
pub(super) enum TIr {
    /// label(idx)
    Label(usize),
    LoopStart,
    LoopEnd,
    Deopt,
    /// branch(dest_idx)
    Br(usize),
    /// conditional branch(%reg, dest_idx, brkind)  : branch when reg was true.
    CondBr(SlotId, usize, BrKind),
    CondBrOpt(CmpKind, SlotId, SlotId, usize, BrKind),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, value)
    LiteralPacked(SlotId, Value),
    Literal(SlotId, Value),
    /// array(%ret, %src, len)
    Array(SlotId, SlotId, u16),
    /// index(%ret, %base, %idx)
    Index(SlotId, SlotId, SlotId),
    /// vm_index_assign(%src, %base, %idx)
    IndexAssign(SlotId, SlotId, SlotId),
    /// float_literal(Fret, f64)
    FLiteral(SlotId, f64),
    LoadConst(SlotId, ConstSiteId),
    FLoadConst(SlotId, ConstSiteId),
    StoreConst(IdentId, SlotId),
    /// nil(%reg)
    Nil(SlotId),
    /// negate(%ret, %src)
    Neg(SlotId, SlotId),
    /// binop(kind, %ret, %lhs, %rhs)
    BinOp(BinOpK, SlotId, SlotId, SlotId, ClassId),
    /// binop with small integer(kind, %ret, %lhs, rhs)
    BinOpRi(BinOpK, SlotId, SlotId, i16, ClassId),
    /// binop with small integer(kind, %ret, lhs, %rhs)
    BinOpIr(BinOpK, SlotId, i16, SlotId, ClassId),
    /// cmp(%ret, %lhs, %rhs, optimizable)
    Cmp(CmpKind, SlotId, SlotId, SlotId, bool, ClassId),
    /// cmpri(%ret, %lhs, rhs: i16, optimizable)
    Cmpri(CmpKind, SlotId, SlotId, i16, bool, ClassId),
    /// return(%ret)
    Ret(SlotId),
    /// move(%dst, %src)
    Mov(SlotId, SlotId),
    /// float_move(Fdst, Fsrc)
    FMov(SlotId, SlotId),
    /// func call(%ret, name, %recv, %args, len)
    MethodCall(SlotId, IdentId, SlotId, SlotId, u16),
    /// method definition(method_name, func_id)
    MethodDef(IdentId, FuncId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(SlotId, SlotId, u16, UsingXmm),
    /// float_binop(kind, Fret, Flhs, Frhs)
    FBinOp(BinOpK, SlotId, SlotId, SlotId),
    /// float_binop(kind, Fret, Flhs, f64)
    FBinOpRf(BinOpK, SlotId, SlotId, f64),
    /// float_binop(kind, Fret, f64, Frhs)
    FBinOpFr(BinOpK, SlotId, f64, SlotId),
    /// float_cmp(%ret, Flhs, Frhs)
    FCmp(CmpKind, SlotId, SlotId, SlotId),
    /// float_cmpri(%ret, Flhs, rhs: f16)
    FCmpRf(CmpKind, SlotId, SlotId, f64),
    /// negate(Fret, Fsrc)
    FNeg(SlotId, SlotId),
    /// load float from register(%src, Fdst)
    FLoadInteger(SlotId, SlotId),
    FLoadFloat(SlotId, SlotId),
    /// store float to register(Fsrc, %dst)
    FStore(SlotId, SlotId),
}

impl std::fmt::Debug for TIr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn optstr(opt: &bool) -> &'static str {
            if *opt {
                "_"
            } else {
                ""
            }
        }
        match self {
            TIr::LoopStart => {
                writeln!(f, "loop_start")
            }
            TIr::LoopEnd => {
                writeln!(f, "loop_end")
            }
            TIr::Deopt => {
                writeln!(f, "deopt")
            }
            TIr::Label(index) => {
                writeln!(f, "label {:05}", index)
            }
            TIr::Br(dest_idx) => {
                writeln!(f, "br => {:05}", dest_idx)
            }
            TIr::CondBr(reg, dest_idx, brkind) => {
                writeln!(f, "cond{}br {:?} => {:05}", brkind.to_s(), reg, dest_idx,)
            }
            TIr::CondBrOpt(cmpkind, lhs, rhs, dest_idx, brkind) => {
                writeln!(f, "_ = {:?} {:?} {:?}", lhs, cmpkind, rhs)?;
                writeln!(f, "cond{}br _ => {:05}", brkind.to_s(), dest_idx)
            }
            TIr::Integer(reg, num) => writeln!(f, "{:?} = {}: i32", reg, num),
            TIr::Symbol(reg, id) => {
                writeln!(f, "{:?} = {:?}", reg, id)
            }
            TIr::LiteralPacked(reg, val) => {
                writeln!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Literal(reg, val) => {
                writeln!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Array(ret, src, len) => {
                writeln!(f, "{:?} = [{:?}; {}]", ret, src, len)
            }
            TIr::Index(ret, base, idx) => {
                writeln!(f, "{:?} = {:?}.[{:?}]", ret, base, idx)
            }
            TIr::IndexAssign(src, base, idx) => {
                writeln!(f, "{:?}.[{:?}] = {:?}", base, idx, src)
            }
            TIr::FLiteral(reg, float) => {
                writeln!(f, "{:?} = {}: f64", reg, float)
            }
            TIr::LoadConst(reg, id) => {
                writeln!(f, "{:?} = const[{:?}]", reg, id)
            }
            TIr::FLoadConst(reg, id) => {
                writeln!(f, "{:?} = const[{:?}]", reg, id,)
            }
            TIr::StoreConst(id, reg) => {
                writeln!(f, "const[{:?}] = {:?}", id, reg)
            }
            TIr::FLoadInteger(src, dst) => {
                writeln!(f, "{:?} = {:?}: i64", dst, src)
            }
            TIr::FLoadFloat(src, dst) => {
                writeln!(f, "{:?} = {:?}: f64", dst, src)
            }
            TIr::FStore(src, dst) => {
                writeln!(f, "{:?} = {:?}", dst, src)
            }
            TIr::Nil(reg) => writeln!(f, "{:?} = nil", reg),
            TIr::Neg(dst, src) => writeln!(f, "{:?} = neg {:?}", dst, src),
            TIr::FNeg(dst, src) => writeln!(f, "{:?} = neg {:?}", dst, src),
            TIr::BinOp(kind, dst, lhs, rhs, class) => {
                writeln!(f, "{:?} = {:?}:{:?} {} {:?}", dst, lhs, class, kind, rhs)
            }
            TIr::BinOpRi(kind, dst, lhs, rhs, class) => {
                writeln!(f, "{:?} = {:?}:{:?} {} {}: i16", dst, lhs, class, kind, rhs)
            }
            TIr::BinOpIr(kind, dst, lhs, rhs, class) => {
                writeln!(f, "{:?} = {}:i16 {} {:?}: {:?}", dst, lhs, kind, rhs, class)
            }
            TIr::FBinOp(kind, dst, lhs, rhs) => {
                writeln!(f, "{:?} = {:?} {} {:?}", dst, lhs, kind, rhs)
            }
            TIr::FBinOpRf(kind, dst, lhs, rhs) => {
                writeln!(f, "{:?} = {:?} {} {}: f64", dst, lhs, kind, rhs)
            }
            TIr::FBinOpFr(kind, dst, lhs, rhs) => {
                writeln!(f, "{:?} = {}: f64 {} {:?}", dst, lhs, kind, rhs)
            }
            TIr::Cmp(kind, dst, lhs, rhs, opt, class) => {
                writeln!(
                    f,
                    "{}{:?} = {:?}:{:?} {:?} {:?}",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::Cmpri(kind, dst, lhs, rhs, opt, class) => {
                writeln!(
                    f,
                    "{}{:?} = {:?}:{:?} {:?} {}: i16",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::FCmp(kind, dst, lhs, rhs) => {
                writeln!(f, "{:?} = {:?} {:?} {:?}", dst, lhs, kind, rhs)
            }
            TIr::FCmpRf(kind, dst, lhs, rhs) => {
                writeln!(f, "{:?} = {:?} {:?} {}: f64", dst, lhs, kind, rhs)
            }

            TIr::Ret(reg) => writeln!(f, "ret {:?}", reg),
            TIr::Mov(dst, src) => writeln!(f, "{:?} = {:?}", dst, src),
            TIr::FMov(dst, src) => writeln!(f, "{:?} = {:?}", dst, src),
            TIr::MethodCall(ret, name, recv, args, len) => {
                writeln!(
                    f,
                    "{} = {:?}.call {:?}({:?}; {})",
                    ret.ret_str(),
                    recv,
                    name,
                    args,
                    len
                )
            }
            TIr::MethodDef(name, func_id) => {
                writeln!(f, "define {:?} {:?}", name, func_id)
            }
            TIr::ConcatStr(ret, args, len, _) => {
                writeln!(f, "{} = concat({:?}; {})", ret.ret_str(), args, len)
            }
        }
    }
}
