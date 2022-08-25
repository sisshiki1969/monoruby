use super::*;

///
/// Type Ir.
///
#[derive(PartialEq)]
pub(super) enum TIr {
    BBLabel(usize),
    LoopStart,
    LoopEnd,
    Deopt,
    /// branch(dest_idx)
    Br(usize),
    /// conditional branch(%reg, dest_idx, brkind)  : branch when reg was true.
    CondBr(SlotId, usize, BrKind),
    CondBrOpt(usize, BrKind),
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
    BinOp(BinOpK, SlotId, SlotId, SlotId, ClassId, ClassId),
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
    ConcatStr(SlotId, SlotId, u16),
    /// float_binop(kind, Fret, Flhs, Frhs)
    FBinOp(BinOpK, SlotId, SlotId, SlotId, ClassId, ClassId),
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
            TIr::BBLabel(pc) => {
                write!(f, ":{:05}", pc)
            }
            TIr::LoopStart => {
                write!(f, "loop_start")
            }
            TIr::LoopEnd => {
                write!(f, "loop_end")
            }
            TIr::Deopt => {
                write!(f, "deopt")
            }
            TIr::Br(dest_idx) => {
                write!(f, "br => {:05}", dest_idx)
            }
            TIr::CondBr(reg, dest_idx, brkind) => {
                write!(f, "cond{}br {:?} => {:05}", brkind.to_s(), reg, dest_idx,)
            }
            TIr::CondBrOpt(dest_idx, brkind) => {
                write!(f, "cond{}br _ => {:05}", brkind.to_s(), dest_idx)
            }
            TIr::Integer(reg, num) => write!(f, "{:?} = {}: i32", reg, num),
            TIr::Symbol(reg, id) => {
                write!(f, "{:?} = {:?}", reg, id)
            }
            TIr::LiteralPacked(reg, val) => {
                write!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Literal(reg, val) => {
                write!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Array(ret, src, len) => {
                write!(f, "{:?} = [{:?}; {}]", ret, src, len)
            }
            TIr::Index(ret, base, idx) => {
                write!(f, "{:?} = {:?}.[{:?}]", ret, base, idx)
            }
            TIr::IndexAssign(src, base, idx) => {
                write!(f, "{:?}.[{:?}] = {:?}", base, idx, src)
            }
            TIr::FLiteral(reg, float) => {
                write!(f, "{:?}F = {}: f64", reg, float)
            }
            TIr::LoadConst(reg, id) => {
                write!(f, "{:?} = const[{:?}]", reg, id)
            }
            TIr::FLoadConst(reg, id) => {
                write!(f, "{:?}F = const[{:?}]", reg, id,)
            }
            TIr::StoreConst(id, reg) => {
                write!(f, "const[{:?}] = {:?}", id, reg)
            }
            TIr::Nil(reg) => write!(f, "{:?} = nil", reg),
            TIr::Neg(dst, src) => write!(f, "{:?} = neg {:?}", dst, src),
            TIr::FNeg(dst, src) => write!(f, "{:?} = neg {:?}", dst, src),
            TIr::BinOp(kind, dst, lhs, rhs, class1, class2) => {
                write!(
                    f,
                    "{:?} = {:?}:[{:?}] {} {:?}:[{:?}]",
                    dst, lhs, class1, kind, rhs, class2
                )
            }
            TIr::BinOpRi(kind, dst, lhs, rhs, class) => {
                write!(
                    f,
                    "{:?} = {:?}:[{:?}] {} {}: i16",
                    dst, lhs, class, kind, rhs
                )
            }
            TIr::BinOpIr(kind, dst, lhs, rhs, class) => {
                write!(
                    f,
                    "{:?} = {}:i16 {} {:?}:[{:?}]",
                    dst, lhs, kind, rhs, class
                )
            }
            TIr::FBinOp(kind, dst, lhs, rhs, class1, class2) => {
                write!(
                    f,
                    "{:?}F = {:?}F:[{:?}] {} {:?}F:[{:?}]",
                    dst, lhs, class1, kind, rhs, class2
                )
            }
            TIr::FBinOpRf(kind, dst, lhs, rhs) => {
                write!(f, "{:?}F = {:?}F {} {}: f64", dst, lhs, kind, rhs)
            }
            TIr::FBinOpFr(kind, dst, lhs, rhs) => {
                write!(f, "{:?}F = {}: f64 {} {:?}F", dst, lhs, kind, rhs)
            }
            TIr::Cmp(kind, dst, lhs, rhs, opt, class) => {
                write!(
                    f,
                    "{}{:?} = {:?}:[{:?}] {:?} {:?}",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::Cmpri(kind, dst, lhs, rhs, opt, class) => {
                write!(
                    f,
                    "{}{:?} = {:?}:[{:?}] {:?} {}: i16",
                    optstr(opt),
                    dst,
                    lhs,
                    class,
                    kind,
                    rhs
                )
            }
            TIr::FCmp(kind, dst, lhs, rhs) => {
                write!(f, "{:?} = {:?}F {:?} {:?}F", dst, lhs, kind, rhs)
            }
            TIr::FCmpRf(kind, dst, lhs, rhs) => {
                write!(f, "{:?} = {:?}F {:?} {}: f64", dst, lhs, kind, rhs)
            }

            TIr::Ret(reg) => write!(f, "ret {:?}", reg),
            TIr::Mov(dst, src) => write!(f, "{:?} = {:?}", dst, src),
            TIr::FMov(dst, src) => write!(f, "{:?}F = {:?}F", dst, src),
            TIr::MethodCall(ret, name, recv, args, len) => {
                write!(
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
                write!(f, "define {:?} {:?}", name, func_id)
            }
            TIr::ConcatStr(ret, args, len) => {
                write!(f, "{} = concat({:?}; {})", ret.ret_str(), args, len)
            }
        }
    }
}
