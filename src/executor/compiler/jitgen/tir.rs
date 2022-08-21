use super::*;

///
/// Type Ir.
///
#[derive(PartialEq)]
pub(super) enum TIr {
    /// label(index, pc)
    Label(usize, BcPc),
    LoopStart,
    LoopEnd,
    Deopt(BcPc, WriteBack),
    /// branch(dest, write_back)
    Br(usize, WriteBack),
    /// conditional branch(%reg, dest, optimizable, write_back)  : branch when reg was true.
    CondBr(SlotId, usize, BrKind, WriteBack),
    CondBrOpt(usize, CmpKind, BrKind, WriteBack, UsingXmm),
    /// integer(%reg, i32)
    Integer(SlotId, i32),
    /// Symbol(%reg, IdentId)
    Symbol(SlotId, IdentId),
    /// literal(%ret, value)
    LiteralPacked(SlotId, Value),
    Literal(SlotId, Value, UsingXmm),
    /// array(%ret, %src, len)
    Array(SlotId, SlotId, u16),
    /// index(%ret, %base, %idx)
    Index(SlotId, SlotId, SlotId, UsingXmm),
    /// vm_index_assign(%src, %base, %idx)
    IndexAssign(SlotId, SlotId, SlotId, UsingXmm),
    /// float_literal(Fret, f64)
    FLiteral(u16, f64),
    LoadConst(SlotId, ConstSiteId, UsingXmm),
    FLoadConst(u16, SlotId, ConstSiteId, UsingXmm, WriteBack),
    StoreConst(SlotId, IdentId, UsingXmm),
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
    FMov(SlotId, u16),
    /// func call(%ret, name, %recv, %args, len)
    MethodCall(SlotId, IdentId, SlotId, SlotId, u16),
    /// method definition(method_name, func_id)
    MethodDef(IdentId, FuncId),
    /// concatenate strings(ret, args, args_len)
    ConcatStr(SlotId, SlotId, u16, UsingXmm),
    /// float_binop(kind, Fret, Flhs, Frhs)
    FBinOp(BinOpK, u16, u16, u16),
    /// float_binop(kind, Fret, Flhs, f64)
    FBinOpRf(BinOpK, u16, u16, f64),
    /// float_binop(kind, Fret, f64, Frhs)
    FBinOpFr(BinOpK, u16, f64, u16),
    /// float_cmp(%ret, Flhs, Frhs, optimizable)
    FCmp(CmpKind, SlotId, u16, u16, bool),
    /// float_cmpri(%ret, Flhs, rhs: f16, optimizable)
    FCmpRf(CmpKind, SlotId, u16, f64, bool),
    /// negate(Fret, Fsrc)
    FNeg(u16, u16),
    /// load float from register(%src, Fdst)
    FLoadInteger(SlotId, u16, BcPc, WriteBack),
    FLoadFloat(SlotId, u16, BcPc, WriteBack),
    /// store float to register(Fsrc, %dst)
    FStore(u16, SlotId),
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
        fn disp_write_back(v: &WriteBack) -> String {
            v.iter()
                .map(|(freg, v)| format!("F{}:{:?} ", freg, v))
                .fold(String::new(), |acc, s| acc + &s)
        }
        match self {
            TIr::LoopStart => {
                writeln!(f, "loop_start")
            }
            TIr::LoopEnd => {
                writeln!(f, "loop_end")
            }
            TIr::Deopt(_pc, wb) => {
                writeln!(f, "deopt {}", disp_write_back(wb))
            }
            TIr::Label(index, pc) => {
                writeln!(f, "label {:05}; {:?}", index, pc.0)
            }
            TIr::Br(disp, wb) => {
                writeln!(f, "br => {:05}; {}", disp, disp_write_back(wb))
            }
            TIr::CondBr(reg, disp, brkind, wb) => {
                writeln!(
                    f,
                    "cond{}br {:?} => {:05}; {}",
                    brkind.to_s(),
                    reg,
                    disp,
                    disp_write_back(wb)
                )
            }
            TIr::CondBrOpt(disp, _, brkind, wb, _) => {
                writeln!(
                    f,
                    "cond{}br _ => {:05}; {}",
                    brkind.to_s(),
                    disp,
                    disp_write_back(wb)
                )
            }
            TIr::Integer(reg, num) => writeln!(f, "{:?} = {}: i32", reg, num),
            TIr::Symbol(reg, id) => {
                writeln!(f, "{:?} = {:?}", reg, id)
            }
            TIr::LiteralPacked(reg, val) => {
                writeln!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Literal(reg, val, _) => {
                writeln!(f, "{:?} = literal[{:?}]", reg, val)
            }
            TIr::Array(ret, src, len) => {
                writeln!(f, "{:?} = [{:?}; {}]", ret, src, len)
            }
            TIr::Index(ret, base, idx, _) => {
                writeln!(f, "{:?} = {:?}.[{:?}]", ret, base, idx)
            }
            TIr::IndexAssign(src, base, idx, _) => {
                writeln!(f, "{:?}.[{:?}] = {:?}", base, idx, src)
            }
            TIr::FLiteral(freg, float) => {
                writeln!(f, "F{} = {}: f64", freg, float)
            }
            TIr::LoadConst(reg, id, _) => {
                writeln!(f, "{:?} = const[{:?}]", reg, id)
            }
            TIr::FLoadConst(freg, reg, id, _, wb) => {
                writeln!(
                    f,
                    "F{} {:?} = const[{:?}] {}",
                    freg,
                    reg,
                    id,
                    disp_write_back(wb)
                )
            }
            TIr::StoreConst(reg, id, _) => {
                writeln!(f, "const[{:?}] = {:?}", id, reg)
            }
            TIr::FLoadInteger(src, fdst, _pc, _wb) => {
                writeln!(f, "F{} = {:?}: i64", fdst, src)
            }
            TIr::FLoadFloat(src, fdst, _pc, _wb) => {
                writeln!(f, "F{} = {:?}: f64", fdst, src)
            }
            TIr::FStore(fsrc, dst) => {
                writeln!(f, "{:?} = F{}", dst, fsrc)
            }
            TIr::Nil(reg) => writeln!(f, "{:?} = nil", reg),
            TIr::Neg(dst, src) => writeln!(f, "{:?} = neg {:?}", dst, src),
            TIr::FNeg(dst, src) => writeln!(f, "F{} = neg F{}", dst, src),
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
                writeln!(f, "F{} = F{} {} F{}", dst, lhs, kind, rhs)
            }
            TIr::FBinOpRf(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = F{} {} {}: f64", dst, lhs, kind, rhs)
            }
            TIr::FBinOpFr(kind, dst, lhs, rhs) => {
                writeln!(f, "F{} = {}: f64 {} F{}", dst, lhs, kind, rhs)
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
            TIr::FCmp(kind, dst, lhs, rhs, opt) => {
                writeln!(f, "{}{:?} = F{} {:?} F{}", optstr(opt), dst, lhs, kind, rhs)
            }
            TIr::FCmpRf(kind, dst, lhs, rhs, opt) => {
                writeln!(
                    f,
                    "{}{:?} = F{} {:?} {}: f64",
                    optstr(opt),
                    dst,
                    lhs,
                    kind,
                    rhs
                )
            }

            TIr::Ret(reg) => writeln!(f, "ret {:?}", reg),
            TIr::Mov(dst, src) => writeln!(f, "{:?} = {:?}", dst, src),
            TIr::FMov(dst, src) => writeln!(f, "{:?} = F{}", dst, src),
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
