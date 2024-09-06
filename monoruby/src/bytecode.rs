use bytecodegen::{
    inst::{BrKind, DynVar, FnInitInfo},
    BcIndex, BinOpK, UnOpK,
};
use jitgen::trace_ir::{OpMode, TraceIr};
use ruruby_parse::CmpKind;

use super::*;

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub(crate) struct Bytecode {
    pub op1: u64,
    pub op2: Bc2,
}

impl Bytecode {
    pub fn is_integer1(&self) -> bool {
        self.classid1() == Some(INTEGER_CLASS)
    }

    pub fn is_integer2(&self) -> bool {
        self.classid2() == Some(INTEGER_CLASS)
    }

    pub fn is_float1(&self) -> bool {
        self.classid1() == Some(FLOAT_CLASS)
    }

    pub fn is_float2(&self) -> bool {
        self.classid2() == Some(FLOAT_CLASS)
    }

    pub fn is_integer_binop(&self) -> bool {
        self.classid1() == Some(INTEGER_CLASS) && self.classid2() == Some(INTEGER_CLASS)
    }

    pub fn is_float_binop(&self) -> bool {
        match (self.classid1(), self.classid2()) {
            (Some(class1), Some(class2)) => match (class1, class2) {
                (INTEGER_CLASS, INTEGER_CLASS) => false,
                (INTEGER_CLASS | FLOAT_CLASS, INTEGER_CLASS | FLOAT_CLASS) => true,
                _ => false,
            },
            _ => false,
        }
    }
    pub fn classid1(&self) -> Option<ClassId> {
        ClassId::from(self.op2.0 as u32)
    }

    pub fn classid2(&self) -> Option<ClassId> {
        ClassId::from((self.op2.0 >> 32) as u32)
    }

    pub fn cached_version(&self) -> u32 {
        let op = self.op2.0;
        (op >> 32) as u32
    }

    pub fn cached_ivarid(&self) -> IvarId {
        let op = self.op2.0;
        IvarId::new((op >> 32) as u32)
    }

    fn fid(&self) -> Option<FuncId> {
        let op = self.op2.0 as u32;
        if op == 0 {
            None
        } else {
            Some(FuncId::new(op))
        }
    }

    /*#[cfg(feature = "dump-bc")]
    pub fn value(&self) -> Option<Value> {
        match self.op2.0 {
            0 => None,
            v => Some(Value::from(v)),
        }
    }*/

    #[cfg(feature = "dump-bc")]
    pub fn into_jit_addr(self) -> u64 {
        self.op2.0
    }

    pub fn from(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::from(0),
        }
    }

    pub fn from_u32(op1: u64, op2: u32) -> Self {
        Self {
            op1,
            op2: Bc2::from(op2 as u64),
        }
    }

    pub fn from_fn_info(op1: u64, fn_info: &FnInitInfo) -> Self {
        let FnInitInfo {
            arg_num,
            req_num,
            info,
            ..
        } = fn_info;
        Bytecode::from_with_num(op1, *req_num as u16, 0, *info as u16, *arg_num as u16)
    }

    pub fn from_with_value(op1: u64, val: Value) -> Self {
        Self {
            op1,
            op2: Bc2::from(val.id()),
        }
    }

    pub fn from_with_func_name_id(op1: u64, name: Option<IdentId>, func_id: FuncId) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((func_id.get() as u64) << 32)
                    + (if let Some(name) = name {
                        name.get() as u64
                    } else {
                        0
                    }),
            ),
        }
    }

    pub fn from_with_class_and_version(op1: u64, class_id: Option<ClassId>, version: u32) -> Self {
        Self {
            op1,
            op2: Bc2::class_and_version(class_id, version),
        }
    }

    pub fn from_with_class2(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::class2(None, None),
        }
    }

    pub fn from_with_ident2(op1: u64, id1: IdentId, id2: IdentId) -> Self {
        Self {
            op1,
            op2: Bc2::ident2(id1, id2),
        }
    }

    pub fn from_with_num(op1: u64, num0: u16, num1: u16, num2: u16, num3: u16) -> Self {
        Self {
            op1,
            op2: Bc2::from(
                ((num3 as u64) << 48)
                    + ((num2 as u64) << 32)
                    + ((num1 as u64) << 16)
                    + (num0 as u64),
            ),
        }
    }

    pub fn u16(&self, id: usize) -> u16 {
        (self.op2.0 >> (id * 16)) as u16
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(transparent)]
pub(crate) struct Bc2(pub u64);

impl Bc2 {
    fn from(op: u64) -> Self {
        Self(op)
    }

    fn class_and_version(class_id: Option<ClassId>, version: u32) -> Self {
        let id: u32 = if let Some(class) = class_id {
            class.into()
        } else {
            0
        };
        Self(((version as u64) << 32) + (id as u64))
    }

    fn class2(class_id1: Option<ClassId>, class_id2: Option<ClassId>) -> Self {
        let id1: u32 = if let Some(class) = class_id1 {
            class.into()
        } else {
            0
        };
        let id2: u32 = if let Some(class) = class_id2 {
            class.into()
        } else {
            0
        };
        Self(((id2 as u64) << 32) + (id1 as u64))
    }

    fn ident2(id1: IdentId, id2: IdentId) -> Self {
        Self(((id2.get() as u64) << 32) + (id1.get() as u64))
    }

    fn get_value(&self) -> Value {
        Value::from(self.0)
    }

    fn get_ident2(&self) -> (IdentId, IdentId) {
        let id1 = IdentId::from(self.0 as u32);
        let id2 = IdentId::from((self.0 >> 32) as u32);
        (id1, id2)
    }
}

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
struct BytecodePtrBase(std::ptr::NonNull<Bytecode>);

impl std::ops::Add<usize> for BytecodePtrBase {
    type Output = BytecodePtr;
    fn add(self, rhs: usize) -> BytecodePtr {
        BytecodePtr::new(unsafe { self.as_ptr().add(rhs) })
    }
}

impl std::ops::Add<BcIndex> for BytecodePtrBase {
    type Output = BytecodePtr;
    fn add(self, rhs: BcIndex) -> BytecodePtr {
        BytecodePtr(unsafe {
            std::ptr::NonNull::new(self.as_ptr().offset(rhs.0 as isize)).unwrap()
        })
    }
}

impl BytecodePtrBase {
    fn as_ptr(&self) -> *mut Bytecode {
        self.0.as_ptr()
    }
}

///
/// Program counter
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub(crate) struct BytecodePtr(std::ptr::NonNull<Bytecode>);

impl std::ops::Sub<BytecodePtrBase> for BytecodePtr {
    type Output = usize;
    fn sub(self, rhs: BytecodePtrBase) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Sub<BytecodePtr> for BytecodePtr {
    type Output = usize;
    fn sub(self, rhs: BytecodePtr) -> usize {
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        offset as usize
    }
}

impl std::ops::Add<isize> for BytecodePtr {
    type Output = BytecodePtr;
    fn add(self, rhs: isize) -> BytecodePtr {
        BytecodePtr::new(unsafe { self.as_ptr().offset(rhs) })
    }
}

impl std::ops::Sub<isize> for BytecodePtr {
    type Output = BytecodePtr;
    fn sub(self, rhs: isize) -> BytecodePtr {
        BytecodePtr::new(unsafe { self.as_ptr().offset(-rhs) })
    }
}

impl std::ops::AddAssign<i32> for BytecodePtr {
    fn add_assign(&mut self, offset: i32) {
        unsafe {
            *self = BytecodePtr::new(self.as_ptr().offset(offset as isize));
        }
    }
}

impl std::ops::Deref for BytecodePtr {
    type Target = Bytecode;
    fn deref(&self) -> &Self::Target {
        unsafe { self.0.as_ref() }
    }
}

impl BytecodePtr {
    fn new(ptr: *mut Bytecode) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    fn opcode(&self) -> u8 {
        (self.op1 >> 48) as u8
    }
}

impl BytecodePtr {
    pub fn as_ptr(&self) -> *mut Bytecode {
        self.0.as_ptr()
    }

    pub fn is_loop_start(&self) -> bool {
        self.opcode() == 14 // TraceIr::LoopStart(_))
    }

    pub fn from_bc(bc: &Bytecode) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    pub fn write2(self, data: u64) {
        unsafe { *((self.as_ptr() as *mut u64).add(1)) = data }
    }

    pub fn cached_fid(self) -> Option<FuncId> {
        (*self).fid()
    }

    pub fn cached_class1(self) -> Option<ClassId> {
        (*(self + 1)).classid1()
    }

    pub fn cached_class0(self) -> Option<ClassId> {
        self.classid1()
    }
}

impl BytecodePtr {
    pub fn trace_ir(&self, store: &Store) -> TraceIr {
        let op = self.op1;
        let opcode = self.opcode();
        if opcode & 0xc0 == 0 {
            let (op1, op2) = dec_wl(op);
            match opcode {
                1 => TraceIr::SingletonMethodDef {
                    obj: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                2 => TraceIr::MethodDef {
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                3 => TraceIr::Br(op2 as i32),
                4 => TraceIr::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIf),
                5 => TraceIr::CondBr(SlotId::new(op1), op2 as i32, false, BrKind::BrIfNot),
                6 => TraceIr::Integer(SlotId::new(op1), op2 as i32),
                7 => TraceIr::Literal(SlotId::new(op1), self.op2.get_value()),
                8 => TraceIr::Nil(SlotId::new(op1)),
                9 => TraceIr::Symbol(SlotId::new(op1), IdentId::from(op2)),
                10 | 18 => TraceIr::LoadConst(SlotId::new(op1), ConstSiteId(op2)),
                11 => TraceIr::StoreConst(SlotId::new(op1), ConstSiteId(op2)),
                12..=13 => TraceIr::CondBr(
                    SlotId::new(op1),
                    op2 as i32,
                    true,
                    BrKind::from(opcode - 12),
                ),
                14 => TraceIr::LoopStart(op2),
                15 => TraceIr::LoopEnd,
                16 => {
                    let class = self.cached_class0();
                    let ivar = self.cached_ivarid();
                    TraceIr::LoadIvar(SlotId::new(op1), IdentId::from(op2), class, ivar)
                }
                17 => {
                    let class = self.cached_class0();
                    let ivar = self.cached_ivarid();
                    TraceIr::StoreIvar(SlotId::new(op1), IdentId::from(op2), class, ivar)
                }
                20 => TraceIr::CheckLocal(SlotId::new(op1), op2 as i32),
                21 => TraceIr::BlockArgProxy(SlotId::new(op1), op2 as usize),
                22 => TraceIr::SingletonClassDef {
                    dst: SlotId::from(op1),
                    base: SlotId::new(op2 as u16),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                23 => TraceIr::BlockArg(SlotId::new(op1), op2 as usize),
                24 => TraceIr::CheckCvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                25 => TraceIr::LoadGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                26 => TraceIr::StoreGvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                27 => TraceIr::LoadCvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                28 => TraceIr::LoadSvar {
                    dst: SlotId::new(op1),
                    id: op2,
                },
                29 => TraceIr::StoreCvar {
                    src: SlotId::new(op1),
                    name: IdentId::from(op2),
                },
                30..=31 => {
                    let cached_fid = self.cached_fid();
                    let is_simple = opcode == 30;
                    let callid: CallSiteId = op2.into();

                    if let Some(fid) = cached_fid
                        && let Some(inline_id) =
                            crate::executor::inline::InlineTable::get_inline(fid)
                    {
                        if fid == OBJECT_SEND_FUNCID {
                            if store[callid].splat_pos.len() == 1
                                && store[callid].pos_num == 1
                                && !store[callid].kw_may_exists()
                            {
                                return TraceIr::InlineObjectSendSplat { inline_id, callid };
                            } else if is_simple {
                                return TraceIr::InlineObjectSend { inline_id, callid };
                            }
                        } else if is_simple {
                            return TraceIr::InlineCall { inline_id, callid };
                        }
                    }
                    TraceIr::MethodCall { callid }
                }
                32..=33 => TraceIr::MethodCallBlock { callid: op2.into() },
                34..=35 => TraceIr::Yield { callid: op2.into() },
                36 => TraceIr::OptCase {
                    cond: SlotId::new(op1),
                    optid: OptCaseId::from(op2),
                },
                37 => TraceIr::NilBr(SlotId::new(op1), op2 as i32),
                38 => TraceIr::Lambda {
                    dst: SlotId::new(op1),
                    func_id: FuncId::new(op2),
                },
                39 => TraceIr::Array {
                    dst: SlotId::new(op1),
                    callid: CallSiteId::from(op2),
                },
                _ => unreachable!("{:016x}", op),
            }
        } else {
            let (op1, op2, op3) = dec_www(op);
            match opcode {
                64 => TraceIr::DefinedYield {
                    dst: SlotId::new(op1),
                },
                65 => TraceIr::DefinedConst {
                    dst: SlotId::new(op1),
                    siteid: ConstSiteId(self.op2.0 as u32),
                },
                66 => TraceIr::DefinedMethod {
                    dst: SlotId::new(op1),
                    recv: SlotId::new(op2),
                    name: IdentId::from(self.op2.0 as u32),
                },
                67 => TraceIr::DefinedGvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                },
                68 => TraceIr::DefinedIvar {
                    dst: SlotId::new(op1),
                    name: IdentId::from(self.op2.0 as u32),
                },
                70 => TraceIr::ClassDef {
                    dst: SlotId::from(op1),
                    base: SlotId::from(op2),
                    superclass: SlotId::from(op3),
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                71 => TraceIr::ModuleDef {
                    dst: SlotId::from(op1),
                    base: SlotId::from(op2),
                    name: IdentId::from((self.op2.0) as u32),
                    func_id: FuncId::new((self.op2.0 >> 32) as u32),
                },
                80 => TraceIr::Ret(SlotId::new(op1)),
                81 => TraceIr::MethodRet(SlotId::new(op1)),
                82 => TraceIr::Break(SlotId::new(op1)),
                83 => TraceIr::Raise(SlotId::new(op1)),
                85 => TraceIr::EnsureEnd,
                86 => TraceIr::ConcatRegexp(SlotId::from(op1), SlotId::new(op2), op3),
                126 => {
                    let kind = UnOpK::Pos;
                    let dst = SlotId::new(op1);
                    let src = SlotId::new(op2);
                    if self.is_integer1() {
                        TraceIr::IUnOp { kind, dst, src }
                    } else if self.is_float1() {
                        TraceIr::FUnOp { kind, dst, src }
                    } else {
                        TraceIr::UnOp { kind, dst, src }
                    }
                }
                127 => TraceIr::BitNot {
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                128 => TraceIr::Not {
                    dst: SlotId::new(op1),
                    src: SlotId::new(op2),
                },
                129 => {
                    let kind = UnOpK::Neg;
                    let dst = SlotId::new(op1);
                    let src = SlotId::new(op2);
                    if self.is_integer1() {
                        TraceIr::IUnOp { kind, dst, src }
                    } else if self.is_float1() {
                        TraceIr::FUnOp { kind, dst, src }
                    } else {
                        TraceIr::UnOp { kind, dst, src }
                    }
                }
                130 => TraceIr::InlineCache,
                132 => TraceIr::Index {
                    dst: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                133 => TraceIr::IndexAssign {
                    src: SlotId::new(op1),
                    base: SlotId::new(op2),
                    idx: SlotId::new(op3),
                },
                134..=141 => TraceIr::Cmp(
                    CmpKind::from(opcode - 134),
                    SlotId::from(op1),
                    OpMode::RR(SlotId::new(op2), SlotId::new(op3)),
                    false,
                ),
                142..=149 => TraceIr::Cmp(
                    CmpKind::from(opcode - 142),
                    SlotId::from(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    false,
                ),
                150 => TraceIr::LoadDynVar(
                    SlotId::new(op1),
                    DynVar {
                        reg: SlotId::new(op2),
                        outer: op3 as usize,
                    },
                ),
                151 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1),
                        outer: op2 as usize,
                    },
                    SlotId::new(op3),
                ),
                154..=161 => TraceIr::Cmp(
                    CmpKind::from(opcode - 154),
                    SlotId::from(op1),
                    OpMode::RR(SlotId(op2), SlotId(op3)),
                    true,
                ),
                162..=169 => TraceIr::Cmp(
                    CmpKind::from(opcode - 162),
                    SlotId::from(op1),
                    OpMode::RI(SlotId::new(op2), op3 as i16),
                    true,
                ),
                170 | 172 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1 as usize,
                    arg_num: self.u16(3) as usize,
                    reqopt_num: op2 as usize,
                    req_num: self.u16(0) as usize,
                    info: self.u16(2) as usize,
                    stack_offset: op3 as usize,
                }),
                171 => TraceIr::ExpandArray {
                    src: SlotId::new(op1),
                    dst: (SlotId::new(op2), op3),
                },
                173 => {
                    let (new, old) = self.op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1),
                    args: SlotId::new(op2),
                    len: op3,
                },
                176 => TraceIr::Mov(SlotId::new(op1), SlotId::new(op2)),
                177..=178 => TraceIr::Range {
                    dst: SlotId::new(op1),
                    start: SlotId::new(op2),
                    end: SlotId::new(op3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => TraceIr::ConcatStr(SlotId::from(op1), SlotId::new(op2), op3),
                180..=189 => {
                    let kind = BinOpK::from(opcode - 180);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::IR(op2 as i16, SlotId::new(op3));
                    if self.is_integer2() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float2() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                190..=199 => {
                    let kind = BinOpK::from(opcode - 190);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::RI(SlotId::new(op2), op3 as i16);
                    if self.is_integer1() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float1() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                200..=209 => {
                    let kind = BinOpK::from(opcode - 200);
                    let dst = SlotId::from(op1);
                    let mode = OpMode::RR(SlotId::new(op2), SlotId::new(op3));
                    if self.is_integer_binop() {
                        TraceIr::IBinOp { kind, dst, mode }
                    } else if self.is_float_binop() {
                        TraceIr::FBinOp { kind, dst, mode }
                    } else {
                        TraceIr::BinOp { kind, dst, mode }
                    }
                }
                _ => unreachable!("{:016x}", op),
            }
        }
    }
}

fn dec_wl(op: u64) -> (u16, u32) {
    ((op >> 32) as u16, op as u32)
}

fn dec_www(op: u64) -> (u16, u16, u16) {
    ((op >> 32) as u16, (op >> 16) as u16, op as u16)
}
