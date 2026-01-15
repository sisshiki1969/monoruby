use ruruby_parse::CmpKind;

use crate::{
    bytecodegen::{
        BinOpK, UnOpK,
        inst::{BrKind, DynVar, FnInitInfo},
    },
    codegen::jitgen::trace_ir::{MethodCacheEntry, TraceIr},
};

use super::*;

///
/// an index of bytecode instruction.
///
#[derive(Clone, Copy, PartialEq, Eq, Hash, Default, PartialOrd)]
#[repr(transparent)]
pub(crate) struct BcIndex(u32);

impl std::fmt::Debug for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::fmt::Display for BcIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ":{:05}", self.0)
    }
}

impl std::ops::Add<usize> for BcIndex {
    type Output = Self;
    fn add(self, rhs: usize) -> Self::Output {
        Self((self.0 as usize + rhs) as u32)
    }
}

impl std::ops::Add<i64> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i64) -> Self::Output {
        Self((self.0 as i64 + rhs) as u32)
    }
}

impl std::ops::Add<i32> for BcIndex {
    type Output = Self;
    fn add(self, rhs: i32) -> Self::Output {
        Self((self.0 as i64 + rhs as i64) as u32)
    }
}

impl std::ops::Sub<Self> for BcIndex {
    type Output = isize;
    fn sub(self, rhs: Self) -> Self::Output {
        (self.0 as isize) - (rhs.0 as isize)
    }
}

impl std::iter::Step for BcIndex {
    fn steps_between(start: &Self, end: &Self) -> (usize, Option<usize>) {
        if start > end {
            (0, None)
        } else {
            let d = end.0 as usize - start.0 as usize;
            (d, Some(d))
        }
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        Some(start + count)
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        if (start.0 as usize) < count {
            None
        } else {
            Some(BcIndex((start.0 as usize - count) as _))
        }
    }
}

impl BcIndex {
    pub(crate) fn from(i: usize) -> Self {
        Self(i as u32)
    }

    pub(crate) fn to_usize(self) -> usize {
        self.0 as usize
    }
}

#[derive(Clone, Copy, PartialEq)]
#[repr(C)]
pub(crate) struct Bytecode {
    pub op1: u64,
    pub op2: Bc2,
}

impl Bytecode {
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
        if op == 0 { None } else { Some(FuncId::new(op)) }
    }

    pub fn into_jit_addr(self) -> *const u8 {
        self.op2.0 as _
    }

    pub fn from(op1: u64) -> Self {
        Self {
            op1,
            op2: Bc2::from(0),
        }
    }

    pub fn from_u16(op1: u64, op2: u16) -> Self {
        Self {
            op1,
            op2: Bc2::from(op2 as u64),
        }
    }

    pub fn from_u32(op1: u64, op2: u32) -> Self {
        Self {
            op1,
            op2: Bc2::from(op2 as u64),
        }
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

    pub fn get_value(&self) -> Value {
        Value::from(self.0)
    }

    pub fn get_ident2(&self) -> (IdentId, IdentId) {
        let id1 = IdentId::from(self.0 as u32);
        let id2 = IdentId::from((self.0 >> 32) as u32);
        (id1, id2)
    }

    pub fn get_u16(&self) -> u16 {
        self.0 as u16
    }
}

///
/// Program counter base.
///
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub(crate) struct BytecodePtrBase(std::ptr::NonNull<Bytecode>);

impl std::ops::Add<usize> for BytecodePtrBase {
    type Output = BytecodePtr;
    fn add(self, rhs: usize) -> BytecodePtr {
        // SAFETY: Pointer arithmetic within the bytecode buffer is safe as long as
        // the offset stays within the allocated bytecode array bounds.
        BytecodePtr::new(unsafe { self.as_ptr().add(rhs) })
    }
}

impl std::ops::Add<BcIndex> for BytecodePtrBase {
    type Output = BytecodePtr;
    fn add(self, rhs: BcIndex) -> BytecodePtr {
        // SAFETY: Pointer arithmetic within the bytecode buffer is safe as long as
        // the offset stays within the allocated bytecode array bounds.
        BytecodePtr(unsafe {
            std::ptr::NonNull::new(self.as_ptr().offset(rhs.0 as isize)).unwrap()
        })
    }
}

impl BytecodePtrBase {
    pub fn from_bc(bc: &Bytecode) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    pub fn as_ptr(&self) -> *mut Bytecode {
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
    type Output = BcIndex;
    fn sub(self, rhs: BytecodePtrBase) -> BcIndex {
        // SAFETY: Both pointers point into the same bytecode buffer, so offset_from
        // is valid. The assert ensures the result is non-negative.
        let offset = unsafe { self.as_ptr().offset_from(rhs.as_ptr()) };
        assert!(offset >= 0, "self:{:?} rhs:{:?}", self, rhs);
        BcIndex(offset as u32)
    }
}

impl std::ops::Add<isize> for BytecodePtr {
    type Output = BytecodePtr;
    fn add(self, rhs: isize) -> BytecodePtr {
        // SAFETY: Pointer arithmetic within the bytecode buffer is safe as long as
        // the offset stays within bounds.
        BytecodePtr::new(unsafe { self.as_ptr().offset(rhs) })
    }
}

impl std::ops::Sub<isize> for BytecodePtr {
    type Output = BytecodePtr;
    fn sub(self, rhs: isize) -> BytecodePtr {
        // SAFETY: Pointer arithmetic within the bytecode buffer is safe as long as
        // the offset stays within bounds.
        BytecodePtr::new(unsafe { self.as_ptr().offset(-rhs) })
    }
}

impl std::ops::AddAssign<i32> for BytecodePtr {
    fn add_assign(&mut self, offset: i32) {
        // SAFETY: Pointer arithmetic within the bytecode buffer is safe as long as
        // the offset stays within bounds.
        unsafe {
            *self = BytecodePtr::new(self.as_ptr().offset(offset as isize));
        }
    }
}

impl std::ops::Deref for BytecodePtr {
    type Target = Bytecode;
    fn deref(&self) -> &Self::Target {
        // SAFETY: NonNull guarantees the pointer is non-null and properly aligned.
        // The bytecode buffer is valid for the lifetime of this reference.
        unsafe { self.0.as_ref() }
    }
}

impl BytecodePtr {
    fn new(ptr: *mut Bytecode) -> Self {
        Self(std::ptr::NonNull::new(ptr).unwrap())
    }

    pub fn opcode(&self) -> u8 {
        (self.op1 >> 48) as u8
    }

    pub fn opcode_sub(&self) -> u8 {
        (self.op1 >> 56) as u8
    }

    pub fn as_ptr(&self) -> *mut Bytecode {
        self.0.as_ptr()
    }

    pub fn from_bc(bc: &Bytecode) -> Self {
        Self(std::ptr::NonNull::from(bc))
    }

    pub fn write2(self, data: u64) {
        // SAFETY: Writing to the second u64 slot of the bytecode instruction.
        // The bytecode buffer has sufficient space for this write.
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

    pub fn method_cache(self) -> Option<MethodCacheEntry> {
        if let Some(cached_class) = self.cached_class1() {
            Some(MethodCacheEntry {
                recv_class: cached_class,
                func_id: self.cached_fid().unwrap(),
                version: (self + 1).cached_version(),
            })
        } else {
            None
        }
    }

    pub fn write_method_cache(self, cache: &MethodCacheEntry) {
        let p = self.as_ptr() as *mut u8;
        // SAFETY: Writing method cache data at specific offsets within the bytecode
        // instruction. The bytecode structure has sufficient space for these writes.
        unsafe {
            (p.add(8) as *mut Option<FuncId>).write(Some(cache.func_id));
            (p.add(24) as *mut Option<ClassId>).write(Some(cache.recv_class));
            (p.add(28) as *mut u32).write(cache.version);
        }
    }
}

impl BytecodePtr {
    pub(crate) fn trace_ir(&self, store: &Store) -> jitgen::trace_ir::TraceIr {
        let op1 = self.op1;
        let op2 = self.op2;
        let opcode = self.opcode();
        if opcode & 0xc0 == 0 {
            let (op1_w, op1_l) = dec_wl(op1);
            match opcode {
                1 => TraceIr::SingletonMethodDef {
                    obj: SlotId::new(op1_w),
                    name: IdentId::from(op2.0 as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                2 => TraceIr::MethodDef {
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                3 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::Br(op1_l as i32)
                }
                4 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(SlotId::new(op1_w), op1_l as i32, false, BrKind::BrIf)
                }
                5 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(SlotId::new(op1_w), op1_l as i32, false, BrKind::BrIfNot)
                }
                6 => TraceIr::Integer(SlotId::new(op1_w), op1_l as i32),
                7 => TraceIr::Literal(SlotId::new(op1_w), op2.get_value()),
                8 => TraceIr::Nil(SlotId::new(op1_w)),
                9 => TraceIr::Symbol(SlotId::new(op1_w), IdentId::from(op1_l)),
                10 | 18 => TraceIr::LoadConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                11 => TraceIr::StoreConst(SlotId::new(op1_w), ConstSiteId(op1_l)),
                12..=13 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CondBr(
                        SlotId::new(op1_w),
                        op1_l as i32,
                        true,
                        BrKind::from(opcode - 12),
                    )
                }
                14 => TraceIr::LoopStart {
                    counter: op1_l,
                    jit_addr: self.into_jit_addr(),
                },
                15 => TraceIr::LoopEnd,
                16 => TraceIr::LoadIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = self.cached_class0() {
                        let ivar = self.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                17 => TraceIr::StoreIvar(
                    SlotId::new(op1_w),
                    IdentId::from(op1_l),
                    if let Some(class) = self.cached_class0() {
                        let ivar = self.cached_ivarid();
                        Some((class, ivar))
                    } else {
                        None
                    },
                ),
                19 => TraceIr::CheckKwRest(SlotId::new(op1_w)),
                20 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::CheckLocal(SlotId::new(op1_w), op1_l as i32)
                }
                21 => TraceIr::BlockArgProxy(SlotId::new(op1_w), op1_l as usize),
                22 => TraceIr::SingletonClassDef {
                    dst: SlotId::from(op1_w),
                    base: SlotId::new(op1_l as u16),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                23 => TraceIr::BlockArg(SlotId::new(op1_w), op1_l as usize),
                24 => TraceIr::CheckCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                25 => TraceIr::LoadGvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                26 => TraceIr::StoreGvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                27 => TraceIr::LoadCvar {
                    dst: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                28 => TraceIr::LoadSvar {
                    dst: SlotId::new(op1_w),
                    id: op1_l,
                },
                29 => TraceIr::StoreCvar {
                    src: SlotId::new(op1_w),
                    name: IdentId::from(op1_l),
                },
                30..=33 => {
                    let callid = op1_l.into();
                    let polymorphic = match self.opcode_sub() {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    };
                    let cache = if let Some(cache) = self.method_cache() {
                        Some(cache)
                    } else {
                        None
                    };
                    TraceIr::MethodCall {
                        _polymorphic: polymorphic,
                        callid,
                        cache,
                    }
                }
                34..=35 => TraceIr::Yield {
                    callid: op1_l.into(),
                },
                36 => {
                    let optid = OptCaseId::from(op1_l);
                    let OptCaseInfo {
                        min,
                        max,
                        offsets,
                        branch_table,
                    } = &store[optid];
                    let else_disp = offsets[0] as i32;
                    let branch_table: Box<[_]> =
                        branch_table.iter().map(|ofs| *ofs as i32).collect();
                    TraceIr::OptCase {
                        cond: SlotId::new(op1_w),
                        min: *min,
                        max: *max,
                        else_disp,
                        branch_table,
                    }
                }
                37 => {
                    //let dest = self.get_bb(bc_pos + 1 + op1_l as i32);
                    TraceIr::NilBr(SlotId::new(op1_w), op1_l as i32)
                }
                38 => TraceIr::Lambda {
                    dst: SlotId::new(op1_w),
                    func_id: FuncId::new(op1_l),
                },
                39 => TraceIr::Array {
                    dst: SlotId::new(op1_w),
                    callid: CallSiteId::from(op1_l),
                },
                40 => {
                    let (_, op1_w2, op1_w3) = dec_www(op1);
                    TraceIr::ArrayTEq {
                        lhs: SlotId::new(op1_w2),
                        rhs: SlotId::new(op1_w3),
                    }
                }
                _ => unreachable!("{:016x}", op1),
            }
        } else {
            let (op1_w1, op2_w2, op3_w3) = dec_www(op1);
            match opcode {
                64 => TraceIr::DefinedYield {
                    dst: SlotId::new(op1_w1),
                },
                65 => TraceIr::DefinedConst {
                    dst: SlotId::new(op1_w1),
                    siteid: ConstSiteId(op2.0 as u32),
                },
                66 => TraceIr::DefinedMethod {
                    dst: SlotId::new(op1_w1),
                    recv: SlotId::new(op2_w2),
                    name: IdentId::from(op2.0 as u32),
                },
                67 => TraceIr::DefinedGvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                68 => TraceIr::DefinedIvar {
                    dst: SlotId::new(op1_w1),
                    name: IdentId::from(op2.0 as u32),
                },
                69 => TraceIr::DefinedSuper {
                    dst: SlotId::new(op1_w1),
                },
                70 => TraceIr::ClassDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    superclass: SlotId::from(op3_w3),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                71 => TraceIr::ModuleDef {
                    dst: SlotId::from(op1_w1),
                    base: SlotId::from(op2_w2),
                    name: IdentId::from((op2.0) as u32),
                    func_id: FuncId::new((op2.0 >> 32) as u32),
                },
                80 => TraceIr::Ret(SlotId::new(op1_w1)),
                81 => TraceIr::MethodRet(SlotId::new(op1_w1)),
                82 => TraceIr::BlockBreak(SlotId::new(op1_w1)),
                83 => TraceIr::Raise(SlotId::new(op1_w1)),
                85 => TraceIr::EnsureEnd,
                86 => TraceIr::ConcatRegexp(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                120 => TraceIr::Not {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                121..=123 => {
                    let kind = UnOpK::from(opcode - 121);
                    let dst = SlotId::new(op1_w1);
                    let src = SlotId::new(op2_w2);
                    TraceIr::UnOp {
                        kind,
                        dst,
                        src,
                        ic: self.classid1(),
                    }
                }
                130 => TraceIr::InlineCache,
                132 => TraceIr::Index {
                    dst: SlotId::new(op1_w1),
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    class: if let Some(base_class) = self.classid1()
                        && let Some(idx_class) = self.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                133 => TraceIr::IndexAssign {
                    base: SlotId::new(op2_w2),
                    idx: SlotId::new(op3_w3),
                    src: SlotId::new(op1_w1),
                    class: if let Some(base_class) = self.classid1()
                        && let Some(idx_class) = self.classid2()
                    {
                        Some((base_class, idx_class))
                    } else {
                        None
                    },
                },
                140..=146 => {
                    let kind = CmpKind::from(opcode - 140);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = self.classid1()
                        && let Some(rhs_class) = self.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinCmp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                148 => TraceIr::LoadDynVar(
                    SlotId::new(op1_w1),
                    DynVar {
                        reg: SlotId::new(op2_w2),
                        outer: op3_w3 as usize,
                    },
                ),
                149 => TraceIr::StoreDynVar(
                    DynVar {
                        reg: SlotId::new(op1_w1),
                        outer: op2_w2 as usize,
                    },
                    SlotId::new(op3_w3),
                ),
                150..=156 => {
                    let kind = CmpKind::from(opcode - 150);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = self.classid1()
                        && let Some(rhs_class) = self.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    let (disp, brkind) = match (*self + 1).trace_ir(store) {
                        TraceIr::CondBr(_, dest, true, brkind) => (dest + 1, brkind),
                        _ => unreachable!(),
                    };
                    TraceIr::BinCmpBr {
                        kind,
                        _dst: dst,
                        lhs,
                        rhs,
                        disp,
                        brkind,
                        ic,
                    }
                }
                160..=168 => {
                    let kind = BinOpK::from(opcode - 160);
                    let dst = SlotId::from(op1_w1);
                    let lhs = SlotId::new(op2_w2);
                    let rhs = SlotId::new(op3_w3);
                    let ic = if let Some(lhs_class) = self.classid1()
                        && let Some(rhs_class) = self.classid2()
                    {
                        Some((lhs_class, rhs_class))
                    } else {
                        None
                    };
                    TraceIr::BinOp {
                        kind,
                        dst,
                        lhs,
                        rhs,
                        ic,
                    }
                }

                170 => TraceIr::InitMethod(FnInitInfo {
                    reg_num: op1_w1 as usize,
                    arg_num: op2_w2 as usize,
                    stack_offset: op3_w3 as usize,
                }),
                171 => TraceIr::ExpandArray {
                    src: SlotId::new(op1_w1),
                    dst: (SlotId::new(op2_w2), op3_w3, {
                        let rest = op2.get_u16();
                        if rest == 0 { None } else { Some(rest - 1) }
                    }),
                },
                172 => {
                    let undef = IdentId::from(dec_wl(op1).1);
                    TraceIr::UndefMethod { undef }
                }
                173 => {
                    let (new, old) = op2.get_ident2();
                    TraceIr::AliasMethod { new, old }
                }
                174 => TraceIr::Hash {
                    dst: SlotId::new(op1_w1),
                    args: SlotId::new(op2_w2),
                    len: op3_w3,
                },
                175 => TraceIr::ToA {
                    dst: SlotId::new(op1_w1),
                    src: SlotId::new(op2_w2),
                },
                176 => TraceIr::Mov(SlotId::new(op1_w1), SlotId::new(op2_w2)),
                177..=178 => TraceIr::Range {
                    dst: SlotId::new(op1_w1),
                    start: SlotId::new(op2_w2),
                    end: SlotId::new(op3_w3),
                    exclude_end: match opcode - 177 {
                        0 => false,
                        1 => true,
                        _ => unreachable!(),
                    },
                },
                179 => TraceIr::ConcatStr(SlotId::from(op1_w1), SlotId::new(op2_w2), op3_w3),
                _ => unreachable!("{:016x}", op1),
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
