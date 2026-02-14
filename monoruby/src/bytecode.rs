use crate::codegen::jitgen::trace_ir::MethodCacheEntry;

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
        Value::from_u64(self.0)
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
                func_id: self.cached_fid()?,
                version: (self + 1).cached_version(),
            })
        } else {
            None
        }
    }

    //pub fn write_method_cache(self, cache: &MethodCacheEntry) {
    //    let p = self.as_ptr() as *mut u8;
    //    // SAFETY: Writing method cache data at specific offsets within the bytecode
    //    // instruction. The bytecode structure has sufficient space for these writes.
    //    unsafe {
    //        (p.add(8) as *mut Option<FuncId>).write(Some(cache.func_id));
    //        (p.add(24) as *mut Option<ClassId>).write(Some(cache.recv_class));
    //        (p.add(28) as *mut u32).write(cache.version);
    //    }
    //}
}

impl BytecodePtr {
    pub(crate) fn next(&self) -> BytecodePtr {
        match self.opcode() {
            // MethodCall and Yield have an extra instruction (InlineCache)
            30..=35 => *self + 2,
            _ => *self + 1,
        }
    }
}
