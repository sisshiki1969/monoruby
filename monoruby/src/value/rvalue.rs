use super::*;
use num::BigInt;
use onigmo_regex::Captures;
use ruruby_parse::Loc;
use ruruby_parse::SourceInfoRef;
use std::mem::ManuallyDrop;

pub use array::*;
pub use binding::*;
pub use complex::ComplexInner;
pub use enumerator::*;
pub use exception::ExceptionInner;
pub use fiber::*;
pub use hash::*;
pub use io::IoInner;
pub use ivar_table::*;
pub use match_data::MatchDataInner;
pub use method::*;
pub use module::{Module, ModuleInner, ModuleType};
pub use proc::*;
pub use range::RangeInner;
pub use regexp::{Regexp, RegexpInner};
pub(crate) use string::pack::*;
pub use string::{Encoding, RString, RStringInner};

mod array;
mod binding;
mod complex;
mod enumerator;
mod exception;
mod fiber;
mod hash;
mod io;
mod ivar_table;
mod match_data;
mod method;
mod module;
mod proc;
mod range;
mod regexp;
mod string;

pub const OBJECT_INLINE_IVAR: usize = 6;
pub const RVALUE_OFFSET_TY: usize = std::mem::offset_of!(RValue, header.meta.ty);
pub const RVALUE_OFFSET_CLASS: usize = std::mem::offset_of!(RValue, header.meta.class);
pub const RVALUE_OFFSET_VAR: usize = std::mem::offset_of!(RValue, var_table);
pub const RVALUE_OFFSET_KIND: usize = std::mem::offset_of!(RValue, kind);
pub const RVALUE_OFFSET_ARY_CAPA: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_CAPA;
pub const RVALUE_OFFSET_INLINE: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_INLINE;
pub const RVALUE_OFFSET_HEAP_PTR: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_HEAP_PTR;
pub const RVALUE_OFFSET_HEAP_LEN: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_HEAP_LEN;

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct ObjTy(std::num::NonZeroU8);

impl std::fmt::Debug for ObjTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let ty = self.0.get();
        write!(
            f,
            "{}",
            match ty {
                1 => "CLASS",
                2 => "MODULE",
                3 => "OBJECT",
                4 => "BIGNUM",
                5 => "FLOAT",
                6 => "STRING",
                7 => "TIME",
                8 => "ARRAY",
                9 => "RANGE",
                10 => "EXCEPTION",
                11 => "PROC",
                12 => "HASH",
                13 => "REGEXP",
                14 => "IO",
                15 => "METHOD",
                16 => "FIBER",
                17 => "ENUMERATOR",
                18 => "GENERATOR",
                19 => "COMPLEX",
                20 => "BINDING",
                21 => "UMETHOD",
                22 => "MATCHDATA",
                _ => unreachable!(),
            }
        )
    }
}

impl ObjTy {
    pub fn get(&self) -> u8 {
        self.0.get()
    }
}

impl ObjTy {
    pub const CLASS: Self = Self(std::num::NonZeroU8::new(1).unwrap());
    pub const MODULE: Self = Self(std::num::NonZeroU8::new(2).unwrap());
    pub const OBJECT: Self = Self(std::num::NonZeroU8::new(3).unwrap());
    pub const BIGNUM: Self = Self(std::num::NonZeroU8::new(4).unwrap());
    pub const FLOAT: Self = Self(std::num::NonZeroU8::new(5).unwrap());
    pub const STRING: Self = Self(std::num::NonZeroU8::new(6).unwrap());
    pub const TIME: Self = Self(std::num::NonZeroU8::new(7).unwrap());
    pub const ARRAY: Self = Self(std::num::NonZeroU8::new(8).unwrap());
    pub const RANGE: Self = Self(std::num::NonZeroU8::new(9).unwrap());
    pub const EXCEPTION: Self = Self(std::num::NonZeroU8::new(10).unwrap());
    pub const PROC: Self = Self(std::num::NonZeroU8::new(11).unwrap());
    pub const HASH: Self = Self(std::num::NonZeroU8::new(12).unwrap());
    pub const REGEXP: Self = Self(std::num::NonZeroU8::new(13).unwrap());
    pub const IO: Self = Self(std::num::NonZeroU8::new(14).unwrap());
    pub const METHOD: Self = Self(std::num::NonZeroU8::new(15).unwrap());
    pub const FIBER: Self = Self(std::num::NonZeroU8::new(16).unwrap());
    pub const ENUMERATOR: Self = Self(std::num::NonZeroU8::new(17).unwrap());
    pub const GENERATOR: Self = Self(std::num::NonZeroU8::new(18).unwrap());
    pub const COMPLEX: Self = Self(std::num::NonZeroU8::new(19).unwrap());
    pub const BINDING: Self = Self(std::num::NonZeroU8::new(20).unwrap());
    pub const UMETHOD: Self = Self(std::num::NonZeroU8::new(21).unwrap());
    pub const MATCHDATA: Self = Self(std::num::NonZeroU8::new(22).unwrap());
}

#[repr(C)]
pub union ObjKind {
    invalid: (),
    class: ManuallyDrop<ModuleInner>,
    object: [Option<Value>; OBJECT_INLINE_IVAR],
    bignum: ManuallyDrop<BigInt>,
    float: f64,
    complex: ManuallyDrop<ComplexInner>,
    string: ManuallyDrop<RStringInner>,
    time: ManuallyDrop<TimeInner>,
    array: ManuallyDrop<ArrayInner>,
    range: ManuallyDrop<RangeInner>,
    exception: ManuallyDrop<Box<ExceptionInner>>,
    proc: ManuallyDrop<ProcInner>,
    hash: ManuallyDrop<HashmapInner>,
    regexp: ManuallyDrop<RegexpInner>,
    io: ManuallyDrop<IoInner>,
    method: ManuallyDrop<MethodInner>,
    umethod: ManuallyDrop<UMethodInner>,
    fiber: ManuallyDrop<FiberInner>,
    enumerator: ManuallyDrop<EnumeratorInner>,
    generator: ManuallyDrop<GeneratorInner>,
    binding: ManuallyDrop<BindingInner>,
    matchdata: ManuallyDrop<MatchDataInner>,
}

impl ObjKind {
    fn invalid() -> Self {
        Self { invalid: () }
    }

    fn class(class: ClassId, superclass: Option<Module>, class_type: ModuleType) -> Self {
        Self {
            class: ManuallyDrop::new(ModuleInner::new(class, superclass, None, class_type)),
        }
    }

    fn singleton_class(class: ClassId, superclass: Option<Module>, singleton: Value) -> Self {
        Self {
            class: ManuallyDrop::new(ModuleInner::new(
                class,
                superclass,
                Some(singleton),
                ModuleType::RealClass,
            )),
        }
    }

    fn object() -> Self {
        Self {
            object: [None; OBJECT_INLINE_IVAR],
        }
    }

    fn bignum(b: BigInt) -> Self {
        Self {
            bignum: ManuallyDrop::new(b),
        }
    }

    fn float(float: f64) -> Self {
        Self { float }
    }

    fn complex(re: Real, im: Real) -> Self {
        Self {
            complex: ManuallyDrop::new(ComplexInner::new(re, im)),
        }
    }

    fn complex_from(complex: num::complex::Complex<Real>) -> Self {
        Self {
            complex: ManuallyDrop::new(ComplexInner::from(complex)),
        }
    }

    fn string_from_inner(inner: RStringInner) -> Self {
        Self {
            string: ManuallyDrop::new(inner),
        }
    }

    fn bytes_from_slice(slice: &[u8]) -> Self {
        Self {
            string: ManuallyDrop::new(RStringInner::bytes(slice)),
        }
    }

    fn bytes_from_vec(vec: Vec<u8>) -> Self {
        Self {
            string: ManuallyDrop::new(RStringInner::bytes_from_vec(vec)),
        }
    }

    fn string_from_string(s: String) -> Self {
        Self {
            string: ManuallyDrop::new(RStringInner::from_string(s)),
        }
    }

    fn string_from_str(s: &str) -> Self {
        Self {
            string: ManuallyDrop::new(RStringInner::from_str(s)),
        }
    }

    fn string_from_vec(vec: Vec<u8>) -> Self {
        Self {
            string: ManuallyDrop::new(RStringInner::string_from_vec(vec)),
        }
    }

    fn array(ary: ArrayInner) -> Self {
        Self {
            array: ManuallyDrop::new(ary),
        }
    }

    fn range(start: Value, end: Value, exclude_end: bool) -> Self {
        Self {
            range: ManuallyDrop::new(RangeInner::new(start, end, exclude_end)),
        }
    }

    fn exception(err: MonorubyErr) -> Self {
        Self {
            exception: ManuallyDrop::new(Box::new(ExceptionInner::new(err))),
        }
    }

    fn hash_from(map: IndexMap<HashKey, Value>) -> Self {
        Self {
            hash: ManuallyDrop::new(HashmapInner::new(map)),
        }
    }

    fn hash_with_default(default: Value) -> Self {
        Self {
            hash: ManuallyDrop::new(HashmapInner::new_with_default(IndexMap::default(), default)),
        }
    }

    fn hash_with_default_proc(default_proc: Proc) -> Self {
        Self {
            hash: ManuallyDrop::new(HashmapInner::new_with_default_proc(
                IndexMap::default(),
                default_proc,
            )),
        }
    }

    fn hash_from_inner(inner: HashmapInner) -> Self {
        Self {
            hash: ManuallyDrop::new(inner),
        }
    }

    fn regexp(regexp: RegexpInner) -> Self {
        Self {
            regexp: ManuallyDrop::new(regexp),
        }
    }

    fn io(io: IoInner) -> Self {
        Self {
            io: ManuallyDrop::new(io),
        }
    }

    fn time(info: TimeInner) -> Self {
        Self {
            time: ManuallyDrop::new(info),
        }
    }

    fn proc(block_data: ProcInner) -> Self {
        Self {
            proc: ManuallyDrop::new(block_data),
        }
    }

    fn method(receiver: Value, func_id: FuncId, owner: ClassId) -> Self {
        Self {
            method: ManuallyDrop::new(MethodInner::new(receiver, func_id, owner)),
        }
    }

    fn unbound_method(func_id: FuncId, owner: ClassId) -> Self {
        Self {
            umethod: ManuallyDrop::new(UMethodInner::new(func_id, owner)),
        }
    }

    fn fiber(proc: Proc) -> Self {
        Self {
            fiber: ManuallyDrop::new(FiberInner::new(proc)),
        }
    }

    fn enumerator(obj: Value, method: IdentId, proc: Proc, args: Vec<Value>) -> Self {
        Self {
            enumerator: ManuallyDrop::new(EnumeratorInner::new(obj, method, proc, args)),
        }
    }

    fn generator(proc: Proc) -> Self {
        Self {
            generator: ManuallyDrop::new(GeneratorInner::new(proc)),
        }
    }

    fn binding_from_outer(outer_lfp: Lfp) -> Self {
        Self {
            binding: ManuallyDrop::new(BindingInner::from(outer_lfp)),
        }
    }

    fn matchdata(captures: Captures, heystack: String, regex: Regexp) -> Self {
        Self {
            matchdata: ManuallyDrop::new(MatchDataInner::from_capture(captures, heystack, regex)),
        }
    }
}

/// Heap-allocated objects.
#[repr(C)]
pub struct RValue {
    /// flags. 8 bytes
    header: Header,
    /// instance variable table. 8 bytes
    #[allow(clippy::box_collection)]
    var_table: Option<Box<MonoVec<Option<Value>>>>,
    /// object data. 48 bytes.
    pub kind: ObjKind,
}

impl std::fmt::Debug for RValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let meta = unsafe { self.header.meta };
        if !self.header.is_live() {
            write!(f, "{:016x} DEAD: {:?}", self.id(), unsafe {
                self.header.meta
            })
        } else {
            write!(
                f,
                "{:016x} RValue {{ class:{:?} {} }}",
                self.id(),
                meta.class,
                if let Some(ty) = meta.ty {
                    format!("{:?}({})", ty, unsafe {
                        match ty {
                            ObjTy::CLASS => format!("{:?}", self.kind.class),
                            ObjTy::MODULE => format!("{:?}", self.kind.class),
                            ObjTy::OBJECT => format!("{:?}", self.kind.object),
                            ObjTy::BIGNUM => format!("{:?}", self.kind.bignum),
                            ObjTy::FLOAT => format!("{:?}", self.kind.float),
                            ObjTy::STRING => format!("{}", self.kind.string.to_str().unwrap()),
                            ObjTy::TIME => format!("{:?}", self.kind.time),
                            ObjTy::ARRAY => format!("{:?}", self.kind.array),
                            ObjTy::RANGE => format!("{:?}", self.kind.range),
                            ObjTy::EXCEPTION => format!("{:?}", self.kind.exception),
                            ObjTy::PROC => format!("{:?}", self.kind.proc),
                            ObjTy::HASH => format!("{:?}", self.kind.hash),
                            ObjTy::REGEXP => format!("{:?}", self.kind.regexp),
                            ObjTy::IO => format!("{:?}", self.kind.io),
                            ObjTy::METHOD => format!("{:?}", self.kind.method),
                            ObjTy::FIBER => format!("{:?}", self.kind.fiber),
                            ObjTy::ENUMERATOR => format!("{:?}", self.kind.enumerator),
                            ObjTy::GENERATOR => format!("{:?}", self.kind.generator),
                            ObjTy::COMPLEX => format!("{:?}", self.kind.complex),
                            ObjTy::BINDING => format!("{:?}", self.kind.binding),
                            ObjTy::UMETHOD => format!("{:?}", self.kind.umethod),
                            ObjTy::MATCHDATA => format!("{:?}", self.kind.matchdata),
                            _ => unreachable!(),
                        }
                    })
                } else {
                    "INVALID".to_string()
                },
            )
        }
    }
}

impl PartialEq for RValue {
    fn eq(&self, other: &Self) -> bool {
        self.id() == other.id()
    }
}

impl std::hash::Hash for RValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        unsafe {
            match self.ty() {
                //ObjTy::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
                ObjTy::BIGNUM => self.as_bignum().hash(state),
                ObjTy::FLOAT => self.as_float().to_bits().hash(state),
                ObjTy::STRING => self.as_rstring().hash(state),
                ObjTy::ARRAY => self.as_array().hash(state),
                ObjTy::RANGE => self.as_range().hash(state),
                ObjTy::HASH => self.as_hashmap().hash(state),
                ObjTy::COMPLEX => self.as_complex().hash(state),
                _ => self.hash(state),
            }
        }
    }
}

impl RValue {
    pub fn swap_kind(&mut self, other: &mut Self) {
        std::mem::swap(&mut self.kind, &mut other.kind);
    }

    pub(crate) fn debug(&self, store: &Store) -> String {
        unsafe {
            match self.ty() {
                ObjTy::CLASS | ObjTy::MODULE => store.debug_class_name(self.as_class_id()),
                ObjTy::TIME => self.as_time().to_string(),
                ObjTy::ARRAY => self.as_array().debug(store),
                ObjTy::OBJECT => self.object_debug(store),
                ObjTy::RANGE => self.as_range().debug(store),
                ObjTy::PROC => self.proc_tos(),
                ObjTy::HASH => self.as_hashmap().debug(store),
                ObjTy::REGEXP => self.as_regex().tos(),
                ObjTy::IO => self.as_io().to_string(),
                ObjTy::EXCEPTION => self.as_exception().message().to_string(),
                ObjTy::METHOD => self.as_method().debug(store),
                ObjTy::FIBER => self.fiber_debug(store),
                ObjTy::ENUMERATOR => self.enumerator_debug(store),
                ObjTy::GENERATOR => self.object_debug(store),
                ObjTy::COMPLEX => self.as_complex().debug(store),
                ObjTy::BINDING => self.object_debug(store),
                ObjTy::UMETHOD => self.as_umethod().debug(store),
                ObjTy::MATCHDATA => self.as_match_data().inspect(),
                _ => format!("{:016x}", self.id()),
            }
        }
    }

    pub(crate) fn to_s(&self, store: &Store) -> String {
        unsafe {
            match self.ty() {
                ObjTy::CLASS | ObjTy::MODULE => store.get_class_name(self.as_class_id()),
                ObjTy::ARRAY => self.as_array().to_s(store),
                ObjTy::OBJECT => self.object_tos(store),
                ObjTy::RANGE => self.as_range().to_s(store),
                ObjTy::PROC => self.proc_tos(),
                ObjTy::HASH => self.as_hashmap().to_s(store),
                ObjTy::METHOD => self.as_method().to_s(store),
                ObjTy::ENUMERATOR => self.enumerator_tos(store),
                ObjTy::GENERATOR => self.object_tos(store),
                ObjTy::BINDING => self.object_tos(store),
                ObjTy::UMETHOD => self.as_umethod().to_s(store),
                ObjTy::MATCHDATA => self.as_match_data().to_s(),
                _ => self.debug(store),
            }
        }
    }

    pub(crate) fn inspect(&self, store: &Store) -> String {
        unsafe {
            match self.ty() {
                ObjTy::OBJECT => self.object_inspect(store),
                ObjTy::EXCEPTION => {
                    let class_name = store.get_class_name(self.class());
                    let msg = self.as_exception().message();
                    format!("#<{class_name}: {msg}>")
                }
                ObjTy::REGEXP => self.as_regex().inspect(),
                ObjTy::MATCHDATA => self.as_match_data().inspect(),
                _ => self.to_s(store),
            }
        }
    }

    fn object_debug(&self, store: &Store) -> String {
        if let Some(name) = self.get_ivar(store, IdentId::_NAME) {
            name.debug_tos(store)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                store.debug_class_name(self.real_class(store).id()),
                self.id()
            )
        }
    }

    fn object_tos(&self, store: &Store) -> String {
        if let Some(name) = self.get_ivar(store, IdentId::_NAME) {
            name.to_s(store)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                store.get_class_name(self.real_class(store).id()),
                self.id()
            )
        }
    }

    fn object_inspect(&self, store: &Store) -> String {
        if let Some(name) = self.get_ivar(store, IdentId::_NAME) {
            name.to_s(store)
        } else {
            let mut s = String::new();
            for (id, v) in self.get_ivars(store).into_iter() {
                s += &format!(" {id}={}", v.inspect(store));
            }
            format!(
                "#<{}:0x{:016x}{s}>",
                store.get_class_name(self.class()),
                self.id()
            )
        }
    }

    fn fiber_debug(&self, store: &Store) -> String {
        let fiber = unsafe { self.as_fiber() };
        let state = match fiber.state() {
            FiberState::Created => "created",
            FiberState::Terminated => "terminated",
            FiberState::Suspended => "suspended",
        };
        let func_id = fiber.func_id();
        format!(
            "#<Fiber:0x{:016x} {} ({state})>",
            self.id(),
            store.iseq(func_id).get_location(),
        )
    }

    fn enumerator_debug(&self, store: &Store) -> String {
        let e = unsafe { self.as_enumerator() };
        format!("#<Enumerator: {} {}>", e.obj.debug(store), e.method)
    }

    fn enumerator_tos(&self, store: &Store) -> String {
        let e = unsafe { self.as_enumerator() };
        format!("#<Enumerator: {} {}>", e.obj.to_s(store), e.method)
    }

    fn proc_tos(&self) -> String {
        format!("#<Proc:0x{:016x}>", self.id())
    }
}

impl RValue {
    // This type of equality is used for comparison for keys of Hash.
    pub(crate) fn eql(&self, other: &Self) -> bool {
        unsafe {
            match (self.ty(), other.ty()) {
                (ObjTy::OBJECT, ObjTy::OBJECT) => self.id() == other.id(),
                (ObjTy::BIGNUM, ObjTy::BIGNUM) => self.as_bignum() == other.as_bignum(),
                (ObjTy::FLOAT, ObjTy::FLOAT) => self.as_float() == other.as_float(),
                (ObjTy::COMPLEX, ObjTy::COMPLEX) => self.as_complex().eql(other.as_complex()),
                (ObjTy::STRING, ObjTy::STRING) => self.as_rstring() == other.as_rstring(),
                (ObjTy::ARRAY, ObjTy::ARRAY) => {
                    let lhs = self.as_array();
                    let rhs = other.as_array();
                    if lhs.len() != rhs.len() {
                        return false;
                    }
                    lhs.iter().zip(rhs.iter()).all(|(a1, a2)| {
                        // Support self-containing arrays.
                        if self.id() == a1.id() && other.id() == a2.id() {
                            true
                        } else if self.id() == a1.id() || other.id() == a2.id() {
                            false
                        } else {
                            a1.eql(a2)
                        }
                    })
                }
                (ObjTy::RANGE, ObjTy::RANGE) => self.as_range().eql(other.as_range()),
                (ObjTy::HASH, ObjTy::HASH) => self.as_hashmap() == other.as_hashmap(),
                //(ObjTy::METHOD, ObjTy::METHOD) => *self.method() == *other.method(),
                //(ObjTy::UNBOUND_METHOD, ObjTy::UNBOUND_METHOD) => *self.method() == *other.method(),
                //(ObjTy::INVALID, _) => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
                //(_, ObjTy::INVALID) => panic!("Invalid rvalue. (maybe GC problem) {:?}", other),
                _ => false,
            }
        }
    }
}

impl alloc::GC<RValue> for RValue {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        assert!(self.header.is_live());
        if alloc.gc_check_and_mark(self) {
            return;
        }
        if let Some(v) = &self.var_table {
            v.iter().for_each(|v| {
                v.map(|v| {
                    v.mark(alloc);
                });
            });
        }
        unsafe {
            match self.ty() {
                ObjTy::CLASS | ObjTy::MODULE => self.as_module().mark(alloc),
                ObjTy::OBJECT => {
                    self.as_object().iter().for_each(|v| {
                        v.map(|v| {
                            v.mark(alloc);
                        });
                    });
                }
                ObjTy::BIGNUM => {}
                ObjTy::FLOAT => {}
                ObjTy::COMPLEX => self.as_complex().mark(alloc),
                ObjTy::STRING => {}
                ObjTy::TIME => {}
                ObjTy::ARRAY => self.as_array().iter().for_each(|v| v.mark(alloc)),
                ObjTy::RANGE => self.as_range().mark(alloc),
                ObjTy::PROC => self.as_proc().mark(alloc),
                ObjTy::HASH => self.as_hashmap().mark(alloc),
                ObjTy::REGEXP => {}
                ObjTy::IO => {}
                ObjTy::EXCEPTION => {}
                ObjTy::METHOD => self.as_method().mark(alloc),
                ObjTy::FIBER => self.as_fiber().mark(alloc),
                ObjTy::ENUMERATOR => self.as_enumerator().mark(alloc),
                ObjTy::GENERATOR => self.as_generator().mark(alloc),
                ObjTy::BINDING => self.as_binding().mark(alloc),
                ObjTy::UMETHOD => {}
                _ => unreachable!("mark {:016x} {:?}", self.id(), self.ty()),
            }
        }
    }
}

impl alloc::GCBox for RValue {
    ///
    /// Free RValue slot.
    ///
    /// note that free() can be called for already free'd rvalue's, because slots in free list
    /// are free'd in the next sweep phase once more.
    ///
    fn free(&mut self) {
        if !self.header.is_live() {
            return;
        }
        unsafe {
            match self.ty() {
                //ObjTy::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                ObjTy::MODULE | ObjTy::CLASS => ManuallyDrop::drop(&mut self.kind.class),
                ObjTy::BIGNUM => ManuallyDrop::drop(&mut self.kind.bignum),
                ObjTy::STRING => ManuallyDrop::drop(&mut self.kind.string),
                ObjTy::TIME => ManuallyDrop::drop(&mut self.kind.time),
                ObjTy::ARRAY => ManuallyDrop::drop(&mut self.kind.array),
                ObjTy::EXCEPTION => ManuallyDrop::drop(&mut self.kind.exception),
                ObjTy::HASH => ManuallyDrop::drop(&mut self.kind.hash),
                ObjTy::REGEXP => ManuallyDrop::drop(&mut self.kind.regexp),
                ObjTy::FIBER => ManuallyDrop::drop(&mut self.kind.fiber),
                ObjTy::ENUMERATOR => ManuallyDrop::drop(&mut self.kind.enumerator),
                ObjTy::GENERATOR => ManuallyDrop::drop(&mut self.kind.generator),
                ObjTy::BINDING => ManuallyDrop::drop(&mut self.kind.binding),
                _ => {}
            }
            self.set_next_none();
            self.var_table = None;
        }
    }

    fn next(&self) -> Option<std::ptr::NonNull<RValue>> {
        let next = unsafe { self.header.next };
        assert!(
            unsafe {
                std::mem::transmute::<Option<std::ptr::NonNull<value::rvalue::RValue>>, u64>(next)
            } & 0b1
                != 1
        );
        next
    }

    fn set_next_none(&mut self) {
        self.header.next = None;
    }

    fn set_next(&mut self, next: *mut RValue) {
        self.header.next = Some(std::ptr::NonNull::new(next).unwrap());
    }

    fn new_invalid() -> Self {
        RValue {
            header: Header { next: None },
            kind: ObjKind::invalid(),
            var_table: None,
        }
    }
}

impl RValue {
    pub(crate) fn id(&self) -> u64 {
        self as *const RValue as u64
    }

    pub(crate) fn ty(&self) -> ObjTy {
        self.header.ty()
    }

    pub(crate) unsafe fn try_ty(&self) -> Option<ObjTy> {
        self.header.meta.ty
    }

    ///
    /// Get class object of *self.
    ///
    pub(crate) fn get_class_obj(&self, store: &Store) -> Module {
        store[self.class()].get_module()
    }

    pub(crate) fn real_class(&self, store: &Store) -> Module {
        self.get_class_obj(store).get_real_class()
    }

    pub(crate) fn get_ivar(&self, store: &Store, name: IdentId) -> Option<Value> {
        let class_id = self.class();
        let id = store[class_id].get_ivarid(name)?;
        self.get_ivar_by_ivarid(id)
    }

    pub(crate) fn get_ivars(&self, store: &Store) -> Vec<(IdentId, Value)> {
        let class_id = self.class();
        store[class_id]
            .ivar_names()
            .filter_map(|(name, id)| self.get_ivar_by_ivarid(*id).map(|v| (*name, v)))
            .collect()
    }

    pub(crate) fn get_ivar_by_ivarid(&self, id: IvarId) -> Option<Value> {
        let mut i = id.into_usize();
        if self.ty() == ObjTy::OBJECT {
            if i < OBJECT_INLINE_IVAR {
                return unsafe { self.as_object()[i] };
            } else {
                i -= OBJECT_INLINE_IVAR;
            }
        }
        if let Some(v) = &self.var_table {
            if v.len() > i {
                return v[i];
            }
        }
        None
    }

    pub(crate) fn set_ivar_by_ivarid(&mut self, id: IvarId, val: Value) {
        let mut i = id.into_usize();
        if self.ty() == ObjTy::OBJECT {
            if i < OBJECT_INLINE_IVAR {
                unsafe { self.as_object_mut()[i] = Some(val) };
                return;
            } else {
                i -= OBJECT_INLINE_IVAR;
            }
        }
        match &mut self.var_table {
            Some(v) => {
                if v.len() <= i {
                    v.resize(i + 1);
                }
                v[i] = Some(val);
            }
            None => {
                let mut v = MonoVec::with_capacity(i + 1);
                v.resize(i + 1);
                v[i] = Some(val);
                self.var_table = Some(Box::new(v));
            }
        }
    }

    pub(crate) fn extend_ivar(&mut self, heap_len: usize) {
        match &mut self.var_table {
            Some(v) => {
                v.resize(heap_len);
            }
            None => {
                let mut v = MonoVec::with_capacity(heap_len);
                v.resize(heap_len);
                self.var_table = Some(Box::new(v));
            }
        }
    }

    pub(super) fn change_class(&mut self, new_class_id: ClassId) {
        self.header.change_class(new_class_id);
    }

    pub(super) fn deep_copy(&self) -> Self {
        RValue {
            header: self.header,
            var_table: self.var_table.clone(),
            kind: unsafe {
                match self.ty() {
                    //ObjTy::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                    ObjTy::CLASS | ObjTy::MODULE => {
                        let class = self.as_module();
                        ObjKind::class(class.id(), class.superclass(), class.class_type().clone())
                    }
                    ObjTy::OBJECT => ObjKind::object(),
                    ObjTy::BIGNUM => ObjKind::bignum(self.as_bignum().clone()),
                    ObjTy::FLOAT => ObjKind {
                        float: self.as_float(),
                    },
                    ObjTy::COMPLEX => ObjKind::complex(
                        self.as_complex().re().deep_copy(),
                        self.as_complex().im().deep_copy(),
                    ),
                    ObjTy::STRING => ObjKind::string_from_inner(self.as_rstring().clone()),
                    ObjTy::TIME => ObjKind::time(self.as_time().clone()),
                    ObjTy::ARRAY => ObjKind::array(ArrayInner::from_iter(
                        self.as_array().iter().map(|v| v.deep_copy()),
                    )),
                    ObjTy::RANGE => {
                        let lhs = self.as_range();
                        ObjKind::range(
                            lhs.start().deep_copy(),
                            lhs.end().deep_copy(),
                            lhs.exclude_end(),
                        )
                    }
                    ObjTy::HASH => {
                        let mut map = IndexMap::default();
                        let hash = self.as_hashmap();
                        for (k, v) in hash.iter() {
                            map.insert(HashKey(k.deep_copy()), v.deep_copy());
                        }
                        ObjKind::hash_from(map)
                    }
                    ObjTy::REGEXP => {
                        let regexp = self.as_regex();
                        ObjKind::regexp(regexp.clone())
                    }
                    _ => unreachable!("clone()"),
                }
            },
        }
    }

    pub(super) fn dup(&self) -> Self {
        RValue {
            header: self.header,
            var_table: self.var_table.clone(),
            kind: unsafe {
                if let Some(ty) = self.try_ty() {
                    match ty {
                        ObjTy::CLASS | ObjTy::MODULE => ObjKind {
                            class: self.kind.class.clone(),
                        },
                        ObjTy::OBJECT => ObjKind {
                            object: self.kind.object.clone(),
                        },
                        ObjTy::BIGNUM => ObjKind {
                            bignum: self.kind.bignum.clone(),
                        },
                        ObjTy::FLOAT => ObjKind {
                            float: self.kind.float,
                        },
                        ObjTy::COMPLEX => ObjKind {
                            complex: ManuallyDrop::new(self.kind.complex.dup()),
                        },
                        ObjTy::STRING => ObjKind {
                            string: self.kind.string.clone(),
                        },
                        ObjTy::TIME => ObjKind {
                            time: self.kind.time.clone(),
                        },
                        ObjTy::ARRAY => ObjKind {
                            array: self.kind.array.clone(),
                        },
                        ObjTy::RANGE => ObjKind {
                            range: self.kind.range.clone(),
                        },
                        ObjTy::PROC => ObjKind {
                            proc: self.kind.proc.clone(),
                        },
                        ObjTy::HASH => ObjKind {
                            hash: self.kind.hash.clone(),
                        },
                        ObjTy::REGEXP => ObjKind {
                            regexp: self.kind.regexp.clone(),
                        },
                        ObjTy::IO => ObjKind {
                            io: self.kind.io.clone(),
                        },
                        ObjTy::EXCEPTION => ObjKind {
                            exception: self.kind.exception.clone(),
                        },
                        ObjTy::METHOD => ObjKind {
                            method: self.kind.method.clone(),
                        },
                        ty => unreachable!("{ty:?}"),
                    }
                } else {
                    unreachable!()
                }
            },
        }
    }

    pub(crate) fn class(&self) -> ClassId {
        if self.header.is_live() {
            self.header.class()
        } else {
            unreachable!("Dead object {:?}", self)
        }
    }

    #[allow(dead_code)]
    pub(crate) fn debug_class(&self) -> Option<ClassId> {
        if self.header.is_live() {
            Some(self.header.class())
        } else {
            None
        }
    }
}

//
// constructors.
//
impl RValue {
    pub(super) fn new_bigint(bigint: BigInt) -> Self {
        RValue {
            header: Header::new(INTEGER_CLASS, ObjTy::BIGNUM),
            kind: ObjKind::bignum(bigint),
            var_table: None,
        }
    }

    pub(super) fn new_float(f: f64) -> Self {
        RValue {
            header: Header::new(FLOAT_CLASS, ObjTy::FLOAT),
            kind: ObjKind::float(f),
            var_table: None,
        }
    }

    pub(super) fn new_complex(re: Real, im: Real) -> Self {
        RValue {
            header: Header::new(COMPLEX_CLASS, ObjTy::COMPLEX),
            kind: ObjKind::complex(re, im),
            var_table: None,
        }
    }

    pub(super) fn new_complex_from(complex: num::Complex<Real>) -> Self {
        RValue {
            header: Header::new(COMPLEX_CLASS, ObjTy::COMPLEX),
            kind: ObjKind::complex_from(complex),
            var_table: None,
        }
    }

    ///
    /// Create new class object with *class_id*.
    ///
    pub(super) fn new_class(
        id: ClassId,
        superclass: Option<Module>,
        class_type: ModuleType,
    ) -> Self {
        RValue {
            header: Header::new(CLASS_CLASS, ObjTy::CLASS),
            kind: ObjKind::class(id, superclass, class_type),
            var_table: None,
        }
    }

    pub(super) fn new_singleton_class(
        id: ClassId,
        superclass: Option<Module>,
        singleton: Value,
    ) -> Self {
        RValue {
            header: Header::new(CLASS_CLASS, ObjTy::CLASS),
            kind: ObjKind::singleton_class(id, superclass, singleton),
            var_table: None,
        }
    }

    ///
    /// Create new module object with *class_id*.
    ///
    pub(super) fn new_module(id: ClassId, superclass: Option<Module>) -> Self {
        RValue {
            header: Header::new(MODULE_CLASS, ObjTy::MODULE),
            kind: ObjKind::class(id, superclass, ModuleType::RealClass),
            var_table: None,
        }
    }

    ///
    /// Create new iclass object with *class_id*.
    ///
    pub(super) fn new_iclass(id: ClassId, superclass: Option<Module>) -> Self {
        RValue {
            header: Header::new(MODULE_CLASS, ObjTy::MODULE),
            kind: ObjKind::class(id, superclass, ModuleType::IClass),
            var_table: None,
        }
    }

    ///
    /// Create new instance object of class *class_id*.
    ///
    pub(super) fn new_object(class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjTy::OBJECT),
            kind: ObjKind::object(),
            var_table: None,
        }
    }

    pub(super) fn new_string(s: String) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::string_from_string(s),
            var_table: None,
        }
    }

    pub(super) fn new_string_from_inner(inner: RStringInner) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::string_from_inner(inner),
            var_table: None,
        }
    }

    pub(super) fn new_string_from_str(s: &str) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::string_from_str(s),
            var_table: None,
        }
    }

    pub(super) fn new_string_from_vec(v: Vec<u8>) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::string_from_vec(v),
            var_table: None,
        }
    }

    pub(super) fn new_bytes(v: Vec<u8>) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::bytes_from_vec(v),
            var_table: None,
        }
    }

    pub(super) fn new_bytes_from_slice(slice: &[u8]) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjTy::STRING),
            kind: ObjKind::bytes_from_slice(slice),
            var_table: None,
        }
    }

    pub(super) fn new_array(ary: ArrayInner) -> Self {
        RValue {
            header: Header::new(ARRAY_CLASS, ObjTy::ARRAY),
            kind: ObjKind::array(ary),
            var_table: None,
        }
    }

    pub(super) fn new_array_with_class(ary: ArrayInner, class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjTy::ARRAY),
            kind: ObjKind::array(ary),
            var_table: None,
        }
    }

    pub(super) fn new_array_from_vec_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjTy::ARRAY),
            kind: ObjKind::array(ArrayInner::from_vec(v)),
            var_table: None,
        }
    }

    pub(super) fn new_hash(map: IndexMap<HashKey, Value>) -> Self {
        RValue {
            header: Header::new(HASH_CLASS, ObjTy::HASH),
            kind: ObjKind::hash_from(map),
            var_table: None,
        }
    }

    pub(super) fn new_hash_from_inner(inner: HashmapInner) -> Self {
        RValue {
            header: Header::new(HASH_CLASS, ObjTy::HASH),
            kind: ObjKind::hash_from_inner(inner),
            var_table: None,
        }
    }

    pub(super) fn new_hash_with_class_and_default(class_id: ClassId, default: Value) -> Self {
        RValue {
            header: Header::new(class_id, ObjTy::HASH),
            kind: ObjKind::hash_with_default(default),
            var_table: None,
        }
    }

    pub(super) fn new_hash_with_class_and_default_proc(
        class_id: ClassId,
        default_proc: Proc,
    ) -> Self {
        RValue {
            header: Header::new(class_id, ObjTy::HASH),
            kind: ObjKind::hash_with_default_proc(default_proc),
            var_table: None,
        }
    }

    pub(super) fn new_regexp(regexp: RegexpInner) -> Self {
        RValue {
            header: Header::new(REGEXP_CLASS, ObjTy::REGEXP),
            kind: ObjKind::regexp(regexp),
            var_table: None,
        }
    }

    pub(super) fn new_exception(err: MonorubyErr) -> Self {
        let class_id = err.class_id();
        RValue {
            header: Header::new(class_id, ObjTy::EXCEPTION),
            kind: ObjKind::exception(err),
            var_table: None,
        }
    }

    pub(super) fn new_io(io: IoInner) -> Self {
        RValue {
            header: Header::new(IO_CLASS, ObjTy::IO),
            kind: ObjKind::io(io),
            var_table: None,
        }
    }

    pub(super) fn new_io_stdin() -> Self {
        Self::new_io(IoInner::stdin())
    }

    pub(super) fn new_io_stdout() -> Self {
        Self::new_io(IoInner::stdout())
    }

    pub(super) fn new_io_stderr() -> Self {
        Self::new_io(IoInner::stderr())
    }

    pub(super) fn new_file(file: std::fs::File, name: String) -> Self {
        let inner = IoInner::file(file, name);
        RValue {
            header: Header::new(FILE_CLASS, ObjTy::IO),
            kind: ObjKind::io(inner),
            var_table: None,
        }
    }

    pub(super) fn new_time(time: TimeInner) -> Self {
        RValue {
            header: Header::new(TIME_CLASS, ObjTy::TIME),
            kind: ObjKind::time(time),
            var_table: None,
        }
    }

    pub(super) fn new_range(start: Value, end: Value, exclude_end: bool) -> Self {
        RValue {
            header: Header::new(RANGE_CLASS, ObjTy::RANGE),
            kind: ObjKind::range(start, end, exclude_end),
            var_table: None,
        }
    }

    pub(super) fn new_proc(block_data: ProcInner) -> Self {
        RValue {
            header: Header::new(PROC_CLASS, ObjTy::PROC),
            kind: ObjKind::proc(block_data),
            var_table: None,
        }
    }

    pub(super) fn new_method(receiver: Value, func_id: FuncId, owner: ClassId) -> Self {
        RValue {
            header: Header::new(METHOD_CLASS, ObjTy::METHOD),
            kind: ObjKind::method(receiver, func_id, owner),
            var_table: None,
        }
    }

    pub(super) fn new_unbound_method(func_id: FuncId, owner: ClassId) -> Self {
        RValue {
            header: Header::new(UMETHOD_CLASS, ObjTy::UMETHOD),
            kind: ObjKind::unbound_method(func_id, owner),
            var_table: None,
        }
    }

    pub(super) fn new_fiber(proc: Proc) -> Self {
        RValue {
            header: Header::new(FIBER_CLASS, ObjTy::FIBER),
            kind: ObjKind::fiber(proc),
            var_table: None,
        }
    }

    pub(super) fn new_enumerator(
        obj: Value,
        method: IdentId,
        proc: Proc,
        args: Vec<Value>,
    ) -> Self {
        RValue {
            header: Header::new(ENUMERATOR_CLASS, ObjTy::ENUMERATOR),
            kind: ObjKind::enumerator(obj, method, proc, args),
            var_table: None,
        }
    }

    pub(super) fn new_generator(proc: Proc) -> Self {
        RValue {
            header: Header::new(GENERATOR_CLASS, ObjTy::GENERATOR),
            kind: ObjKind::generator(proc),
            var_table: None,
        }
    }

    pub(super) fn new_binding(outer_lfp: Lfp) -> Self {
        let outer_lfp = outer_lfp.move_frame_to_heap();
        assert!(!outer_lfp.on_stack());
        RValue {
            header: Header::new(BINDING_CLASS, ObjTy::BINDING),
            kind: ObjKind::binding_from_outer(outer_lfp),
            var_table: None,
        }
    }

    pub(super) fn new_match_data(captures: Captures, heystack: &str, regex: Regexp) -> Self {
        RValue {
            header: Header::new(MATCHDATA_CLASS, ObjTy::MATCHDATA),
            kind: ObjKind::matchdata(captures, heystack.to_string(), regex),
            var_table: None,
        }
    }
}

impl RValue {
    pub fn unpack(&self) -> RV {
        unsafe {
            if let Some(ty) = self.try_ty() {
                match ty {
                    ObjTy::BIGNUM => RV::BigInt(self.as_bignum()),
                    ObjTy::FLOAT => RV::Float(self.as_float()),
                    ObjTy::STRING => RV::String(self.as_rstring()),
                    ObjTy::COMPLEX => RV::Complex(self.as_complex()),
                    _ => RV::Object(self),
                }
            } else {
                RV::Invalid
            }
        }
    }
}

impl RValue {
    /// This function is only used for system assertion.
    pub(crate) fn eq(lhs: &Self, rhs: &Self) -> bool {
        unsafe {
            match (lhs.ty(), rhs.ty()) {
                (ObjTy::BIGNUM, ObjTy::BIGNUM) => lhs.as_bignum() == rhs.as_bignum(),
                (ObjTy::FLOAT, ObjTy::FLOAT) => lhs.as_float() == rhs.as_float(),
                (ObjTy::COMPLEX, ObjTy::COMPLEX) => {
                    lhs.as_complex().re() == rhs.as_complex().re()
                        && lhs.as_complex().im() == rhs.as_complex().im()
                }
                (ObjTy::STRING, ObjTy::STRING) => lhs.as_rstring() == rhs.as_rstring(),
                (ObjTy::ARRAY, ObjTy::ARRAY) => {
                    let lhs = lhs.as_array();
                    let rhs = rhs.as_array();
                    if lhs.len() != rhs.len() {
                        return false;
                    }
                    lhs.iter().zip(rhs.iter()).for_each(|(lhs, rhs)| {
                        Value::assert_eq(*lhs, *rhs);
                    });
                    true
                }
                (ObjTy::RANGE, ObjTy::RANGE) => lhs.as_range() == rhs.as_range(),
                (ObjTy::HASH, ObjTy::HASH) => {
                    let lhs = lhs.as_hashmap();
                    let rhs = rhs.as_hashmap();
                    if lhs.len() != rhs.len() {
                        return false;
                    }
                    lhs.iter().zip(rhs.iter()).for_each(|(lhs, rhs)| {
                        Value::assert_eq(lhs.0, rhs.0);
                        Value::assert_eq(lhs.1, rhs.1);
                    });
                    true
                }
                _ => false,
            }
        }
    }
}

impl RValue {
    unsafe fn as_object(&self) -> &[Option<value::Value>; OBJECT_INLINE_IVAR] {
        &self.kind.object
    }

    unsafe fn as_object_mut(&mut self) -> &mut [Option<value::Value>; OBJECT_INLINE_IVAR] {
        &mut self.kind.object
    }

    pub(super) unsafe fn as_module(&self) -> &ModuleInner {
        &self.kind.class
    }

    pub(super) unsafe fn as_module_mut(&mut self) -> &mut ModuleInner {
        &mut self.kind.class
    }

    unsafe fn as_class_id(&self) -> ClassId {
        self.as_module().id()
    }

    pub(super) unsafe fn as_float(&self) -> f64 {
        self.kind.float
    }

    unsafe fn as_bignum(&self) -> &BigInt {
        &self.kind.bignum
    }

    pub(super) unsafe fn as_complex(&self) -> &ComplexInner {
        &self.kind.complex
    }

    pub(super) unsafe fn as_rstring(&self) -> &RStringInner {
        &self.kind.string
    }

    pub(super) unsafe fn as_rstring_mut(&mut self) -> &mut RStringInner {
        &mut self.kind.string
    }

    pub(super) unsafe fn as_str(&self) -> &str {
        self.kind.string.check_utf8().unwrap()
    }

    /*pub(crate) fn as_string_mut(&mut self) -> &mut InnerVec {
        unsafe { &mut *self.kind.bytes }
    }*/

    pub(crate) unsafe fn as_array(&self) -> &ArrayInner {
        &self.kind.array
    }

    pub(super) unsafe fn as_array_mut(&mut self) -> &mut ArrayInner {
        &mut self.kind.array
    }

    pub(super) unsafe fn as_range(&self) -> &RangeInner {
        &self.kind.range
    }

    pub(super) unsafe fn as_exception(&self) -> &ExceptionInner {
        &self.kind.exception
    }

    pub(super) unsafe fn as_exception_mut(&mut self) -> &mut ExceptionInner {
        &mut self.kind.exception
    }

    pub(crate) unsafe fn as_hashmap(&self) -> &HashmapInner {
        &self.kind.hash
    }

    pub(super) unsafe fn as_hashmap_mut(&mut self) -> &mut HashmapInner {
        &mut self.kind.hash
    }

    pub(super) unsafe fn as_regex(&self) -> &RegexpInner {
        &self.kind.regexp
    }

    pub(super) unsafe fn as_regex_mut(&mut self) -> &mut RegexpInner {
        &mut self.kind.regexp
    }

    pub(super) unsafe fn as_io(&self) -> &IoInner {
        &self.kind.io
    }

    pub(super) unsafe fn as_io_mut(&mut self) -> &mut IoInner {
        &mut self.kind.io
    }

    pub(super) unsafe fn as_proc(&self) -> &ProcInner {
        &self.kind.proc
    }

    pub(super) unsafe fn as_proc_mut(&mut self) -> &mut ProcInner {
        &mut self.kind.proc
    }

    pub(super) fn as_time(&self) -> &TimeInner {
        unsafe { &self.kind.time }
    }

    pub(crate) fn as_time_mut(&mut self) -> &mut TimeInner {
        unsafe { &mut self.kind.time }
    }

    pub(crate) fn as_method(&self) -> &MethodInner {
        assert_eq!(self.ty(), ObjTy::METHOD);
        unsafe { &self.kind.method }
    }

    pub(crate) fn as_umethod(&self) -> &UMethodInner {
        assert_eq!(self.ty(), ObjTy::UMETHOD);
        unsafe { &self.kind.umethod }
    }

    pub(super) unsafe fn as_fiber(&self) -> &FiberInner {
        assert_eq!(self.ty(), ObjTy::FIBER);
        &self.kind.fiber
    }

    pub(super) unsafe fn as_fiber_mut(&mut self) -> &mut FiberInner {
        assert_eq!(self.ty(), ObjTy::FIBER);
        &mut self.kind.fiber
    }

    pub(super) unsafe fn as_enumerator(&self) -> &EnumeratorInner {
        &self.kind.enumerator
    }

    pub(super) unsafe fn as_enumerator_mut(&mut self) -> &mut EnumeratorInner {
        &mut self.kind.enumerator
    }

    pub(super) unsafe fn as_generator(&self) -> &GeneratorInner {
        &self.kind.generator
    }

    pub(super) unsafe fn as_generator_mut(&mut self) -> &mut GeneratorInner {
        &mut self.kind.generator
    }

    pub(super) unsafe fn as_binding(&self) -> &BindingInner {
        &self.kind.binding
    }

    pub(super) unsafe fn as_binding_mut(&mut self) -> &mut BindingInner {
        &mut self.kind.binding
    }

    pub(super) unsafe fn as_match_data(&self) -> &MatchDataInner {
        &self.kind.matchdata
    }
}

impl RValue {
    /// Pack `self` into `Value`(64-bit data representation).
    ///
    /// This method consumes `self` and allocates it on the heap, returning `Value`,
    /// a wrapped raw pointer.
    pub(crate) fn pack(self) -> Value {
        let ptr = alloc::ALLOC.with(|alloc| alloc.borrow_mut().alloc(self));
        Value::from_ptr(ptr)
    }
}

#[derive(Clone, Copy)]
#[repr(C)]
union Header {
    meta: Metadata,
    next: Option<std::ptr::NonNull<RValue>>,
}

#[derive(Debug, Clone, Copy)]
#[repr(C)]
struct Metadata {
    flag: u16,
    ty: Option<ObjTy>,
    _padding: u8,
    class: Option<ClassId>,
}

impl Header {
    fn new(class: ClassId, ty: ObjTy) -> Self {
        Header {
            meta: Metadata {
                flag: 1,
                ty: Some(ty),
                _padding: 0,
                class: Some(class),
            },
        }
    }

    fn is_live(&self) -> bool {
        unsafe { self.meta.flag & 0b1 == 1 && self.meta.ty.is_some() }
    }

    fn class(&self) -> ClassId {
        assert!(self.is_live(), "dead RVALUE. {:?}", unsafe { self.meta });
        unsafe { self.meta.class.unwrap() }
    }

    fn ty(&self) -> ObjTy {
        unsafe { self.meta.ty.unwrap() }
    }

    fn change_class(&mut self, class: ClassId) {
        self.meta.class = Some(class);
    }
}
