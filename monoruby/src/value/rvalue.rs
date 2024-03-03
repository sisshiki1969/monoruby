use super::*;
use num::BigInt;
use ruruby_parse::Loc;
use ruruby_parse::SourceInfoRef;
use std::mem::ManuallyDrop;

pub use array::*;
pub use complex::ComplexInner;
pub use complex::Real;
pub use enumerator::*;
pub use fiber::*;
pub use hash::*;
pub use io::IoInner;
pub use ivar_table::*;
pub use method::MethodInner;
pub use module::*;
pub use regexp::RegexpInner;
pub use string::StringInner;

mod array;
mod complex;
mod enumerator;
mod fiber;
mod hash;
mod io;
mod ivar_table;
mod method;
mod module;
mod regexp;
mod string;

pub const OBJECT_INLINE_IVAR: usize = 6;
pub const RVALUE_OFFSET_TY: usize = 2;
pub const RVALUE_OFFSET_VAR: usize = std::mem::offset_of!(RValue, var_table);
pub const RVALUE_OFFSET_KIND: usize = std::mem::offset_of!(RValue, kind);
pub const RVALUE_OFFSET_ARY_CAPA: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_CAPA;
pub const RVALUE_OFFSET_INLINE: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_INLINE;
pub const RVALUE_OFFSET_HEAP_PTR: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_HEAP_PTR;
pub const RVALUE_OFFSET_HEAP_LEN: usize = RVALUE_OFFSET_KIND + smallvec::OFFSET_HEAP_LEN;

pub const PROCINNER_OUTER: i64 = std::mem::offset_of!(ProcInner, outer_lfp) as _;
pub const PROCINNER_FUNCID: i64 = std::mem::offset_of!(ProcInner, func_id) as _;

#[repr(C)]
pub union ObjKind {
    invalid: (),
    class: ManuallyDrop<ModuleInner>,
    object: [Option<Value>; OBJECT_INLINE_IVAR],
    bignum: ManuallyDrop<BigInt>,
    float: f64,
    complex: ManuallyDrop<ComplexInner>,
    string: ManuallyDrop<StringInner>,
    time: ManuallyDrop<TimeInner>,
    array: ManuallyDrop<ArrayInner>,
    range: ManuallyDrop<RangeInner>,
    exception: ManuallyDrop<Box<ExceptionInner>>,
    proc: ManuallyDrop<ProcInner>,
    hash: ManuallyDrop<HashInner>,
    regexp: ManuallyDrop<RegexpInner>,
    io: ManuallyDrop<IoInner>,
    method: ManuallyDrop<MethodInner>,
    fiber: ManuallyDrop<FiberInner>,
    enumerator: ManuallyDrop<EnumeratorInner>,
    generator: ManuallyDrop<GeneratorInner>,
}

#[allow(dead_code)]
impl ObjKind {
    pub const INVALID: u8 = 0;
    pub const CLASS: u8 = 1;
    pub const MODULE: u8 = 2;
    pub const OBJECT: u8 = 3;
    pub const BIGNUM: u8 = 4;
    pub const FLOAT: u8 = 5;
    pub const BYTES: u8 = 6;
    pub const TIME: u8 = 7;
    pub const ARRAY: u8 = 8;
    pub const RANGE: u8 = 9;
    pub const EXCEPTION: u8 = 10;
    pub const PROC: u8 = 11;
    pub const HASH: u8 = 12;
    pub const REGEXP: u8 = 13;
    pub const IO: u8 = 14;
    pub const METHOD: u8 = 15;
    pub const FIBER: u8 = 16;
    pub const ENUMERATOR: u8 = 17;
    pub const GENERATOR: u8 = 18;
    pub const COMPLEX: u8 = 19;
}

impl ObjKind {
    fn invalid() -> Self {
        Self { invalid: () }
    }

    fn class(class: ClassId, superclass: Option<Module>, class_type: ModuleType) -> Self {
        Self {
            class: ManuallyDrop::new(ModuleInner::new(class, superclass, class_type)),
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

    fn complex(re: complex::Real, im: complex::Real) -> Self {
        Self {
            complex: ManuallyDrop::new(ComplexInner::new(re, im)),
        }
    }

    fn complex_from(complex: num::complex::Complex<Real>) -> Self {
        Self {
            complex: ManuallyDrop::new(ComplexInner::from(complex)),
        }
    }

    fn bytes(s: StringInner) -> Self {
        Self {
            string: ManuallyDrop::new(s),
        }
    }

    fn bytes_from_slice(slice: &[u8]) -> Self {
        Self {
            string: ManuallyDrop::new(StringInner::from_slice(slice)),
        }
    }

    fn bytes_from_vec(vec: Vec<u8>) -> Self {
        Self {
            string: ManuallyDrop::new(StringInner::from_vec(vec)),
        }
    }

    fn array(ary: ArrayInner) -> Self {
        Self {
            array: ManuallyDrop::new(ary),
        }
    }

    fn range(start: Value, end: Value, exclude_end: bool) -> Self {
        Self {
            range: ManuallyDrop::new(RangeInner {
                start,
                end,
                exclude_end: u32::from(exclude_end),
            }),
        }
    }

    fn exception(kind: IdentId, msg: String, trace: Vec<(Loc, SourceInfoRef)>) -> Self {
        Self {
            exception: ManuallyDrop::new(Box::new(ExceptionInner {
                class_name: kind,
                msg,
                trace,
            })),
        }
    }

    fn exception_from(mut err: MonorubyErr, globals: &Globals) -> Self {
        let kind = IdentId::get_id(err.get_class_name());
        let msg = err.show(globals);
        Self {
            exception: ManuallyDrop::new(Box::new(ExceptionInner {
                class_name: kind,
                msg,
                trace: err.take_trace(),
            })),
        }
    }

    fn hash(map: IndexMap<HashKey, Value>) -> Self {
        Self {
            hash: ManuallyDrop::new(HashInner::new(map)),
        }
    }

    fn hash_from_inner(inner: HashInner) -> Self {
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

    fn method(receiver: Value, func_id: FuncId) -> Self {
        Self {
            method: ManuallyDrop::new(MethodInner::new(receiver, func_id)),
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
            write!(f, "{:016x} DEAD: next:{:?}", self.id(), unsafe {
                self.header.next
            })
        } else {
            write!(
                f,
                "{:016x} RValue {{ class:{:?} {} }}",
                self.id(),
                meta.class,
                unsafe {
                    match meta.ty as u16 {
                        0 => "<INVALID>".to_string(),
                        1 => format!("CLASS({:?})", self.kind.class),
                        2 => format!("MODULE({:?})", self.kind.class),
                        3 => format!("OBJECT({:?})", self.kind.object),
                        4 => format!("BIGNUM({:?})", self.kind.bignum),
                        5 => format!("FLOAT({:?})", self.kind.float),
                        6 => format!("STRING({:?})", self.kind.string.as_str()),
                        7 => format!("TIME({:?})", self.kind.time),
                        8 => format!("ARRAY({:?})", self.kind.array),
                        9 => format!("RANGE({:?})", self.kind.range),
                        10 => format!("EXCEPTION({:?})", self.kind.exception),
                        11 => format!("PROC({:?})", self.kind.proc),
                        12 => format!("HASH({:?})", self.kind.hash),
                        13 => format!("REGEXP({:?})", self.kind.regexp),
                        14 => format!("IO({:?})", self.kind.io),
                        15 => format!("METHOD({:?})", self.kind.method),
                        16 => format!("FIBER({:?})", self.kind.fiber),
                        17 => format!("ENUMERATOR({:?})", self.kind.enumerator),
                        18 => format!("GENERATOR({:?})", self.kind.generator),
                        19 => format!("COMPLEX({:?})", self.kind.complex),
                        _ => unreachable!(),
                    }
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
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
                ObjKind::BIGNUM => self.as_bignum().hash(state),
                ObjKind::FLOAT => self.as_float().to_bits().hash(state),
                ObjKind::BYTES => self.as_bytes().hash(state),
                ObjKind::ARRAY => self.as_array().hash(state),
                ObjKind::RANGE => self.as_range().hash(state),
                ObjKind::HASH => self.as_hash().hash(state),
                ObjKind::COMPLEX => self.as_complex().hash(state),
                _ => self.hash(state),
            }
        }
    }
}

impl RValue {
    pub(crate) fn to_s(&self, globals: &Globals) -> String {
        unsafe {
            match self.ty() {
                ObjKind::CLASS | ObjKind::MODULE => globals.get_class_name(self.as_class_id()),
                ObjKind::TIME => self.as_time().to_string(),
                ObjKind::ARRAY => self.as_array().to_s(globals),
                ObjKind::OBJECT => self.object_tos(globals),
                ObjKind::RANGE => self.range_tos(globals),
                ObjKind::PROC => self.proc_tos(),
                ObjKind::HASH => self.hash_tos(globals),
                ObjKind::REGEXP => self.regexp_tos(),
                ObjKind::IO => self.as_io().to_string(),
                ObjKind::EXCEPTION => self.as_exception().msg().to_string(),
                ObjKind::METHOD => self.as_method().to_s(globals),
                ObjKind::FIBER => self.fiber_tos(globals),
                ObjKind::ENUMERATOR => self.enumerator_tos(globals),
                ObjKind::GENERATOR => self.object_tos(globals),
                ObjKind::COMPLEX => {
                    let re = self.as_complex().re();
                    let im = self.as_complex().im();
                    format!("{}+{}i", globals.to_s(re.get()), globals.to_s(im.get()))
                }
                _ => format!("{:016x}", self.id()),
            }
        }
    }

    pub(crate) fn inspect(&self, globals: &Globals) -> String {
        unsafe {
            match self.ty() {
                ObjKind::OBJECT => self.object_inspect(globals),
                ObjKind::EXCEPTION => {
                    let class_name = globals.get_class_name(self.class());
                    let msg = self.as_exception().msg();
                    format!("#<{class_name}: {msg}>")
                }
                ObjKind::GENERATOR => self.object_tos(globals),
                ObjKind::COMPLEX => {
                    let re = self.as_complex().re();
                    let im = self.as_complex().im();
                    format!("({}+{}i)", globals.to_s(re.get()), globals.to_s(im.get()))
                }
                _ => self.to_s(globals),
            }
        }
    }

    pub(crate) fn inspect2(&self, globals: &Globals) -> String {
        match self.ty() {
            ObjKind::ARRAY => self.as_array().inspect2(globals),
            ObjKind::HASH => self.hash_inspect2(globals),
            _ => self.inspect(globals),
        }
    }

    fn object_tos(&self, globals: &Globals) -> String {
        if let Some(name) = self.get_ivar(globals, IdentId::_NAME) {
            globals.to_s(name)
        } else {
            format!(
                "#<{}:0x{:016x}>",
                globals.get_class_name(self.real_class(globals).id()),
                self.id()
            )
        }
    }

    fn object_inspect(&self, globals: &Globals) -> String {
        if let Some(name) = self.get_ivar(globals, IdentId::_NAME) {
            globals.to_s(name)
        } else {
            let mut s = String::new();
            for (id, v) in self.get_ivars(globals).into_iter() {
                s += &format!(" {id}={}", globals.inspect(v));
            }
            format!(
                "#<{}:0x{:016x}{s}>",
                globals.get_class_name(self.class()),
                self.id()
            )
        }
    }

    fn fiber_tos(&self, globals: &Globals) -> String {
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
            globals[func_id].as_ruby_func().get_location(),
        )
    }

    fn enumerator_tos(&self, globals: &Globals) -> String {
        let e = unsafe { self.as_enumerator() };
        format!("#<Enumerator: {} {}>", globals.to_s(e.obj), e.method)
    }

    fn proc_tos(&self) -> String {
        format!("#<Proc:0x{:016x}>", self.id())
    }

    fn regexp_tos(&self) -> String {
        format!("/{}/", self.as_regex().as_str())
    }

    fn range_tos(&self, globals: &Globals) -> String {
        let range = self.as_range();
        format!(
            "{}{}{}",
            globals.inspect(range.start),
            if range.exclude_end() { "..." } else { ".." },
            globals.inspect(range.end),
        )
    }

    fn hash_tos(&self, globals: &Globals) -> String {
        let hash = self.as_hash();
        match hash.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in hash.iter() {
                    let k_inspect = if k.id() == self.id() {
                        "{...}".to_string()
                    } else {
                        globals.inspect(k)
                    };
                    let v_inspect = if v.id() == self.id() {
                        "{...}".to_string()
                    } else {
                        globals.inspect(v)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                format! {"{{{}}}", result}
            }
        }
    }

    fn hash_inspect2(&self, globals: &Globals) -> String {
        let hash = self.as_hash();
        match hash.len() {
            0 => "{}".to_string(),
            _ => {
                let mut result = "".to_string();
                let mut first = true;
                for (k, v) in hash.iter().take(3) {
                    let k_inspect = if k.id() == self.id() {
                        "{...}".to_string()
                    } else {
                        globals.inspect2(k)
                    };
                    let v_inspect = if v.id() == self.id() {
                        "{...}".to_string()
                    } else {
                        globals.inspect2(v)
                    };
                    result = if first {
                        format!("{k_inspect}=>{v_inspect}")
                    } else {
                        format!("{result}, {k_inspect}=>{v_inspect}")
                    };
                    first = false;
                }
                format! {"{{{} .. }}", result}
            }
        }
    }
}

impl RValue {
    // This type of equality is used for comparison for keys of Hash.
    pub(crate) fn eql(&self, other: &Self) -> bool {
        unsafe {
            match (self.ty(), other.ty()) {
                (ObjKind::OBJECT, ObjKind::OBJECT) => self.id() == other.id(),
                (ObjKind::BIGNUM, ObjKind::BIGNUM) => self.as_bignum() == other.as_bignum(),
                (ObjKind::FLOAT, ObjKind::FLOAT) => self.as_float() == other.as_float(),
                (ObjKind::COMPLEX, ObjKind::COMPLEX) => self.as_complex().eql(&other.as_complex()),
                (ObjKind::BYTES, ObjKind::BYTES) => self.as_bytes() == other.as_bytes(),
                (ObjKind::ARRAY, ObjKind::ARRAY) => {
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
                (ObjKind::RANGE, ObjKind::RANGE) => self.as_range().eql(other.as_range()),
                (ObjKind::HASH, ObjKind::HASH) => self.as_hash() == other.as_hash(),
                //(ObjKind::METHOD, ObjKind::METHOD) => *self.method() == *other.method(),
                //(ObjKind::UNBOUND_METHOD, ObjKind::UNBOUND_METHOD) => *self.method() == *other.method(),
                (ObjKind::INVALID, _) => panic!("Invalid rvalue. (maybe GC problem) {:?}", self),
                (_, ObjKind::INVALID) => panic!("Invalid rvalue. (maybe GC problem) {:?}", other),
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
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                ObjKind::CLASS | ObjKind::MODULE => {
                    let module = self.as_module();
                    if let Some(class) = module.superclass_value() {
                        class.mark(alloc)
                    }
                    if let Some(val) = module.is_singleton() {
                        val.mark(alloc)
                    }
                }
                ObjKind::OBJECT => {
                    self.as_object().iter().for_each(|v| {
                        v.map(|v| {
                            v.mark(alloc);
                        });
                    });
                }
                ObjKind::BIGNUM => {}
                ObjKind::FLOAT => {}
                ObjKind::COMPLEX => {
                    self.as_complex().re().get().mark(alloc);
                    self.as_complex().im().get().mark(alloc);
                }
                ObjKind::BYTES => {}
                ObjKind::TIME => {}
                ObjKind::ARRAY => {
                    self.as_array().iter().for_each(|v| v.mark(alloc));
                }
                ObjKind::RANGE => {
                    let range = self.as_range();
                    range.start.mark(alloc);
                    range.end.mark(alloc);
                }
                ObjKind::PROC => {}
                ObjKind::HASH => {
                    for (k, v) in self.as_hash().iter() {
                        k.mark(alloc);
                        v.mark(alloc);
                    }
                }
                ObjKind::REGEXP => {}
                ObjKind::IO => {}
                ObjKind::EXCEPTION => {}
                ObjKind::METHOD => self.as_method().receiver().mark(alloc),
                ObjKind::FIBER => self.as_fiber().mark(alloc),
                ObjKind::ENUMERATOR => self.as_enumerator().mark(alloc),
                ObjKind::GENERATOR => self.as_generator().mark(alloc),
                _ => unreachable!("mark {:016x} {}", self.id(), self.ty()),
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
                ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                ObjKind::MODULE | ObjKind::CLASS => ManuallyDrop::drop(&mut self.kind.class),
                ObjKind::BIGNUM => ManuallyDrop::drop(&mut self.kind.bignum),
                ObjKind::BYTES => ManuallyDrop::drop(&mut self.kind.string),
                ObjKind::TIME => ManuallyDrop::drop(&mut self.kind.time),
                ObjKind::ARRAY => ManuallyDrop::drop(&mut self.kind.array),
                ObjKind::EXCEPTION => ManuallyDrop::drop(&mut self.kind.exception),
                ObjKind::HASH => ManuallyDrop::drop(&mut self.kind.hash),
                ObjKind::REGEXP => ManuallyDrop::drop(&mut self.kind.regexp),
                ObjKind::FIBER => ManuallyDrop::drop(&mut self.kind.fiber),
                ObjKind::ENUMERATOR => ManuallyDrop::drop(&mut self.kind.enumerator),
                ObjKind::GENERATOR => ManuallyDrop::drop(&mut self.kind.generator),
                _ => {}
            }
            self.set_next_none();
            self.var_table = None;
        }
    }

    fn next(&self) -> Option<std::ptr::NonNull<RValue>> {
        let next = unsafe { self.header.next };
        assert!(unsafe { std::mem::transmute::<_, u64>(next) } & 0b1 != 1);
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

    pub(crate) fn ty(&self) -> u8 {
        self.header.ty()
    }

    ///
    /// Get class object of *self.
    ///
    pub(crate) fn get_class_obj(&self, globals: &Globals) -> Module {
        self.class().get_module(globals)
    }

    pub(crate) fn real_class(&self, globals: &Globals) -> Module {
        self.get_class_obj(globals).get_real_class()
    }

    pub(crate) fn get_ivar(&self, globals: &Globals, name: IdentId) -> Option<Value> {
        let class_id = self.class();
        let id = globals.store[class_id].get_ivarid(name)?;
        self.get_var(id)
    }

    pub(crate) fn get_ivars(&self, globals: &Globals) -> Vec<(IdentId, Value)> {
        let class_id = self.class();
        globals.store[class_id]
            .ivar_names()
            .filter_map(|(name, id)| self.get_var(*id).map(|v| (*name, v)))
            .collect()
    }

    pub(crate) fn get_var(&self, id: IvarId) -> Option<Value> {
        let mut i = id.into_usize();
        if self.ty() == ObjKind::OBJECT {
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

    /*pub(crate) extern "C" fn get_ivar(base: &mut RValue, id: IvarId) -> Value {
        base.get_var(id).unwrap_or_default()
    }*/

    pub(crate) fn set_var(&mut self, id: IvarId, val: Value) {
        let mut i = id.into_usize();
        if self.ty() == ObjKind::OBJECT {
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

    pub(crate) extern "C" fn set_ivar(base: &mut RValue, id: IvarId, val: Value) {
        base.set_var(id, val)
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
                    ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                    ObjKind::CLASS | ObjKind::MODULE => {
                        let class = self.as_module();
                        ObjKind::class(class.id(), class.superclass(), class.class_type())
                    }
                    ObjKind::OBJECT => ObjKind::object(),
                    ObjKind::BIGNUM => ObjKind::bignum(self.as_bignum().clone()),
                    ObjKind::FLOAT => ObjKind {
                        float: self.as_float(),
                    },
                    ObjKind::COMPLEX => ObjKind::complex(
                        self.as_complex().re().deep_copy(),
                        self.as_complex().im().deep_copy(),
                    ),
                    ObjKind::BYTES => ObjKind::bytes_from_slice(self.as_bytes()),
                    ObjKind::TIME => ObjKind::time(self.as_time().clone()),
                    ObjKind::ARRAY => ObjKind::array(ArrayInner::from_iter(
                        self.as_array().iter().map(|v| v.deep_copy()),
                    )),
                    ObjKind::RANGE => {
                        let lhs = self.as_range();
                        ObjKind::range(
                            lhs.start.deep_copy(),
                            lhs.end.deep_copy(),
                            lhs.exclude_end != 0,
                        )
                    }
                    ObjKind::HASH => {
                        let mut map = IndexMap::default();
                        let hash = self.as_hash();
                        for (k, v) in hash.iter() {
                            map.insert(HashKey(k.deep_copy()), v.deep_copy());
                        }
                        ObjKind::hash(map)
                    }
                    ObjKind::REGEXP => {
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
                match self.ty() {
                    ObjKind::INVALID => panic!("Invalid rvalue. (maybe GC problem) {:?}", &self),
                    ObjKind::CLASS | ObjKind::MODULE => ObjKind {
                        class: self.kind.class.clone(),
                    },
                    ObjKind::OBJECT => ObjKind {
                        object: self.kind.object,
                    },
                    ObjKind::BIGNUM => ObjKind {
                        bignum: self.kind.bignum.clone(),
                    },
                    ObjKind::FLOAT => ObjKind {
                        float: self.kind.float,
                    },
                    ObjKind::COMPLEX => ObjKind {
                        complex: ManuallyDrop::new(self.kind.complex.dup()),
                    },
                    ObjKind::BYTES => ObjKind {
                        string: self.kind.string.clone(),
                    },
                    ObjKind::TIME => ObjKind {
                        time: self.kind.time.clone(),
                    },
                    ObjKind::ARRAY => ObjKind {
                        array: self.kind.array.clone(),
                    },
                    ObjKind::RANGE => ObjKind {
                        range: self.kind.range.clone(),
                    },
                    ObjKind::PROC => ObjKind {
                        proc: self.kind.proc.clone(),
                    },
                    ObjKind::HASH => ObjKind {
                        hash: self.kind.hash.clone(),
                    },
                    ObjKind::REGEXP => ObjKind {
                        regexp: self.kind.regexp.clone(),
                    },
                    ObjKind::IO => ObjKind {
                        io: self.kind.io.clone(),
                    },
                    ObjKind::EXCEPTION => ObjKind {
                        exception: self.kind.exception.clone(),
                    },
                    ObjKind::METHOD => ObjKind {
                        method: self.kind.method.clone(),
                    },
                    _ => unreachable!("clone()"),
                }
            },
        }
    }

    pub(crate) fn class(&self) -> ClassId {
        self.header.class()
    }
}

//
// constructors.
//
impl RValue {
    pub(super) fn new_bigint(bigint: BigInt) -> Self {
        RValue {
            header: Header::new(INTEGER_CLASS, ObjKind::BIGNUM),
            kind: ObjKind::bignum(bigint),
            var_table: None,
        }
    }

    pub(super) fn new_float(f: f64) -> Self {
        RValue {
            header: Header::new(FLOAT_CLASS, ObjKind::FLOAT),
            kind: ObjKind::float(f),
            var_table: None,
        }
    }

    pub(super) fn new_complex(re: complex::Real, im: complex::Real) -> Self {
        RValue {
            header: Header::new(COMPLEX_CLASS, ObjKind::COMPLEX),
            kind: ObjKind::complex(re, im),
            var_table: None,
        }
    }

    pub(super) fn new_complex_from(complex: num::Complex<Real>) -> Self {
        RValue {
            header: Header::new(COMPLEX_CLASS, ObjKind::COMPLEX),
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
            header: Header::new(CLASS_CLASS, ObjKind::CLASS),
            kind: ObjKind::class(id, superclass, class_type),
            var_table: None,
        }
    }

    ///
    /// Create new module object with *class_id*.
    ///
    pub(super) fn new_module(id: ClassId, superclass: Option<Module>) -> Self {
        RValue {
            header: Header::new(MODULE_CLASS, ObjKind::MODULE),
            kind: ObjKind::class(id, superclass, ModuleType::RealClass),
            var_table: None,
        }
    }

    ///
    /// Create new iclass object with *class_id*.
    ///
    pub(super) fn new_iclass(id: ClassId, superclass: Option<Module>) -> Self {
        RValue {
            header: Header::new(MODULE_CLASS, ObjKind::MODULE),
            kind: ObjKind::class(id, superclass, ModuleType::IClass),
            var_table: None,
        }
    }

    ///
    /// Create new instance object of class *class_id*.
    ///
    pub(super) fn new_object(class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjKind::OBJECT),
            kind: ObjKind::object(),
            var_table: None,
        }
    }

    pub(super) fn new_string(s: String) -> Self {
        Self::new_bytes(s.into_bytes())
    }

    pub(super) fn new_string_from_inner(s: StringInner) -> Self {
        Self::new_bytes_from_inner(s)
    }

    pub(super) fn new_bytes(v: Vec<u8>) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes_from_vec(v),
            var_table: None,
        }
    }

    pub(super) fn new_bytes_from_inner(s: StringInner) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes(s),
            var_table: None,
        }
    }

    pub(super) fn new_bytes_from_slice(slice: &[u8]) -> Self {
        RValue {
            header: Header::new(STRING_CLASS, ObjKind::BYTES),
            kind: ObjKind::bytes_from_slice(slice),
            var_table: None,
        }
    }

    pub(super) fn new_array(ary: ArrayInner) -> Self {
        RValue {
            header: Header::new(ARRAY_CLASS, ObjKind::ARRAY),
            kind: ObjKind::array(ary),
            var_table: None,
        }
    }

    pub(super) fn new_array_with_class(v: Vec<Value>, class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjKind::ARRAY),
            kind: ObjKind::array(ArrayInner::from_vec(v)),
            var_table: None,
        }
    }

    pub(super) fn new_hash(map: IndexMap<HashKey, Value>) -> Self {
        RValue {
            header: Header::new(HASH_CLASS, ObjKind::HASH),
            kind: ObjKind::hash(map),
            var_table: None,
        }
    }

    pub(super) fn new_hash_from_inner(inner: HashInner) -> Self {
        RValue {
            header: Header::new(HASH_CLASS, ObjKind::HASH),
            kind: ObjKind::hash_from_inner(inner),
            var_table: None,
        }
    }

    pub(super) fn new_hash_with_class(map: IndexMap<HashKey, Value>, class_id: ClassId) -> Self {
        RValue {
            header: Header::new(class_id, ObjKind::HASH),
            kind: ObjKind::hash(map),
            var_table: None,
        }
    }

    pub(super) fn new_regexp(regexp: RegexpInner) -> Self {
        RValue {
            header: Header::new(REGEXP_CLASS, ObjKind::REGEXP),
            kind: ObjKind::regexp(regexp),
            var_table: None,
        }
    }

    pub(super) fn new_exception(
        kind: IdentId,
        msg: String,
        trace: Vec<(Loc, SourceInfoRef)>,
        class_id: ClassId,
    ) -> Self {
        RValue {
            header: Header::new(class_id, ObjKind::EXCEPTION),
            kind: ObjKind::exception(kind, msg, trace),
            var_table: None,
        }
    }

    pub(super) fn new_exception_from_err(
        globals: &Globals,
        err: MonorubyErr,
        class_id: ClassId,
    ) -> Self {
        RValue {
            header: Header::new(class_id, ObjKind::EXCEPTION),
            kind: ObjKind::exception_from(err, globals),
            var_table: None,
        }
    }

    pub(super) fn new_io(io: IoInner) -> Self {
        RValue {
            header: Header::new(IO_CLASS, ObjKind::IO),
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

    pub(super) fn new_time(time: TimeInner) -> Self {
        RValue {
            header: Header::new(TIME_CLASS, ObjKind::TIME),
            kind: ObjKind::time(time),
            var_table: None,
        }
    }

    pub(super) fn new_range(start: Value, end: Value, exclude_end: bool) -> Self {
        RValue {
            header: Header::new(RANGE_CLASS, ObjKind::RANGE),
            kind: ObjKind::range(start, end, exclude_end),
            var_table: None,
        }
    }

    pub(super) fn new_proc(block_data: ProcInner) -> Self {
        RValue {
            header: Header::new(PROC_CLASS, ObjKind::PROC),
            kind: ObjKind::proc(block_data),
            var_table: None,
        }
    }

    pub(super) fn new_method(receiver: Value, func_id: FuncId) -> Self {
        RValue {
            header: Header::new(METHOD_CLASS, ObjKind::METHOD),
            kind: ObjKind::method(receiver, func_id),
            var_table: None,
        }
    }

    pub(super) fn new_fiber(proc: Proc) -> Self {
        RValue {
            header: Header::new(FIBER_CLASS, ObjKind::FIBER),
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
            header: Header::new(ENUMERATOR_CLASS, ObjKind::ENUMERATOR),
            kind: ObjKind::enumerator(obj, method, proc, args),
            var_table: None,
        }
    }

    pub(super) fn new_generator(proc: Proc) -> Self {
        RValue {
            header: Header::new(GENERATOR_CLASS, ObjKind::GENERATOR),
            kind: ObjKind::generator(proc),
            var_table: None,
        }
    }
}

impl RValue {
    pub fn unpack(&self) -> RV {
        unsafe {
            match self.ty() {
                ObjKind::BIGNUM => RV::BigInt(self.as_bignum()),
                ObjKind::FLOAT => RV::Float(self.as_float()),
                ObjKind::BYTES => RV::String(self.as_bytes()),
                ObjKind::COMPLEX => RV::Complex(self.as_complex()),
                _ => RV::Object(self),
            }
        }
    }
}

impl RValue {
    /// This function is only used for system assertion.
    pub(crate) fn eq(lhs: &Self, rhs: &Self) -> bool {
        unsafe {
            match (lhs.ty(), rhs.ty()) {
                (ObjKind::BIGNUM, ObjKind::BIGNUM) => lhs.as_bignum() == rhs.as_bignum(),
                (ObjKind::FLOAT, ObjKind::FLOAT) => lhs.as_float() == rhs.as_float(),
                (ObjKind::COMPLEX, ObjKind::COMPLEX) => {
                    lhs.as_complex().re() == rhs.as_complex().re()
                        && lhs.as_complex().im() == rhs.as_complex().im()
                }
                (ObjKind::BYTES, ObjKind::BYTES) => lhs.as_bytes() == rhs.as_bytes(),
                (ObjKind::ARRAY, ObjKind::ARRAY) => {
                    let lhs = lhs.as_array();
                    let rhs = rhs.as_array();
                    lhs.len() == rhs.len()
                        && lhs
                            .iter()
                            .zip(rhs.iter())
                            .all(|(lhs, rhs)| Value::eq(*lhs, *rhs))
                }
                (ObjKind::RANGE, ObjKind::RANGE) => lhs.as_range() == rhs.as_range(),
                (ObjKind::HASH, ObjKind::HASH) => {
                    let lhs = lhs.as_hash();
                    let rhs = rhs.as_hash();
                    lhs.len() == rhs.len()
                        && lhs
                            .iter()
                            .zip(rhs.iter())
                            .all(|(lhs, rhs)| Value::eq(lhs.0, rhs.0) && Value::eq(lhs.1, rhs.1))
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

    pub(super) unsafe fn as_class_id(&self) -> ClassId {
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

    /*pub(super) unsafe fn as_complex_mut(&mut self) -> &mut ComplexInner {
        &mut self.kind.complex
    }*/

    pub(super) fn as_bytes(&self) -> &StringInner {
        unsafe { &self.kind.string }
    }

    pub(super) fn as_bytes_mut(&mut self) -> &mut StringInner {
        unsafe { &mut self.kind.string }
    }

    pub(super) fn as_str(&self) -> std::borrow::Cow<'_, str> {
        unsafe { self.kind.string.as_str() }
    }

    /*pub(crate) fn as_string_mut(&mut self) -> &mut InnerVec {
        unsafe { &mut *self.kind.bytes }
    }*/

    pub(crate) fn as_array(&self) -> &ArrayInner {
        unsafe { &self.kind.array }
    }

    pub(super) fn as_array_mut(&mut self) -> &mut ArrayInner {
        unsafe { &mut self.kind.array }
    }

    pub(super) fn as_range(&self) -> &RangeInner {
        unsafe { &self.kind.range }
    }

    pub fn as_exception(&self) -> &ExceptionInner {
        unsafe { &self.kind.exception }
    }

    pub(crate) fn as_hash(&self) -> &HashInner {
        unsafe { &self.kind.hash }
    }

    pub(super) fn as_hash_mut(&mut self) -> &mut HashInner {
        unsafe { &mut self.kind.hash }
    }

    pub(super) fn as_regex(&self) -> &RegexpInner {
        unsafe { &self.kind.regexp }
    }

    pub(crate) fn as_io(&self) -> &IoInner {
        unsafe { &self.kind.io }
    }

    pub(super) fn as_io_mut(&mut self) -> &mut IoInner {
        unsafe { &mut self.kind.io }
    }

    pub(super) fn as_proc(&self) -> &ProcInner {
        unsafe { &self.kind.proc }
    }

    pub(super) fn as_proc_mut(&mut self) -> &mut ProcInner {
        unsafe { &mut self.kind.proc }
    }

    pub(super) fn as_time(&self) -> &TimeInner {
        unsafe { &self.kind.time }
    }

    /*pub(crate) fn as_time_mut(&mut self) -> &mut TimeInfo {
        unsafe { &mut *self.kind.time }
    }*/

    pub(crate) fn as_method(&self) -> &MethodInner {
        unsafe { &self.kind.method }
    }

    pub(super) unsafe fn as_fiber(&self) -> &FiberInner {
        &self.kind.fiber
    }

    pub(super) unsafe fn as_fiber_mut(&mut self) -> &mut FiberInner {
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
    ty: u16,
    class: ClassId,
}

impl Header {
    fn new(class: ClassId, ty: u8) -> Self {
        Header {
            meta: Metadata {
                flag: 1,
                ty: ty as u16,
                class,
            },
        }
    }

    fn is_live(&self) -> bool {
        unsafe { self.meta.flag & 0b1 == 1 }
    }

    fn class(&self) -> ClassId {
        assert!(self.is_live());
        unsafe { self.meta.class }
    }

    fn ty(&self) -> u8 {
        unsafe { self.meta.ty as u8 }
    }

    fn change_class(&mut self, class: ClassId) {
        self.meta.class = class;
    }
}

#[derive(Debug, Clone)]
pub struct ExceptionInner {
    class_name: IdentId,
    msg: String,
    trace: Vec<(Loc, SourceInfoRef)>,
}

impl ExceptionInner {
    pub fn kind(&self) -> MonorubyErrKind {
        if self.class_name == IdentId::get_id("StopIteration") {
            return MonorubyErrKind::StopIteration;
        }
        MonorubyErrKind::Runtime
    }

    pub fn msg(&self) -> &str {
        &self.msg
    }

    pub fn trace(&self) -> Vec<(Loc, SourceInfoRef)> {
        self.trace.clone()
    }

    pub fn get_error_message(&self) -> String {
        format!("{} ({:?})", self.msg, self.class_name)
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
#[repr(C)]
pub struct RangeInner {
    pub start: Value,
    pub end: Value,
    pub exclude_end: u32,
}

impl RangeInner {
    pub(crate) fn eql(&self, other: &Self) -> bool {
        self.start.eql(&other.start)
            && self.end.eql(&other.end)
            && self.exclude_end() == other.exclude_end()
    }

    pub fn exclude_end(&self) -> bool {
        self.exclude_end != 0
    }
}

#[monoruby_object]
pub struct Proc(Value);

impl Proc {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::PROC));
        Proc(val)
    }

    pub(crate) fn from(block: ProcInner) -> Self {
        Proc(Value::new_proc(block))
    }

    pub(crate) fn from_parts(outer_lfp: Lfp, func_id: FuncId) -> Self {
        Proc(Value::new_proc(ProcInner::from(outer_lfp, func_id)))
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct ProcInner {
    outer_lfp: Lfp,
    func_id: FuncId,
}

impl alloc::GC<RValue> for ProcInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.outer_lfp.mark(alloc)
    }
}

impl ProcInner {
    pub(crate) fn from(outer_lfp: Lfp, func_id: FuncId) -> Self {
        Self { outer_lfp, func_id }
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn outer_lfp(&self) -> Lfp {
        self.outer_lfp
    }

    pub fn self_val(&self) -> Value {
        self.outer_lfp.self_val()
    }
}
