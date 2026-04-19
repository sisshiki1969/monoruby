use super::*;

#[monoruby_object]
pub struct Proc(Value);

impl Proc {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::PROC));
        Proc(val)
    }

    pub(crate) fn try_new(val: Value) -> Option<Self> {
        if val.ty() == Some(ObjTy::PROC) {
            Some(Proc(val))
        } else {
            None
        }
    }

    pub(crate) fn from_outer(outer_lfp: Lfp, func_id: FuncId, pc: BytecodePtr) -> Self {
        Proc(Value::new_proc(ProcInner::from_parts(
            Some(outer_lfp),
            func_id,
            outer_lfp.self_val(),
            pc,
        )))
    }

    /// Raw pointer to this Proc's `outer_lfp` slot inside the heap-allocated
    /// RValue. Used by lazy heap promotion to register the slot as an
    /// escapee so it gets rewritten when the captured stack frame is
    /// promoted.
    ///
    /// Reads/writes 8 bytes — `Option<Lfp>` and `Lfp` share the same
    /// representation (NonNull niche), so writing a non-null `Lfp`
    /// through this pointer leaves the field as `Some(new)`.
    pub(crate) fn outer_lfp_slot_ptr(&mut self) -> *mut Lfp {
        &mut self.outer_lfp as *mut Option<Lfp> as *mut Lfp
    }
}

#[derive(Debug, Clone)]
pub struct ProcInner {
    outer_lfp: Option<Lfp>,
    func_id: FuncId,
    self_value: Value,
    pc: BytecodePtr,
}

impl alloc::GC<RValue> for ProcInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(outer_lfp) = &self.outer_lfp {
            outer_lfp.mark(alloc);
        }
        self.self_value.mark(alloc);
    }
}

impl ProcInner {
    pub(crate) fn from_parts(
        outer_lfp: Option<Lfp>,
        func_id: FuncId,
        self_value: Value,
        pc: BytecodePtr,
    ) -> Self {
        ProcInner {
            outer_lfp,
            func_id,
            self_value,
            pc,
        }
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn outer_lfp(&self) -> Option<Lfp> {
        self.outer_lfp
    }

    pub fn self_val(&self) -> Value {
        self.self_value
    }

    pub(crate) fn source(&self) -> BytecodePtr {
        self.pc
    }
}
