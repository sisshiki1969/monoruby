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
}

#[derive(Debug, Clone)]
pub struct ProcInner {
    outer_lfp: Option<Lfp>,
    func_id: FuncId,
    self_value: Value,
    pc: BytecodePtr,
    /// An arity to report in place of the block's own, or
    /// [`Self::NO_ARITY_OVERRIDE`].
    ///
    /// `Enumerable#map` hands `#each` a block of its own, and a
    /// Ruby-level block cannot carry an arity it did not declare — so a
    /// redefined `#each` saw `-1` where CRuby, which copies the user
    /// block's min/max onto its internal one, reports the user block's
    /// arity (#1556). It sits in `ProcInner`'s existing tail padding,
    /// so the `RValue` does not grow.
    arity_override: i32,
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
    /// `arity_override` sentinel: report the block's own arity.
    /// `i32::MIN` is not a value any parameter list produces.
    pub const NO_ARITY_OVERRIDE: i32 = i32::MIN;

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
            arity_override: Self::NO_ARITY_OVERRIDE,
        }
    }

    /// The arity to report, when it is not the block's own.
    pub fn arity_override(&self) -> Option<i64> {
        match self.arity_override {
            Self::NO_ARITY_OVERRIDE => None,
            n => Some(n as i64),
        }
    }

    pub(crate) fn set_arity_override(&mut self, arity: i64) {
        self.arity_override = arity.clamp(i32::MIN as i64 + 1, i32::MAX as i64) as i32;
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
