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

    pub(crate) fn from(block: ProcInner) -> Self {
        Proc(Value::new_proc(block))
    }

    pub(crate) fn from_parts(outer_lfp: Lfp, func_id: FuncId) -> Self {
        Proc(Value::new_proc(ProcInner::new(outer_lfp, func_id)))
    }
}

#[derive(Debug, Clone)]
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
    pub fn new(outer_lfp: Lfp, func_id: FuncId) -> Self {
        ProcInner { outer_lfp, func_id }
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
