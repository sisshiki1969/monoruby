use super::*;

#[monoruby_object]
pub struct Binding(Value);

impl Binding {
    /*fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjTy::BINDING));
        Binding(val)
    }*/

    pub(crate) fn try_new(val: Value) -> Option<Self> {
        if val.ty() == Some(ObjTy::BINDING) {
            Some(Binding(val))
        } else {
            None
        }
    }

    pub fn new(outer_lfp: Lfp) -> Self {
        Binding(Value::new_binding(outer_lfp, None))
    }

    pub(crate) fn from_outer(outer_lfp: Lfp, call_site_pc: BytecodePtr) -> Self {
        Binding(Value::new_binding(outer_lfp, Some(call_site_pc)))
    }

    /// Raw pointer to this Binding's `outer_lfp` slot. Used by lazy
    /// heap promotion to register the slot as an escapee.
    pub(crate) fn outer_lfp_slot_ptr(&mut self) -> *mut Lfp {
        &mut self.outer_lfp as *mut Lfp
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct BindingInner {
    binding_lfp: Option<Lfp>,
    outer_lfp: Lfp,
    pub(crate) pc: Option<BytecodePtr>,
}

impl alloc::GC<RValue> for BindingInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(binding) = self.binding_lfp {
            binding.mark(alloc)
        } else {
            self.outer_lfp.mark(alloc)
        }
    }
}

impl BindingInner {
    pub(crate) fn from(outer_lfp: Lfp, pc: Option<BytecodePtr>) -> Self {
        Self {
            binding_lfp: None,
            outer_lfp,
            pc,
        }
    }

    pub fn binding(&self) -> Option<Lfp> {
        self.binding_lfp
    }

    pub fn outer_lfp(&self) -> Lfp {
        self.outer_lfp
    }

    pub fn outer_fid(&self) -> FuncId {
        self.outer_lfp.func_id()
    }

    pub fn func_id(&self) -> Option<FuncId> {
        Some(self.binding_lfp?.func_id())
    }

    pub fn self_val(&self) -> Value {
        self.outer_lfp.self_val()
    }

    pub fn set_inner(&mut self, binding_lfp: Lfp) {
        self.binding_lfp = Some(binding_lfp);
    }
}
