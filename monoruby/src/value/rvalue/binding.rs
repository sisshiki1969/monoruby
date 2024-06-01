use super::*;

#[monoruby_object]
pub struct Binding(Value);

impl Binding {
    /*fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::BINDING));
        Binding(val)
    }*/

    pub(crate) fn try_new(val: Value) -> Option<Self> {
        if val.ty() == Some(ObjKind::BINDING) {
            Some(Binding(val))
        } else {
            None
        }
    }

    pub fn from_outer(outer_lfp: Lfp) -> Self {
        Binding(Value::new_binding(outer_lfp))
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct BindingInner {
    binding_lfp: Option<Lfp>,
    outer_lfp: Lfp,
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
    pub(crate) fn from(outer_lfp: Lfp) -> Self {
        Self {
            binding_lfp: None,
            outer_lfp,
        }
    }

    pub fn binding(&self) -> Option<Lfp> {
        self.binding_lfp
    }

    pub fn outer_lfp(&self) -> Lfp {
        self.outer_lfp
    }

    pub fn func_id(&self) -> Option<FuncId> {
        Some(self.binding_lfp?.meta().func_id())
    }

    pub fn self_val(&self) -> Value {
        self.outer_lfp.self_val()
    }

    pub fn set_inner(&mut self, binding_lfp: Lfp) {
        self.binding_lfp = Some(binding_lfp);
    }
}
