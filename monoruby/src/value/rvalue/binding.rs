use super::*;

#[monoruby_object]
pub struct Binding(Value);

impl Binding {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::PROC));
        Binding(val)
    }

    pub(crate) fn from(binding_lfp: Lfp) -> Self {
        Binding(Value::new_binding(binding_lfp))
    }
}

#[derive(Debug, Clone)]
#[repr(C)]
pub struct BindingInner {
    binding_lfp: Lfp,
}

impl alloc::GC<RValue> for BindingInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.binding_lfp.mark(alloc)
    }
}

impl BindingInner {
    pub(crate) fn from(binding_lfp: Lfp) -> Self {
        Self { binding_lfp }
    }

    pub fn binding(&self) -> Lfp {
        self.binding_lfp
    }

    pub fn func_id(&self) -> FuncId {
        self.binding_lfp.meta().func_id()
    }

    pub fn self_val(&self) -> Value {
        self.binding_lfp.self_val()
    }
}
