use super::*;

#[derive(Debug, Clone, PartialEq, Hash)]
#[repr(C)]
pub struct MethodInner {
    receiver: Value,
    func_id: FuncId,
    owner: ClassId,
}

impl alloc::GC<RValue> for MethodInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.receiver.mark(alloc);
    }
}

impl MethodInner {
    pub fn new(receiver: Value, func_id: FuncId, owner: ClassId) -> Self {
        Self {
            receiver,
            func_id,
            owner,
        }
    }

    pub fn receiver(&self) -> Value {
        self.receiver
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn debug(&self, store: &Store) -> String {
        format!(
            "#<Method: {}#{}()>",
            store.debug_class_name(self.receiver.class()),
            store[self.func_id()].name().unwrap()
        )
    }

    pub fn to_s(&self, store: &Store) -> String {
        format!(
            "#<Method: {}#{}()>",
            store.get_class_name(self.receiver.class()),
            store[self.func_id()].name().unwrap()
        )
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
#[repr(C)]
pub struct UMethodInner {
    func_id: FuncId,
    owner: ClassId,
}

impl UMethodInner {
    pub fn new(func_id: FuncId, owner: ClassId) -> Self {
        Self { func_id, owner }
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    pub fn debug(&self, store: &Store) -> String {
        format!(
            "#<UnboundMethod: {}#{}()>",
            store.debug_class_name(self.owner),
            store[self.func_id()].name().unwrap()
        )
    }

    pub fn to_s(&self, store: &Store) -> String {
        format!(
            "#<UnboundMethod: {}#{}()>",
            store.get_class_name(self.owner),
            store[self.func_id()].name().unwrap()
        )
    }
}
