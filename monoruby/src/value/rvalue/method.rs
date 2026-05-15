use super::*;

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct MethodInner {
    receiver: Value,
    func_id: FuncId,
    owner: ClassId,
    /// `Some(sym)` marks this Method as a `method_missing` proxy created
    /// via `respond_to_missing?` for the target name `sym`. In that case
    /// `func_id` is the resolved `method_missing` function and calling the
    /// Method dispatches `receiver.method_missing(sym, ...)`.
    mm_name: Option<IdentId>,
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
            mm_name: None,
        }
    }

    pub fn new_method_missing(
        receiver: Value,
        mm_func_id: FuncId,
        target: IdentId,
        owner: ClassId,
    ) -> Self {
        Self {
            receiver,
            func_id: mm_func_id,
            owner,
            mm_name: Some(target),
        }
    }

    pub fn receiver(&self) -> Value {
        self.receiver
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    ///
    /// If this Method is a `method_missing` proxy, return the target name.
    ///
    pub fn method_missing_name(&self) -> Option<IdentId> {
        self.mm_name
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
    /// `Some(sym)` marks this UnboundMethod as a `method_missing` proxy
    /// (created via `Method#unbind` on a `respond_to_missing?` Method).
    mm_name: Option<IdentId>,
}

impl UMethodInner {
    pub fn new(func_id: FuncId, owner: ClassId) -> Self {
        Self {
            func_id,
            owner,
            mm_name: None,
        }
    }

    pub fn new_method_missing(mm_func_id: FuncId, target: IdentId, owner: ClassId) -> Self {
        Self {
            func_id: mm_func_id,
            owner,
            mm_name: Some(target),
        }
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    ///
    /// If this UnboundMethod is a `method_missing` proxy, return the target name.
    ///
    pub fn method_missing_name(&self) -> Option<IdentId> {
        self.mm_name
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
