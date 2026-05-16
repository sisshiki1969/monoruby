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
    /// The name this Method was looked up by (`obj.method(:x)` / the
    /// alias name). `None` falls back to the func's stored name.
    lookup_name: Option<IdentId>,
    /// The method's original definition name (tracked through
    /// `alias_method` / `define_method`). `None` falls back to the
    /// func's stored name.
    original_name: Option<IdentId>,
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
            lookup_name: None,
            original_name: None,
        }
    }

    pub fn new_named(
        receiver: Value,
        func_id: FuncId,
        owner: ClassId,
        lookup_name: IdentId,
        original_name: IdentId,
    ) -> Self {
        Self {
            receiver,
            func_id,
            owner,
            mm_name: None,
            lookup_name: Some(lookup_name),
            original_name: Some(original_name),
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
            lookup_name: Some(target),
            original_name: Some(target),
        }
    }

    /// Lookup name (the name this Method was obtained by). Falls back
    /// to the func's stored name when not explicitly tracked.
    pub fn lookup_name(&self, store: &Store) -> IdentId {
        self.lookup_name
            .or_else(|| store[self.func_id].name())
            .unwrap_or_else(|| IdentId::get_id(""))
    }

    /// Original definition name (tracked through alias/define_method).
    pub fn original_name(&self, store: &Store) -> IdentId {
        self.original_name
            .or_else(|| store[self.func_id].name())
            .unwrap_or_else(|| IdentId::get_id(""))
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
    /// See `MethodInner::lookup_name` / `original_name`.
    lookup_name: Option<IdentId>,
    original_name: Option<IdentId>,
}

impl UMethodInner {
    pub fn new(func_id: FuncId, owner: ClassId) -> Self {
        Self {
            func_id,
            owner,
            mm_name: None,
            lookup_name: None,
            original_name: None,
        }
    }

    pub fn new_named(
        func_id: FuncId,
        owner: ClassId,
        lookup_name: IdentId,
        original_name: IdentId,
    ) -> Self {
        Self {
            func_id,
            owner,
            mm_name: None,
            lookup_name: Some(lookup_name),
            original_name: Some(original_name),
        }
    }

    pub fn new_method_missing(mm_func_id: FuncId, target: IdentId, owner: ClassId) -> Self {
        Self {
            func_id: mm_func_id,
            owner,
            mm_name: Some(target),
            lookup_name: Some(target),
            original_name: Some(target),
        }
    }

    pub fn func_id(&self) -> FuncId {
        self.func_id
    }

    pub fn owner(&self) -> ClassId {
        self.owner
    }

    /// Lookup name (the name this UnboundMethod was obtained by).
    pub fn lookup_name(&self, store: &Store) -> IdentId {
        self.lookup_name
            .or_else(|| store[self.func_id].name())
            .unwrap_or_else(|| IdentId::get_id(""))
    }

    /// Original definition name (tracked through alias/define_method).
    pub fn original_name(&self, store: &Store) -> IdentId {
        self.original_name
            .or_else(|| store[self.func_id].name())
            .unwrap_or_else(|| IdentId::get_id(""))
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
