use super::*;
use std::sync::{LazyLock, RwLock};

static INLINE_INFO: LazyLock<RwLock<InlineTable>> =
    LazyLock::new(|| RwLock::new(InlineTable::default()));

#[derive(Debug, Clone, Copy)]
pub(crate) struct InlineMethodId(usize);

impl std::convert::Into<usize> for InlineMethodId {
    fn into(self) -> usize {
        self.0
    }
}

impl InlineMethodId {
    pub fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Debug, Default)]
pub(crate) struct InlineTable(HashMap<FuncId, InlineMethodId>);
impl InlineTable {
    pub fn get_inline(func_id: FuncId) -> Option<InlineMethodId> {
        INLINE_INFO.read().unwrap().get(func_id)
    }

    pub(super) fn add_inline(func_id: FuncId, id: InlineMethodId) {
        INLINE_INFO.write().unwrap().add(func_id, id);
    }

    fn get(&self, func_id: FuncId) -> Option<InlineMethodId> {
        self.0.get(&func_id).cloned()
    }

    fn add(&mut self, func_id: FuncId, id: InlineMethodId) {
        self.0.insert(func_id, id);
    }
}
