use crate::*;
use std::sync::{LazyLock, RwLock};

static INLINE_INFO: LazyLock<RwLock<InlineTable>> =
    LazyLock::new(|| RwLock::new(InlineTable::default()));

#[derive(Debug, Default)]
pub struct InlineTable(HashMap<FuncId, InlineMethod>);
impl InlineTable {
    pub(crate) fn get_inline(func_id: FuncId) -> Option<InlineMethod> {
        INLINE_INFO.read().unwrap().get(func_id).cloned()
    }

    pub(super) fn add_inline(func_id: FuncId, inline_id: InlineMethod) {
        INLINE_INFO.write().unwrap().add(func_id, inline_id);
    }

    fn get(&self, func_id: FuncId) -> Option<&InlineMethod> {
        self.0.get(&func_id)
    }

    fn add(&mut self, func_id: FuncId, inline_id: InlineMethod) {
        self.0.insert(func_id, inline_id);
    }
}
