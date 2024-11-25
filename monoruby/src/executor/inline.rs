use super::*;

pub(crate) struct InlineFuncInfo {
    pub(crate) inline_gen: Box<InlineGen>,
    #[cfg(feature = "dump-bc")]
    pub name: String,
}

#[derive(Default)]
pub(crate) struct InlineTable(HashMap<FuncId, InlineFuncInfo>);
impl InlineTable {
    pub fn get_inline(&self, func_id: FuncId) -> Option<&InlineFuncInfo> {
        self.0.get(&func_id)
    }

    pub(crate) fn add_inline(&mut self, func_id: FuncId, info: InlineFuncInfo) {
        self.0.insert(func_id, info);
    }
}
