use super::*;

#[allow(non_camel_case_types)]
pub(crate) enum InlineFuncInfo {
    InlineGen(Box<InlineGen>),
    CFunc_F_F(extern "C" fn(f64) -> f64),
}

impl InlineFuncInfo {
    pub(crate) fn new_inline_gen(f: Box<InlineGen>) -> Self {
        InlineFuncInfo::InlineGen(f)
    }

    pub(crate) fn new_cfunc_f_f(f: extern "C" fn(f64) -> f64) -> Self {
        InlineFuncInfo::CFunc_F_F(f)
    }
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
