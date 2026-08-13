use super::*;

#[allow(non_camel_case_types)]
pub(crate) enum InlineFuncInfo {
    InlineGen(Box<InlineGen>),
    /// A generator whose emitted code never consults the receiver's (or an
    /// argument's) class — the type has no `ClassId` parameters to consult.
    /// The JIT fires these even behind a polymorphic class-set guard, where
    /// the other variants must be skipped (their code assumes the single
    /// compile-time receiver class).
    InlineGenClassIndependent(Box<InlineGenClassIndependent>),
    CFunc_F_F(unsafe extern "C" fn(f64) -> f64),
    CFunc_FF_F(extern "C" fn(f64, f64) -> f64),
}

impl InlineFuncInfo {
    pub(crate) fn new_inline_gen(f: Box<InlineGen>) -> Self {
        InlineFuncInfo::InlineGen(f)
    }

    pub(crate) fn new_inline_gen_class_independent(f: Box<InlineGenClassIndependent>) -> Self {
        InlineFuncInfo::InlineGenClassIndependent(f)
    }

    pub(crate) fn new_cfunc_f_f(f: unsafe extern "C" fn(f64) -> f64) -> Self {
        InlineFuncInfo::CFunc_F_F(f)
    }

    pub(crate) fn new_cfunc_ff_f(f: extern "C" fn(f64, f64) -> f64) -> Self {
        InlineFuncInfo::CFunc_FF_F(f)
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
