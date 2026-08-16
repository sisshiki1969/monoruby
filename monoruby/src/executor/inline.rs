use super::*;

#[allow(non_camel_case_types)]
pub(crate) enum InlineFuncInfo {
    InlineGen(Box<InlineGen>),
    /// A numeric binary operator / comparison generator, fired guard-free
    /// from the binop/cmp bytecode dispatchers under the basic-op license
    /// (and from `compile_method_call` for explicit sends). See
    /// [`InlineGenBinary`].
    InlineGenBinary(Box<InlineGenBinary>),
    /// A numeric unary operator generator, fired guard-free from the `UnOp`
    /// dispatcher under the basic-op license (and from
    /// `compile_method_call` for explicit sends). See [`InlineGenUnary`].
    InlineGenUnary(Box<InlineGenUnary>),
    CFunc_F_F(unsafe extern "C" fn(f64) -> f64),
    CFunc_FF_F(extern "C" fn(f64, f64) -> f64),
}

impl InlineFuncInfo {
    pub(crate) fn new_inline_gen(f: Box<InlineGen>) -> Self {
        InlineFuncInfo::InlineGen(f)
    }

    pub(crate) fn new_inline_gen_binary(f: Box<InlineGenBinary>) -> Self {
        InlineFuncInfo::InlineGenBinary(f)
    }

    pub(crate) fn new_inline_gen_unary(f: Box<InlineGenUnary>) -> Self {
        InlineFuncInfo::InlineGenUnary(f)
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
