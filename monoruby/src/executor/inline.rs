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

///
/// How a builtin treats its receiver: the escape summary the JIT consults
/// before eliding the allocation of an Array literal that flows into it as
/// the receiver (see `doc/escape_analysis.md`).
///
#[derive(Debug, Clone, Copy, Default)]
pub(crate) struct RecvEscape {
    ///
    /// The receiver does not escape: the builtin never mutates it, never
    /// stores it anywhere, and never returns it (or an Enumerator over it).
    /// Its elements may flow out, its identity may not. A literal of
    /// immediates (`[:a, :b].include?(x)`) can then be handed the literal's
    /// own template Array instead of a fresh copy.
    ///
    pub transparent: bool,
    ///
    /// An allocation-free lowering of the call (argument-less unless the op
    /// says otherwise, block-less) over the elements of a literal kept in
    /// frame slots (`[a, b].max`) or of a literal template (`{a: 1}.size`).
    ///
    pub elements: Option<ElementsOp>,
}

impl RecvEscape {
    pub(crate) fn transparent() -> Self {
        Self {
            transparent: true,
            elements: None,
        }
    }

    pub(crate) fn elements(op: ElementsOp) -> Self {
        Self {
            transparent: true,
            elements: Some(op),
        }
    }
}

///
/// The element-wise lowerings of [`RecvEscape::elements`].
///
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ElementsOp {
    /// `min` — compare the elements in place (`runtime::opt_array_min`).
    Min,
    /// `max` — likewise (`runtime::opt_array_max`).
    Max,
    /// `size` / `length` — the literal's length (a Hash's distinct keys),
    /// a constant.
    Size,
    /// `empty?` — a constant.
    Empty,
    /// `first` — the first element (or nil).
    First,
    /// `last` — the last element (or nil).
    Last,
    /// `Hash#[]` — the value of a compile-time constant key, when the
    /// key is present; absent, the call is not folded (`default` would
    /// be dispatched).
    HashIndex,
    /// `Hash#fetch` — likewise, for the one-argument form on a present
    /// key.
    HashFetch,
    /// `Hash#key?` and its aliases — a constant, for a constant key.
    HashHasKey,
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
