use super::*;

pub const RANGE_START_OFFSET: usize = RVALUE_OFFSET_KIND + std::mem::offset_of!(RangeInner, start);
pub const RANGE_END_OFFSET: usize = RVALUE_OFFSET_KIND + std::mem::offset_of!(RangeInner, end);
pub const RANGE_EXCLUDE_END_OFFSET: usize =
    RVALUE_OFFSET_KIND + std::mem::offset_of!(RangeInner, exclude_end);

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct RangeInner {
    start: Value,
    end: Value,
    exclude_end: u32,
}

impl GC<RValue> for RangeInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.start.mark(alloc);
        self.end.mark(alloc);
    }
}

impl RangeInner {
    ///
    /// The remember-on-promote half of `mark` above. It must cover exactly
    /// what `mark` covers — a field marked but not reported here would be
    /// dropped from the remembered set and then freed under a live
    /// reference.
    ///
    pub(crate) fn young_child_exists(&self, alloc: &Allocator<RValue>) -> bool {
        let is_young = |v: Value| v.try_rvalue().is_some_and(|rv| !alloc.is_old(rv));
        is_young(self.start) || is_young(self.end)
    }
}

impl RubyEql<Executor, Globals, MonorubyErr> for RangeInner {
    fn eql(&self, other: &Self, vm: &mut Executor, globals: &mut Globals) -> Result<bool> {
        Ok(self.start.eql(&other.start, vm, globals)?
            && self.end.eql(&other.end, vm, globals)?
            && self.exclude_end() == other.exclude_end())
    }
}

impl RubyHash<Executor, Globals, MonorubyErr> for RangeInner {
    fn ruby_hash<H: std::hash::Hasher>(
        &self,
        state: &mut H,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<()> {
        self.start.ruby_hash(state, vm, globals)?;
        self.end.ruby_hash(state, vm, globals)?;
        self.exclude_end().hash(state);
        Ok(())
    }
}

impl RangeInner {
    /// `exclude_end` sentinel for a `Range.allocate`d, not-yet-initialized
    /// range. Only `Range#initialize` consults it (to reject a second
    /// initialize per CRuby); every other reader treats it as truthy,
    /// so an uninitialized range degrades to `nil...nil` instead of
    /// CRuby's per-method TypeError.
    pub const UNINITIALIZED: u32 = 2;

    pub fn new(start: Value, end: Value, exclude_end: bool) -> Self {
        RangeInner {
            start,
            end,
            exclude_end: if exclude_end { 1 } else { 0 },
        }
    }

    pub fn new_uninit() -> Self {
        RangeInner {
            start: Value::nil(),
            end: Value::nil(),
            exclude_end: Self::UNINITIALIZED,
        }
    }

    pub fn is_initialized(&self) -> bool {
        self.exclude_end != Self::UNINITIALIZED
    }

    /// Fill in the fields from `Range#initialize`. Write barriers are the
    /// caller's responsibility (it holds the owning `Value`).
    pub fn initialize(&mut self, start: Value, end: Value, exclude_end: bool) {
        self.start = start;
        self.end = end;
        self.exclude_end = if exclude_end { 1 } else { 0 };
    }

    pub fn start(&self) -> Value {
        self.start
    }

    pub fn end(&self) -> Value {
        self.end
    }

    pub fn try_fixnum(&self) -> Option<(i64, i64)> {
        let start = self.start.try_fixnum()?;
        let mut end = self.end.try_fixnum()?;
        if !self.exclude_end() {
            end += 1
        }
        Some((start, end))
    }

    pub fn exclude_end(&self) -> bool {
        self.exclude_end != 0
    }

    pub(super) fn debug(&self, store: &Store) -> String {
        format!(
            "{}{}{}",
            self.start.debug(store),
            if self.exclude_end() { "..." } else { ".." },
            self.end.debug(store),
        )
    }

    pub(super) fn to_s(&self, store: &Store) -> String {
        // Matches CRuby: renders each endpoint with `to_s`, so a nil
        // endpoint (beginless/endless range) renders as an empty
        // string. `(nil..5).to_s == "..5"`, `(nil..nil).to_s == ".."`.
        format!(
            "{}{}{}",
            self.start.to_s(store),
            if self.exclude_end() { "..." } else { ".." },
            self.end.to_s(store),
        )
    }

    pub(super) fn inspect(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        // Matches CRuby: renders each endpoint with `inspect`, but a
        // single nil endpoint is elided (one-sided range). Only when
        // BOTH endpoints are nil does the output keep the literal
        // "nil..nil" so `(nil..nil).inspect` is distinguishable from
        // `(..).to_s`.
        let sep = if self.exclude_end() { "..." } else { ".." };
        if self.start.is_nil() && self.end.is_nil() {
            return format!("nil{sep}nil");
        }
        let start = if self.start.is_nil() {
            String::new()
        } else {
            self.start.inspect_inner(store, set)
        };
        let end = if self.end.is_nil() {
            String::new()
        } else {
            self.end.inspect_inner(store, set)
        };
        format!("{start}{sep}{end}")
    }
}
