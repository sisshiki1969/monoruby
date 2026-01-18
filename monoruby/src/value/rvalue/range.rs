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
    pub fn new(start: Value, end: Value, exclude_end: bool) -> Self {
        RangeInner {
            start,
            end,
            exclude_end: if exclude_end { 1 } else { 0 },
        }
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
        format!(
            "{}{}{}",
            self.start.inspect(store),
            if self.exclude_end() { "..." } else { ".." },
            self.end.inspect(store),
        )
    }
}
