use super::*;

#[derive(Debug, Clone, PartialEq, Hash)]
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
