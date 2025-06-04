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

    pub(crate) fn eql(&self, other: &Self) -> bool {
        self.start.eql(&other.start)
            && self.end.eql(&other.end)
            && self.exclude_end() == other.exclude_end()
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
