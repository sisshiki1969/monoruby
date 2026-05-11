use super::*;

pub const AS_BEGIN_OFFSET: usize =
    RVALUE_OFFSET_KIND + std::mem::offset_of!(ArithmeticSequenceInner, begin);
pub const AS_END_OFFSET: usize =
    RVALUE_OFFSET_KIND + std::mem::offset_of!(ArithmeticSequenceInner, end);
pub const AS_STEP_OFFSET: usize =
    RVALUE_OFFSET_KIND + std::mem::offset_of!(ArithmeticSequenceInner, step);
pub const AS_EXCLUDE_END_OFFSET: usize =
    RVALUE_OFFSET_KIND + std::mem::offset_of!(ArithmeticSequenceInner, exclude_end);

#[derive(Debug, Clone, PartialEq)]
#[repr(C)]
pub struct ArithmeticSequenceInner {
    begin: Value,
    end: Value,
    step: Value,
    exclude_end: u32,
}

impl GC<RValue> for ArithmeticSequenceInner {
    fn mark(&self, alloc: &mut Allocator<RValue>) {
        self.begin.mark(alloc);
        self.end.mark(alloc);
        self.step.mark(alloc);
    }
}

impl ArithmeticSequenceInner {
    pub fn new(begin: Value, end: Value, step: Value, exclude_end: bool) -> Self {
        ArithmeticSequenceInner {
            begin,
            end,
            step,
            exclude_end: if exclude_end { 1 } else { 0 },
        }
    }

    pub fn begin(&self) -> Value {
        self.begin
    }

    pub fn end(&self) -> Value {
        self.end
    }

    pub fn step(&self) -> Value {
        self.step
    }

    pub fn exclude_end(&self) -> bool {
        self.exclude_end != 0
    }

    /// CRuby format: `((begin..end).step(step))` — or `((begin...end).step(step))`
    /// for exclusive end. `nil` endpoints are elided to the empty string
    /// (so `(0..).step(2)` prints as `((0..).step(2))`, not
    /// `((0..nil).step(2))`). `step.inspect` is used directly so a Float
    /// step prints as `0.5` rather than `(1/2)`.
    pub(super) fn inspect(&self, store: &Store, set: &mut HashSet<u64>) -> String {
        let sep = if self.exclude_end() { "..." } else { ".." };
        let lo = if self.begin.is_nil() {
            String::new()
        } else {
            self.begin.inspect_inner(store, set)
        };
        let hi = if self.end.is_nil() {
            String::new()
        } else {
            self.end.inspect_inner(store, set)
        };
        let step_part = if self.step.is_nil() {
            String::new()
        } else {
            self.step.inspect_inner(store, set)
        };
        format!("(({lo}{sep}{hi}).step({step_part}))")
    }
}
