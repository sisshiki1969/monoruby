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
}
