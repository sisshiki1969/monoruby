use crate::*;

#[derive(Debug)]
pub struct EnumeratorInner {
    internal: Option<Fiber>,
    proc: Proc,
    yielder: Value,
    buffer: Option<Array>,
}

impl alloc::GC<RValue> for EnumeratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(internal) = self.internal {
            internal.mark(alloc)
        }
        self.proc.mark(alloc);
        self.yielder.mark(alloc);
        if let Some(buf) = self.buffer {
            buf.mark(alloc)
        }
    }
}

impl EnumeratorInner {
    pub(crate) fn new(proc: Proc) -> Self {
        let internal = Some(Fiber::new(proc));
        Self {
            internal,
            proc,
            yielder: Value::yielder_object(),
            buffer: None,
        }
    }

    pub(crate) fn create_internal(&self) -> Fiber {
        Fiber::new(self.proc)
    }

    pub(crate) fn yielder(&self) -> Value {
        self.yielder
    }

    pub fn rewind(&mut self) {
        self.internal = Some(self.create_internal());
        self.buffer = None;
    }

    ///
    /// Peek next yield value from the enumerator.
    ///
    pub fn peek(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(ary) = self.buffer {
            Ok(ary.peel())
        } else {
            let ary = self.yield_next_values(vm, globals)?;
            self.buffer = Some(ary);
            Ok(ary.peel())
        }
    }

    ///
    /// Get next yield value from the enumerator.
    ///
    pub fn next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let v = self.next_values(vm, globals)?.peel();
        Ok(v)
    }

    ///
    /// Get next yield value from the enumerator.
    ///
    pub fn next_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        if let Some(ary) = std::mem::take(&mut self.buffer) {
            Ok(ary)
        } else {
            self.yield_next_values(vm, globals)
        }
    }

    ///
    /// Yield next value from the enumerator.
    ///
    /// If the enumerator has been exhausted, return StopIteration error.
    ///
    fn yield_next_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        let mut internal = self.internal.unwrap();
        let (ary, is_return) = internal.enum_yield_values(vm, globals, self.yielder)?;
        if is_return {
            Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            ))
        } else {
            Ok(ary)
        }
    }
}
