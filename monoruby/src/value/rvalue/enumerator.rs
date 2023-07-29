use crate::*;

#[derive(Debug)]
pub struct EnumeratorInner {
    internal: Option<Value>,
    block: Box<BlockData>,
    yielder: Value,
    buffer: Option<Value>,
}

impl Drop for EnumeratorInner {
    fn drop(&mut self) {
        //unsafe { ManuallyDrop::drop(&mut self.internal) };
    }
}

impl alloc::GC<RValue> for EnumeratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        if let Some(internal) = self.internal {
            internal.mark(alloc)
        }
        self.block.mark(alloc);
        self.yielder.mark(alloc);
        if let Some(buf) = self.buffer {
            buf.mark(alloc)
        }
    }
}

impl EnumeratorInner {
    pub(crate) fn new_enumerator(data: BlockData) -> Self {
        let internal = Some(Value::new_fiber(data.clone()));
        Self {
            internal,
            block: Box::new(data),
            yielder: Value::yielder_object(),
            buffer: None,
        }
    }

    pub(crate) fn create_internal(&self) -> Value {
        Value::new_fiber((*self.block).clone())
    }

    pub(crate) fn yielder(&self) -> Value {
        self.yielder
    }

    ///
    /// Peek next yield value from the enumerator.
    ///
    pub fn peek(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = self.buffer {
            Ok(v)
        } else {
            let v = self.yield_next(vm, globals)?;
            self.buffer = Some(v);
            Ok(v)
        }
    }

    ///
    /// Get next yield value from the enumerator.
    ///
    pub fn next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = std::mem::take(&mut self.buffer) {
            Ok(v)
        } else {
            self.yield_next(vm, globals)
        }
    }

    ///
    /// Yield next value from the enumerator.
    ///
    /// If the enumerator has been exhausted, return StopIteration error.
    ///
    fn yield_next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let mut internal = self.internal.unwrap();
        let (v, is_return) = internal
            .as_fiber_mut()
            .enum_resume(vm, globals, self.yielder)?;
        if is_return {
            Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            ))
        } else {
            Ok(v)
        }
    }
}
