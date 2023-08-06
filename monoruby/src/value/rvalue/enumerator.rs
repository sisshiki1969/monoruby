use crate::*;

#[monoruby_object]
pub struct Enumerator(Value);

#[derive(Debug)]
pub struct EnumeratorInner {
    pub obj: Value,
    pub method: IdentId,
    internal: Option<Fiber>,
    pub proc: Proc,
    buffer: Option<Array>,
}

impl alloc::GC<RValue> for EnumeratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.obj.mark(alloc);
        if let Some(internal) = self.internal {
            internal.mark(alloc);
        }
        self.proc.mark(alloc);
        if let Some(buf) = self.buffer {
            buf.mark(alloc)
        }
    }
}

impl EnumeratorInner {
    pub(crate) fn new(obj: Value, method: IdentId, proc: Proc) -> Self {
        Self {
            obj,
            method,
            proc,
            internal: None,
            buffer: None,
        }
    }

    pub fn rewind(&mut self) {
        self.internal = Some(Fiber::new(self.proc));
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
        if self.internal.is_none() {
            self.rewind();
        }
        let (ary, is_return) =
            self.internal
                .unwrap()
                .enum_yield_values(vm, globals, Value::yielder_object())?;
        if is_return {
            Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            ))
        } else {
            Ok(ary)
        }
    }
}

#[monoruby_object]
pub struct Generator(Value);

#[derive(Debug)]
pub struct GeneratorInner {
    internal: Fiber,
    proc: Proc,
    yielder: Value,
}

impl alloc::GC<RValue> for GeneratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        self.internal.mark(alloc);
        self.proc.mark(alloc);
        self.yielder.mark(alloc);
    }
}

impl GeneratorInner {
    pub fn new(proc: Proc) -> Self {
        let internal = Fiber::new(proc);
        Self {
            internal,
            proc,
            yielder: Value::yielder_object(),
        }
    }

    pub fn create_internal(&self) -> Fiber {
        Fiber::new(self.proc)
    }

    pub fn yielder(&self) -> Value {
        self.yielder
    }

    pub fn rewind(&mut self) {
        self.internal = self.create_internal();
    }

    /*
    ///
    /// Yield next value from the enumerator.
    ///
    /// If the enumerator has been exhausted, return StopIteration error.
    ///
    fn yield_next_values(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Array> {
        let (ary, is_return) = self.internal.enum_yield_values(vm, globals, self.yielder)?;
        if is_return {
            Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            ))
        } else {
            Ok(ary)
        }
    }*/
}
