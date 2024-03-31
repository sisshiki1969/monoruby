use super::*;

#[monoruby_object]
pub struct Enumerator(Value);

#[derive(Debug)]
pub struct EnumeratorInner {
    pub obj: Value,
    pub method: IdentId,
    internal: Option<Fiber>,
    pub proc: Proc,
    pub args: Box<Vec<Value>>,
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
    pub(crate) fn new(obj: Value, method: IdentId, proc: Proc, args: Vec<Value>) -> Self {
        Self {
            obj,
            method,
            internal: None,
            proc,
            args: Box::new(args),
            buffer: None,
        }
    }
}

impl Enumerator {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::ENUMERATOR));
        Self(val)
    }

    pub fn rewind(&mut self) {
        self.internal = Some(Fiber::from(self.proc));
        self.buffer = None;
    }
}

impl Enumerator {
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
        let mut internal = self.internal.unwrap();
        let v = internal.enum_yield_values(vm, globals, *self, Value::nil())?;
        if internal.is_terminated() {
            return Err(MonorubyErr::stopiterationerr(
                "iteration reached an end".to_string(),
            ));
        }
        Ok(Array::new(v))
    }
}

#[monoruby_object]
pub struct Generator(Value);

impl Generator {
    pub(crate) fn new(val: Value) -> Self {
        assert_eq!(val.ty(), Some(ObjKind::GENERATOR));
        Self(val)
    }
}

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
        let internal = Fiber::from(proc);
        Self {
            internal,
            proc,
            yielder: Value::yielder_object(),
        }
    }

    pub fn create_internal(&self) -> Fiber {
        Fiber::from(self.proc)
    }

    pub fn yielder(&self) -> Value {
        self.yielder
    }

    pub fn rewind(&mut self) {
        self.internal = self.create_internal();
    }
}
