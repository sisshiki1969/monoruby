use crate::*;
use std::mem::ManuallyDrop;

#[derive(Debug)]
pub struct EnumeratorInner {
    kind: EnumeratorKind,
    buffer: Option<Value>,
}

#[derive(Debug)]
pub enum EnumeratorKind {
    Generator {
        internal: Box<ManuallyDrop<FiberInner>>,
        yielder: Option<Value>,
    },
}

impl Drop for EnumeratorInner {
    fn drop(&mut self) {
        //unsafe { ManuallyDrop::drop(&mut self.internal) };
    }
}

impl alloc::GC<RValue> for EnumeratorInner {
    fn mark(&self, alloc: &mut alloc::Allocator<RValue>) {
        match &self.kind {
            EnumeratorKind::Generator { internal, yielder } => {
                internal.mark(alloc);
                if let Some(v) = yielder {
                    v.mark(alloc)
                }
            }
        }
        if let Some(buf) = self.buffer {
            buf.mark(alloc)
        }
    }
}

impl EnumeratorInner {
    pub fn new_generator(data: BlockData) -> Self {
        let kind = EnumeratorKind::Generator {
            internal: Box::new(ManuallyDrop::new(FiberInner::new(data))),
            yielder: None,
        };
        Self { kind, buffer: None }
    }

    pub fn peek(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = self.buffer {
            Ok(v)
        } else {
            let v = self.yield_next(vm, globals)?;
            self.buffer = Some(v);
            Ok(v)
        }
    }

    pub fn next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = std::mem::take(&mut self.buffer) {
            Ok(v)
        } else {
            self.yield_next(vm, globals)
        }
    }

    fn yield_next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        match &mut self.kind {
            EnumeratorKind::Generator { internal, yielder } => match internal.state() {
                FiberState::Created => {
                    let y = Value::object(unsafe { crate::executor::YIELDER.unwrap().id() });
                    *yielder = Some(y);
                    internal.init();
                    let arg = Arg::from(&y);
                    internal.invoke_fiber(vm, globals, arg, 1)
                }
                FiberState::Suspended => internal.resume_fiber(vm, yielder.unwrap()),
                FiberState::Terminated => Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                )),
            },
        }
    }
}
