use crate::*;

#[derive(Debug)]
pub struct EnumeratorInner {
    kind: EnumeratorKind,
    buffer: Option<Value>,
}

#[derive(Debug)]
pub enum EnumeratorKind {
    Generator {
        internal: Value,
        yielder: Option<Value>,
    },
    Iterator {
        internal: Option<Value>,
        obj: Value,
        method: IdentId,
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
            EnumeratorKind::Iterator { internal, obj, .. } => {
                if let Some(internal) = internal {
                    internal.mark(alloc)
                }
                obj.mark(alloc)
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
            internal: Value::new_fiber(data),
            yielder: None,
        };
        Self { kind, buffer: None }
    }

    pub fn new_iterator(obj: Value, method: IdentId) -> Self {
        let kind = EnumeratorKind::Iterator {
            internal: None,
            obj,
            method,
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
            EnumeratorKind::Generator { internal, yielder } => match internal.as_fiber().state() {
                FiberState::Created => {
                    let y = Value::object(unsafe { crate::executor::YIELDER.unwrap().id() });
                    *yielder = Some(y);
                    let arg = Arg::from(&y);
                    internal.as_fiber_mut().init();
                    internal.as_fiber_mut().invoke_fiber(vm, globals, arg, 1)
                }
                FiberState::Suspended => internal.as_fiber_mut().resume_fiber(vm, yielder.unwrap()),
                FiberState::Terminated => Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                )),
            },
            EnumeratorKind::Iterator {
                internal,
                obj,
                method,
            } => {
                if internal.is_none() {
                    let func_id = globals.find_method(*obj, *method, false)?;
                    let func_data = globals.compile_on_demand(func_id).clone();
                    let block_data = BlockData::new(None, func_data);
                    let mut fiber = Value::new_fiber(block_data);
                    fiber.as_fiber_mut().init();
                    *internal = Some(fiber);
                }
                let mut internal = internal.unwrap();
                match internal.as_fiber().state() {
                    FiberState::Created => {
                        let nil = Value::nil();
                        let arg = Arg::from(&nil);
                        internal.as_fiber_mut().invoke_fiber(vm, globals, arg, 1)
                    }
                    FiberState::Suspended => internal.as_fiber_mut().resume_fiber(vm, Value::nil()),
                    FiberState::Terminated => Err(MonorubyErr::stopiterationerr(
                        "iteration reached an end".to_string(),
                    )),
                }
                //unimplemented!()
            }
        }
    }
}
