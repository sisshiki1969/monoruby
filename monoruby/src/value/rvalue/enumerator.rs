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
    pub(crate) fn new_generator(data: BlockData) -> Self {
        let internal = Some(Value::new_fiber(data.clone()));
        Self {
            internal,
            block: Box::new(data),
            yielder: Value::yielder_object(),
            buffer: None,
        }
    }

    pub fn peek(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = self.buffer {
            Ok(v)
        } else {
            let (v, is_return) = self.yield_next(vm, globals)?;
            if is_return {
                return Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ));
            }
            self.buffer = Some(v);
            Ok(v)
        }
    }

    pub fn next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        if let Some(v) = std::mem::take(&mut self.buffer) {
            Ok(v)
        } else {
            let (v, is_return) = self.yield_next(vm, globals)?;
            if is_return {
                Err(MonorubyErr::stopiterationerr(
                    "iteration reached an end".to_string(),
                ))
            } else {
                Ok(v)
            }
        }
    }

    fn yield_next(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<(Value, bool)> {
        let mut internal = self.internal.unwrap();
        let v = internal
            .as_fiber_mut()
            .enum_resume(vm, globals, self.yielder)?;
        let is_return = matches!(
            self.internal.unwrap().as_fiber().state(),
            FiberState::Terminated
        );
        Ok((v, is_return))
    }

    pub(crate) fn iterate(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        block_data: &BlockData,
    ) -> Result<Value> {
        let mut internal = Value::new_fiber((*self.block).clone());
        let len = vm.temp_len();
        vm.temp_push(internal);
        loop {
            match internal
                .as_fiber_mut()
                .enum_resume(vm, globals, self.yielder)
            {
                Ok(v) => {
                    if matches!(internal.as_fiber().state(), FiberState::Terminated) {
                        vm.temp_clear(len);
                        return Ok(v);
                    }
                    vm.invoke_block(globals, block_data, &[v])?;
                }
                Err(err) => {
                    vm.temp_clear(len);
                    return Err(err);
                }
            };
        }
    }
}
