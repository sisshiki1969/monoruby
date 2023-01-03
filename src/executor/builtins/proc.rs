use crate::executor::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_singleton_func(PROC_CLASS, "new", new, 0);
    globals.define_builtin_func(PROC_CLASS, "call", call, -1);
}

/// ### Proc.new
extern "C" fn new(
    vm: &mut Executor,
    globals: &mut Globals,
    _self_val: Value,
    _arg: Arg,
    _len: usize,
    block: Option<Value>,
) -> Option<Value> {
    if let Some(block_handler) = block {
        vm.generate_proc(globals, block_handler)
    } else {
        globals.err_create_proc_no_block();
        None
    }
}

/// ### Proc#call
extern "C" fn call(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    arg: Arg,
    len: usize,
    _: Option<Value>,
) -> Option<Value> {
    let block_data = self_val.as_proc();
    let res = vm.invoke_proc(globals, block_data, &arg.to_vec(len))?;
    Some(res)
}

impl Executor {
    fn generate_proc(&mut self, globals: &mut Globals, block_handler: Value) -> Option<Value> {
        if let Some(_bh) = block_handler.try_fixnum() {
            let cfp = self.cfp.prev();
            let lfp = cfp.lfp();
            self.move_frame_to_heap(lfp);
        }
        crate::executor::op::_dump_stacktrace(self, globals);
        let block_data = globals.get_block_data(block_handler, self);
        Some(Value::new_proc(block_data))
    }

    /// ## return
    /// - the address of outer in *lfp*.
    fn move_frame_to_heap(&self, lfp: LFP) -> u64 {
        if self.within_stack(lfp) {
            let mut cfp = lfp.cfp_address();
            let len = LBP_SELF as usize + 8 * lfp.meta().reg_num as usize;
            let v = unsafe {
                std::slice::from_raw_parts((lfp.0 as usize + 8 - len) as *const u8, len)
                    .to_vec()
                    .into_boxed_slice()
            };
            let mut heap_lfp = LFP((Box::into_raw(v) as *mut u64 as usize + len - 8) as _);
            cfp.set_lfp(heap_lfp);
            let outer = heap_lfp.outer();
            if outer != 0 {
                let outer_lfp = LFP((outer + LBP_OUTER as u64) as _);
                let outer = self.move_frame_to_heap(outer_lfp);
                heap_lfp.set_outer(outer);
            }
            heap_lfp
        } else {
            lfp
        }
        .outer_address()
    }
}

#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn proc_new() {
        run_test_no_result_check("Proc.new {}");
        run_test_error("Proc.new");
        run_test(
            "
            a = 100
            p = Proc.new {|x, y|
                a += x / y
            }
            p.call(42, 7)
        ",
        );
        run_test(
            "
        a = 100
        p = nil
        1.times {
          p = Proc.new {
            3.times {
              a+=1
            }
          }
        }
        p.call
        a
        ",
        )
    }
}
