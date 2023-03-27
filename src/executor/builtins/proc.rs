use crate::executor::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(PROC_CLASS, "new", new, 0);
    globals.define_builtin_func(PROC_CLASS, "call", call, -1);
}

/// ### Proc.new
fn new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    if let Some(block_handler) = lfp.block() {
        vm.generate_proc(globals, block_handler)
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

/// ### Proc#call
fn call(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let self_ = lfp.self_val();
    let block_data = self_.as_proc();
    let res = vm.invoke_proc(globals, block_data, &arg.to_vec(len))?;
    Ok(res)
}

impl Executor {
    fn generate_proc(
        &mut self,
        globals: &mut Globals,
        block_handler: BlockHandler,
    ) -> Result<Value> {
        if let Some(_bh) = block_handler.try_proxy() {
            let lfp = self.cfp().prev().unwrap().lfp();
            unsafe {
                self.move_frame_to_heap(lfp);
            }
        }
        let block_data = self.get_block_data(globals, block_handler);
        Ok(Value::new_proc(block_data))
    }

    /// ## return
    /// - the address of outer in *lfp*.
    unsafe fn move_frame_to_heap(&self, lfp: LFP) -> DFP {
        if self.within_stack(lfp) {
            let mut cfp = lfp.cfp();
            let mut heap_lfp = lfp.move_to_heap();
            cfp.set_lfp(heap_lfp);
            if let Some(outer) = heap_lfp.outer() {
                let outer_lfp = outer.lfp();
                let outer = self.move_frame_to_heap(outer_lfp);
                heap_lfp.set_outer(Some(outer));
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

    #[test]
    fn proc2() {
        run_test(
            "
            a = 100
            p = nil
            q = nil
            3.times {
              p = Proc.new {
                3.times {
                  a+=1
                }
              }
              q = Proc.new {
                5.times {
                  a+=10
                }
              }
            }
            p.call
            q.call
            a
        ",
        );
    }

    #[test]
    fn proc_param() {
        run_test(
            "
            a = []
            p = Proc.new {|x|
                a << x
            }
            5.times(&p)
            a
        ",
        );
    }
}
