use crate::executor::*;

//
// Proc class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Proc", PROC_CLASS);
    globals.define_builtin_class_func(PROC_CLASS, "new", new, 0);
    globals.define_builtin_func(PROC_CLASS, "call", call, -1);
}

/// ### Proc.new
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    if let Some(bh) = lfp.block() {
        let p = vm.generate_proc(globals, bh)?;
        Ok(p.into())
    } else {
        Err(MonorubyErr::create_proc_no_block())
    }
}

/// ### Proc#call
#[monoruby_builtin]
fn call(vm: &mut Executor, globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    vm.invoke_proc(globals, lfp.self_val(), &lfp.to_vec())
}

impl Executor {
    pub fn generate_proc(&mut self, globals: &mut Globals, bh: BlockHandler) -> Result<Proc> {
        if bh.try_proxy().is_some() {
            let outer_lfp = self.cfp().prev().unwrap().lfp();
            self.move_frame_to_heap(outer_lfp);
            let proc = Proc::new(globals.get_block_data(self.cfp(), bh));
            Ok(proc)
        } else if bh.try_proc() {
            Ok(bh.0.into())
        } else {
            unimplemented!()
        }
    }

    ///
    /// Generate a proc object for Enumerator.
    ///
    /// this method use current `self` for the `obj` field for the Enumerator.
    ///
    /// ### args
    /// - *method*: the method name to generate a proc.
    ///
    /// ### return
    /// - the generated proc object.
    ///
    pub fn generate_enumerator(&mut self, globals: &mut Globals, method: IdentId) -> Result<Value> {
        let func_id = globals.compile_script(
            format!(
                r#"
            self.{} do |*x|
              __enum_yield *x
            end
        "#,
                method
            ),
            "",
        )?;
        let func_data = globals.compile_on_demand(func_id).clone();
        let outer_lfp = self.move_frame_to_heap(self.cfp().lfp());
        let proc = Proc::from(outer_lfp, func_data);
        let self_val = outer_lfp.self_val();
        let e = Value::new_enumerator(self_val, method, proc);
        //eprintln!(
        //    "gen {:016x} self: {:?} obj: {:?}",
        //    e.get(),
        //    globals.to_s(proc.self_val()),
        //    globals.to_s(self_val)
        //);
        Ok(e)
    }

    /// Move the frame to heap.
    ///
    /// If the frame is already on the heap, do nothing.
    ///
    /// ### args
    /// - *lfp*: the address of the frame to move.
    ///
    /// ### return
    /// - the frame moved to the heap.
    ///
    pub fn move_frame_to_heap(&self, lfp: LFP) -> LFP {
        if self.within_stack(lfp) {
            unsafe {
                let mut cfp = lfp.cfp();
                let mut heap_lfp = lfp.move_to_heap();
                cfp.set_lfp(heap_lfp);
                if let Some(outer) = heap_lfp.outer() {
                    let outer_lfp = outer.lfp();
                    let outer = self.move_frame_to_heap(outer_lfp).outer_address();
                    heap_lfp.set_outer(Some(outer));
                }
                heap_lfp
            }
        } else {
            lfp
        }
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
