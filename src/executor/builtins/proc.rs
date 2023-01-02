use crate::*;

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
    let block_data = if let Some(block_handler) = block {
        globals.get_block_data(block_handler, vm)
    } else {
        globals.err_create_proc_no_block();
        return None;
    };
    let proc = Value::new_proc(block_data);
    Some(proc)
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

#[cfg(test)]
mod test {
    use crate::tests::*;

    #[test]
    fn proc_new() {
        run_test_no_result_check("Proc.new {}");
        run_test_error("Proc.new");
        run_test("a = 100; p = Proc.new {|x, y| a += x / y}; p.call(42, 7)");
    }
}
