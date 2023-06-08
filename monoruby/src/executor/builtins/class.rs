use super::*;
use monoruby_attr::monoruby_builtin;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_func(CLASS_CLASS, "new", class_new, -1);
    globals.define_builtin_func(
        CLASS_CLASS,
        "new",
        new,
        -1,
        /*inline_class_new,
        analysis_class_new,*/
    );
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
    globals.define_builtin_func(CLASS_CLASS, "allocate", allocate, 0);
}

/// ### Class.new
/// - new(superclass = Object) -> Class
/// - [NOT SUPPORTED] new(superclass = Object) {|klass| ... } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/s/new.html]
#[monoruby_builtin]
fn class_new(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: LFP,
    arg: Arg,
    len: usize,
) -> Result<Value> {
    Executor::check_number_of_arguments(len, 0..=1)?;
    let superclass = if len == 0 {
        None
    } else {
        arg[0].expect_class(globals)?;
        Some(arg[0].as_class())
    };
    let obj = globals.new_unnamed_class(superclass);
    Ok(obj)
}

/// ### Class#new
/// - new(*args, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
///
/// !! We must call Object#initialize.
#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg, len: usize) -> Result<Value> {
    let obj = __allocate(vm, globals, lfp, arg, 0)?;
    vm.invoke_method2_if_exists(globals, IdentId::INITIALIZE, obj, arg, len)?;
    Ok(obj)
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/superclass.html]
#[monoruby_builtin]
fn superclass(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let class = lfp.self_val().as_class();
    Ok(class.superclass_value().unwrap_or_default())
}

/// ### Class#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: LFP,
    _arg: Arg,
    _len: usize,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let obj = Value::new_object(class_id);
    Ok(obj)
}

/*fn inline_class_new(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    method_info: &MethodInfo,
    ret: SlotId,
    _pc: BcPc,
    _deopt: DestLabel,
) {
    let MethodInfo { recv, .. } = method_info;
    gen.load_rdi(*recv);
    ctx.dealloc_xmm(ret);
    let using = ctx.get_xmm_using();
    gen.xmm_save(&using);
    monoasm!( &mut gen.jit,
        movq rax, (allocate_instance);
        call rax;
    );
    gen.xmm_restore(&using);
    //gen.jit_handle_error(ctx, pc);
    gen.store_rax(ret);
}

fn analysis_class_new(info: &mut SlotInfo, method_info: &MethodInfo, ret: SlotId) {
    let MethodInfo {
        recv, args, len, ..
    } = method_info;
    info.use_non_float(*recv);
    for r in args.0..args.0 + len {
        info.use_non_float(SlotId(r));
    }
    info.def_as(ret, false);
}

extern "C" fn allocate_instance(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::new_object(class_id)
}*/

#[cfg(test)]
mod test {
    use super::tests::*;

    #[test]
    fn test_class() {
        run_test("Time.superclass.to_s");
        run_test(
            r#"
        class A
          def f
            42
          end
        end
        A.new.f"#,
        );
        run_test(
            r#"
        class A
          class B
            def f
              42
            end
          end
        end
        A::B.new.f"#,
        );
    }

    #[test]
    fn initializer() {
        run_test(
            r#"
        class A
          attr_accessor :w, :x, :y, :z
          def initialize(x,y,z)
            @w = 42
            @x = x
            @y = y
            @z = z
          end
        end
        a = A.new(7, 11, 17)
        [a.w, a.x, a.y, a.z]
        "#,
        );
    }
}
