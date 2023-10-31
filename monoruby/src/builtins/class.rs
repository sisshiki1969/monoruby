use super::*;
use monoruby_attr::monoruby_builtin;

//
// Class class
//

pub(super) fn init(globals: &mut Globals) {
    let module = MODULE_CLASS.get_module(globals);
    globals.define_builtin_class_by_str("Class", CLASS_CLASS, module, OBJECT_CLASS);
    globals.define_builtin_class_func(CLASS_CLASS, "new", class_new);
    globals.define_builtin_inline_func(CLASS_CLASS, "new", new, inline_class_new, analysis::v_v_vv);
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass);
    globals.define_builtin_func(CLASS_CLASS, "allocate", allocate);
}

/// ### Class.new
/// - new(superclass = Object) -> Class
/// - [NOT SUPPORTED] new(superclass = Object) {|klass| ... } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/s/new.html]
#[monoruby_builtin]
fn class_new(_vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    MonorubyErr::check_number_of_arguments_range(len, 0..=1)?;
    lfp.expect_no_block()?;
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
pub(super) fn new(vm: &mut Executor, globals: &mut Globals, lfp: LFP, arg: Arg) -> Result<Value> {
    let len = lfp.arg_len();
    let obj = __allocate(vm, globals, lfp, arg)?;
    vm.invoke_method_if_exists(globals, IdentId::INITIALIZE, obj, arg, len, lfp.block())?;
    Ok(obj)
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/superclass.html]
#[monoruby_builtin]
fn superclass(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let class = lfp.self_val().as_class();
    Ok(class.superclass_value().unwrap_or_default())
}

/// ### Class#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: LFP, _arg: Arg) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let obj = Value::object(class_id);
    Ok(obj)
}

fn inline_class_new(
    gen: &mut Codegen,
    ctx: &mut BBContext,
    callsite: &CallSiteInfo,
    pc: BcPc,
    _deopt: DestLabel,
) {
    let CallSiteInfo {
        recv,
        args,
        pos_num,
        ret,
        ..
    } = *callsite;
    gen.fetch_slots(ctx, &[recv]);
    gen.fetch_callargs(ctx, callsite);
    ctx.release(ret);
    let using = ctx.get_xmm_using();
    gen.xmm_save(&using);
    gen.load_rdi(recv);
    let cached_version = gen.jit.const_i32(-1);
    let cached_funcid = gen.jit.const_i32(-1);
    let class_version = gen.class_version;
    let slow_path = gen.jit.label();
    let checked = gen.jit.label();
    let exit = gen.jit.label();
    monoasm!( &mut gen.jit,
        movq rax, (allocate_instance);
        call rax;
        movq r15, rax; // r15 <- new instance
        movl rax, [rip + class_version];
        cmpl rax, [rip + cached_version];
        jne  slow_path;
        movl rax, [rip + cached_funcid];
    checked:
        testq rax, rax;
        je  exit;
    );
    monoasm!( &mut gen.jit,
        movq rdi, rbx;
        movq rsi, r12;
        movq rdx, rax;
        movq rcx, r15;
        lea r8, [r14 - (crate::executor::jitgen::conv(args))];
        movl r9, (pos_num);
        subq rsp, 16;
        movq [rsp], 0;
        movq rax, (gen.method_invoker2);
        call rax;
        addq rsp, 16;
        testq rax, rax;
        jne  exit;
        xorq r15, r15;
    exit:
        movq rax, r15;
    );
    gen.xmm_restore(&using);
    gen.jit_handle_error(ctx, pc);
    if let Some(ret) = ret {
        gen.store_rax(ret);
    }

    gen.jit.select_page(1);
    monoasm!( &mut gen.jit,
    slow_path:
        movq rdi, r12;
        movq rsi, r15;
        movq rax, (runtime::check_initializer);
        call rax;
        movl [rip + cached_funcid], rax;
        movl rdi, [rip + class_version];
        movl [rip + cached_version], rdi;
        jmp  checked;
    );
    gen.jit.select_page(0);
}

extern "C" fn allocate_instance(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::object(class_id)
}

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

    #[test]
    fn class_with_parents() {
        run_test(
            r#"
        class A
            class B
                class C
                end
                $x = C
            end
        end
        $x
        "#,
        );
        run_test("Math::DomainError");
    }
}
