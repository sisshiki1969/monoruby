use super::*;

//
// Class class
//

pub fn gen_class_new_object() -> Box<InlineGen> {
    Box::new(gen_class_new(allocate_object))
}

pub(super) fn init(globals: &mut Globals) {
    let module = globals.store.classes[MODULE_CLASS].get_module();
    globals.define_builtin_class_by_str("Class", CLASS_CLASS, module, OBJECT_CLASS);
    globals.define_builtin_class_func_with(CLASS_CLASS, "new", class_new, 0, 1, false);
    globals.define_builtin_inline_func_with(
        CLASS_CLASS,
        "new",
        new,
        gen_class_new_object(),
        analysis::v_v_vv,
        0,
        0,
        true,
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
fn class_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    lfp.expect_no_block()?;
    let superclass = if lfp.try_arg(0).is_none() {
        None
    } else {
        Some(lfp.arg(0).expect_class(globals)?)
    };
    let obj = globals.store.classes.new_unnamed_class(superclass);
    Ok(obj)
}

/// ### Class#new
///
/// - new(*args, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
#[monoruby_builtin]
pub(super) fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let obj = vm.invoke_method_inner(globals, IdentId::ALLOCATE, lfp.self_val(), &[], None)?;
    vm.invoke_method_if_exists(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
    )?;
    Ok(obj)
}

/// ### Class#superclass
/// - superclass -> Class | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/superclass.html]
#[monoruby_builtin]
fn superclass(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class = lfp.self_val().as_class();
    match class.get_real_superclass() {
        Some(class) => Ok(class.into()),
        None => Ok(Value::nil()),
    }
}

/// ### Class#allocate
/// - allocate -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    let obj = Value::object(class_id);
    Ok(obj)
}

pub(super) fn gen_class_new(
    f: extern "C" fn(Value) -> Value,
) -> impl Fn(&mut AsmIr, &Store, &mut BBContext, CallSiteId, BytecodePtr) {
    move |ir: &mut AsmIr, store: &Store, bb: &mut BBContext, callid: CallSiteId, pc: BytecodePtr| {
        let callsite = &store[callid];
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            dst,
            ..
        } = *callsite;
        ir.writeback_acc(bb);
        ir.write_back_callargs_and_dst(bb, callsite);
        ir.stack2reg(recv, GP::Rdi);
        let using = bb.get_using_xmm();
        let error = ir.new_error(bb, pc);
        ir.inline(move |gen, labels| {
            let cached_version = gen.jit.data_i32(-1);
            let cached_funcid = gen.jit.data_i32(-1);
            let class_version = gen.class_version_label();
            let slow_path = gen.jit.label();
            let checked = gen.jit.label();
            let initialize = gen.jit.label();
            let exit = gen.jit.label();
            let error = labels[error];
            gen.xmm_save(using);
            monoasm!( &mut gen.jit,
                movq rax, (f);
                call rax;
                movq r15, rax; // r15 <- new instance
                movl rax, [rip + class_version];
                cmpl rax, [rip + cached_version];
                jne  slow_path;
                movl rax, [rip + cached_funcid];
            checked:
                testq rax, rax;
                jne  initialize;
            exit:
                movq rax, r15;
            );

            gen.xmm_restore(using);
            gen.handle_error(error);

            gen.jit.select_page(1);
            monoasm!( &mut gen.jit,
            initialize:
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, rax;
                movq rcx, r15;
                lea r8, [r14 - (crate::executor::jitgen::conv(args))];
                movl r9, (pos_num);
                subq rsp, 8;
                // TODO: Currently inline call does not support calling with block.
                xorq rax, rax;
                pushq rax;
                movq rax, (gen.method_invoker2);
                call rax;
                addq rsp, 16;
                testq rax, rax;
                jne  exit;
                xorq r15, r15;
                jmp  exit;
            slow_path:
                movq rdi, r12;
                movq rsi, r15;
                movq rax, (check_initializer);
                call rax;
                movl [rip + cached_funcid], rax;
                movl rdi, [rip + class_version];
                movl [rip + cached_version], rdi;
                jmp  checked;
            );
            gen.jit.select_page(0);
        });
        ir.rax2acc(bb, dst);
    }
}

extern "C" fn allocate_object(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::object(class_id)
}

extern "C" fn check_initializer(globals: &mut Globals, receiver: Value) -> Option<FuncId> {
    globals.check_method(receiver, IdentId::INITIALIZE)
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
