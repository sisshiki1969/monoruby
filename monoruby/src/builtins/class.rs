use super::*;

//
// Class class
//

pub fn gen_class_new_object() -> Box<InlineGen> {
    Box::new(gen_class_new(allocate_object))
}

pub(super) fn init(globals: &mut Globals) {
    let module = globals.store[MODULE_CLASS].get_module();
    globals.define_builtin_class("Class", CLASS_CLASS, module, OBJECT_CLASS, ObjTy::CLASS);
    globals.define_builtin_class_func_with(CLASS_CLASS, "new", class_new, 0, 1, false);
    globals.define_builtin_inline_func(
        CLASS_CLASS,
        "allocate",
        allocate,
        Box::new(class_allocate),
        0,
    );
    globals.define_builtin_inline_funcs_with_kw(
        CLASS_CLASS,
        "new",
        &[],
        new,
        gen_class_new_object(),
        0,
        0,
        true,
        &[],
        true,
    );
    globals.define_builtin_func(CLASS_CLASS, "superclass", superclass, 0);
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
    let obj = globals.store.define_unnamed_class(superclass).as_val();
    Ok(obj)
}

/// ### Class#new
///
/// - new(*args, **kw, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
#[monoruby_builtin]
pub(super) fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let obj =
        vm.invoke_method_inner(globals, IdentId::ALLOCATE, lfp.self_val(), &[], None, None)?;

    vm.invoke_method_if_exists(
        globals,
        IdentId::INITIALIZE,
        obj,
        &lfp.arg(0).as_array(),
        lfp.block(),
        if let Some(kw) = lfp.try_arg(1)
            && let Some(kw) = kw.try_hash_ty()
            && !kw.is_empty()
        {
            Some(kw)
        } else {
            None
        },
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
) -> impl Fn(
    &mut AbstractContext,
    &mut AsmIr,
    &JitContext,
    &Store,
    &CallSiteInfo,
    ClassId,
    BytecodePtr,
) -> bool {
    move |bb: &mut AbstractContext,
          ir: &mut AsmIr,
          _: &JitContext,
          _: &Store,
          callsite: &CallSiteInfo,
          _: ClassId,
          pc: BytecodePtr| {
        if !callsite.is_simple() {
            return false;
        }
        let CallSiteInfo {
            recv,
            args,
            pos_num,
            dst,
            ..
        } = *callsite;
        bb.writeback_acc(ir);
        bb.load(ir, recv, GP::Rdi);
        bb.write_back_recv_and_callargs(ir, callsite);
        let using_xmm = bb.get_using_xmm();
        let error = ir.new_error(bb, pc);
        ir.xmm_save(using_xmm);
        ir.inline(move |r#gen, _, _| {
            let cached_version = r#gen.jit.data_i32(-1);
            let cached_funcid = r#gen.jit.data_i32(-1);
            let class_version = r#gen.class_version_label();
            let slow_path = r#gen.jit.label();
            let checked = r#gen.jit.label();
            let initialize = r#gen.jit.label();
            let exit = r#gen.jit.label();
            monoasm!( &mut r#gen.jit,
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

            r#gen.jit.select_page(1);
            monoasm!( &mut r#gen.jit,
            initialize:
                movq rdi, rbx;
                movq rsi, r12;
                movq rdx, rax;
                movq rcx, r15;
                lea r8, [r14 - (crate::executor::jitgen::conv(args))];
                movl r9, (pos_num);
                // TODO: Currently inline call does not support calling with block or keyword arguments.
                movq rax, (r#gen.method_invoker2);
                call rax;
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
            r#gen.jit.select_page(0);
        });
        ir.xmm_restore(using_xmm);
        ir.handle_error(error);
        bb.def_rax2acc(ir, dst);
        true
    }
}

fn class_allocate(
    bb: &mut AbstractContext,
    ir: &mut AsmIr,
    _: &JitContext,
    _: &Store,
    callsite: &CallSiteInfo,
    _: ClassId,
    _: BytecodePtr,
) -> bool {
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    bb.load(ir, recv, GP::Rdi);
    bb.write_back_recv_and_callargs(ir, callsite);
    let using_xmm = bb.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm!( &mut r#gen.jit,
            movq rax, (allocate_object);
            call rax;
        );
    });
    ir.xmm_restore(using_xmm);
    bb.def_rax2acc(ir, dst);
    true
}

extern "C" fn allocate_object(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::object(class_id)
}

extern "C" fn check_initializer(globals: &mut Globals, receiver: Value) -> Option<FuncId> {
    globals.check_method(receiver, IdentId::INITIALIZE)
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

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
        run_test_with_prelude(
            r##"
        $res = []
        C.new(1,2,3, a:1, b:2)
        $res
        "##,
            r##"
        class C
          def initialize(*a)
            $res << a.inspect
          end
        end
        "##,
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
