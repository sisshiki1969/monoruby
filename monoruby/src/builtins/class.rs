use super::*;

//
// Class class
//

pub fn gen_class_new_object() -> Box<InlineGen> {
    Box::new(gen_class_new(allocate_object))
}

pub(super) fn init(globals: &mut Globals) {
    // class methods
    globals.define_builtin_class_func_with(CLASS_CLASS, "new", class_new, 0, 1, false);
    // Override allocate on Class's singleton class so that Class.allocate
    // creates a proper (uninitialized) Class object instead of a plain object.
    globals.define_builtin_class_func(CLASS_CLASS, "allocate", class_allocate, 0);

    // instance methods
    globals.define_builtin_inline_func(
        CLASS_CLASS,
        "allocate",
        allocate,
        Box::new(inline_allocate),
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
    // hook method (no-op by default, overridden by startup.rb)
    globals.define_private_builtin_func(CLASS_CLASS, "inherited", class_noop_hook, 1);
}

/// No-op hook for Class#inherited (overridden by startup.rb).
#[monoruby_builtin]
fn class_noop_hook(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::nil())
}

/// ### Class.new
/// - new(superclass = Object) -> Class
/// - [NOT SUPPORTED] new(superclass = Object) {|klass| ... } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/s/new.html]
#[monoruby_builtin]
fn class_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let superclass = if lfp.try_arg(0).is_none() {
        None
    } else {
        Some(lfp.arg(0).expect_class(globals)?)
    };
    let superclass_val = superclass
        .unwrap_or_else(|| globals.store.object_class())
        .as_val();
    let m = globals.store.define_unnamed_class(superclass);
    let obj = m.as_val();
    if let Some(block) = lfp.block() {
        vm.module_eval(globals, m, block)?;
    }
    vm.invoke_method_inner(
        globals,
        IdentId::INHERITED,
        superclass_val,
        &[obj],
        None,
        None,
    )?;
    Ok(obj)
}

/// ### Class.allocate
/// - allocate -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn class_allocate(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    lfp.expect_no_block()?;
    let obj = globals.store.allocate_uninit_class();
    Ok(obj.as_val())
}

/// ### Class#new
///
/// - new(*args, **kw, &block) -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/new.html]
#[monoruby_builtin]
pub(super) fn new(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let obj =
        vm.invoke_method_inner(globals, IdentId::ALLOCATE, lfp.self_val(), &[], None, None)?;

    vm.invoke_method_inner(
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
fn superclass(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
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
fn allocate(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    match class_id {
        TRUE_CLASS | FALSE_CLASS | NIL_CLASS | SYMBOL_CLASS => {
            return Err(MonorubyErr::typeerr(&format!(
                "allocator undefined for {}",
                class_id.get_name(globals)
            )));
        }
        _ => {}
    }
    let obj = Value::object(class_id);
    Ok(obj)
}

/// allocate that raises "allocator undefined for ClassName".
/// Used for classes that cannot be instantiated (TrueClass, FalseClass, NilClass, Symbol, Integer, Float).
#[monoruby_builtin]
pub(super) fn undef_allocate(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    Err(MonorubyErr::typeerr(&format!(
        "allocator undefined for {}",
        class_id.get_name(globals)
    )))
}

pub(super) fn gen_class_new(
    f: extern "C" fn(Value) -> Value,
) -> impl Fn(
    &mut AbstractState,
    &mut AsmIr,
    &JitContext,
    &Store,
    CallSiteId,
    ClassId,
    Option<ClassId>,
) -> bool {
    move |state: &mut AbstractState,
          ir: &mut AsmIr,
          _: &JitContext,
          store: &Store,
          callid: CallSiteId,
          _: ClassId,
          _: Option<ClassId>| {
        let callsite = &store[callid];
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
        state.writeback_acc(ir);
        state.load(ir, recv, GP::Rdi);
        state.write_back_recv_and_callargs(ir, callsite);
        let using_xmm = state.get_using_xmm();
        let error = ir.new_error(state);
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
        state.def_rax2acc(ir, dst);
        true
    }
}

fn inline_allocate(
    state: &mut AbstractState,
    ir: &mut AsmIr,
    _: &JitContext,
    store: &Store,
    callid: CallSiteId,
    self_class: ClassId,
    _: Option<ClassId>,
) -> bool {
    let callsite = &store[callid];
    if !callsite.is_simple() {
        return false;
    }
    let CallSiteInfo { recv, dst, .. } = *callsite;
    let mut self_module = store[self_class].get_module();
    if let Some(origin) = self_module.is_singleton() {
        self_module = origin.as_class();
    }
    state.load(ir, recv, GP::Rdi);
    state.write_back_recv_and_callargs(ir, callsite);
    let using_xmm = state.get_using_xmm();
    ir.xmm_save(using_xmm);
    ir.inline(move |r#gen, _, _| {
        monoasm!( &mut r#gen.jit,
            movl rsi, (self_module.id().u32());
            movq rax, (allocate_object2);
            call rax;
        );
    });
    ir.xmm_restore(using_xmm);
    state.def_reg2acc_class(ir, GP::Rax, dst, self_module.id());
    true
}

extern "C" fn allocate_object(class_val: Value) -> Value {
    let class_id = class_val.as_class_id();
    Value::object(class_id)
}

extern "C" fn allocate_object2(class_val: Value, self_class: ClassId) -> Value {
    let class_id = class_val.as_class_id();
    debug_assert_eq!(class_id, self_class);
    //eprintln!(
    //    "allocate_object: class_id={:?}, self_class={:?}",
    //    class_id, self_class
    //);
    Value::object(self_class)
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
    fn inherited_callback() {
        run_test_once(
            r##"
        $res = []
        class A
          def self.inherited(subclass)
            $res << "inherited:#{subclass}"
          end
        end
        class B < A
        end
        $res
        "##,
        );
        run_test_once(
            r##"
        $res = []
        class A
          def self.inherited(subclass)
            $res << "inherited:#{subclass}"
          end
        end
        class B < A
        end
        class C < B
        end
        $res
        "##,
        );
    }

    #[test]
    fn inherited_callback_class_new() {
        run_test(
            r##"
        $res = []
        class A
          def self.inherited(subclass)
            $res << "inherited"
          end
        end
        Class.new(A)
        $res
        "##,
        );
    }

    #[test]
    fn class_new() {
        // Class.new creates an anonymous class whose superclass is Object
        run_test("Class.new.superclass");
        // Class.new with explicit superclass
        run_test(
            r#"
            class A; end
            Class.new(A).superclass
            "#,
        );
        // Instance of anonymous class
        run_test(
            r#"
            k = Class.new
            k.new.is_a?(k)
            "#,
        );
        // Methods defined on anonymous class
        run_test(
            r#"
            k = Class.new
            k.define_method(:foo) { 42 }
            k.new.foo
            "#,
        );
        // Inherits methods from superclass
        run_test(
            r#"
            class A
              def greet; "hello"; end
            end
            k = Class.new(A)
            k.new.greet
            "#,
        );
        // Class.new with non-class argument raises TypeError
        run_test_error("Class.new(42)");
        run_test_error("Class.new(:sym)");
    }

    #[test]
    fn class_new_with_block() {
        // Class.new with block yields the new class
        run_test(
            r#"
            k = Class.new { |c| c.define_method(:foo) { 42 } }
            k.new.foo
            "#,
        );
        // Block receives the anonymous class as argument
        run_test(
            r#"
            res = nil
            k = Class.new { |c| res = c }
            res == k
            "#,
        );
        // Class.new(superclass) with block
        run_test(
            r#"
            class A
              def greet; "hello"; end
            end
            k = Class.new(A) { def shout; "HI"; end }
            [k.new.greet, k.new.shout]
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

    #[test]
    fn alias_keyword() {
        run_test(
            r#"
            class Foo
              def bar; "bar"; end
              alias baz bar
            end
            Foo.new.baz
            "#,
        );
    }

    #[test]
    fn alias_keyword_with_symbol() {
        run_test(
            r#"
            class Foo
              def bar; "bar"; end
              alias :baz :bar
            end
            Foo.new.baz
            "#,
        );
        run_test(
            r#"
            class Foo
              def bar; "bar"; end
              alias :'baz' bar
            end
            Foo.new.baz
            "#,
        );
        run_test(
            r#"
            class Foo
              def bar; "bar"; end
              alias :"baz" :bar
            end
            Foo.new.baz
            "#,
        );
    }

    #[test]
    fn alias_keyword_interpolated_symbol() {
        run_test(
            r##"
            class Foo
              def bar; "bar"; end
              x = "baz"
              alias :"#{x}" bar
            end
            Foo.new.baz
            "##,
        );
        run_test(
            r##"
            class Foo
              def hello; "hello"; end
              alias greet :"hel#{"lo"}"
            end
            Foo.new.greet
            "##,
        );
        run_test(
            r##"
            class Foo
              def bar; "bar"; end
              x = "ba"
              y = "r"
              alias :"foo" :"#{x}#{y}"
            end
            Foo.new.foo
            "##,
        );
    }

    #[test]
    fn alias_global_var() {
        // Aliasing a global variable to a special variable used to be
        // unsupported, but Phase C of the GvarTable refactor made it work
        // the same way as CRuby.
        run_test(
            r#"
            "hello world" =~ /world/
            alias $MATCH $&
            $MATCH
            "#,
        );
        run_test(
            r#"
            $a = 42
            alias $b $a
            $b
            "#,
        );
        run_test(
            r#"
            $a = 10
            alias $b $a
            $b = 99
            [$a, $b]
            "#,
        );
    }

    #[test]
    fn def_operator_dmul() {
        run_test(
            r#"
            class Foo
              def **(other)
                "pow #{other}"
              end
            end
            Foo.new ** 3
            "#,
        );
    }

    //#[test]
    //fn def_operator_not() {
    //    // Parsing `def !` is now supported; verify it doesn't cause a parse error.
    //    run_test(
    //        r#"
    //        class Foo
    //          def !
    //            "negated"
    //          end
    //        end
    //        !Foo.new
    //        "#,
    //    );
    //}

    #[test]
    fn def_operator_unmatch() {
        // Parsing `def !~` is now supported; verify it doesn't cause a parse error.
        run_test(
            r#"
            class Foo
              def !~(other)
                "not match #{other}"
              end
            end
            Foo.new !~ /regex/
            "#,
        );
    }

    #[test]
    fn class_allocate() {
        // Class.allocate returns an instance of Class
        run_test("Class.allocate.class");
        // Class.allocate returns a Class object (is_a?(Class))
        run_test("Class.allocate.is_a?(Class)");
        // Normal class allocate still works
        run_test("String.allocate.class");
        run_test(
            r#"
            class Foo; end
            Foo.allocate.class
            "#,
        );
        // Class.allocate returns distinct objects
        run_test("Class.allocate.equal?(Class.allocate)");
        // Hash.allocate returns a Hash
        run_test("Hash.allocate.class");
        run_test("Hash.allocate.is_a?(Hash)");
        // Hash subclass allocate
        run_test(
            r#"
            class MyHash < Hash; end
            MyHash.allocate.class
            "#,
        );
        // Array.allocate returns an Array
        run_test("Array.allocate.class");
        // Array subclass allocate
        run_test(
            r#"
            class MyArray < Array; end
            MyArray.allocate.class
            "#,
        );
        // allocator undefined for immediate classes
        run_test_error("NilClass.allocate");
        run_test_error("TrueClass.allocate");
        run_test_error("FalseClass.allocate");
        run_test_error("Symbol.allocate");
        // String.allocate returns an empty String
        run_test("String.allocate.class");
        run_test("String.allocate");
        run_test(
            r#"
            class MyString < String; end
            MyString.allocate.class
            "#,
        );
        // Time.allocate returns a Time
        run_test("Time.allocate.class");
        // Range.allocate returns a Range
        run_test("Range.allocate.class");
        // Exception.allocate returns an Exception
        run_test("Exception.allocate.class");
        run_test(
            r#"
            class MyError < StandardError; end
            MyError.allocate.class
            "#,
        );
        // Regexp.allocate returns a Regexp
        run_test("Regexp.allocate.class");
        // IO.allocate returns an IO
        run_test("IO.allocate.class");
        // allocator undefined for special classes
        run_test_error("Proc.allocate");
        run_test_error("Method.allocate");
        run_test_error("UnboundMethod.allocate");
        run_test_error("Binding.allocate");
        run_test_error("Struct.allocate");
    }

    #[test]
    fn unmatch_operator() {
        // Default !~ (negation of =~)
        run_test(r#""hello" !~ /ell/"#);
        run_test(r#""hello" !~ /xyz/"#);
        // Override both =~ and !~
        run_test(
            r#"
            class Bar
              def =~(other)
                "matched"
              end
              def !~(other)
                "custom not match"
              end
            end
            [Bar.new =~ "x", Bar.new !~ "x"]
            "#,
        );
        // Override only =~; default !~ should negate it
        run_test(
            r#"
            class Baz
              def =~(other)
                true
              end
            end
            Baz.new !~ "x"
            "#,
        );
    }
}
