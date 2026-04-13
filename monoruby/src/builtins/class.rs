use super::*;

//
// Class class
//

/// Returns the JIT inliner for `Class#new`. The inliner reads the receiver
/// class's `alloc_func` from `ClassInfo` at JIT-compile time, embeds the raw
/// function pointer into the generated code, and calls it directly (ABI:
/// `extern "C" fn(ClassId, &mut Globals) -> Value`), matching the slow-path
/// `call_alloc_func` helper.
pub fn gen_class_new_object() -> Box<InlineGen> {
    Box::new(gen_class_new_inline)
}

pub(super) fn init(globals: &mut Globals) {
    // Class.allocate / Class#new on a class object both produce a fresh,
    // uninitialized Class. Install that as Class's alloc_func so it flows
    // through the unified `Class#allocate` path.
    globals.store[CLASS_CLASS].set_alloc_func(class_alloc_func);
    // class methods
    globals.define_builtin_class_func_with(CLASS_CLASS, "new", class_new, 0, 1, false);

    // instance methods. The default `Class#allocate` consults the receiver
    // class's `ClassInfo.alloc_func`, raising TypeError when None.
    globals.define_builtin_func(CLASS_CLASS, "allocate", allocate, 0);
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
    globals.define_builtin_func(CLASS_CLASS, "attached_object", attached_object, 0);
    // Class#initialize is private and raises TypeError for already-initialized classes.
    // Class.allocate creates an "uninitialized" class (no superclass, no name).
    globals.define_private_builtin_func_with(
        CLASS_CLASS,
        "initialize",
        class_initialize,
        0,
        1,
        false,
    );
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
    let superclass = if let Some(arg) = lfp.try_arg(0) {
        Some(expect_class_for_superclass(arg, globals)?)
    } else {
        None
    };
    let superclass_val = superclass
        .unwrap_or_else(|| globals.store.object_class())
        .as_val();
    let m = globals.store.define_unnamed_class(superclass);
    let obj = m.as_val();
    vm.invoke_method_inner(
        globals,
        IdentId::INHERITED,
        superclass_val,
        &[obj],
        None,
        None,
    )?;
    if let Some(block) = lfp.block() {
        vm.module_eval(globals, m, block)?;
    }
    Ok(obj)
}

/// Allocator installed on `Class` itself. Produces a new uninitialized
/// `Class` value (no name, no superclass), used by both `Class.allocate`
/// and the bootstrap path of `Class.new`.
pub(crate) extern "C" fn class_alloc_func(_class_id: ClassId, globals: &mut Globals) -> Value {
    globals.store.allocate_uninit_class().as_val()
}

/// Validate a value used as a superclass for `Class.new`/`Class#initialize`.
/// Raises TypeError with a CRuby-compatible message when the value is not a
/// real (non-singleton) Class.
fn expect_class_for_superclass(val: Value, globals: &Globals) -> Result<Module> {
    match val.is_class() {
        Some(class) if class.is_singleton().is_none() => Ok(class),
        _ => {
            let name = val.inspect(&globals.store);
            Err(MonorubyErr::typeerr(format!(
                "superclass must be an instance of Class (given an instance of {name})"
            )))
        }
    }
}

/// ### Class#attached_object
/// - attached_object -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/attached_object.html]
#[monoruby_builtin]
fn attached_object(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class = lfp.self_val().as_class();
    match class.is_singleton() {
        Some(attached) => Ok(attached),
        None => {
            let name = class.id().get_name(&globals.store);
            Err(MonorubyErr::typeerr(format!(
                "`{name}' is not a singleton class"
            )))
        }
    }
}

/// ### Class#initialize
/// - initialize(superclass = Object) -> Class
///
/// Private. Raises TypeError if the receiver has already been initialized
/// (e.g. any normal class) or if the requested superclass is `Class` itself.
/// monoruby treats a class created via `Class.allocate` as uninitialized: such
/// a class has neither a name nor a superclass set.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/initialize.html]
#[monoruby_builtin]
fn class_initialize(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_module = lfp.self_val().as_class();
    let class_id = self_module.id();
    let info = &globals.store[class_id];
    if info.get_name().is_some() || self_module.superclass().is_some() {
        return Err(MonorubyErr::typeerr("already initialized class"));
    }
    let superclass = if let Some(arg) = lfp.try_arg(0) {
        let sc = expect_class_for_superclass(arg, globals)?;
        if sc.id() == CLASS_CLASS {
            return Err(MonorubyErr::typeerr("can't make subclass of Class"));
        }
        sc
    } else {
        globals.store.object_class()
    };
    self_module.set_superclass(Some(superclass));
    Ok(lfp.self_val())
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
    // Class#new dispatches to the receiver's C-level allocator (`alloc_func`),
    // bypassing any user-defined Ruby `def self.allocate`. Matches CRuby's
    // `rb_class_alloc` semantics.
    let self_val = lfp.self_val();
    let class_id = self_val.as_class_id();
    let obj = call_alloc_func(globals, class_id)?;

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
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let class = lfp.self_val().as_class();
    match class.get_real_superclass() {
        Some(class) => Ok(class.into()),
        None => {
            // Distinguish uninitialized classes (from Class.allocate) from BasicObject.
            // An uninitialized class has neither a name nor a superclass.
            let info = &globals.store[class.id()];
            if info.get_name().is_none() && class.superclass().is_none() && class.id() != BASIC_OBJECT_CLASS {
                return Err(MonorubyErr::typeerr("uninitialized class"));
            }
            Ok(Value::nil())
        }
    }
}

/// ### Class#allocate
/// - allocate -> object
///
/// Default `allocate` for every class. Looks up `ClassInfo.alloc_func` on
/// the receiver class and invokes it. Raises TypeError when the class has
/// no allocator (None).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Class/i/allocate.html]
#[monoruby_builtin]
fn allocate(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_id = lfp.self_val().as_class_id();
    call_alloc_func(globals, class_id)
}

/// Look up the receiver class's `alloc_func` and invoke it; raise the
/// standard "allocator undefined" TypeError when None.
pub(crate) fn call_alloc_func(globals: &mut Globals, class_id: ClassId) -> Result<Value> {
    match globals.store[class_id].alloc_func() {
        Some(f) => Ok(f(class_id, globals)),
        None => Err(MonorubyErr::typeerr(format!(
            "allocator undefined for {}",
            class_id.get_name(globals)
        ))),
    }
}

/// JIT inliner for `Class#new`. Resolves the receiver class's `alloc_func`
/// at compile time and emits a direct C call, then falls through to the
/// usual cached-`initialize` dispatch used by the previous implementation.
///
/// Bails to the slow path (Rust `new`, which raises `TypeError`) when the
/// class has no allocator, mirroring `call_alloc_func`.
pub(super) fn gen_class_new_inline(
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
    // When `Class#new` is called on a class object, the receiver's class is
    // that class's singleton (metaclass). Unwrap it to the attached class
    // so we read the right `alloc_func`.
    let mut self_module = store[self_class].get_module();
    if let Some(origin) = self_module.is_singleton() {
        self_module = origin.as_class();
    }
    let class_id = self_module.id();

    let alloc_func = match store[class_id].alloc_func() {
        Some(f) => f,
        None => return false,
    };

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
            // alloc_func(class_id, &mut Globals) -> Value
            movl rdi, (class_id.u32());
            movq rsi, r12;
            movq rax, (alloc_func);
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
    fn class_new_block_runs_after_inherited_hook() {
        // `Class.new(sup) { block }` must fire `inherited` before yielding the block.
        run_test(
            r##"
        $res = []
        class D
          def self.inherited(sub)
            $res << "inherited:#{self}"
          end
        end
        k = Class.new(D) { $res << "block" }
        $res
        "##,
        );
    }

    #[test]
    fn class_allocate_superclass_raises() {
        // Uninitialized class (from Class.allocate) must raise TypeError on #superclass.
        run_test_error("Class.allocate.superclass");
        // But BasicObject#superclass must still return nil (not raise).
        run_test("BasicObject.superclass.inspect");
    }

    #[test]
    fn class_new_bypasses_user_allocate() {
        // User-level `def self.allocate` must NOT be called by Class#new
        // (CRuby uses an internal C-level allocator).
        run_test(
            r#"
        klass = Class.new do
          def self.allocate
            raise "allocate should not be called"
          end
        end
        instance = klass.new
        [instance.is_a?(klass), instance.class == klass]
        "#,
        );
        // But `klass.allocate` called directly still dispatches to user override.
        run_test_error(
            r#"
        klass = Class.new do
          def self.allocate
            raise "boom"
          end
        end
        klass.allocate
        "#,
        );
        // Built-in undef_allocate (e.g. Complex) must still prevent instantiation.
        run_test_error("Complex.new(1)");
        run_test_error("Rational.new(1)");
    }

    #[test]
    fn class_dup_preserves_singleton_class() {
        // `def self.foo` on the original must survive `.dup`.
        run_test(
            r#"
        klass = Class.new do
          def hello; "hello"; end
          def self.message; "text"; end
        end
        d = klass.dup
        [d.new.hello, d.message]
        "#,
        );
        // `extend`-ed module must remain in the dup's singleton-class ancestor chain.
        run_test(
            r#"
        mod = Module.new do
          def greet; "hi"; end
        end
        klass = Class.new
        klass.extend(mod)
        d = klass.dup
        d.greet
        "#,
        );
        // Superclass singleton methods are still inherited through the dup.
        run_test(
            r#"
        sup = Class.new do
          def self.message; "text"; end
          def hello; "hello"; end
        end
        klass = Class.new(sup)
        d = klass.dup
        [d.new.hello, d.message]
        "#,
        );
    }

    #[test]
    fn defined_scoped_const_via_self() {
        // `defined?(self::C)` must resolve the constant against the parent
        // expression (here: self, the receiver in the inherited hook).
        run_test(
            r##"
        $res = []
        parent = Class.new do
          def self.inherited(subclass)
            $res << defined?(self::C)
            $res << const_defined?(:C)
            $res << constants
          end
        end
        class parent::C < parent; end
        $res
        "##,
        );
        // `defined?` returns nil when the scoped constant is missing.
        run_test(
            r#"
        m = Module.new
        defined?(m::NOPE).inspect
        "#,
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
        // alias chain: $c -> $b -> $a
        run_test(
            r#"
            $a = 1
            alias $b $a
            alias $c $b
            $c = 42
            [$a, $b, $c]
            "#,
        );
    }

    #[test]
    fn special_gvar_pre_post_match() {
        // Pre-match ($`), matched ($&), post-match ($') backed by hooked
        // getters in GvarTable.
        run_test(
            r#"
            "hello world" =~ /world/
            [$`, $&, $']
            "#,
        );
        run_test(
            r#"
            "abcXYZdef" =~ /XYZ/
            [$`, $&, $']
            "#,
        );
        // After a non-matching regex, all three reset to nil.
        run_test(
            r#"
            "foo" =~ /X/
            [$`, $&, $']
            "#,
        );
    }

    #[test]
    fn special_gvar_nth_match() {
        // $1..$9 backed by hooked getter that parses the integer suffix
        // out of the IdentId at runtime.
        run_test(
            r#"
            "2024-04-11" =~ /(\d+)-(\d+)-(\d+)/
            [$1, $2, $3]
            "#,
        );
        // $~ is an alias-capable hooked variable (writing nil clears it).
        run_test(
            r#"
            "abc" =~ /(.)(.)(.)/
            alias $md $~
            $md.to_a
            "#,
        );
    }

    #[test]
    fn special_gvar_load_path_aliases() {
        // `$:` should alias `$LOAD_PATH` (both backed by the same hooked
        // getter in GvarTable::init_builtin_gvars).
        run_test(
            r#"
            $LOAD_PATH.class == Array
            "#,
        );
        run_test(
            r#"
            $LOAD_PATH.equal?($:)
            "#,
        );
        // $LOADED_FEATURES has the right class and reflects the loaded
        // files. (Note: monoruby reconstructs a fresh array on each
        // access, so `equal?` against `$"` is NOT tested here.)
        run_test(
            r#"
            $LOADED_FEATURES.class == Array
            "#,
        );
        run_test(
            r#"
            $".class == Array
            "#,
        );
    }

    #[test]
    fn special_gvar_program_name_alias() {
        // $PROGRAM_NAME is an alias of $0; assigning to one is visible
        // through the other.
        run_test(
            r#"
            $PROGRAM_NAME == $0
            "#,
        );
    }

    #[test]
    fn special_gvar_match_data_clear_via_nil_assignment() {
        // `$~ = nil` is the one supported write to a hooked match data
        // variable; it must clear $~, $&, $', $`, and $1..$N.
        run_test(
            r#"
            "abc" =~ /(b)/
            $~ = nil
            [$~, $&, $', $`, $1]
            "#,
        );
    }

    #[test]
    fn defined_hooked_special_vars() {
        // `defined?` consults GvarTable::defined_runtime, which treats
        // $~/$LOAD_PATH/$LOADED_FEATURES as always defined and treats
        // $&/$'/$`/$N as defined only when the underlying $~ has a
        // value (mirroring CRuby's BACK_REF / NTH_REF semantics).
        run_test(
            r#"
            [defined?($~), defined?($&), defined?($'), defined?($`),
             defined?($1), defined?($LOAD_PATH), defined?($:),
             defined?($LOADED_FEATURES), defined?($")]
            "#,
        );
        // After a successful match, $&/$'/$`/$N also become defined.
        run_test(
            r#"
            "abc" =~ /(b)/
            [defined?($&), defined?($'), defined?($`), defined?($1)]
            "#,
        );
        // But undefined names are still nil.
        run_test(
            r#"
            defined?($there_is_no_such_global_xxxxx)
            "#,
        );
    }

    #[test]
    fn write_to_readonly_special_var_raises() {
        // Hooked variables registered with `setter == None` raise on
        // assignment, both directly...
        run_test_error(r#"$& = "x""#);
        run_test_error(r#"$' = "x""#);
        run_test_error(r#"$` = "x""#);
        run_test_error(r#"$1 = "x""#);
        run_test_error(r#"$LOAD_PATH = []"#);
    }

    #[test]
    fn write_through_alias_to_readonly_raises() {
        // ...and via aliases. CRuby reports the alias name in the error
        // message; we don't necessarily match the message text, but we do
        // match the fact that it raises.
        run_test_error(
            r#"
            alias $ro_alias $&
            $ro_alias = "x"
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
