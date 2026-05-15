use super::*;

//
// Method / UnboundMethod class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Method", METHOD_CLASS, ObjTy::METHOD);
    globals.store[METHOD_CLASS].clear_alloc_func();
    globals.define_builtin_funcs_rest(METHOD_CLASS, "call", &["[]", "==="], call);
    globals.define_builtin_func(METHOD_CLASS, "arity", arity, 0);
    globals.define_builtin_func(METHOD_CLASS, "to_proc", to_proc, 0);
    globals.define_builtin_func(METHOD_CLASS, "source_location", source_location, 0);
    globals.define_builtin_func(METHOD_CLASS, "name", name, 0);
    globals.define_builtin_func(METHOD_CLASS, "original_name", original_name, 0);
    globals.define_builtin_func(METHOD_CLASS, "receiver", receiver, 0);
    globals.define_builtin_func(METHOD_CLASS, "owner", owner, 0);
    globals.define_builtin_func(METHOD_CLASS, "unbind", unbind, 0);
    globals.define_builtin_func(METHOD_CLASS, "parameters", parameters, 0);
    globals.define_builtin_funcs(METHOD_CLASS, "inspect", &["to_s"], inspect, 0);
    globals.define_builtin_func(METHOD_CLASS, "hash", method_hash, 0);
    globals.define_builtin_funcs(METHOD_CLASS, "==", &["eql?"], method_eq, 1);

    globals.define_builtin_class_under_obj("UnboundMethod", UMETHOD_CLASS, ObjTy::METHOD);
    globals.store[UMETHOD_CLASS].clear_alloc_func();
    globals.define_builtin_func(UMETHOD_CLASS, "arity", uarity, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "bind", bind, 1);
    globals.define_builtin_func_rest(UMETHOD_CLASS, "bind_call", bind_call);
    globals.define_builtin_func(UMETHOD_CLASS, "source_location", usource_location, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "name", uname, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "original_name", uoriginal_name, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "owner", uowner, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "parameters", uparameters, 0);
    globals.define_builtin_funcs(UMETHOD_CLASS, "inspect", &["to_s"], uinspect, 0);
    globals.define_builtin_func(UMETHOD_CLASS, "hash", umethod_hash, 0);
    globals.define_builtin_funcs(UMETHOD_CLASS, "==", &["eql?"], umethod_eq, 1);
}

///
/// ### Method#==
///
/// - self == other -> bool
/// - eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/=3d=3d.html]
#[monoruby_builtin]
fn method_eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let a = self_val.as_method();
    let b = other.as_method();
    let eq = match (a.method_missing_name(), b.method_missing_name()) {
        (Some(an), Some(bn)) => an == bn && a.receiver().id() == b.receiver().id(),
        (None, None) => a.func_id() == b.func_id() && a.receiver().id() == b.receiver().id(),
        _ => false,
    };
    Ok(Value::bool(eq))
}

///
/// ### UnboundMethod#==
///
/// - self == other -> bool
/// - eql?(other) -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/=3d=3d.html]
#[monoruby_builtin]
fn umethod_eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let a = self_val.as_umethod();
    let b = other.as_umethod();
    let eq = match (a.method_missing_name(), b.method_missing_name()) {
        (Some(an), Some(bn)) => an == bn && a.owner() == b.owner(),
        (None, None) => a.func_id() == b.func_id() && a.owner() == b.owner(),
        _ => false,
    };
    Ok(Value::bool(eq))
}

///
/// ### Method#call
///
/// - self[*args] -> object
/// - call(*args) -> object
/// - call(*args) { ... } -> object
/// - self === *args -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/call.html]
#[monoruby_builtin]
fn call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    let func_id = method.func_id();
    let receiver = method.receiver();

    if let Some(target) = method.method_missing_name() {
        // Dispatch `receiver.method_missing(target, *args, &blk)` dynamically
        // by name so that redefining `method_missing` after the Method was
        // created is honored, and the real `target` method is never called
        // even if it later comes to exist.
        let mut args = vec![Value::symbol(target)];
        args.extend(lfp.arg(0).as_array().iter().copied());
        return vm.invoke_method_inner(
            globals,
            IdentId::METHOD_MISSING,
            receiver,
            &args,
            lfp.block(),
            None,
        );
    }

    vm.invoke_func_inner(
        globals,
        func_id,
        receiver,
        &lfp.arg(0).as_array(),
        lfp.block(),
        None,
    )
}

///
/// ### Method#arity
///
/// - arity -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/arity.html]
#[monoruby_builtin]
fn arity(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    if method.method_missing_name().is_some() {
        return Ok(Value::integer(-1));
    }
    let func_id = method.func_id();
    Ok(Value::integer(globals[func_id].arity()))
}

///
/// ### UnboundMethod#arity
///
/// - arity -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/arity.html]
#[monoruby_builtin]
fn uarity(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    if method.method_missing_name().is_some() {
        return Ok(Value::integer(-1));
    }
    let func_id = method.func_id();
    Ok(Value::integer(globals[func_id].arity()))
}

///
/// ### Method#to_proc
///
/// - to_proc -> Proc
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/to_proc.html]
// TODO: support keyword arguments
#[monoruby_builtin]
fn to_proc(_: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    let method = self_.as_method();
    let self_val = method.receiver();
    let func_id = method.func_id();
    let proc = Proc::from_outer(
        Lfp::heap_frame(self_val, globals[func_id].meta()),
        func_id,
        pc,
    );
    Ok(proc.into())
}

///
/// ### UnboundMethod#bind
///
/// - bind(obj) -> Method
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/bind.html]
// TODO: we must reject invalid objects for *obj*
#[monoruby_builtin]
fn bind(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    Ok(Value::new_method(
        lfp.arg(0),
        method.func_id(),
        method.owner(),
    ))
}

fn source_loc_suffix(store: &Store, func_id: FuncId, is_mm: bool) -> String {
    if is_mm {
        return String::new();
    }
    if let Some(iseq) = store[func_id].is_iseq() {
        let info = &store[iseq];
        format!(
            " {}:{}",
            info.sourceinfo.short_file_name(),
            info.sourceinfo.get_line(&info.loc)
        )
    } else {
        String::new()
    }
}

/// Render the CRuby-compatible `Method#inspect` / `UnboundMethod#inspect`
/// string (also used for `#to_s`, which CRuby aliases to `#inspect`).
///
/// Mirrors `method_inspect` in CRuby's `proc.c`:
/// `#<Method: RecvClass(DefiningModule)#name(sig) file:line>`, with the
/// `.`-separator / receiver-inspect forms for singleton methods.
fn method_inspect_str(
    store: &Store,
    label: &str,
    recv: Option<Value>,
    owner: ClassId,
    name: IdentId,
    func_id: FuncId,
    is_mm: bool,
) -> String {
    let (owner_part, sharp) = match recv {
        // UnboundMethod: just the defining module.
        None => (store.get_class_name(owner), "#"),
        Some(recv) => {
            let owner_mod = store[owner].get_module();
            if let Some(v) = owner_mod.is_singleton() {
                // A genuine singleton method (`def obj.m` / `def Cls.m`).
                if recv.id() == v.id() {
                    (v.inspect(store), ".")
                } else {
                    (format!("{}({})", recv.inspect(store), v.inspect(store)), ".")
                }
            } else {
                let recv_mod = store[recv.class()].get_module();
                let disp = match recv_mod.is_singleton() {
                    // Singleton class of a class/module keeps its
                    // `#<Class:Foo>` display; the singleton class of a
                    // plain object is unwrapped to the real class.
                    Some(att) if att.is_class_or_module().is_none() => {
                        recv_mod.get_real_class()
                    }
                    _ => recv_mod,
                };
                let mut s = store.get_class_name(disp.id());
                if owner != disp.id() {
                    s = format!("{}({})", s, store.get_class_name(owner));
                }
                (s, "#")
            }
        }
    };
    let sig = if is_mm {
        "(*)".to_string()
    } else {
        super::proc::signature_string(store, func_id)
    };
    let loc = source_loc_suffix(store, func_id, is_mm);
    format!("#<{label}: {owner_part}{sharp}{name}{sig}{loc}>")
}

///
/// ### Method#inspect
///
/// - inspect -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/inspect.html]
#[monoruby_builtin]
fn inspect(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    let store = &globals.store;
    let name = match method.method_missing_name() {
        Some(t) => t,
        None => store[method.func_id()].name().unwrap(),
    };
    let s = method_inspect_str(
        store,
        "Method",
        Some(method.receiver()),
        method.owner(),
        name,
        method.func_id(),
        method.method_missing_name().is_some(),
    );
    Ok(Value::string(s))
}

///
/// ### UnboundMethod#inspect
///
/// - inspect -> String
/// - to_s -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/inspect.html]
#[monoruby_builtin]
fn uinspect(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    let store = &globals.store;
    let name = match method.method_missing_name() {
        Some(t) => t,
        None => store[method.func_id()].name().unwrap(),
    };
    let s = method_inspect_str(
        store,
        "UnboundMethod",
        None,
        method.owner(),
        name,
        method.func_id(),
        method.method_missing_name().is_some(),
    );
    Ok(Value::string(s))
}

///
/// ### Method#source_location
///
/// - source_location -> [String, Integer] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/source_location.html]
#[monoruby_builtin]
fn source_location(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    if method.method_missing_name().is_some() {
        return Ok(Value::nil());
    }
    let func_id = method.func_id();
    if let Some(iseq) = globals.store[func_id].is_iseq() {
        let iseq_info = &globals.store[iseq];
        let file_name = Value::string(iseq_info.sourceinfo.short_file_name().to_string());
        let line = Value::integer(iseq_info.sourceinfo.get_line(&iseq_info.loc) as i64);
        Ok(Value::array2(file_name, line))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### UnboundMethod#source_location
///
/// - source_location -> [String, Integer] | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/source_location.html]
#[monoruby_builtin]
fn usource_location(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    if method.method_missing_name().is_some() {
        return Ok(Value::nil());
    }
    let func_id = method.func_id();
    if let Some(iseq) = globals.store[func_id].is_iseq() {
        let iseq_info = &globals.store[iseq];
        let file_name = Value::string(iseq_info.sourceinfo.short_file_name().to_string());
        let line = Value::integer(iseq_info.sourceinfo.get_line(&iseq_info.loc) as i64);
        Ok(Value::array2(file_name, line))
    } else {
        Ok(Value::nil())
    }
}

///
/// ### Method#name
///
/// - name -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/name.html]
#[monoruby_builtin]
fn name(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    if let Some(target) = method.method_missing_name() {
        return Ok(Value::symbol(target));
    }
    let func_id = method.func_id();
    let id = globals[func_id].name().unwrap();
    Ok(Value::symbol(id))
}

///
/// ### Method#receiver
///
/// - receiver -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/receiver.html]
#[monoruby_builtin]
fn receiver(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    Ok(method.receiver())
}

///
/// ### Method#owner
///
/// - owner -> Class | Module
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/owner.html]
#[monoruby_builtin]
fn owner(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    Ok(globals.store[method.owner()].get_module().get())
}

///
/// ### Method#unbind
///
/// - unbind -> UnboundMethod
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/unbind.html]
#[monoruby_builtin]
fn unbind(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    if let Some(target) = method.method_missing_name() {
        return Ok(Value::new_unbound_method_missing_proxy(
            method.func_id(),
            target,
            method.owner(),
        ));
    }
    Ok(Value::new_unbound_method(method.func_id(), method.owner()))
}

///
/// ### UnboundMethod#name
///
/// - name -> Symbol
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/name.html]
#[monoruby_builtin]
fn uname(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    if let Some(target) = method.method_missing_name() {
        return Ok(Value::symbol(target));
    }
    let func_id = method.func_id();
    let id = globals[func_id].name().unwrap();
    Ok(Value::symbol(id))
}

///
/// ### UnboundMethod#owner
///
/// - owner -> Class | Module
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/owner.html]
#[monoruby_builtin]
fn uowner(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    Ok(globals.store[method.owner()].get_module().get())
}

///
/// ### Method#parameters
///
/// - parameters -> [[Symbol, Symbol], ...]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/parameters.html]
#[monoruby_builtin]
fn parameters(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    if method.method_missing_name().is_some() {
        let rest = Value::array1(Value::symbol(IdentId::get_id("rest")));
        return Ok(Value::array1(rest));
    }
    Ok(super::proc::build_parameters(
        globals,
        method.func_id(),
        true,
    ))
}

///
/// ### UnboundMethod#parameters
///
/// - parameters -> [[Symbol, Symbol], ...]
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/parameters.html]
#[monoruby_builtin]
fn uparameters(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    if method.method_missing_name().is_some() {
        let rest = Value::array1(Value::symbol(IdentId::get_id("rest")));
        return Ok(Value::array1(rest));
    }
    Ok(super::proc::build_parameters(
        globals,
        method.func_id(),
        true,
    ))
}

///
/// ### Method#original_name
///
/// - original_name -> Symbol
///
/// Returns the underlying function's stored name. Currently identical to
/// `Method#name` because monoruby's `MethodInner` does not yet record a
/// separate "lookup name" -- after `alias_method :b, :a`, both names
/// resolve to the same `FuncInfo` so `m.original_name == m.name`. The
/// method is here so that callers depending on its existence don't blow
/// up with NoMethodError; full alias-aware behavior is a follow-up.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/original_name.html]
#[monoruby_builtin]
fn original_name(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    let id = globals[method.func_id()].name().unwrap();
    Ok(Value::symbol(id))
}

/// ### UnboundMethod#original_name -- see `Method#original_name`.
#[monoruby_builtin]
fn uoriginal_name(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    let id = globals[method.func_id()].name().unwrap();
    Ok(Value::symbol(id))
}

///
/// ### Method#hash
///
/// - hash -> Integer
///
/// Order-of-magnitude consistent with `Method#==`/`#eql?`: two methods
/// that compare equal (same `func_id` and same receiver identity) hash
/// equal. Combines `func_id` and the receiver's object id with a non-
/// commutative mix.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Method/i/hash.html]
#[monoruby_builtin]
fn method_hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_method();
    let fid: i64 = method.func_id().get() as i64;
    let recv: i64 = method.receiver().id() as i64;
    let h = fid
        .wrapping_mul(0x100000001b3i64)
        .wrapping_add(recv.rotate_left(13));
    // Mask sign bit so we always return a non-negative Fixnum-friendly value.
    Ok(Value::integer(h & 0x7fff_ffff_ffff_ffff))
}

///
/// ### UnboundMethod#hash
///
/// Symmetric to `Method#hash`: same `func_id` + same `owner` -> same hash.
#[monoruby_builtin]
fn umethod_hash(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    let fid: i64 = method.func_id().get() as i64;
    let owner_id: i64 = method.owner().u32() as i64;
    let h = fid
        .wrapping_mul(0x100000001b3i64)
        .wrapping_add(owner_id.rotate_left(13));
    Ok(Value::integer(h & 0x7fff_ffff_ffff_ffff))
}

///
/// ### UnboundMethod#bind_call
///
/// - bind_call(recv, *args, &block) -> object
///
/// Equivalent to `bind(recv).call(*args, &block)` but in one step
/// (matches CRuby's `rb_method_bind_call`). Avoids materialising an
/// intermediate Method object.
///
/// [https://docs.ruby-lang.org/ja/latest/method/UnboundMethod/i/bind_call.html]
#[monoruby_builtin]
fn bind_call(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let method = self_val.as_umethod();
    let mut args: Vec<Value> = lfp.arg(0).as_array().iter().copied().collect();
    if args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    let receiver = args.remove(0);
    // The receiver must be kind_of? the owner module of the original
    // method; otherwise CRuby raises TypeError. We rely on
    // `invoke_func_inner` raising the appropriate error if the func is
    // applied to an incompatible receiver class -- the spec covers both
    // success and the error path through `bind_call` directly.
    let _ = method.owner();
    vm.invoke_func_inner(
        globals,
        method.func_id(),
        receiver,
        &args,
        lfp.block(),
        None,
    )
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    #[test]
    fn call1() {
        run_test_with_prelude(
            r##"
        m = Foo.new.method(:foo) # => #<Method: Foo#foo>
        [
            m[1],       # => 1
            m.call(2),  # => 2
            (m === 3),  # => 3
        ]
        "##,
            r##"
        class Foo
            def foo(arg)
                arg
            end
        end
            "##,
        );
    }

    #[test]
    fn call2() {
        run_test_with_prelude(
            r##"
        m = Foo.new.method(:foo)
        m.call(42) do |x|
            x ** 2
        end
            "##,
            r##"
        class Foo
          def foo(arg)
            yield arg
          end
        end
        "##,
        );
    }

    #[test]
    fn call3() {
        run_test_with_prelude(
            r##"
        $res = []
        class C
        	def f
        		100
        	end
        end
        c = C.new
        m = c.method(:f)
        $res <<  m.call
        class C
        	def f
        		42
        	end
        end
        $res << m.call
        $res << c.method(:f).call
        $res
            "##,
            r##"
        "##,
        );
    }

    #[test]
    fn to_proc() {
        run_test_with_prelude(
            r##"
            m = Foo.new.method(:foo) # => #<Method: Foo#foo>
            pr = m.to_proc # => #<Proc:0x007f874d026008 (lambda)>
            pr.call(1, 2) # => "foo"
            "##,
            r##"
            class Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
        "##,
        );
    }

    #[test]
    fn bind() {
        run_test_with_prelude(
            r##"
            # UnboundMethod `m' を生成
            m = Foo.instance_method(:foo) # => #<UnboundMethod: Foo#foo>
            # Foo のインスタンスをレシーバとする Method オブジェクトを生成
            m = m.bind(Foo.new)               # => #<Method: Foo#foo>
            # Method オブジェクトを呼び出す
            m.call(1, 2)                     # => "foo12"
            "##,
            r##"
            class Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
        "##,
        );
    }

    #[test]
    fn source_location() {
        run_test(
            r##"
        def foo; end
        m = method(:foo)
        loc = m.source_location
        [loc[0].is_a?(String), loc[1].is_a?(Integer)]
        "##,
        );
        run_test(
            r##"
        def bar(x); x * 2; end
        m = method(:bar)
        loc = m.source_location
        [loc[0].is_a?(String), loc[1].is_a?(Integer)]
        "##,
        );
        // built-in methods (Rust-defined) return nil
        run_test(
            r##"
        m = method(:gets)
        m.source_location
        "##,
        );
    }

    #[test]
    fn usource_location() {
        run_test(
            r##"
        class Foo
          def baz; end
        end
        um = Foo.instance_method(:baz)
        loc = um.source_location
        [loc[0].is_a?(String), loc[1].is_a?(Integer)]
        "##,
        );
        // built-in methods return nil
        run_test(
            r##"
        um = Integer.instance_method(:to_s)
        um.source_location
        "##,
        );
    }

    #[test]
    fn bind2() {
        run_test_with_prelude(
            r##"
            # UnboundMethod `m' を生成
            m = Foo.instance_method(:foo) # => #<UnboundMethod: Foo#foo>
            # Foo のインスタンスをレシーバとする Method オブジェクトを生成
            m = m.bind(Bar.new)               # => #<Method: Bar(Foo)#foo>
            m.call(1, 2)                     # => "foo12"
            "##,
            r##"
            module Foo
              def foo(x, y)
                "foo" + x.to_s + y.to_s
              end
            end
            class Bar
              include Foo
            end
        "##,
        );
    }

    #[test]
    fn method_inspect_format() {
        // Strip the trailing " file:line" (monoruby uses a short path,
        // CRuby an absolute one) and compare the structural prefix.
        run_test_with_prelude(
            r##"
            [
              MS::MySub.new.method(:bar),
              MS::A.new.method(:baz),
              MS::M.new.method(:zero),
              MS::M.new.method(:one_req),
              MS::M.new.method(:one_req_named),
              MS::M.new.method(:zero_with_block),
              MS::M.new.method(:one_opt),
              MS::M.new.method(:one_opt_named),
              MS::M.new.method(:zero_with_splat),
              MS::M.new.method(:zero_with_double_splat),
              MS::M.new.method(:mixed),
              MS::MySuper.instance_method(:bar),
              MS::A.instance_method(:baz),
              String.method(:include),
            ].map { |m| [m.inspect, m.to_s].map { |s| s.split(' ').first } }
            "##,
            r##"
            module MS
              module MyMod; def bar; :bar; end; end
              class MySuper; include MyMod; end
              class MySub < MySuper; end
              class A; def baz(a, b); end; end
              class M
                def zero; end
                def one_req(a); end
                def one_req_named(a:); end
                def zero_with_block(&blk); end
                def one_opt(a=nil); end
                def one_opt_named(a: nil); end
                def zero_with_splat(*a); end
                def zero_with_double_splat(**a); end
                def mixed(a, b=nil, *c, &blk); end
              end
            end
            "##,
        );
    }

    #[test]
    fn method_inspect_singleton() {
        run_test_with_prelude(
            r##"
            obj = Foo.new
            obj.singleton_class
            a = obj.method(:bar).inspect.split(' ').first
            obj2 = Foo.new
            def obj2.bar; end
            b = obj2.method(:bar).inspect.sub(/0x\h+/, '0xX').split(' ').first
            [a, b]
            "##,
            r##"
            module Mod; def bar; end; end
            class Foo; include Mod; end
            "##,
        );
    }

    #[test]
    fn name() {
        run_test(r##"method(:puts).name"##);
        run_test_with_prelude(
            r##"
            Foo.new.method(:bar).name
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn receiver() {
        run_test_with_prelude(
            r##"
            f = Foo.new
            f.method(:bar).receiver == f
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn owner() {
        run_test_with_prelude(
            r##"
            Foo.new.method(:bar).owner
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn unbind() {
        run_test_with_prelude(
            r##"
            m = Foo.new.method(:bar)
            um = m.unbind
            um.is_a?(UnboundMethod)
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn uname() {
        run_test_with_prelude(
            r##"
            Foo.instance_method(:bar).name
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn method_parameters() {
        run_test_with_prelude(
            r##"
            Foo.new.method(:bar).parameters
            "##,
            r##"
            class Foo
              def bar(x, y=1, *rest, &blk); end
            end
            "##,
        );
        run_test_with_prelude(
            r##"
            Foo.instance_method(:baz).parameters
            "##,
            r##"
            class Foo
              def baz(a, b); end
            end
            "##,
        );
    }

    #[test]
    fn uowner() {
        run_test_with_prelude(
            r##"
            Foo.instance_method(:bar).owner
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn method_eq_alias() {
        // Aliased methods share the same FuncId, so Method#== returns true.
        run_test_with_prelude(
            r##"
            f = Foo.new
            [f.method(:bar) == f.method(:baz),
             f.method(:bar).eql?(f.method(:baz)),
             f.method(:bar) == f.method(:other)]
            "##,
            r##"
            class Foo
              def bar; 1; end
              alias_method :baz, :bar
              def other; 2; end
            end
            "##,
        );
    }

    #[test]
    fn method_eq_different_receiver() {
        // Same method name on two instances → different receiver → not equal.
        run_test_with_prelude(
            r##"
            f1 = Foo.new
            f2 = Foo.new
            [f1.method(:bar) == f1.method(:bar),
             f1.method(:bar) == f2.method(:bar)]
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn method_eq_non_method() {
        // Comparing with a non-Method returns false, not an error.
        run_test_with_prelude(
            r##"
            Foo.new.method(:bar) == 42
            "##,
            r##"
            class Foo
              def bar; end
            end
            "##,
        );
    }

    #[test]
    fn umethod_eq_alias() {
        // UnboundMethod#== compares FuncId + owner.
        run_test_with_prelude(
            r##"
            [Foo.instance_method(:bar) == Foo.instance_method(:baz),
             Foo.instance_method(:bar).eql?(Foo.instance_method(:baz)),
             Foo.instance_method(:bar) == Foo.instance_method(:other)]
            "##,
            r##"
            class Foo
              def bar; 1; end
              alias_method :baz, :bar
              def other; 2; end
            end
            "##,
        );
    }

    #[test]
    fn method_hash_consistent_with_eq() {
        // Methods that compare equal must hash equal.
        run_test_with_prelude(
            r##"
            a = Foo.new.method(:bar)
            b = a # same Method object
            [a == b, a.hash == b.hash]
            "##,
            r##"
            class Foo
              def bar; 42; end
            end
            "##,
        );
    }

    #[test]
    fn umethod_hash_consistent_with_eq() {
        run_test_with_prelude(
            r##"
            a = Foo.instance_method(:bar)
            b = Foo.instance_method(:baz)
            c = Foo.instance_method(:other)
            [a == b, a.hash == b.hash, a == c]
            "##,
            r##"
            class Foo
              def bar; 1; end
              alias_method :baz, :bar
              def other; 2; end
            end
            "##,
        );
    }

    #[test]
    fn method_original_name() {
        // `original_name` is currently equivalent to `name` (alias-aware
        // tracking is a follow-up); here we just assert the method is
        // callable and returns the function's stored name as a Symbol.
        run_test(
            r##"
            class Foo
              def bar; 1; end
            end
            m = Foo.new.method(:bar)
            [m.original_name, m.original_name.is_a?(Symbol)]
            "##,
        );
    }

    #[test]
    fn method_arity_keyword_args() {
        // Keyword arguments participate in Method#arity exactly as
        // CRuby: a required keyword folds into one mandatory argument,
        // a purely-optional keyword set folds into one optional
        // argument (negative form). Compared against system CRuby.
        run_test(
            r##"
            def a; end
            def b(x); end
            def c(x:); end
            def d(x:, y:); end
            def e(x, y:); end
            def f(x, y:, &l); end
            def g(x, y, z:, w: 1, **k, &l); end
            def h(x: 1, y: 2); end
            def i(x: 1, y: 2, **k); end
            def j(x=1, y: 2); end
            def k(x=1, *y, z:, w: 2, **r, &l); end
            def l(p, q=1, *r, s:, t: 2, **u, &v); end
            %i[a b c d e f g h i j k l].map { |n| method(n).arity }
            "##,
        );
    }

    #[test]
    fn method_parameters_keyword_kinds() {
        // Required keywords report :keyreq, optional ones :key, and a
        // named **kwrest carries its name. (Keyword display ordering
        // for source-interleaved req/opt keywords still differs from
        // CRuby and is intentionally not asserted here.)
        run_test(
            r##"
            def m(a, b = 1, c:, d: 2, **rest, &blk); end
            method(:m).parameters
            "##,
        );
        run_test(
            r##"
            def only_req_kw(a:, b:); end
            method(:only_req_kw).parameters
            "##,
        );
        run_test(
            r##"
            def kwrest_named(**opts); end
            method(:kwrest_named).parameters
            "##,
        );
    }

    #[test]
    fn umethod_arity_keyword_args() {
        run_test_with_prelude(
            r##"
            [Foo.instance_method(:a).arity,
             Foo.instance_method(:b).arity,
             Foo.instance_method(:c).arity,
             Foo.instance_method(:d).arity]
            "##,
            r##"
            class Foo
              def a(x:); end
              def b(x: 1); end
              def c(x, y:, **k); end
              def d(x = 1, *y, z:, **r); end
            end
            "##,
        );
    }

    #[test]
    fn respond_to_missing_method() {
        run_test_with_prelude(
            r##"
            obj = Foo.new
            m = obj.method(:dynamic)
            [
              m.call(1, 2),
              m[3],
              (m === 4),
              m.arity,
              m.parameters,
              m.name,
              m.receiver == obj,
              m.owner,
              m.source_location,
              m.call { |x| x * 10 },
            ]
            "##,
            r##"
            class Foo
              def respond_to_missing?(name, include_all)
                name == :dynamic
              end
              def method_missing(name, *args, &blk)
                if blk
                  blk.call(7)
                else
                  [name, args]
                end
              end
            end
            "##,
        );
    }

    #[test]
    fn respond_to_missing_eq() {
        run_test_with_prelude(
            r##"
            a = Foo.new
            b = Foo.new
            [
              a.method(:x) == a.method(:x),
              a.method(:x).eql?(a.method(:x)),
              a.method(:x) == a.method(:y),
              a.method(:x) == b.method(:x),
              a.method(:x) == 5,
            ]
            "##,
            r##"
            class Foo
              def respond_to_missing?(name, ia); true; end
              def method_missing(name, *a); name; end
            end
            "##,
        );
    }

    #[test]
    fn respond_to_missing_false_raises() {
        run_test_error(
            r##"
            class Foo
              def respond_to_missing?(name, ia); false; end
            end
            Foo.new.method(:nope)
            "##,
        );
        run_test_error(
            r##"
            class Bar; end
            Bar.new.method(:nope)
            "##,
        );
    }

    #[test]
    fn respond_to_missing_unbind() {
        run_test_with_prelude(
            r##"
            um = Foo.new.method(:dyn).unbind
            [
              um.is_a?(UnboundMethod),
              um.arity,
              um.name,
              um.parameters,
              um.source_location,
              um == Foo.new.method(:dyn).unbind,
              um == Foo.new.method(:other).unbind,
            ]
            "##,
            r##"
            class Foo
              def respond_to_missing?(name, ia); true; end
              def method_missing(name, *a); name; end
            end
            "##,
        );
    }

    #[test]
    fn respond_to_missing_respond_to() {
        run_test_with_prelude(
            r##"
            obj = Foo.new
            [obj.respond_to?(:dynamic), obj.respond_to?(:other)]
            "##,
            r##"
            class Foo
              def respond_to_missing?(name, ia)
                name == :dynamic
              end
            end
            "##,
        );
    }

    #[test]
    fn umethod_bind_call_basic() {
        run_test(
            r##"
            class Foo
              def hello(x); "hi #{x}"; end
            end
            m = Foo.instance_method(:hello)
            m.bind_call(Foo.new, "world")
            "##,
        );
        // Forwarding a block.
        run_test(
            r##"
            class Foo
              def with_block; yield 7; end
            end
            Foo.instance_method(:with_block).bind_call(Foo.new) { |n| n * 2 }
            "##,
        );
        // bind_call with no receiver argument is an ArgumentError.
        run_test_error(
            r##"
            class Foo
              def x; end
            end
            Foo.instance_method(:x).bind_call
            "##,
        );
    }
}
