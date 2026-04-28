use super::*;
use rubymap::RubyEql;

pub(crate) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Struct", STRUCT_CLASS, ObjTy::CLASS);
    // Struct itself is not allocatable (`Struct.allocate` and `Struct.new(1)`
    // both raise). Subclasses created via `Struct.new(:a, :b, ...)` get the
    // default allocator installed in `define_struct_class`.
    globals.store[STRUCT_CLASS].clear_alloc_func();
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "new", struct_new);
    globals.define_builtin_class_func_rest(STRUCT_CLASS, "initialize", struct_initialize);

    // Instance-level initialize lives on Struct itself (private, matching
    // CRuby) so that a user's `def initialize ... super(...) end` inside a
    // `Struct.new(...) do ... end` block can reach the default member-
    // assignment initializer via super.
    //
    // `kw_rest=true` collects keyword arguments into a separate Hash
    // (delivered as `lfp.try_arg(1)`), so Ruby 3.2+ "implicit kwargs"
    // calls like `T.new(a: 1, b: 2)` are distinguishable from a literal
    // positional Hash `T.new({a: 1, b: 2})` — the former unpacks into
    // members, the latter is a single positional argument.
    globals.define_private_builtin_func_with_kw(
        STRUCT_CLASS,
        "initialize",
        initialize,
        0,
        0,
        true,
        &[],
        true,
    );

    globals.define_builtin_func(STRUCT_CLASS, "inspect", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "to_s", inspect, 0);
    globals.define_builtin_func(STRUCT_CLASS, "members", members, 0);
    globals.define_builtin_func(STRUCT_CLASS, "==", eq, 1);
    globals.define_builtin_func(STRUCT_CLASS, "eql?", eql, 1);
    globals.define_builtin_func(STRUCT_CLASS, "!=", ne, 1);
    globals.define_builtin_func(STRUCT_CLASS, "hash", hash, 0);
}

///
/// Struct.[]
/// - new(*args, keyword_init: nil) -> Class
/// - new(*args, keyword_init: nil) {|subclass| block } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/=5b=5d.html]
#[monoruby_builtin]
fn struct_new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut args: Vec<Value> = lfp.arg(0).as_array().iter().copied().collect();

    // Strip a trailing `keyword_init:` Hash argument before classifying the
    // first positional. monoruby's `Struct.new` doesn't declare kwargs in
    // its signature, so kwargs land as a positional Hash; we extract
    // `keyword_init:` here and remember it as `/keyword_init`.
    let keyword_init_arg = if let Some(last) = args.last()
        && let Some(h) = last.try_hash_ty()
    {
        let key = Value::symbol_from_str("keyword_init");
        if let Ok(Some(v)) = h.get(key, vm, globals)
            && h.len() == 1
        {
            args.pop();
            Some(v)
        } else {
            None
        }
    } else {
        None
    };

    // First-arg classification:
    // * `nil` -> anonymous struct (consume the arg).
    // * String starting with uppercase -> named struct constant.
    // * Object with #to_str returning an uppercase-starting String -> ditto.
    // * Anything else -> first positional becomes a member name.
    let name = if let Some(arg0) = args.first().copied() {
        if arg0.is_nil() {
            args.remove(0);
            None
        } else if let Some(s) = arg0.is_str() {
            if s.starts_with(|c: char| c.is_ascii_uppercase()) {
                let n = IdentId::get_id(s);
                args.remove(0);
                Some(n)
            } else {
                return Err(MonorubyErr::identifier_must_be_constant(s));
            }
        } else if arg0.try_symbol().is_none()
            && let Some(coerced) =
                vm.invoke_method_if_exists(globals, IdentId::TO_STR, arg0, &[], None, None)?
            && coerced.is_str().is_some()
        {
            let s = coerced.as_str();
            if s.starts_with(|c: char| c.is_ascii_uppercase()) {
                let n = IdentId::get_id(&s);
                args.remove(0);
                Some(n)
            } else {
                return Err(MonorubyErr::identifier_must_be_constant(&s));
            }
        } else {
            None
        }
    } else {
        None
    };

    // Warn (via Ruby's `$stderr`, so spec `should complain(/constant/)` sees
    // it) when redefining an existing same-name constant on the receiver,
    // matching CRuby's `Struct.new('Person', ...)` behaviour.
    if let Some(n) = name {
        let parent_class = lfp.self_val().as_class().id();
        let prev = globals
            .store
            .get_constant_noautoload(parent_class, n)
            .is_some();
        if prev {
            let parent_name = globals.store[parent_class]
                .get_name()
                .unwrap_or_default()
                .to_string();
            let qual = if parent_name.is_empty() {
                n.get_name().to_string()
            } else {
                format!("{parent_name}::{}", n.get_name())
            };
            let msg = format!("warning: already initialized constant {qual}\n");
            let stderr_id = IdentId::get_id("$stderr");
            let stderr = globals.get_gvar(stderr_id).unwrap_or(Value::nil());
            let write_id = IdentId::get_id("write");
            let _ = vm.invoke_method_inner(
                globals,
                write_id,
                stderr,
                &[Value::string(msg)],
                None,
                None,
            );
        }
    }

    let new_struct = globals
        .store
        .define_struct_class(name, lfp.self_val().as_class())
        .as_val();

    if let Some(v) = keyword_init_arg {
        globals
            .store
            .set_ivar(new_struct, IdentId::get_id("/keyword_init"), v)
            .unwrap();
    }

    vm.invoke_method_inner(
        globals,
        IdentId::INITIALIZE,
        new_struct,
        &args,
        lfp.block(),
        None,
    )?;

    Ok(new_struct)
}

///
/// Struct.[]
/// - new(*args, keyword_init: nil) -> Class
/// - new(*args, keyword_init: nil) {|subclass| block } -> Class
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/=5b=5d.html]
#[monoruby_builtin]
fn struct_initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut new_struct = lfp.self_val();
    let new_module = new_struct.as_class();
    let class_id = new_module.id();
    let args = lfp.arg(0).as_array();

    globals.define_builtin_class_inline_funcs_catch_all(
        class_id,
        "new",
        &["[]"],
        new,
        Box::new(super::class::gen_class_new_object()),
    );
    globals.define_builtin_class_func(class_id, "members", struct_members, 0);
    // `keyword_init?` is defined per Struct subclass (matching CRuby —
    // it is intentionally NOT inherited by direct `class X < Struct`
    // subclasses, only by classes produced by `Struct.new`).
    globals.define_builtin_class_func(class_id, "keyword_init?", keyword_init_p, 0);
    // `initialize`, `==`, `eql?`, `!=`, `hash` are inherited from `Struct`
    // (installed in `init`). Defining them per-subclass would shadow any
    // module the user later `include`s into the subclass — and the `hash`
    // spec explicitly tests that an included module's `hash` overrides the
    // Struct default. Inheriting from `Struct` keeps the user-overrideable
    // method-lookup chain intact.

    let members = ArrayInner::from_iter(args.iter().cloned());

    // Reject duplicate member names (matches CRuby's
    // `rb_struct_define_without_accessor`).
    let mut seen: Vec<IdentId> = Vec::with_capacity(members.len());
    for arg in members.iter() {
        let name = arg.expect_symbol_or_string(globals)?;
        if seen.contains(&name) {
            return Err(MonorubyErr::argumenterr(format!(
                "duplicate member: {name:?}"
            )));
        }
        seen.push(name);
    }

    // Slot-based readers/writers: each member name gets a dedicated
    // `FuncKind::StructReader { slot_index }` / `StructWriter`, baking
    // the slot index into the function metadata so the JIT can compile
    // them to direct memory accesses (mirrors how `attr_reader` /
    // `attr_writer` lower to inline ivar loads). Slot indices match
    // the position in `/members`.
    //
    // `method_added` is fired after each registration so user hooks
    // (e.g. `def self.method_added`) see the same `[:x, :x=, :y, :y=]`
    // sequence that `attr_reader`/`attr_writer` would have produced.
    let inline = members.len() <= crate::value::STRUCT_INLINE_SLOTS;
    for (i, arg) in members.iter().enumerate() {
        let name = arg.expect_symbol_or_string(globals)?;
        let slot = i as u16;
        globals.define_struct_reader(class_id, name, slot, inline, Visibility::Public);
        vm.invoke_method_added(globals, class_id, name)?;
        let writer_name =
            globals.define_struct_writer(class_id, name, slot, inline, Visibility::Public);
        vm.invoke_method_added(globals, class_id, writer_name)?;
    }

    new_struct.set_instance_var(&mut globals.store, "/members", Value::array(members))?;

    if let Some(bh) = lfp.block() {
        vm.module_eval(globals, new_module, bh)?;
    };
    Ok(Value::nil())
}

#[monoruby_builtin]
fn struct_members(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let members = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

#[monoruby_builtin]
fn new(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, pc: BytecodePtr) -> Result<Value> {
    super::class::__new(vm, globals, lfp, pc)
}

fn get_members(globals: &mut Globals, mut class: Module) -> Result<Array> {
    let mut members = None;
    loop {
        if let Some(m) = globals
            .store
            .get_ivar(class.as_val(), IdentId::get_id("/members"))
        {
            members = Some(m);
            break;
        } else if let Some(s) = class.superclass()
            && s.id() != STRUCT_CLASS
        {
            class = s;
        } else {
            break;
        }
    }
    Ok(members
        .ok_or_else(|| MonorubyErr::runtimeerr("no ivar /members."))?
        .as_array())
}

#[monoruby_builtin]
fn initialize(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let pos_args = lfp.arg(0).as_array();
    let kw_args_val = lfp.try_arg(1).filter(|v| {
        v.try_hash_ty().map(|h| !h.is_empty()).unwrap_or(false)
    });
    let kw_args = kw_args_val.and_then(|v| v.try_hash_ty());
    let mut self_val = lfp.self_val();
    let class_obj = self_val.get_class_obj(globals);
    let members = get_members(globals, class_obj)?;
    let keyword_init = is_keyword_init(globals, class_obj);

    if keyword_init {
        // `Struct.new(:a, :b, keyword_init: true)`. Accepts kwargs (preferred)
        // or a single Hash. Combinations of positional + kwargs raise.
        if !pos_args.is_empty() && kw_args.is_some() {
            return Err(MonorubyErr::argumenterr(format!(
                "wrong number of arguments (given {}, expected 0)",
                pos_args.len()
            )));
        }
        let hash_opt = if let Some(kw) = kw_args {
            Some(kw)
        } else if pos_args.is_empty() {
            None
        } else if pos_args.len() == 1 {
            // `T.new({...})` accepted as a single Hash.
            match pos_args.get(0).copied().and_then(|v| v.try_hash_ty()) {
                Some(h) => Some(h),
                None => {
                    return Err(MonorubyErr::argumenterr(format!(
                        "wrong number of arguments (given {}, expected 0)",
                        pos_args.len()
                    )));
                }
            }
        } else {
            return Err(MonorubyErr::argumenterr(format!(
                "wrong number of arguments (given {}, expected 0)",
                pos_args.len()
            )));
        };
        if let Some(hash) = hash_opt {
            // Validate all keys are declared members.
            let mut unknown: Vec<String> = Vec::new();
            let known: Vec<IdentId> = members.iter().filter_map(|m| m.try_symbol()).collect();
            for (k, _) in hash.iter() {
                if let Some(sym) = k.try_symbol() {
                    if !known.contains(&sym) {
                        unknown.push(sym.get_name().to_string());
                    }
                } else {
                    return Err(MonorubyErr::argumenterr("non-symbol key in keyword args"));
                }
            }
            if !unknown.is_empty() {
                return Err(MonorubyErr::argumenterr(format!(
                    "unknown keywords: {}",
                    unknown.join(", ")
                )));
            }
            for (i, m) in members.iter().enumerate() {
                let sym = m.try_symbol().unwrap();
                let key = Value::symbol(sym);
                let v = hash.get(key, vm, globals)?.unwrap_or(Value::nil());
                self_val.as_struct_mut().set(i, v);
            }
        } else {
            for i in 0..members.len() {
                self_val.as_struct_mut().set(i, Value::nil());
            }
        }
        return Ok(Value::nil());
    }

    // Non-keyword_init struct: a kwargs-only call (no positionals) is
    // promoted to keyword_init style — `T.new(a: 1)` for `T = Struct.new(:a)`
    // sets member `a` to 1, matching Ruby 3.2+ implicit-kwargs behavior. If
    // any positionals are present, the kwargs are appended as a single Hash
    // positional (`type.new("a", b: "b")` -> `s.b == {b: "b"}`), preserving
    // the historical behavior the spec still tests.
    if let Some(kw_hash) = kw_args {
        if pos_args.is_empty() {
            // Validate kwargs are member symbols.
            let known: Vec<IdentId> = members.iter().filter_map(|m| m.try_symbol()).collect();
            for (k, _) in kw_hash.iter() {
                if let Some(sym) = k.try_symbol() {
                    if !known.contains(&sym) {
                        return Err(MonorubyErr::argumenterr(format!(
                            "unknown keywords: {}",
                            sym.get_name()
                        )));
                    }
                }
            }
            for (i, m) in members.iter().enumerate() {
                let sym = m.try_symbol().unwrap();
                let v = kw_hash
                    .get(Value::symbol(sym), vm, globals)?
                    .unwrap_or(Value::nil());
                self_val.as_struct_mut().set(i, v);
            }
            return Ok(Value::nil());
        } else {
            // pos + kwargs: kwargs become one extra positional Hash.
            let total = pos_args.len() + 1;
            if total > members.len() {
                return Err(MonorubyErr::argumenterr("struct size differs"));
            }
            let _ = kw_hash;
            let kw_val = kw_args_val.unwrap();
            for i in 0..members.len() {
                let val = if i < pos_args.len() {
                    pos_args.get(i).copied().unwrap_or(Value::nil())
                } else if i == pos_args.len() {
                    kw_val
                } else {
                    Value::nil()
                };
                self_val.as_struct_mut().set(i, val);
            }
            return Ok(Value::nil());
        }
    }

    if members.len() < pos_args.len() {
        return Err(MonorubyErr::argumenterr("struct size differs"));
    };
    // Slot-only storage (Phase 6): members live in the per-instance
    // `StructInner` slot vector and are NOT also kept as ivars. This is
    // why `Struct#instance_variables` returns `[]` and
    // `Struct#instance_variable_get(:@a)` returns nil even after
    // `S.new(1).a == 1`, matching CRuby's `RStruct` semantics.
    for i in 0..members.len() {
        let val = pos_args.get(i).copied().unwrap_or(Value::nil());
        self_val.as_struct_mut().set(i, val);
    }
    Ok(Value::nil())
}

/// Returns whether *class_obj* (a Struct subclass) was created with
/// `keyword_init: true` (any truthy value, matching `keyword_init?`).
fn is_keyword_init(globals: &Globals, class_obj: Module) -> bool {
    let v = match globals
        .store
        .get_ivar(class_obj.as_val(), IdentId::get_id("/keyword_init"))
    {
        Some(v) => v,
        None => return false,
    };
    if v.is_nil() || v == Value::bool(false) {
        false
    } else {
        true
    }
}

#[monoruby_builtin]
fn inspect(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let class_id = self_val.class();
    let struct_class = globals.store[class_id].get_module();
    // Class name: fully qualified path (`M::S`), bypassing any user-
    // defined `#name` so e.g. `def self.name; "x"; end` cannot affect
    // inspect. CRuby's `rb_struct_inspect` uses `rb_class_real`-derived
    // path for the same reason. Anonymous classes (and any ancestor in
    // the path that's anonymous) are detected via `get_name() == None`
    // — in that case render `#<struct member=...>` with no class name.
    let class_name = if globals.store[class_id].get_name().is_some()
        && let Some(qualified) = qualified_real_class_name(globals, class_id)
    {
        Some(qualified)
    } else {
        None
    };

    let mut inspect = String::from("#<struct");
    if let Some(name) = &class_name {
        inspect.push(' ');
        inspect.push_str(name);
    }

    let members = get_members(globals, struct_class)?;
    let slots = self_val.try_struct();
    let mut first = true;
    for (i, m) in members.iter().enumerate() {
        let name = m.try_symbol().unwrap();
        // Slot-based read: by Phase 3 the per-instance slot vec is the
        // canonical store. Fall back to the ivar (legacy) if the value
        // is somehow unsynced -- a paranoia net during the migration.
        let val = match slots.and_then(|s| s.try_get(i)) {
            Some(v) => v.inspect(&globals.store),
            None => "nil".to_string(),
        };
        inspect.push_str(if first { " " } else { ", " });
        first = false;
        inspect.push_str(&format!("{name:?}={val}"));
    }
    inspect.push('>');

    Ok(Value::string(inspect))
}

/// Returns the fully-qualified class name (`M::S`) by walking the
/// parent chain. Returns `None` if any ancestor (including the class
/// itself) is anonymous, matching CRuby's "drop class label entirely
/// if any segment is anonymous" rule for `Struct#inspect`.
fn qualified_real_class_name(globals: &Globals, class_id: ClassId) -> Option<String> {
    let parents = globals.store.get_parents(class_id);
    if parents.iter().any(|s| s.starts_with("#<")) {
        // `get_parents` renders anonymous segments as `#<Class:...>`.
        return None;
    }
    let v: Vec<String> = parents.into_iter().rev().collect();
    if v.is_empty() {
        None
    } else {
        Some(v.join("::"))
    }
}

///
/// Struct#==
/// - self == other -> bool
///
/// Recursive structures are handled via `exec_recursive_paired`: a
/// `(self_id, other_id)` pair already on the comparison stack returns
/// `true` (matching CRuby — recursive equal pairs are treated as equal
/// unless a difference is found elsewhere).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let lhs_struct = match self_val.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(false)),
    };
    let rhs_struct = match other.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(false)),
    };
    if lhs_struct.len() != rhs_struct.len() {
        return Ok(Value::bool(false));
    }
    let len = lhs_struct.len();
    crate::value::exec_recursive_paired(
        self_val.id(),
        other.id(),
        || {
            let lhs_struct = self_val.try_struct().unwrap();
            let rhs_struct = other.try_struct().unwrap();
            for i in 0..len {
                let lhs = lhs_struct.get(i);
                let rhs = rhs_struct.get(i);
                if vm.ne_values_bool(globals, lhs, rhs)? {
                    return Ok(Value::bool(false));
                }
            }
            Ok(Value::bool(true))
        },
        Value::bool(true),
    )
}

///
/// Struct#eql?
/// - self.eql?(other) -> bool
///
/// Type-strict comparison: each pair of slots is compared with `eql?`
/// (so `1.eql?(1.0)` is false). Recursive structures use the same
/// `exec_recursive_paired` machinery as `==`.
#[monoruby_builtin]
fn eql(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(false));
    }
    let lhs_struct = match self_val.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(false)),
    };
    let rhs_struct = match other.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(false)),
    };
    if lhs_struct.len() != rhs_struct.len() {
        return Ok(Value::bool(false));
    }
    let len = lhs_struct.len();
    crate::value::exec_recursive_paired(
        self_val.id(),
        other.id(),
        || {
            let lhs_struct = self_val.try_struct().unwrap();
            let rhs_struct = other.try_struct().unwrap();
            for i in 0..len {
                let lhs = lhs_struct.get(i);
                let rhs = rhs_struct.get(i);
                if !lhs.eql(&rhs, vm, globals)? {
                    return Ok(Value::bool(false));
                }
            }
            Ok(Value::bool(true))
        },
        Value::bool(true),
    )
}

///
/// Struct#!=
///
#[monoruby_builtin]
fn ne(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let other = lfp.arg(0);
    if self_val.class() != other.class() {
        return Ok(Value::bool(true));
    }
    let lhs_struct = match self_val.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(true)),
    };
    let rhs_struct = match other.try_struct() {
        Some(s) => s,
        None => return Ok(Value::bool(true)),
    };
    if lhs_struct.len() != rhs_struct.len() {
        return Ok(Value::bool(true));
    }
    let len = lhs_struct.len();
    let result = crate::value::exec_recursive_paired(
        self_val.id(),
        other.id(),
        || {
            let lhs_struct = self_val.try_struct().unwrap();
            let rhs_struct = other.try_struct().unwrap();
            for i in 0..len {
                let lhs = lhs_struct.get(i);
                let rhs = rhs_struct.get(i);
                if vm.ne_values_bool(globals, lhs, rhs)? {
                    return Ok(Value::bool(true));
                }
            }
            Ok(Value::bool(false))
        },
        Value::bool(false),
    )?;
    Ok(result)
}

///
/// Struct#hash
///
/// Order-dependent hash that includes the receiver's class so different
/// Struct subclasses with identical content do not collide. Recursive
/// structures hash to a sentinel via `HASH_RECURSION_GUARD`.
#[monoruby_builtin]
fn hash(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::hash::Hasher;
    let self_val = lfp.self_val();
    let id = self_val.id();
    let class_hash = (self_val.class().u32() as u64).wrapping_mul(0x100000001b3);
    crate::value::exec_recursive_outer(
        id,
        || {
            let mut hasher = std::hash::DefaultHasher::new();
            hasher.write_u64(class_hash);
            if let Some(s) = self_val.try_struct() {
                for i in 0..s.len() {
                    let v = s.get(i);
                    let h = v.calculate_hash(vm, globals)?;
                    hasher.write_u64(h);
                }
            }
            Ok(Value::integer(
                (hasher.finish() & 0x7fff_ffff_ffff_ffff) as i64,
            ))
        },
        // On any recursion at any depth, the outer call returns this
        // sentinel, so two structs that cycle at different depths still
        // hash to the same value.
        Value::integer((class_hash & 0x7fff_ffff_ffff_ffff) as i64),
    )
}

#[monoruby_builtin]
fn members(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let class_obj = lfp.self_val().get_class_obj(globals).as_val();
    let members = globals
        .store
        .get_ivar(class_obj, IdentId::get_id("/members"))
        .unwrap();
    Ok(members)
}

///
/// `extern "C"` runtime helper for `FuncKind::StructWriter`. The
/// JIT-emitted wrapper for a Struct member writer calls this to do
/// the frozen-check + slot store. Returns `Some(val)` on success and
/// `None` (with the error stored on `vm`) on FrozenError. Mirrors the
/// shape of `set_instance_var_with_cache`.
pub(crate) extern "C" fn set_struct_slot_with_check(
    vm: &mut Executor,
    globals: &mut Globals,
    mut self_val: Value,
    val: Value,
    slot_index: u32,
) -> Option<Value> {
    if let Err(err) = self_val.ensure_not_frozen(&globals.store) {
        vm.set_error(err);
        return None;
    }
    self_val.as_struct_mut().set(slot_index as usize, val);
    Some(val)
}

///
/// Struct.keyword_init? -> true | false | nil
///
/// Whether the struct was created with `keyword_init: true`. Returns
/// `nil` if no `keyword_init:` was passed (or it was passed as `nil`),
/// `false` if it was passed as `false`, and `true` for any other truthy
/// value (`Struct.new(:x, keyword_init: 1).keyword_init? == true`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Struct/s/keyword_init=3f.html]
#[monoruby_builtin]
fn keyword_init_p(
    _vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let v = globals
        .store
        .get_ivar(lfp.self_val(), IdentId::get_id("/keyword_init"))
        .unwrap_or(Value::nil());
    if v.is_nil() {
        Ok(Value::nil())
    } else if v == Value::bool(false) {
        Ok(Value::bool(false))
    } else {
        Ok(Value::bool(true))
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn struct_test() {
        let prelude = r#"
        Customer = Struct.new(:name, :address) do
            def greeting
                "Hello #{name} at #{address}!"
            end
        end
        "#;
        let code = r#"
        [Customer.new("Dave", "New York").greeting, Customer["Gave", "Hawaii"].greeting]
        "#;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_subclass() {
        let prelude = r##"
        class S < Struct.new(:name, :address)
        end
        "##;
        let code = r##"
        s = S.new("Dave", "New York")
        "#{s.name} #{s.address} #{s} #{s.inspect}"
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_new() {
        run_test(
            r###"
            Struct.new("Foo", :a, :b).to_s
        "###,
        );
        run_test(
            r###"
            Struct.new(:a, :b, :c).new.members
        "###,
        );
        run_test(
            r###"
            Struct.new(:a, :b, :c).members
        "###,
        );
    }

    #[test]
    fn struct_inspect() {
        let code = r###"
        S = Struct.new(:a,:b)
        s = S.new(100,200)
        [s.a, s.b, s.inspect]
        "###;
        run_test(code);
    }

    #[test]
    fn struct_eq() {
        let prelude = r##"
        S = Struct.new(:a, :b)
        T = Struct.new(:a, :b)
        "##;
        let code = r##"
        res = []
        # equal structs
        res << (S.new(1, 2) == S.new(1, 2))
        # different values
        res << (S.new(1, 2) == S.new(1, 3))
        # different struct classes with same members and values
        res << (S.new(1, 2) == T.new(1, 2))
        # comparison with non-struct
        res << (S.new(1, 2) == [1, 2])
        # eql? behaves the same as ==
        res << (S.new(1, 2).eql?(S.new(1, 2)))
        res << (S.new(1, 2).eql?(S.new(1, 3)))
        # nested structs
        res << (S.new(S.new(1, 2), 3) == S.new(S.new(1, 2), 3))
        res << (S.new(S.new(1, 2), 3) == S.new(S.new(1, 9), 3))
        # default (nil) members
        res << (S.new == S.new)
        res << (S.new(1) == S.new)
        res
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_ne() {
        let prelude = r##"
        S = Struct.new(:a, :b)
        T = Struct.new(:a, :b)
        "##;
        let code = r##"
        res = []
        # equal structs
        res << (S.new(1, 2) != S.new(1, 2))
        # different values
        res << (S.new(1, 2) != S.new(1, 3))
        # different struct classes
        res << (S.new(1, 2) != T.new(1, 2))
        # comparison with non-struct
        res << (S.new(1, 2) != [1, 2])
        # nested structs
        res << (S.new(S.new(1, 2), 3) != S.new(S.new(1, 2), 3))
        res << (S.new(S.new(1, 2), 3) != S.new(S.new(1, 9), 3))
        # default (nil) members
        res << (S.new != S.new)
        res << (S.new(1) != S.new)
        res
        "##;
        run_test_with_prelude(code, prelude);
    }

    #[test]
    fn struct_method_added() {
        run_test(
            r##"
        $added = []
        S = Struct.new(:x, :y) do
          def self.method_added(name)
            $added << name
          end
        end
        $added
        "##,
        );
        run_test_once(
            r##"
        $added = []
        class Module
          alias_method :orig_method_added, :method_added
          def method_added(name)
            $added << name if self.ancestors.include?(Struct)
            orig_method_added(name)
          end
        end
        S = Struct.new(:x, :y)
        $added
        "##,
        );
    }

    #[test]
    fn struct_custom_initialize_super() {
        // A user-defined `initialize` inside `Struct.new(...) do ... end` must
        // be able to reach the default member-assignment initializer via
        // `super(...)`. Regression test for a bug where the subclass shadowed
        // the default initialize, leaving `super` to fall through to
        // `Object#initialize` and silently drop all positional args (so every
        // member ended up `nil`).
        run_test_with_prelude(
            r##"
            v = V.new(7)
            [v.a, v.b, v.c]
            "##,
            r##"
            V = Struct.new(:a, :b, :c) do
              def initialize(a)
                super(a, 99, -1)
              end
            end
            "##,
        );
    }

    #[test]
    fn struct_initialize_visibility() {
        // `initialize` on Struct must be private, matching CRuby. Calling
        // `instance.initialize(...)` directly should raise NoMethodError.
        run_test(
            r##"
            S = Struct.new(:a, :b)
            [
              Struct.private_method_defined?(:initialize, false),
              Struct.public_method_defined?(:initialize, false),
              S.instance_method(:initialize).owner == Struct,
            ]
            "##,
        );
    }

    #[test]
    fn struct_block_constant_lexical_scope() {
        // CRuby semantics: a constant assigned inside the `Struct.new ... do
        // ... end` block goes to the *lexical* scope (top-level here), not to
        // the new Struct subclass. Methods defined in the same block resolve
        // the constant via the same lexical chain.
        run_test_with_prelude(
            r##"
            [S.new(0).flag, S.constants, Object.constants.include?(:FLAGS)]
            "##,
            r##"
            S = Struct.new(:x) do
              FLAGS = { foo: 1 }
              def flag
                FLAGS[:foo]
              end
            end
            "##,
        );
    }

    // ----- Tests for the iteration / accessor / introspection sweep -----

    #[test]
    fn struct_iteration_methods() {
        // PR for core/method/struct sweep: Struct gains a Ruby-side
        // implementation of Enumerable-friendly accessors.
        let prelude = r#"
        S = Struct.new(:a, :b, :c)
        s = S.new(10, 20, 30)
        "#;
        run_test_with_prelude(r#"s.size"#, prelude);
        run_test_with_prelude(r#"s.length"#, prelude);
        run_test_with_prelude(r#"s.to_a"#, prelude);
        run_test_with_prelude(r#"s.values"#, prelude);
        run_test_with_prelude(r#"s.deconstruct"#, prelude);
        run_test_with_prelude(r#"s.to_h"#, prelude);
        run_test_with_prelude(r#"s.each.to_a"#, prelude);
        run_test_with_prelude(r#"s.each_pair.to_a"#, prelude);
    }

    #[test]
    fn struct_index_access() {
        let prelude = r#"
        S = Struct.new(:a, :b, :c)
        s = S.new(10, 20, 30)
        "#;
        run_test_with_prelude(r#"[s[0], s[1], s[2]]"#, prelude);
        run_test_with_prelude(r#"[s[:a], s[:b], s[:c]]"#, prelude);
        run_test_with_prelude(r#"[s["a"], s["b"], s["c"]]"#, prelude);
        run_test_with_prelude(r#"[s[-1], s[-3]]"#, prelude);
        run_test_with_prelude(
            r#"
            t = S.new(10, 20, 30)
            t[0] = 100
            t[:b] = 200
            t.to_a
            "#,
            prelude,
        );
        // Out-of-range index raises IndexError; unknown member raises NameError.
        run_test_error(r#"S = Struct.new(:a); S.new(1)[5]"#);
        run_test_error(r#"S = Struct.new(:a); S.new(1)[:nope]"#);
    }

    #[test]
    fn struct_values_at() {
        let prelude = r#"
        S = Struct.new(:a, :b, :c)
        s = S.new(10, 20, 30)
        "#;
        run_test_with_prelude(r#"s.values_at(0, 2)"#, prelude);
        run_test_with_prelude(r#"s.values_at(0..2)"#, prelude);
        run_test_with_prelude(r#"s.values_at(0..3)"#, prelude);
        run_test_with_prelude(r#"s.values_at"#, prelude);
        run_test_error(r#"S = Struct.new(:a, :b); S.new.values_at(3)"#);
        run_test_error(r#"S = Struct.new(:a, :b); S.new.values_at(-3)"#);
        run_test_error(r#"S = Struct.new(:a, :b); S.new.values_at("x")"#);
    }

    #[test]
    fn struct_dig_traversal() {
        let prelude = r#"
        Inner = Struct.new(:b)
        Outer = Struct.new(:a)
        o = Outer.new(Inner.new({k: 1}))
        "#;
        run_test_with_prelude(r#"o.dig(:a, :b, :k)"#, prelude);
        // Unknown key returns nil (not NameError) when used via dig.
        run_test_with_prelude(r#"o.dig(:nope, 0)"#, prelude);
        // Intermediate nil short-circuits.
        run_test_with_prelude(r#"o.dig(:a, :b, :missing, 0)"#, prelude);
    }

    #[test]
    fn struct_deconstruct_keys_basic() {
        let prelude = r#"
        S = Struct.new(:x, :y, :z)
        s = S.new(1, 2, 3)
        "#;
        run_test_with_prelude(r#"s.deconstruct_keys([:x, :y])"#, prelude);
        run_test_with_prelude(r#"s.deconstruct_keys(["x", "y"])"#, prelude);
        run_test_with_prelude(r#"s.deconstruct_keys([0, 1])"#, prelude);
        run_test_with_prelude(r#"s.deconstruct_keys([0, :x])"#, prelude);
        run_test_with_prelude(r#"s.deconstruct_keys(nil)"#, prelude);
        // More keys requested than struct has -> empty hash.
        run_test_with_prelude(r#"s.deconstruct_keys([:x, :y, :z, :w])"#, prelude);
    }

    #[test]
    fn struct_inspect_qualified_name() {
        // Nested struct uses fully-qualified module path; anonymous
        // structs render without a class label.
        run_test(
            r#"
            module M
              N = Struct.new(:a, :b)
            end
            M::N.new(1, 2).inspect
            "#,
        );
        run_test(r#"Struct.new(:x).new("hello").inspect"#);
    }

    #[test]
    fn struct_initialize_explicit_nil() {
        // Members not provided to `new` are explicitly set to nil so they
        // appear in `instance_variables` (matching CRuby's behavior of
        // initializing all member slots).
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(1)
            [s.a, s.b, s.c]
            "#,
        );
    }

    #[test]
    fn struct_duplicate_member_raises() {
        run_test_error(r#"Struct.new(:a, :b, :a)"#);
    }

    #[test]
    fn struct_keyword_init_q() {
        // `keyword_init?` is defined on each Struct subclass. monoruby's
        // implementation currently always returns nil because we don't
        // yet track the `keyword_init:` arg passed to `Struct.new`; the
        // test asserts the method exists and returns nil (matching CRuby
        // for the no-`keyword_init:` form).
        run_test(r#"Struct.new(:a, :b).keyword_init?"#);
    }

    // ----- Slot-array storage migration regressions -----

    #[test]
    fn struct_members_not_visible_as_ivars() {
        // Members live in the per-instance slot vector, not as ivars.
        // `instance_variables` therefore returns `[]` (matching CRuby
        // `RStruct` semantics) and `instance_variable_get(:@a)` returns
        // nil rather than the member value.
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(1, 2, 3)
            s.instance_variables
            "#,
        );
        run_test(
            r#"
            S = Struct.new(:a, :b)
            s = S.new("v", 2)
            s.instance_variable_get(:@a).nil?
            "#,
        );
        // User-set ivars on a Struct still appear in the introspection.
        run_test(
            r#"
            S = Struct.new(:a)
            s = S.new(1)
            s.instance_variable_set(:@x, 99)
            [s.instance_variables, s.instance_variable_get(:@x)]
            "#,
        );
    }

    #[test]
    fn struct_member_access_via_slots() {
        // Reader and writer accessors round-trip through the slot
        // vector. After mutation, the value reflects the new slot
        // contents, not any stale ivar.
        run_test(
            r#"
            S = Struct.new(:x, :y)
            s = S.new(1, 2)
            s.x = 100
            s[:y] = 200
            [s.x, s.y, s.to_a]
            "#,
        );
    }

    #[test]
    fn struct_dup_copies_slots() {
        // Dup'd Struct instances have independent slot vectors;
        // mutating one doesn't affect the other.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            a = S.new(1, 2)
            b = a.dup
            b.a = 99
            [a.a, b.a]
            "#,
        );
    }

    #[test]
    fn struct_default_nil_members() {
        // Members not provided to `new` are nil and appear in `to_a`,
        // not as ivars.
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(1)
            [s.to_a, s.instance_variables]
            "#,
        );
    }

    // ----- Struct.new edge cases -----

    #[test]
    fn struct_new_keyword_init_kwarg() {
        // `Struct.new(:a, :b, keyword_init: true|false|nil)` parses the
        // trailing kwarg and stores it on the new class. `keyword_init?`
        // returns the stored value verbatim. Member parsing must not
        // see the kwarg as a positional Symbol.
        run_test(
            r#"
            S = Struct.new(:a, :b, keyword_init: true)
            [S.keyword_init?, S.members]
            "#,
        );
        run_test(
            r#"
            S = Struct.new(:a, :b, keyword_init: false)
            [S.keyword_init?, S.members]
            "#,
        );
        run_test(
            r#"
            S = Struct.new(:a, :b, keyword_init: nil)
            [S.keyword_init?, S.members]
            "#,
        );
        // No kwarg at all -> nil.
        run_test(r#"Struct.new(:a, :b).keyword_init?"#);
    }

    #[test]
    fn struct_new_first_arg_nil() {
        // `nil` as the first arg makes the struct anonymous (not stored
        // under `Struct::Foo`); members start at the second arg.
        run_test(
            r#"
            A = Struct.new(nil, :x, :y)
            [A.name.nil? || A.name.match?(/^A$/) ? :ok : A.name, A.members]
            "#,
        );
        run_test(
            r#"
            klass = Struct.new(nil, :a)
            klass.new(42).a
            "#,
        );
    }

    #[test]
    fn struct_new_first_arg_string_constant() {
        // A String beginning with an uppercase letter names the struct
        // and registers it under `Struct`. Lowercase raises NameError.
        // We use `run_test_once` because re-defining `Struct::Foo` 25
        // times in a row would conflict with the warn-on-redefine
        // semantics of CRuby.
        run_test_once(
            r#"
            klass = Struct.new("StructSlotTestNamed", :a, :b)
            [klass == Struct::StructSlotTestNamed, klass.members]
            "#,
        );
        run_test_error(r#"Struct.new("lowercase_invalid", :a)"#);
    }

    // ----- Struct#initialize argument-count behavior -----

    #[test]
    fn struct_initialize_arg_count_mismatch() {
        // Too many positional arguments raises ArgumentError. Fewer
        // arguments than members are accepted; missing slots are nil.
        run_test_error(
            r#"
            S = Struct.new(:a, :b)
            S.new(1, 2, 3)
            "#,
        );
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            [S.new.to_a, S.new(1).to_a, S.new(1, 2).to_a, S.new(1, 2, 3).to_a]
            "#,
        );
    }

    // ----- Struct#==, Struct#!= heterogeneous comparisons -----

    #[test]
    fn struct_eq_with_non_struct() {
        // `==` against a non-Struct (Array, nil, String, Integer)
        // returns false. `!=` is the negation.
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(1, 2, 3)
            [s == [1, 2, 3], s == nil, s == "x", s == 1, s == :sym]
            "#,
        );
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(1, 2, 3)
            [s != [1, 2, 3], s != nil, s != "x"]
            "#,
        );
        // Different Struct subclasses with identical members/values are
        // NOT equal -- class identity matters.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            T = Struct.new(:a, :b)
            [S.new(1, 2) == T.new(1, 2), S.new(1, 2) != T.new(1, 2)]
            "#,
        );
    }

    // ----- Slot reader/writer JIT-compiled accessor -----

    #[test]
    fn struct_slot_reader_jitted() {
        // `run_test` runs the body 25 times, well past the JIT
        // compilation threshold (5 calls in test mode), so this also
        // exercises the JIT-inlined `LoadStructSlot` path.
        run_test(
            r#"
            S = Struct.new(:a, :b, :c)
            s = S.new(10, 20, 30)
            n = 0
            1000.times { n += s.a + s.b + s.c }
            n
            "#,
        );
    }

    #[test]
    fn struct_slot_writer_jitted() {
        // Hot loop hits the JIT-inlined `StoreStructSlot` after the
        // class guard. The frozen-check happens via the `GuardFrozen`
        // deopt; here the receiver is mutable so the deopt never
        // fires.
        run_test(
            r#"
            S = Struct.new(:counter)
            s = S.new(0)
            1000.times { s.counter = s.counter + 1 }
            s.counter
            "#,
        );
    }

    #[test]
    fn struct_writer_frozen_raises() {
        // The wrapper-level frozen check (via `set_struct_slot_with_check`)
        // raises FrozenError on a frozen receiver.
        run_test(
            r#"
            S = Struct.new(:x)
            s = S.new(1).freeze
            begin
              s.x = 2
              :no_error
            rescue FrozenError
              :ok
            end
            "#,
        );
    }

    #[test]
    fn struct_member_method_metadata() {
        // Member readers/writers are still introspectable as Ruby
        // methods of the struct subclass even though they live in
        // dedicated `FuncKind::StructReader` / `StructWriter` slots.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            [
              S.instance_method(:a).arity,
              S.instance_method(:a=).arity,
              S.instance_method(:a).owner == S,
            ]
            "#,
        );
    }

    // ----- Struct#hash -----

    #[test]
    fn struct_hash_basic_invariants() {
        // Same class + same content -> same hash. Hash is an Integer.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            a = S.new(1, 2)
            b = S.new(1, 2)
            [a.hash == b.hash, a.hash.is_a?(Integer)]
            "#,
        );
        // Same class + different content -> different hash (overwhelmingly
        // likely; a collision would be a real PRF surprise).
        run_test(
            r#"
            S = Struct.new(:a, :b)
            S.new(1, 2).hash != S.new(1, 3).hash
            "#,
        );
        // Different struct classes with identical content -> different hash.
        // The class identity is folded into the hash, matching CRuby.
        run_test(
            r#"
            A = Struct.new(:x)
            B = Struct.new(:x)
            A.new(1).hash != B.new(1).hash
            "#,
        );
        // dup'd struct hashes the same as the original.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            s = S.new(10, 20)
            s.hash == s.dup.hash
            "#,
        );
    }

    #[test]
    fn struct_hash_eql_implies_same_hash() {
        // The contract Hash relies on: `a.eql?(b)` => `a.hash == b.hash`.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            a = S.new(1, "x")
            b = S.new(1, "x")
            [a.eql?(b), a.hash == b.hash]
            "#,
        );
    }

    #[test]
    fn struct_hash_recursive_self_cycle() {
        // Self-recursive struct must hash without StackOverflow. Equal
        // self-cycles still produce equal hashes.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            x = S.new(nil, "tag")
            x.a = x
            y = S.new(nil, "tag")
            y.a = y
            [x.hash.is_a?(Integer), x.hash == y.hash]
            "#,
        );
    }

    #[test]
    fn struct_hash_recursive_outer_semantics() {
        // Two structs with cycles at *different* depths still produce the
        // same hash. This is what `exec_recursive_outer` buys us — any
        // recursion at any depth surfaces to the outer call as the same
        // sentinel, so depth-shape doesn't leak into the hash.
        run_test(
            r#"
            S = Struct.new(:a, :b)
            x = S.new(nil, "tag")
            x.a = x                # depth-1 cycle
            y = S.new(nil, "tag")
            y.a = x                # depth-2 cycle (y -> x -> x)
            x.hash == y.hash
            "#,
        );
    }

    #[test]
    fn struct_hash_module_override() {
        // An `include`d module's `hash` shadows Struct's: the per-subclass
        // installation is gone, so method lookup walks `S -> iclass(mod) ->
        // Struct` and finds the module's `hash` first.
        run_test(
            r#"
            mod = Module.new do
              def hash
                42
              end
            end
            S = Struct.new(:arg) do
              include mod
            end
            S.new(1).hash
            "#,
        );
    }

    #[test]
    fn struct_hash_keyword_init() {
        // keyword_init structs hash like any other struct: members in slot
        // order, class folded in.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            a = S.new(name: "elefant", legs: 4)
            b = S.new(name: "elefant", legs: 4)
            c = S.new(name: "elefant", legs: 2)
            [a.hash == b.hash, a.hash != c.hash]
            "#,
        );
    }

    // ----- Struct#initialize: kwargs, keyword_init: true, error paths -----

    #[test]
    fn struct_init_implicit_kwargs() {
        // Plain struct (no `keyword_init:`) accepts kwargs whose keys are
        // member names — Ruby 3.2+ implicit-kwargs behavior. Equivalent to
        // a positional call.
        run_test(
            r#"
            T = Struct.new(:version, :platform)
            pos = T.new("3.2", "OS")
            kw = T.new(version: "3.2", platform: "OS")
            [pos == kw, kw.version, kw.platform]
            "#,
        );
        // Subset of kwargs is allowed: missing members default to nil.
        run_test(
            r#"
            T = Struct.new(:a, :b, :c)
            s = T.new(b: 20)
            [s.a, s.b, s.c]
            "#,
        );
    }

    #[test]
    fn struct_init_explicit_hash_is_positional() {
        // A literal Hash positional is one positional value (NOT
        // unpacked as kwargs), distinguishing it from `T.new(a: 1)`.
        run_test(
            r#"
            T = Struct.new(:a, :b)
            s = T.new({a: 1, b: 2})
            [s.a, s.b]
            "#,
        );
    }

    #[test]
    fn struct_init_pos_plus_kwargs_appends() {
        // `T.new("a", b: "b")` for a plain struct: kwargs become a single
        // positional Hash appended after positionals. The CRuby spec
        // explicitly tests this "treats keyword arguments as a positional
        // parameter" behavior for non-`keyword_init:` structs.
        run_test(
            r#"
            T = Struct.new(:a, :b)
            s = T.new("a", b: "b")
            [s.a, s.b]
            "#,
        );
        // 3-member struct, 1 positional + kwargs -> kwargs become member 1,
        // member 2 stays nil.
        run_test(
            r#"
            T = Struct.new(:a, :b, :c)
            s = T.new("a", b: "b", c: "c")
            [s.a, s.b, s.c]
            "#,
        );
    }

    #[test]
    fn struct_init_implicit_kwargs_unknown_key_raises() {
        // Plain struct: kwargs key that doesn't name a member is
        // ArgumentError (not silently dropped).
        run_test_error(
            r#"
            T = Struct.new(:a, :b)
            T.new(c: 1)
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_accepts_kwargs() {
        // The canonical keyword_init: true call: kwargs map to members.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            s = S.new(name: "elefant", legs: 4)
            [s.name, s.legs]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_accepts_single_hash() {
        // keyword_init: true also accepts a single positional Hash (treated
        // as kwargs). The keys are still validated against members.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            s = S.new({name: "elefant", legs: 4})
            [s.name, s.legs]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_partial_keys() {
        // keyword_init: true with a subset of keys: missing members default
        // to nil.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            s = S.new(name: "elefant")
            [s.name, s.legs]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_no_args() {
        // keyword_init: true with no args at all: every member is nil.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            s = S.new
            [s.name, s.legs]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_truthy_value_normalized() {
        // `keyword_init: 1` (truthy non-true) acts as `keyword_init: true`,
        // and `keyword_init?` returns the canonical `true`.
        run_test(
            r#"
            S = Struct.new(:x, keyword_init: 1)
            s = S.new(x: 7)
            [S.keyword_init?, s.x]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_false_behaves_positional() {
        // keyword_init: false reverts to positional semantics.
        run_test(
            r#"
            S = Struct.new(:name, :legs, keyword_init: false)
            s = S.new("elefant", 4)
            [S.keyword_init?, s.name, s.legs]
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_unknown_keys_raises() {
        // `unknown keywords: foo` for any kwarg key not in members.
        run_test_error(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            S.new(name: "elefant", legs: 4, foo: "bar")
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_positional_raises() {
        // keyword_init: true rejects positional-only calls.
        run_test_error(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            S.new("elefant", 4)
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_single_non_hash_raises() {
        // keyword_init: true rejects a single non-Hash positional.
        run_test_error(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            S.new("elefant")
            "#,
        );
    }

    #[test]
    fn struct_init_keyword_init_true_pos_plus_kwargs_raises() {
        // keyword_init: true rejects mixing positional + kwargs, even when
        // kwargs alone would have been valid.
        run_test_error(
            r#"
            S = Struct.new(:name, :legs, keyword_init: true)
            S.new("elefant", legs: 4)
            "#,
        );
    }

    #[test]
    fn struct_init_too_many_positional_raises() {
        // Plain struct: passing more positional args than members is
        // ArgumentError ("struct size differs").
        run_test_error(
            r#"
            S = Struct.new(:a, :b)
            S.new(1, 2, 3)
            "#,
        );
    }

    #[test]
    fn struct_init_pos_plus_kwargs_overflow_raises() {
        // `pos + kwargs-as-hash` total exceeds members -> ArgumentError.
        // (`type.new("a", "b", c: "c")` for a 2-member struct: 2 positionals
        // + 1 trailing kwargs Hash = 3 effective args.)
        run_test_error(
            r#"
            S = Struct.new(:a, :b)
            S.new("a", "b", c: "c")
            "#,
        );
    }
}
