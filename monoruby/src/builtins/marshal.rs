use super::*;
use num::BigInt;

//
// Marshal module
//

pub(super) fn init(globals: &mut Globals) {
    let klass = globals.define_toplevel_module("Marshal").id();
    // CRuby: Marshal.dump(obj, port=nil, level=-1).
    //  - 1 arg : returns the serialized bytes as a String.
    //  - 2 args: 2nd arg is either an IO-like writer (the bytes are
    //            written to it and the writer is returned) OR an
    //            Integer recursion-depth limit.
    //  - 3 args: obj + io + limit.
    // The recursion limit is accepted for API parity but not enforced
    // (monoruby tracks visit cycles per-call via `in_progress`).
    globals.define_builtin_module_func_with(klass, "dump", dump, 1, 3, false);
    // CRuby: Marshal.load(source, proc=nil). The optional proc is
    // called once per loaded value during traversal; ignored here
    // because the spec set that depends on the arity passes nil
    // anyway, and surfacing a more useful error later is safer than
    // an arity refusal up front.
    globals.define_builtin_module_func_with(klass, "load", load, 1, 2, false);
    globals.define_builtin_module_func_with(klass, "restore", load, 1, 2, false);
}

/// ### Marshal.dump
/// - dump(obj) -> String
/// - dump(obj, port) -> port (port responds to #write)
/// - dump(obj, level) -> String
/// - dump(obj, port, level) -> port
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/dump.html]
#[monoruby_builtin]
fn dump(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let obj = lfp.arg(0);
    // Classify args 1 and 2 into (port, _level). A two-arg call with
    // an Integer second arg is the "(obj, level)" form, NOT a port.
    let (port, _level) = match (lfp.try_arg(1), lfp.try_arg(2)) {
        (None, _) => (None, None),
        (Some(a), None) => {
            if a.is_kind_of(&globals.store, INTEGER_CLASS) {
                (None, Some(a))
            } else {
                (Some(a), None)
            }
        }
        (Some(a), Some(b)) => (Some(a), Some(b)),
    };
    let mut buf: Vec<u8> = Vec::new();
    // Marshal version header
    buf.push(0x04);
    buf.push(0x08);
    let mut symbols: Vec<IdentId> = Vec::new();
    let mut in_progress: std::collections::HashSet<u64> = std::collections::HashSet::new();
    marshal_dump_value(&mut buf, obj, globals, &mut symbols, &mut in_progress)?;
    match port {
        None => Ok(Value::bytes(buf)),
        Some(port) => {
            // CRuby: raises TypeError "instance of IO needed" via the
            // duck-type check `port.respond_to?(:write)`. Drive the
            // standard `respond_to?` dispatch instead of touching
            // method-table internals so a user-defined responder still
            // works.
            let write_id = IdentId::get_id("write");
            let respond_to = IdentId::get_id("respond_to?");
            let responds = match vm.invoke_method_inner(
                globals,
                respond_to,
                port,
                &[Value::symbol(write_id)],
                None,
                None,
            ) {
                Ok(v) => v.as_bool(),
                Err(_) => false,
            };
            if !responds {
                return Err(MonorubyErr::typeerr("instance of IO needed"));
            }
            let bytes = Value::bytes(buf);
            vm.invoke_method_inner(globals, write_id, port, &[bytes], None, None)?;
            Ok(port)
        }
    }
}

/// ### Marshal.load
/// - load(source) -> Object
/// - load(source, proc) -> Object (proc's return value replaces the loaded object)
///
/// [https://docs.ruby-lang.org/ja/latest/method/Marshal/m/load.html]
#[monoruby_builtin]
fn load(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let source = lfp.arg(0);
    // CRuby's Marshal.load also accepts an IO-like reader (anything
    // responding to #read). For the moment we only handle the
    // String-of-bytes form; IO readers can be added once the spec
    // arity gap stops swallowing 50+ examples.
    let data = source.expect_bytes(&globals.store)?;
    if data.len() < 2 {
        return Err(MonorubyErr::argumenterr("marshal data too short"));
    }
    if data[0] != 0x04 || data[1] != 0x08 {
        return Err(MonorubyErr::typeerr(format!(
            "incompatible marshal file format (can't be read)\n\
             \tformat version {}.{} required; {}.{} given",
            4, 8, data[0], data[1]
        )));
    }
    let mut cursor = MarshalReader::new(&data[2..]);
    let value = cursor.read_value(vm, globals)?;
    // CRuby's Marshal.load(src, proc): the proc is called with the
    // loaded object, and *its return value replaces* the value
    // returned by `load`. (CRuby walks every sub-object too — we
    // only hook the top-level for now; that already covers the
    // shape the ruby/spec arity gap was blocking on.)
    if let Some(proc_arg) = lfp.try_arg(1)
        && let Some(proc) = proc_arg.is_proc()
    {
        return vm.invoke_proc(globals, &proc, &[value]);
    }
    Ok(value)
}

// ============================================================
// Marshal reader (deserializer)
// ============================================================

struct MarshalReader<'a> {
    data: &'a [u8],
    pos: usize,
    /// Symbol table for back-references (';' tag)
    symbols: Vec<IdentId>,
    /// Object table for back-references ('@' tag)
    objects: Vec<Value>,
}

impl<'a> MarshalReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        MarshalReader {
            data,
            pos: 0,
            symbols: Vec::new(),
            objects: Vec::new(),
        }
    }

    fn read_byte(&mut self) -> Result<u8> {
        if self.pos >= self.data.len() {
            return Err(MonorubyErr::argumenterr("marshal data too short"));
        }
        let b = self.data[self.pos];
        self.pos += 1;
        Ok(b)
    }

    fn read_bytes(&mut self, n: usize) -> Result<&'a [u8]> {
        if self.pos + n > self.data.len() {
            return Err(MonorubyErr::argumenterr("marshal data too short"));
        }
        let slice = &self.data[self.pos..self.pos + n];
        self.pos += n;
        Ok(slice)
    }

    /// Read a Marshal-format integer.
    ///
    /// Encoding (inverse of marshal_write_fixnum):
    /// - 0x00 → 0
    /// - 0x06..0x7f (1..122 after subtracting 5) → positive small int
    /// - 0x80..0xfa (-123..-1 after adding 5) → negative small int
    /// - 0x01..0x04 → positive multi-byte (1..4 LE bytes follow)
    /// - 0xfc..0xff → negative multi-byte (1..4 LE bytes follow, count = -n)
    fn read_fixnum(&mut self) -> Result<i32> {
        let b = self.read_byte()? as i8;
        if b == 0 {
            return Ok(0);
        }
        if b > 0 {
            if b <= 4 {
                // 1..4 positive bytes follow
                let nbytes = b as usize;
                let bytes = self.read_bytes(nbytes)?;
                let mut result: u32 = 0;
                for i in 0..nbytes {
                    result |= (bytes[i] as u32) << (i * 8);
                }
                Ok(result as i32)
            } else {
                // small positive: b - 5
                Ok(b as i32 - 5)
            }
        } else {
            // b < 0
            if b >= -4 {
                // -1..-4: negative multi-byte
                let nbytes = (-b) as usize;
                let bytes = self.read_bytes(nbytes)?;
                let mut result: u32 = 0xffffffff;
                for i in 0..nbytes {
                    // Clear byte position and set from data
                    result &= !(0xff << (i * 8));
                    result |= (bytes[i] as u32) << (i * 8);
                }
                Ok(result as i32)
            } else {
                // small negative: b + 5
                Ok(b as i32 + 5)
            }
        }
    }

    /// Read a symbol (type ':') and register it in the symbol table.
    fn read_new_symbol(&mut self) -> Result<IdentId> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let name = std::str::from_utf8(bytes)
            .map_err(|_| MonorubyErr::argumenterr("invalid symbol encoding in marshal data"))?;
        let id = IdentId::get_id(name);
        self.symbols.push(id);
        Ok(id)
    }

    /// Read a symbol, handling both ':' (new symbol) and ';' (symbol reference).
    fn read_symbol(&mut self) -> Result<IdentId> {
        let tag = self.read_byte()?;
        match tag {
            b':' => self.read_new_symbol(),
            b';' => {
                let idx = self.read_fixnum()? as usize;
                self.symbols.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad symbol reference in marshal data: {}",
                        idx
                    ))
                })
            }
            _ => Err(MonorubyErr::argumenterr(format!(
                "expected symbol tag (':' or ';'), got 0x{:02x}",
                tag
            ))),
        }
    }

    /// Read and return the next marshalled value.
    fn read_value(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let tag = self.read_byte()?;
        match tag {
            b'0' => Ok(Value::nil()),
            b'T' => Ok(Value::bool(true)),
            b'F' => Ok(Value::bool(false)),
            b'i' => {
                let n = self.read_fixnum()?;
                Ok(Value::integer(n as i64))
            }
            b'l' => self.read_bignum(),
            b'f' => self.read_float(),
            b':' => {
                let id = self.read_new_symbol()?;
                Ok(Value::symbol(id))
            }
            b';' => {
                let idx = self.read_fixnum()? as usize;
                let id = self.symbols.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad symbol reference in marshal data: {}",
                        idx
                    ))
                })?;
                Ok(Value::symbol(id))
            }
            b'"' => self.read_raw_string(Encoding::Ascii8),
            b'I' => self.read_ivar_wrapped(vm, globals),
            b'[' => self.read_array(vm, globals),
            b'{' => self.read_hash(vm, globals),
            b'@' => {
                // Object reference
                let idx = self.read_fixnum()? as usize;
                self.objects.get(idx).copied().ok_or_else(|| {
                    MonorubyErr::argumenterr(format!(
                        "bad object reference in marshal data: {}",
                        idx
                    ))
                })
            }
            b'o' => self.read_user_object(vm, globals),
            b'u' => self.read_user_marshal(vm, globals),
            // 'U' (TYPE_USRMARSHAL): an object whose class defines
            // `marshal_load`. Format: symbol(class) + inner value.
            // On load, allocate a fresh instance of the class and
            // drive `instance.marshal_load(value)`.
            b'U' => self.read_usr_marshal(vm, globals),
            // 'C' (TYPE_USERCLASS) wraps a built-in value in a
            // user-defined subclass (e.g. `class S < String; end`'s
            // instance). Format: symbol + inner value.
            b'C' => self.read_user_class(vm, globals),
            // 'e' (TYPE_EXTENDED) wraps a value that's been extended
            // with a module (`obj.extend(M)`). Format: symbol + inner
            // value (possibly another 'e' for further extensions).
            b'e' => self.read_extended(vm, globals),
            // '/' (TYPE_REGEXP) — `length(varint) + source-bytes +
            // 1-byte options flag` (CRuby option bits: 1=/i, 2=/x,
            // 4=/m, 0x10=FIXEDENCODING, 0x20=NOENCODING).
            b'/' => self.read_regexp(),
            _ => Err(MonorubyErr::argumenterr(format!(
                "unsupported marshal type tag: 0x{:02x} ('{}')",
                tag,
                if tag.is_ascii_graphic() {
                    tag as char
                } else {
                    '?'
                }
            ))),
        }
    }

    /// Read a Bignum value.
    /// Format: sign_byte('+'/'-') + marshal_int(word_count) + LE 16-bit words
    fn read_bignum(&mut self) -> Result<Value> {
        let sign_byte = self.read_byte()?;
        let word_count = self.read_fixnum()? as usize;
        let byte_count = word_count * 2;
        let bytes = self.read_bytes(byte_count)?;
        let sign = if sign_byte == b'-' {
            num::bigint::Sign::Minus
        } else {
            num::bigint::Sign::Plus
        };
        let n = BigInt::from_bytes_le(sign, bytes);
        let val = Value::bigint(n);
        self.objects.push(val);
        Ok(val)
    }

    /// Read a Float value.
    /// Format: marshal_int(string_length) + string_bytes
    fn read_float(&mut self) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let s = std::str::from_utf8(bytes)
            .map_err(|_| MonorubyErr::argumenterr("invalid float encoding in marshal data"))?;
        let f = match s {
            "inf" => f64::INFINITY,
            "-inf" => f64::NEG_INFINITY,
            "nan" => f64::NAN,
            "-0" => -0.0_f64,
            "0" => 0.0_f64,
            _ => s.parse::<f64>().map_err(|_| {
                MonorubyErr::argumenterr(format!("invalid float value in marshal data: {}", s))
            })?,
        };
        let val = Value::float(f);
        self.objects.push(val);
        Ok(val)
    }

    /// Read a raw string (after the '"' tag has already been consumed).
    /// Format: marshal_int(length) + bytes
    fn read_raw_string(&mut self, encoding: Encoding) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        let val = if encoding.is_utf8_compatible() {
            Value::string_from_inner(RStringInner::from_encoding(bytes, Encoding::Utf8))
        } else {
            // Non-UTF-8 encodings (Ascii8 / dummy): preserve the
            // declared encoding tag verbatim. Marshal data records
            // ASCII-8BIT for `String#b` etc., and round-tripping
            // dummy encodings should be lossless.
            Value::string_from_inner(RStringInner::from_encoding(bytes, encoding))
        };
        self.objects.push(val);
        Ok(val)
    }

    /// Read an instance-variable-wrapped object.
    /// Format: inner_object + marshal_int(ivar_count) + (symbol + value) pairs
    ///
    /// The most common case is a UTF-8 string: I + " + data + 1 + :E + T
    fn read_ivar_wrapped(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let inner_tag = self.read_byte()?;
        match inner_tag {
            b'"' => {
                // String with instance variables (typically encoding)
                let len = self.read_fixnum()? as usize;
                let bytes = self.read_bytes(len)?;
                let ivar_count = self.read_fixnum()? as usize;
                let mut encoding = Encoding::Ascii8;
                for _ in 0..ivar_count {
                    let sym = self.read_symbol()?;
                    let sym_name = sym.get_name();
                    if sym_name == "E" {
                        // Encoding flag
                        let enc_val_tag = self.read_byte()?;
                        match enc_val_tag {
                            b'T' => encoding = Encoding::Utf8,
                            b'F' => encoding = Encoding::Ascii8,
                            _ => {
                                // Could be a string encoding name
                                self.pos -= 1;
                                let enc_val = self.read_value(vm, globals)?;
                                if let Some(s) = enc_val.is_rstring_inner() {
                                    let enc_name = std::str::from_utf8(s.as_bytes()).unwrap_or("");
                                    encoding = Encoding::try_from_str(enc_name)
                                        .unwrap_or(Encoding::Ascii8);
                                }
                            }
                        }
                    } else {
                        // Skip other instance variables
                        let _val = self.read_value(vm, globals)?;
                    }
                }
                let val = Value::string_from_inner(RStringInner::from_encoding(bytes, encoding));
                self.objects.push(val);
                Ok(val)
            }
            _ => {
                // Other I-wrapped types (e.g. Regexp with encoding)
                self.pos -= 1;
                let val = self.read_value(vm, globals)?;
                // Read and skip instance variables
                let ivar_count = self.read_fixnum()? as usize;
                for _ in 0..ivar_count {
                    let _sym = self.read_symbol()?;
                    let _val = self.read_value(vm, globals)?;
                }
                Ok(val)
            }
        }
    }

    /// Read an Array.
    /// Format: marshal_int(length) + elements
    fn read_array(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Reserve an object slot so nested references work correctly
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil()); // placeholder
        let mut elems = Vec::with_capacity(len);
        for _ in 0..len {
            let val = self.read_value(vm, globals)?;
            elems.push(val);
        }
        let arr = Value::array_from_vec(elems);
        self.objects[obj_idx] = arr;
        Ok(arr)
    }

    /// Read a Hash.
    /// Format: marshal_int(length) + (key + value) pairs
    fn read_hash(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Reserve an object slot
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil()); // placeholder
        let mut map = RubyMap::default();
        for _ in 0..len {
            let key = self.read_value(vm, globals)?;
            let val = self.read_value(vm, globals)?;
            map.insert(key, val, vm, globals)?;
        }
        let hash = Value::hash(map);
        self.objects[obj_idx] = hash;
        Ok(hash)
    }

    /// Read a user-defined object ('o' tag).
    /// Format: symbol(class_name) + marshal_int(ivar_count) + (symbol + value) pairs
    ///
    /// We create a generic object with instance variables stored as a Hash.
    fn read_user_object(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let ivar_count = self.read_fixnum()? as usize;
        // Reserve an object slot
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil());
        let class_name = class_sym.get_name();

        // Look up the class by checking constants on Object
        let class_name_id = IdentId::get_id(&class_name);
        let (obj, native_class) = match globals
            .get_constant(OBJECT_CLASS, class_name_id)
            .and_then(|state| state.loaded_value())
        {
            Some(class_val) => {
                if let Some(module) = class_val.is_class_or_module() {
                    // Classes with a custom allocator have internal storage
                    // (e.g. Range, Time, Exception). Creating a generic
                    // Value::object for them would violate later invariants
                    // and cause native panics when methods run. Flag them so
                    // we can reconstruct or raise.
                    let uses_default = globals.store[module.id()]
                        .alloc_func()
                        .map(|f| f as *const () == crate::default_alloc_func as *const ())
                        .unwrap_or(true);
                    (Value::object(module.id()), !uses_default)
                } else {
                    return Err(MonorubyErr::argumenterr(format!(
                        "undefined class/module {}",
                        class_name
                    )));
                }
            }
            _ => {
                return Err(MonorubyErr::argumenterr(format!(
                    "undefined class/module {}",
                    class_name
                )));
            }
        };
        // Read instance variables.
        let mut ivars: Vec<(IdentId, Value)> = Vec::with_capacity(ivar_count);
        for _ in 0..ivar_count {
            let ivar_sym = self.read_symbol()?;
            let val = self.read_value(vm, globals)?;
            ivars.push((ivar_sym, val));
        }
        // Built-in classes with internal storage (e.g. Range) can't be
        // represented as generic 'o' objects. If the class is Range, try to
        // reconstruct a real Range from its @begin/@end/@excl ivars.
        let built_obj = if native_class && class_name != "Range" {
            return Err(MonorubyErr::argumenterr(format!(
                "can't load instance of {} from generic marshal object",
                class_name
            )));
        } else if class_name == "Range" {
            let mut begin = Value::nil();
            let mut end = Value::nil();
            let mut excl = false;
            for (name, val) in &ivars {
                match name.get_name().as_str() {
                    "begin" => begin = *val,
                    "end" => end = *val,
                    "excl" => excl = val.as_bool(),
                    _ => {}
                }
            }
            Value::range(begin, end, excl)
        } else {
            for (name, val) in &ivars {
                globals.set_ivar(obj, *name, *val)?;
            }
            obj
        };
        self.objects[obj_idx] = built_obj;
        Ok(built_obj)
    }

    /// Read a user-defined object ('u' tag, TYPE_USERDEF).
    /// Format: `symbol(class_name) + length(varint) + raw-bytes`.
    /// On load CRuby calls the class's `_load` *class* method with
    /// the raw bytes as a String and returns the result. (This is
    /// distinct from `'U'` / `TYPE_USRMARSHAL`, which dispatches to
    /// the *instance* method `marshal_load`.)
    ///
    /// Until this fix, monoruby read the symbol and then ran
    /// `read_value` to consume the trailing payload — but `'u'`'s
    /// payload is *not* a nested marshal value; it's a raw byte
    /// string. The misalignment surfaced as bogus
    /// "unsupported marshal type tag: 0xNN ('?')" errors on the
    /// next dispatch because `read_value` reinterpreted the length
    /// byte as a tag.
    fn read_user_marshal(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?.to_vec();
        let class_name = class_sym.get_name();
        let class_name_id = IdentId::get_id(&class_name);
        let class_val = globals
            .get_constant(OBJECT_CLASS, class_name_id)
            .and_then(|state| state.loaded_value())
            .ok_or_else(|| {
                MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
            })?;
        let module = class_val.is_class_or_module().ok_or_else(|| {
            MonorubyErr::argumenterr(format!("{} is not a class", class_name))
        })?;
        // Hand the raw payload as a binary String to `_load` (CRuby
        // semantics — the payload is whatever `_dump` produced, so
        // ASCII-8BIT preserves the bytes verbatim).
        let payload = Value::bytes(bytes);
        let load_id = IdentId::get_id("_load");
        vm.invoke_method_inner(globals, load_id, module.as_val(), &[payload], None, None)
    }

    /// Read a user-marshal object ('U' tag, TYPE_USRMARSHAL).
    /// Format: `symbol(class_name) + inner-value`.
    /// On load CRuby allocates a fresh instance of the class and
    /// then drives `instance.marshal_load(value)` — `instance` is
    /// returned (the `marshal_load` return value is ignored).
    fn read_usr_marshal(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let value = self.read_value(vm, globals)?;
        let class_name = class_sym.get_name();
        let class_name_id = IdentId::get_id(&class_name);
        let class_val = globals
            .get_constant(OBJECT_CLASS, class_name_id)
            .and_then(|state| state.loaded_value())
            .ok_or_else(|| {
                MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
            })?;
        let module = class_val.is_class_or_module().ok_or_else(|| {
            MonorubyErr::argumenterr(format!("{} is not a class", class_name))
        })?;
        // Allocate without running initialize (CRuby `allocate`
        // shape). `call_alloc_func` raises TypeError if the class
        // never declared an allocator — surface that verbatim.
        let instance = crate::builtins::class::call_alloc_func(globals, module.id())?;
        // Drive `instance.marshal_load(value)`. The return value is
        // discarded; the spec checks the receiver's resulting state.
        let marshal_load_id = IdentId::get_id("marshal_load");
        vm.invoke_method_inner(globals, marshal_load_id, instance, &[value], None, None)?;
        Ok(instance)
    }

    /// Read a user-class wrapper ('C' tag).
    /// Format: symbol(class_name) + inner value.
    /// The inner is a built-in value (String, Array, Hash, …) that was
    /// produced by an instance of a subclass; on load we reconstruct
    /// the inner natively and then swap its class to the named subclass.
    fn read_user_class(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let mut inner = self.read_value(vm, globals)?;
        let class_name = class_sym.get_name();
        let class_name_id = IdentId::get_id(&class_name);
        let class_val = globals
            .get_constant(OBJECT_CLASS, class_name_id)
            .and_then(|state| state.loaded_value())
            .ok_or_else(|| {
                MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
            })?;
        let module = class_val.is_class_or_module().ok_or_else(|| {
            MonorubyErr::argumenterr(format!("{} is not a class", class_name))
        })?;
        // CRuby's 'C' wraps a built-in (whose RValue layout is fixed),
        // so it is safe to swap the class on the already-allocated
        // inner — the storage type does not change, only the
        // user-visible class.
        inner.change_class(module.id());
        Ok(inner)
    }

    /// Read a Regexp ('/' tag).
    ///
    /// Format: `length(varint) + source-bytes + 1-byte options`.
    /// The options byte uses CRuby's bit layout — `/i` = 0x01,
    /// `/x` = 0x02, `/m` = 0x04, `FIXEDENCODING` = 0x10,
    /// `NOENCODING` = 0x20. monoruby's `RegexpInner::*_FLAG` bits
    /// happen to use the same values for the first three, so the
    /// CRuby byte can be passed through verbatim.
    fn read_regexp(&mut self) -> Result<Value> {
        use crate::value::rvalue::RegexpInner;
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?.to_vec();
        let opt_byte = self.read_byte()? as u32;
        // Use the default UTF-8 onigmo encoding; the ivar wrapper
        // ('I') around `/` sets the right per-string encoding ivar
        // afterwards if the original regex was non-UTF-8.
        let src = match String::from_utf8(bytes.clone()) {
            Ok(s) => s,
            Err(_) => {
                // Non-UTF-8 source — re-encode lossily so onigmo gets
                // a valid `&str`. The 'I' wrapper restores the right
                // declared encoding for inspection.
                String::from_utf8_lossy(&bytes).into_owned()
            }
        };
        let inner = RegexpInner::with_option_kcode(
            src,
            opt_byte,
            onigmo_regex::OnigmoEncoding::UTF8,
            None,
            None,
        )?;
        Ok(Value::regexp(inner))
    }

    /// Read an extended-object wrapper ('e' tag).
    /// Format: symbol(module_name) + inner value (possibly another 'e').
    /// On load, fetch the module by name and drive `inner.extend(module)`
    /// through the standard dispatch so the module is added to the
    /// inner's singleton-class ancestor chain.
    fn read_extended(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let module_sym = self.read_symbol()?;
        let inner = self.read_value(vm, globals)?;
        let module_name = module_sym.get_name();
        let module_name_id = IdentId::get_id(&module_name);
        let module_val = globals
            .get_constant(OBJECT_CLASS, module_name_id)
            .and_then(|state| state.loaded_value())
            .ok_or_else(|| {
                MonorubyErr::argumenterr(format!("undefined module {}", module_name))
            })?;
        // Drive Kernel#extend so the singleton-class plumbing matches a
        // hand-coded `inner.extend(Mod)` exactly (Module#extended hook,
        // ancestor placement, etc.). The return value is the receiver.
        let extend_id = IdentId::get_id("extend");
        vm.invoke_method_inner(globals, extend_id, inner, &[module_val], None, None)?;
        Ok(inner)
    }
}

// ============================================================
// Marshal writer helpers
// ============================================================

/// Write a Marshal-format integer (used for lengths and fixnum values).
///
/// Encoding rules:
/// - 0 → 0x00
/// - 1..122 → n + 5
/// - -123..-1 → n - 5
/// - |n| > 122 → sign byte (1..4 or -1..-4) followed by that many LE bytes
fn marshal_write_fixnum(buf: &mut Vec<u8>, n: i32) {
    if n == 0 {
        buf.push(0x00);
    } else if n > 0 && n <= 122 {
        buf.push((n + 5) as u8);
    } else if n < 0 && n >= -123 {
        buf.push((n - 5) as u8);
    } else if n > 0 {
        let bytes = n.to_le_bytes();
        let len = if n <= 0xff {
            1
        } else if n <= 0xffff {
            2
        } else if n <= 0xffffff {
            3
        } else {
            4
        };
        buf.push(len);
        buf.extend_from_slice(&bytes[..len as usize]);
    } else {
        // negative, |n| > 123
        let bytes = n.to_le_bytes();
        let len = if n >= -0x100 {
            1
        } else if n >= -0x10000 {
            2
        } else if n >= -0x1000000 {
            3
        } else {
            4
        };
        buf.push((-len) as u8);
        buf.extend_from_slice(&bytes[..len as usize]);
    }
}

/// Write a Bignum in Marshal format.
///
/// Format: 'l' + sign_byte('+'/'-') + marshal_int(word_count) + LE 16-bit words
fn marshal_write_bignum(buf: &mut Vec<u8>, n: &BigInt) {
    use num::bigint::Sign;
    buf.push(b'l');
    let (sign, le_bytes): (num::bigint::Sign, Vec<u8>) = n.to_bytes_le();
    buf.push(if sign == Sign::Minus { b'-' } else { b'+' });
    // word count in 16-bit (2-byte) units, rounded up
    let word_count = (le_bytes.len() + 1) / 2;
    marshal_write_fixnum(buf, word_count as i32);
    // Write bytes, padding to even length if needed
    buf.extend_from_slice(&le_bytes);
    if le_bytes.len() % 2 != 0 {
        buf.push(0x00);
    }
}

/// Format a float for Marshal in the same way as CRuby's ruby_dtoa (shortest representation).
///
/// CRuby uses David Gay's dtoa mode 0 which produces the shortest decimal string
/// that uniquely identifies the float, formatted like `%g` (switching to exponential
/// when exponent < -4 or when there are trailing zeros in integer part).
///
/// Examples: 1.0 → "1", 10.0 → "1e1", 12.0 → "12", 0.1 → "0.1", 1.23e5 → "1.23e5"
fn float_to_marshal_string(f: f64) -> String {
    let neg = f.is_sign_negative();
    let f_abs = f.abs();

    // Get shortest digits from dtoa
    let s = dtoa::Buffer::new().format(f_abs).to_string();

    // Parse the dtoa output to extract significant digits and decimal exponent.
    // dtoa produces forms like "1.0", "123.0", "0.001", "0.1", "3.14159265358979",
    // or "1.23e100" for very large/small numbers.
    let (digits, exponent) = parse_dtoa_output(&s);

    // Format using ruby_dtoa rules:
    // - If exponent >= significand length and exponent > 1: use exponential (e.g. 10.0 → "1e1")
    // - If exponent <= -5: use exponential (e.g. 1e-5 → "1e-5")
    // - Otherwise: use decimal notation
    let prefix = if neg { "-" } else { "" };
    let ndigits = digits.len() as i32;

    if exponent <= -4
        || (exponent > ndigits && exponent > 1)
        || (exponent >= 1 && has_trailing_zeros(&digits, exponent))
    {
        // Exponential form
        let exp = exponent - 1;
        let sig = if ndigits == 1 {
            digits.clone()
        } else {
            format!("{}.{}", &digits[..1], &digits[1..])
        };
        format!("{prefix}{sig}e{exp}")
    } else if exponent <= 0 {
        // 0.00...0digits form
        let zeros = (-exponent) as usize;
        format!("{prefix}0.{}{}", "0".repeat(zeros), digits)
    } else if exponent >= ndigits {
        // All digits before decimal, no fraction
        format!("{prefix}{}", digits)
    } else {
        // Mixed: some digits before decimal, some after
        let exp = exponent as usize;
        format!("{prefix}{}.{}", &digits[..exp], &digits[exp..])
    }
}

/// Check if the integer representation would have trailing zeros.
/// digits = significant digits, exponent = position of decimal point from left.
/// E.g., digits="1", exponent=2 means "100" → has trailing zeros.
fn has_trailing_zeros(digits: &str, exponent: i32) -> bool {
    exponent > digits.len() as i32
}

/// Parse dtoa output into (significant_digits, exponent).
/// The exponent is the power of 10 such that the value equals 0.{digits} * 10^exponent.
/// Actually, it's the number of digits before the decimal point in the standard form.
///
/// E.g.: "1.0" → ("1", 1), "123.0" → ("123", 3), "0.001" → ("1", -2),
///        "3.14" → ("314", 1), "1.23e100" → ("123", 101)
fn parse_dtoa_output(s: &str) -> (String, i32) {
    if let Some(e_pos) = s.find('e') {
        let mantissa = &s[..e_pos];
        let exp_str = &s[e_pos + 1..];
        let exp_val: i32 = exp_str.parse().unwrap();
        let (digits, dot_exp) = parse_decimal(mantissa);
        (digits, dot_exp + exp_val)
    } else {
        parse_decimal(s)
    }
}

/// Parse a decimal string (no 'e') into (significant_digits, exponent).
/// "1.0" → ("1", 1), "123.0" → ("123", 3), "0.001" → ("1", -2), "0.1" → ("1", 0)
fn parse_decimal(s: &str) -> (String, i32) {
    if let Some(dot_pos) = s.find('.') {
        let int_part = &s[..dot_pos];
        let frac_part = &s[dot_pos + 1..];
        // Combine all significant digits (strip leading zeros from combined)
        let mut all_digits = String::new();
        all_digits.push_str(int_part);
        all_digits.push_str(frac_part);
        // Remove trailing zeros from fraction part contribution
        let trimmed = all_digits.trim_end_matches('0');
        if trimmed.is_empty() || trimmed == "0" {
            return ("0".to_string(), 1);
        }
        // Remove leading zeros
        let trimmed = trimmed.trim_start_matches('0');
        let exponent = if int_part == "0" {
            // 0.XYZ case: exponent is -(number of leading zeros in frac)
            let leading_zeros = frac_part.len() - frac_part.trim_start_matches('0').len();
            -(leading_zeros as i32)
        } else {
            int_part.len() as i32
        };
        (trimmed.to_string(), exponent)
    } else {
        // No decimal point (shouldn't happen with dtoa, but handle it)
        let trimmed = s.trim_end_matches('0');
        if trimmed.is_empty() {
            ("0".to_string(), 1)
        } else {
            (trimmed.to_string(), s.len() as i32)
        }
    }
}

/// Write a float in Marshal format.
///
/// Format: 'f' + marshal_int(string_length) + string_bytes
/// Special cases: 0.0 → "0", -0.0 → "-0", inf → "inf", -inf → "-inf", nan → "nan"
fn marshal_write_float(buf: &mut Vec<u8>, f: f64) {
    buf.push(b'f');
    let s = if f == 0.0 {
        if f.is_sign_negative() {
            "-0".to_string()
        } else {
            "0".to_string()
        }
    } else if f.is_infinite() {
        if f.is_sign_negative() {
            "-inf".to_string()
        } else {
            "inf".to_string()
        }
    } else if f.is_nan() {
        "nan".to_string()
    } else {
        float_to_marshal_string(f)
    };
    marshal_write_fixnum(buf, s.len() as i32);
    buf.extend_from_slice(s.as_bytes());
}

/// Write a Symbol in Marshal format, with symbol table dedup.
///
/// If the symbol has been seen before, write ';' + index.
/// Otherwise, write ':' + marshal_int(length) + bytes and record it.
fn marshal_write_symbol(buf: &mut Vec<u8>, id: IdentId, symbols: &mut Vec<IdentId>) {
    if let Some(idx) = symbols.iter().position(|&s| s == id) {
        buf.push(b';');
        marshal_write_fixnum(buf, idx as i32);
    } else {
        buf.push(b':');
        let name = id.get_name();
        let bytes = name.as_bytes();
        marshal_write_fixnum(buf, bytes.len() as i32);
        buf.extend_from_slice(bytes);
        symbols.push(id);
    }
}

/// Write a String in Marshal format.
///
/// - ASCII-8BIT: '"' + marshal_int(length) + bytes
/// - UTF-8: 'I' + '"' + marshal_int(length) + bytes + ivar_count(1) + :E + true
fn marshal_write_string(buf: &mut Vec<u8>, s: &RStringInner, symbols: &mut Vec<IdentId>) {
    let bytes = s.as_bytes();
    match s.encoding() {
        Encoding::Utf8 => {
            buf.push(b'I');
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(bytes);
            // Instance variable: encoding = UTF-8
            marshal_write_fixnum(buf, 1); // 1 ivar
            marshal_write_symbol(buf, IdentId::get_id("E"), symbols);
            buf.push(b'T'); // true
        }
        Encoding::UsAscii | Encoding::Ascii8 => {
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(bytes);
        }
        // Dummy / non-UTF-8 encodings: write the bytes opaquely
        // without an encoding ivar. Marshal round-trip will lose
        // the declared encoding (consistent with monoruby's prior
        // ASCII-8BIT-only behaviour for these names).
        _ => {
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(bytes);
        }
    }
}

fn marshal_dump_value(
    buf: &mut Vec<u8>,
    obj: Value,
    globals: &Globals,
    symbols: &mut Vec<IdentId>,
    in_progress: &mut std::collections::HashSet<u64>,
) -> Result<()> {
    match obj.unpack() {
        RV::Nil => {
            buf.push(b'0'); // 0x30
        }
        RV::Bool(true) => {
            buf.push(b'T'); // 0x54
        }
        RV::Bool(false) => {
            buf.push(b'F'); // 0x46
        }
        RV::Fixnum(n) => {
            // Fixnums that fit in 30-bit signed range use 'i' format
            // Larger fixnums must use bignum 'l' format
            if n >= -0x40000000 && n <= 0x3fffffff {
                buf.push(b'i');
                marshal_write_fixnum(buf, n as i32);
            } else {
                // i64 values outside 30-bit range → treat as bignum
                let big = BigInt::from(n);
                marshal_write_bignum(buf, &big);
            }
        }
        RV::BigInt(n) => {
            // Check if it fits in fixnum range
            use num::ToPrimitive;
            if let Some(i) = n.to_i32() {
                if i >= -0x40000000 && i <= 0x3fffffff {
                    buf.push(b'i');
                    marshal_write_fixnum(buf, i);
                    return Ok(());
                }
            }
            marshal_write_bignum(buf, n);
        }
        RV::Float(f) => {
            marshal_write_float(buf, f);
        }
        RV::Symbol(id) => {
            marshal_write_symbol(buf, id, symbols);
        }
        RV::String(s) => {
            marshal_write_string(buf, s, symbols);
        }
        RV::Object(_rv) => {
            let obj_id = obj.id();
            if !in_progress.insert(obj_id) {
                return Err(MonorubyErr::argumenterr("can't dump cyclic reference"));
            }
            let r = (|| -> Result<()> {
                // Check for Array and Hash via ty()
                match obj.ty() {
                    Some(ObjTy::ARRAY) => {
                        let inner = obj.as_array_inner();
                        buf.push(b'[');
                        marshal_write_fixnum(buf, inner.len() as i32);
                        for elem in inner.iter() {
                            marshal_dump_value(buf, *elem, globals, symbols, in_progress)?;
                        }
                    }
                    Some(ObjTy::RANGE) => {
                        // Serialize Range as a generic 'o' object with the
                        // three ivars CRuby uses: @begin, @end, @excl.
                        let range = obj.as_range();
                        let begin = range.start();
                        let end = range.end();
                        let excl = range.exclude_end();
                        buf.push(b'o');
                        marshal_write_symbol(buf, IdentId::get_id("Range"), symbols);
                        marshal_write_fixnum(buf, 3);
                        marshal_write_symbol(buf, IdentId::get_id("begin"), symbols);
                        marshal_dump_value(buf, begin, globals, symbols, in_progress)?;
                        marshal_write_symbol(buf, IdentId::get_id("end"), symbols);
                        marshal_dump_value(buf, end, globals, symbols, in_progress)?;
                        marshal_write_symbol(buf, IdentId::get_id("excl"), symbols);
                        buf.push(if excl { b'T' } else { b'F' });
                    }
                    Some(ObjTy::HASH) => {
                        let inner = obj.as_hashmap_inner();
                        buf.push(b'{');
                        marshal_write_fixnum(buf, inner.len() as i32);
                        for (k, v) in inner.iter() {
                            marshal_dump_value(buf, k, globals, symbols, in_progress)?;
                            marshal_dump_value(buf, v, globals, symbols, in_progress)?;
                        }
                    }
                    Some(ObjTy::OBJECT) => {
                        // User-defined object with instance variables: 'o' tag
                        let class_id = obj.class();
                        let class_name = globals.get_class_name(class_id);
                        let class_name_id = IdentId::get_id(&class_name);
                        let ivars = globals.get_ivars(obj);
                        buf.push(b'o');
                        marshal_write_symbol(buf, class_name_id, symbols);
                        marshal_write_fixnum(buf, ivars.len() as i32);
                        for (name, val) in ivars {
                            marshal_write_symbol(buf, name, symbols);
                            marshal_dump_value(buf, val, globals, symbols, in_progress)?;
                        }
                    }
                    _ => {
                        return Err(MonorubyErr::typeerr(format!(
                            "no _dump_data is defined for class {}",
                            globals.get_class_name(obj.class())
                        )));
                    }
                }
                Ok(())
            })();
            in_progress.remove(&obj_id);
            r?;
        }
        _ => {
            return Err(MonorubyErr::typeerr(format!(
                "no _dump_data is defined for class {}",
                globals.get_class_name(obj.class())
            )));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    /// `Marshal.load` 'C' (TYPE_USERCLASS) and 'e' (TYPE_EXTENDED)
    /// tag readers. The dump side is unchanged — only the load
    /// dispatch is exercised here.
    ///
    /// CRuby generates 'C'-tagged bytes for any built-in (`String`,
    /// `Array`, `Hash`, …) instance whose actual class is a user
    /// subclass, and 'e'-tagged bytes when the instance has been
    /// `obj.extend(Module)`'d. monoruby's reader now reconstructs
    /// `Marshal.load` 'u' (TYPE_USERDEF) reader: read the length-
    /// prefixed payload (a raw byte string, *not* a nested marshal
    /// value), then call the class's `_load` class method with it.
    /// The earlier stub mis-decoded the payload as a nested value,
    /// so the length byte was reinterpreted as a tag and the next
    /// dispatch raised
    /// `unsupported marshal type tag: 0xNN ('?')`.
    #[test]
    fn marshal_load_user_def() {
        run_test(
            r##"
            class UD
              def _dump(level); "payload-data"; end
              def self._load(s); n = new; n.instance_variable_set(:@x, s); n; end
            end
            # CRuby-produced bytes for `UD.new`'s _dump value.
            bytes = "\x04\x08\x49\x75\x3a\x07\x55\x44\x11\x70\x61\x79\x6c\x6f\x61\x64\x2d\x64\x61\x74\x61\x06\x3a\x06\x45\x46"
            o = Marshal.load(bytes)
            [o.class, o.instance_variable_get(:@x)]
            "##,
        );
    }

    /// the inner natively, then swaps class / drives `extend` so the
    /// `Marshal.load` 'U' (TYPE_USRMARSHAL) reader: allocate an
    /// instance of the named class and drive its `marshal_load`
    /// with the inner value. CRuby preserves the call shape
    /// (the return value of `marshal_load` is ignored — the
    /// instance itself is the result).
    #[test]
    fn marshal_load_usr_marshal() {
        run_test(
            r##"
            class UMS
              def marshal_dump; "hello-data"; end
              def marshal_load(d); @loaded = d; end
            end
            # CRuby-produced bytes for `UMS.new`'s marshal_dump
            # ("hello-data" packed as an IVar-wrapped String).
            bytes = "\x04\x08\x55\x3a\x08\x55\x4d\x53\x49\x22\x0f\x68\x65\x6c\x6c\x6f\x2d\x64\x61\x74\x61\x06\x3a\x06\x45\x46"
            o = Marshal.load(bytes)
            [o.class, o.instance_variable_get(:@loaded)]
            "##,
        );
    }

    /// loaded object is an instance of the user class / extended
    /// with the named module.
    #[test]
    fn marshal_load_regexp() {
        // CRuby-produced bytes for `/abc/i`: header / 'I' wrap / '/' tag /
        // length(3) / "abc" / 0x01 (=/i) / 1 ivar (E => false). The
        // assertion checks the user-visible state (class / source /
        // options), not Regexp#== — internal-flag round-tripping is
        // out of scope for this load-side test.
        run_test(
            r##"
            bytes = "\x04\x08\x49\x2f\x08\x61\x62\x63\x01\x06\x3a\x06\x45\x46"
            r = Marshal.load(bytes)
            [r.class, r.source, r.options]
            "##,
        );
        // Multi-flag: `/foo/imx` ⇒ flags byte 0x07.
        run_test(
            r##"
            bytes = "\x04\x08\x49\x2f\x08\x66\x6f\x6f\x07\x06\x3a\x06\x45\x46"
            r = Marshal.load(bytes)
            [r.source, r.options]
            "##,
        );
    }

    #[test]
    fn marshal_load_userclass_and_extended() {
        run_test(
            r##"
            class MarshalUC < String; end
            # CRuby-produced bytes for `MarshalUC.new("hi")` (the I'/C
            # wrapper carries the encoding ivar; we only care that the
            # loaded value is an MS instance with the right content).
            bytes = "\x04\x08\x49\x43\x3a\x0e\x4d\x61\x72\x73\x68\x61\x6c\x55\x43\x22\x07\x68\x69\x06\x3a\x06\x45\x46"
            l = Marshal.load(bytes)
            [l, l.class]
            "##,
        );
        run_test(
            r##"
            module MarshalExt; end
            # CRuby-produced bytes for `"x".extend(MarshalExt)`.
            bytes = "\x04\x08\x49\x65\x3a\x0f\x4d\x61\x72\x73\x68\x61\x6c\x45\x78\x74\x22\x06\x78\x06\x3a\x06\x45\x46"
            l = Marshal.load(bytes)
            [l, l.singleton_class.ancestors.include?(MarshalExt)]
            "##,
        );
    }

    /// Error arms of `read_user_class` / `read_extended` —
    /// the constant lookup miss and the "constant exists but is not
    /// a class/module" branches that 88.88% patch coverage flagged.
    #[test]
    fn marshal_load_userclass_and_extended_errors() {
        // 'C' tag naming a constant that does not exist on Object.
        run_test_error(
            r##"
            bytes = "\x04\x08\x49\x43\x3a\x16\x4d\x61\x72\x73\x68\x61\x6c\x55\x43\x5f\x4d\x69\x73\x73\x69\x6e\x67\x22\x07\x68\x69\x06\x3a\x06\x45\x46"
            Marshal.load(bytes)
            "##,
        );
        // 'e' tag naming a missing module.
        run_test_error(
            r##"
            bytes = "\x04\x08\x49\x65\x3a\x0d\x4d\x5f\x65\x5f\x6d\x69\x73\x73\x22\x06\x78\x06\x3a\x06\x45\x46"
            Marshal.load(bytes)
            "##,
        );
        // 'C' tag pointing at a constant that exists but isn't a class.
        run_test_error(
            r##"
            MARSHAL_NOT_A_CLASS = 42
            bytes = "\x04\x08\x49\x43\x3a\x18\x4d\x41\x52\x53\x48\x41\x4c\x5f\x4e\x4f\x54\x5f\x41\x5f\x43\x4c\x41\x53\x53\x22\x07\x68\x69\x06\x3a\x06\x45\x46"
            Marshal.load(bytes)
            "##,
        );
    }

    /// CRuby's `Marshal.dump(obj, port=nil, level=-1)` accepts 1..3
    /// args, with an Integer second arg being interpreted as the
    /// recursion-depth limit (not a port). `Marshal.load` accepts
    /// `(source, proc=nil)`. Cover both arity arms and the IO-port
    /// write-back form.
    #[test]
    fn marshal_dump_load_arity() {
        run_test_with_prelude(
            r##"
            res = []
            # (obj, level) — Integer 2nd arg is the depth limit, NOT a port.
            res << (Marshal.dump(123, -1) == Marshal.dump(123))
            # (obj, port) — bytes flow to the writer; the call returns the writer.
            io = StringIO.new
            ret = Marshal.dump([1, 2, 3], io)
            res << ret.equal?(io)
            res << (Marshal.load(io.string) == [1, 2, 3])
            # (obj, port, level) — the level is accepted (and ignored).
            io2 = StringIO.new
            Marshal.dump("hi", io2, -1)
            res << (Marshal.load(io2.string) == "hi")
            # Non-writer port ⇒ TypeError, matching CRuby's text.
            begin
              Marshal.dump(1, Object.new)
            rescue TypeError => e
              res << e.message
            end
            # Marshal.load accepts a proc as the second arg (ignored).
            res << Marshal.load(Marshal.dump(:sym), proc { |_| })
            res
            "##,
            r##"require "stringio""##,
        );
    }

    #[test]
    fn marshal_dump_load() {
        //run_test("Marshal.dump(-0.0)"); // monoruby doesn't preserve -0.0 literal yet
        run_tests(&[
            "Marshal.dump(nil)",
            "Marshal.dump(true)",
            "Marshal.dump(false)",
            "Marshal.dump(0)",
            "Marshal.dump(1)",
            "Marshal.dump(-1)",
            "Marshal.dump(5)",
            "Marshal.dump(-5)",
            "Marshal.dump(122)",
            "Marshal.dump(-123)",
            "Marshal.dump(123)",
            "Marshal.dump(-124)",
            "Marshal.dump(255)",
            "Marshal.dump(256)",
            "Marshal.dump(65535)",
            "Marshal.dump(65536)",
            "Marshal.dump(0xffffff)",
            "Marshal.dump(0x1000000)",
            "Marshal.dump(0x3fffffff)",
            "Marshal.dump(-0x40000000)",
            "Marshal.dump(0x40000000)",
            "Marshal.dump(-0x40000001)",
            "Marshal.dump(2**64)",
            "Marshal.dump(-(2**64))",
            "Marshal.dump(0.0)",
            "Marshal.dump(1.0)",
            "Marshal.dump(-1.5)",
            "Marshal.dump(1.0/0)",
            "Marshal.dump(-1.0/0)",
            "Marshal.dump(0.0/0.0)",
            "Marshal.dump(3.14159265358979)",
            "Marshal.dump(100.0)",
            "Marshal.dump(12.0)",
            "Marshal.dump(0.1)",
            "Marshal.dump(0.001)",
            "Marshal.dump(1.23e5)",
            "Marshal.dump(1.23e-5)",
            "Marshal.dump(1.7976931348623157e308)",
            "Marshal.dump(0.0001)",
            "Marshal.dump(0.00001)",
            "Marshal.dump(-100.0)",
            "Marshal.dump(-1.23e-5)",
            "Marshal.dump(2.718281828459045)",
            "Marshal.dump(10.0)",
            "Marshal.dump(1000.0)",
            "Marshal.dump(:hello)",
            "Marshal.dump(:foo)",
            "Marshal.dump(:\"\")",
            "Marshal.dump(:a)",
            "Marshal.dump(:marshal_test_symbol)",
            r#"Marshal.dump("hello")"#,
            r#"Marshal.dump("")"#,
            r#"Marshal.dump("a")"#,
            r#"Marshal.dump("hello world")"#,
            r#"Marshal.dump("hello".b)"#,
            r#"Marshal.dump("".b)"#,
            "Marshal.dump([])",
            "Marshal.dump([1, 2, 3])",
            r#"Marshal.dump([1, "hello", :foo, nil, true, false])"#,
            "Marshal.dump([[1, 2], [3, 4]])",
            "Marshal.dump({})",
            r#"Marshal.dump({a: 1, b: 2})"#,
            r#"Marshal.dump({"key" => "value"})"#,
            "Marshal.load(Marshal.dump(nil))",
            "Marshal.load(Marshal.dump(true))",
            "Marshal.load(Marshal.dump(false))",
            "Marshal.load(Marshal.dump(0))",
            "Marshal.load(Marshal.dump(1))",
            "Marshal.load(Marshal.dump(-1))",
            "Marshal.load(Marshal.dump(122))",
            "Marshal.load(Marshal.dump(-123))",
            "Marshal.load(Marshal.dump(123))",
            "Marshal.load(Marshal.dump(-124))",
            "Marshal.load(Marshal.dump(255))",
            "Marshal.load(Marshal.dump(256))",
            "Marshal.load(Marshal.dump(65535))",
            "Marshal.load(Marshal.dump(65536))",
            "Marshal.load(Marshal.dump(0xffffff))",
            "Marshal.load(Marshal.dump(0x1000000))",
            "Marshal.load(Marshal.dump(0x3fffffff))",
            "Marshal.load(Marshal.dump(-0x40000000))",
            "Marshal.load(Marshal.dump(0x40000000))",
            "Marshal.load(Marshal.dump(-0x40000001))",
            "Marshal.load(Marshal.dump(2**64))",
            "Marshal.load(Marshal.dump(-(2**64)))",
            "Marshal.load(Marshal.dump(0.0))",
            "Marshal.load(Marshal.dump(1.0))",
            "Marshal.load(Marshal.dump(-1.5))",
            "Marshal.load(Marshal.dump(3.14159265358979))",
            "Marshal.load(Marshal.dump(100.0))",
            "Marshal.load(Marshal.dump(0.1))",
            "Marshal.load(Marshal.dump(:hello))",
            "Marshal.load(Marshal.dump(:foo))",
            "Marshal.load(Marshal.dump(:a))",
            r#"Marshal.load(Marshal.dump("hello"))"#,
            r#"Marshal.load(Marshal.dump(""))"#,
            r#"Marshal.load(Marshal.dump("hello world"))"#,
            r#"Marshal.load(Marshal.dump("hello".b))"#,
            "Marshal.load(Marshal.dump([]))",
            "Marshal.load(Marshal.dump([1, 2, 3]))",
            r#"Marshal.load(Marshal.dump([1, "hello", :foo, nil, true, false]))"#,
            "Marshal.load(Marshal.dump([[1, 2], [3, 4]]))",
            "Marshal.load(Marshal.dump({}))",
            r#"Marshal.load(Marshal.dump({a: 1, b: 2}))"#,
            r#"Marshal.load(Marshal.dump({"key" => "value"}))"#,
            r#"Marshal.load(Marshal.dump({a: [1, 2, 3], b: "hello", c: {d: :e}}))"#,
        ]);
    }

    #[test]
    fn marshal_dump_user_object() {
        run_test(
            r#"
            class Foo
              def initialize(x, y)
                @x = x
                @y = y
              end
            end
            Marshal.dump(Foo.new(1, 2))
        "#,
        );
        run_test(
            r#"
            class Bar
              def initialize
                @name = "hello"
                @value = 42
                @items = [1, 2, 3]
              end
            end
            Marshal.dump(Bar.new)
        "#,
        );
    }

    #[test]
    fn marshal_roundtrip_user_object() {
        run_test(
            r#"
            class Foo
              attr_accessor :x, :y
              def initialize(x, y)
                @x = x
                @y = y
              end
            end
            obj = Marshal.load(Marshal.dump(Foo.new(1, 2)))
            [obj.class.name, obj.x, obj.y]
        "#,
        );
        run_test(
            r#"
            class Baz
              attr_accessor :name, :items
              def initialize
                @name = "test"
                @items = [1, :foo, nil]
              end
            end
            obj = Marshal.load(Marshal.dump(Baz.new))
            [obj.name, obj.items]
        "#,
        );
    }

    #[test]
    fn marshal_load_errors() {
        // Empty data
        run_test_error(r#"Marshal.load("")"#);
        // Too short (only 1 byte)
        run_test_error(r#"Marshal.load("\x04")"#);
        // Wrong version
        run_test_error(r#"Marshal.load("\x03\x08")"#);
        // Unsupported type tag
        run_test_error(r#"Marshal.load("\x04\x08Q")"#);
    }

    #[test]
    fn marshal_dump_unsupported() {
        // Regexp is not supported
        run_test_error(r#"Marshal.dump(/foo/)"#);
    }

    #[test]
    fn marshal_dump_cyclic_array() {
        // Recursive arrays must raise ArgumentError, not overflow the stack.
        run_test_error(
            r#"
            a = []
            a << a
            Marshal.dump(a)
            "#,
        );
    }

    #[test]
    fn marshal_dump_cyclic_hash() {
        run_test_error(
            r#"
            h = {}
            h[:self] = h
            Marshal.dump(h)
            "#,
        );
    }

    #[test]
    fn marshal_range_roundtrip() {
        // A Range reconstructed from the 'o' format should behave as a
        // real Range (responds to begin/end/exclude_end?).
        run_tests(&[
            r#"
            r = Marshal.load(Marshal.dump(1..10))
            [r.class.to_s, r.begin, r.end, r.exclude_end?]
            "#,
            r#"
            r = Marshal.load(Marshal.dump(1...10))
            [r.begin, r.end, r.exclude_end?]
            "#,
        ]);
    }

    #[test]
    fn data_define_basic() {
        // Data.define returns a class with attribute readers for each
        // given field name.
        run_test(
            r#"
            klass = Data.define(:a, :b)
            inst = klass.new(1, 2)
            [inst.a, inst.b]
            "#,
        );
        run_test(r#"Data.define.is_a?(Class)"#);
    }

    #[test]
    fn marshal_version_constants() {
        run_tests(&[
            r##"Marshal::MAJOR_VERSION"##,
            r##"Marshal::MINOR_VERSION"##,
            r##"[Marshal::MAJOR_VERSION, Marshal::MINOR_VERSION]"##,
        ]);
    }
}
