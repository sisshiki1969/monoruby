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
    // (cyclic structures are resolved through the object link table).
    globals.define_builtin_module_func_with(klass, "dump", dump, 1, 3, false);
    // CRuby: Marshal.load(source, proc=nil). The optional proc is
    // called once per loaded value during traversal; ignored here
    // because the spec set that depends on the arity passes nil
    // anyway, and surfacing a more useful error later is safer than
    // an arity refusal up front.
    // CRuby: Marshal.load(source, proc = nil, freeze: false). With
    // `freeze: true`, every reconstructed object (except classes and
    // modules) is frozen in place, deeply.
    globals.define_builtin_module_func_with_kw(klass, "load", load, 1, 2, false, &["freeze"], false);
    globals.define_builtin_module_func_with_kw(
        klass,
        "restore",
        load,
        1,
        2,
        false,
        &["freeze"],
        false,
    );
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
    // Object link table: every non-immediate object written takes a slot
    // in dump order; a repeated reference is emitted as a back-reference.
    let mut objects: Vec<u64> = Vec::new();
    marshal_dump_value(&mut buf, obj, vm, globals, &mut symbols, &mut objects)?;
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
    // `freeze:` keyword (positional slot after the optional proc).
    let freeze = lfp.try_arg(2).is_some_and(|v| v.as_bool());
    let mut cursor = MarshalReader::new(&data[2..]);
    cursor.freeze = freeze;
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
    /// `Marshal.load(.., freeze: true)`: deep-freeze reconstructed values.
    freeze: bool,
}

impl<'a> MarshalReader<'a> {
    fn new(data: &'a [u8]) -> Self {
        MarshalReader {
            data,
            pos: 0,
            symbols: Vec::new(),
            objects: Vec::new(),
            freeze: false,
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
        let mut value = self.read_value_inner(vm, globals)?;
        // `freeze: true` deep-freezes every reconstructed object except
        // classes and modules (which CRuby leaves mutable). Immediates
        // are already frozen, so skip packed values. Because this runs on
        // every recursive `read_value`, sub-objects are frozen too.
        if self.freeze && !value.is_packed_value() && value.is_class_or_module().is_none() {
            value.set_frozen();
        }
        Ok(value)
    }

    fn read_value_inner(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
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
            // 'd' (TYPE_DATA): a wrapped C-data object whose class
            // defines `_load_data`. Format: symbol(class) + inner
            // value. On load, allocate an instance and drive
            // `instance._load_data(value)`.
            b'd' => self.read_data(vm, globals),
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
            // 'S' (TYPE_STRUCT) — `symbol(class_name) +
            // member_count(varint) + (member_sym + value)*`.
            // The class must already exist (created via
            // `Struct.new("Name", ...)`); CRuby raises TypeError if
            // the declared members don't match the class's actual
            // member list.
            b'S' => self.read_struct(vm, globals),
            // 'c' (TYPE_CLASS) — `length(varint) + name-bytes`.
            // Resolves to the named class. CRuby raises ArgumentError
            // when the constant is missing and TypeError when it
            // resolves to a module instead of a class.
            b'c' => self.read_class_ref(globals),
            // 'm' (TYPE_MODULE) — same shape as 'c' but the constant
            // must resolve to a *module*. (Class ⇒ TypeError.)
            b'm' => self.read_module_ref(globals),
            // 'M' (TYPE_MODULE_OLD) — legacy tag that accepts either
            // a class or a module (Ruby 1.8 wrote this for both).
            b'M' => self.read_class_or_module_ref(globals),
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
        // CRuby's float payload is a NUL-terminated decimal string; any
        // bytes after the first '\0' are a legacy mantissa extension
        // (raw, non-UTF-8) used for exact reconstruction. We parse the
        // textual prefix, which reproduces the value to full precision.
        let text = match bytes.iter().position(|&b| b == 0) {
            Some(nul) => &bytes[..nul],
            None => bytes,
        };
        let s = std::str::from_utf8(text)
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
                // String with instance variables (encoding + optional
                // user ivars).
                let len = self.read_fixnum()? as usize;
                let bytes = self.read_bytes(len)?.to_vec();
                let (encoding, user_ivars) = self.read_encoding_ivars(vm, globals)?;
                let val = Value::string_from_inner(RStringInner::from_encoding(&bytes, encoding));
                self.objects.push(val);
                for (sym, ivar_val) in user_ivars {
                    globals.set_ivar(val, sym, ivar_val)?;
                }
                Ok(val)
            }
            b'u' => {
                // Encoding-wrapped userdef ('I' + 'u'): the payload string
                // handed to `_load` carries the encoding recorded by the
                // trailing ivar block.
                let class_sym = self.read_symbol()?;
                let len = self.read_fixnum()? as usize;
                let bytes = self.read_bytes(len)?.to_vec();
                let (encoding, _user_ivars) = self.read_encoding_ivars(vm, globals)?;
                self.finish_user_marshal(vm, globals, class_sym, &bytes, encoding)
            }
            _ => {
                // Other I-wrapped types: an Array / Hash / object with
                // user instance variables, or a Regexp carrying its
                // encoding ivar. Read the payload with the non-freezing
                // inner reader so the ivars can be applied before the
                // outer `read_value` freezes the whole thing (under
                // `freeze: true`).
                self.pos -= 1;
                let val = self.read_value_inner(vm, globals)?;
                let ivar_count = self.read_fixnum()? as usize;
                for _ in 0..ivar_count {
                    let sym = self.read_symbol()?;
                    let ivar_val = self.read_value(vm, globals)?;
                    // `:E` / `:encoding` describe the payload's encoding
                    // (already applied when the value was built); every
                    // other symbol is a real user instance variable.
                    let name = sym.get_name();
                    if name != "E" && name != "encoding" {
                        globals.set_ivar(val, sym, ivar_val)?;
                    }
                }
                Ok(val)
            }
        }
    }

    /// Read an Array.
    /// Format: marshal_int(length) + elements
    fn read_array(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Create the array up-front and register it so a self-reference
        // encountered while reading the elements links back to this very
        // object (CRuby resolves cyclic arrays this way).
        let arr = Value::array_from_vec(Vec::with_capacity(len));
        self.objects.push(arr);
        let mut array = arr.as_array();
        for _ in 0..len {
            let val = self.read_value(vm, globals)?;
            array.push(val);
        }
        Ok(arr)
    }

    /// Read a Hash.
    /// Format: marshal_int(length) + (key + value) pairs
    fn read_hash(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let len = self.read_fixnum()? as usize;
        // Create the hash up-front and register it so a self-reference in
        // a key or value links back to this object (cyclic hashes).
        let hash = Value::hash(RubyMap::default());
        self.objects.push(hash);
        let mut map = hash.as_hash();
        for _ in 0..len {
            let key = self.read_value(vm, globals)?;
            let val = self.read_value(vm, globals)?;
            map.insert(key, val, vm, globals)?;
        }
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

        // Look up the class, walking a `::`-separated path so nested
        // classes (e.g. `MarshalSpec::Foo`) resolve correctly.
        let module = resolve_class_path(globals, &class_name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
        })?;
        // Classes with a custom allocator have internal storage (e.g.
        // Range, Time, Exception). Creating a generic Value::object for
        // them would violate later invariants and cause native panics when
        // methods run. Flag them so we can reconstruct or raise.
        let native_class = !globals.store[module.id()]
            .alloc_func()
            .map(|f| f as *const () == crate::default_alloc_func as *const ())
            .unwrap_or(true);
        let obj = Value::object(module.id());
        // Read instance variables.
        let mut ivars: Vec<(IdentId, Value)> = Vec::with_capacity(ivar_count);
        for _ in 0..ivar_count {
            let ivar_sym = self.read_symbol()?;
            let val = self.read_value(vm, globals)?;
            ivars.push((ivar_sym, val));
        }
        // Built-in classes with internal storage (e.g. Range, Exception)
        // can't be represented as generic 'o' objects; reconstruct the
        // real value from its ivars, or raise for the ones we can't.
        let built_obj = if class_name == "Range" {
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
        } else if is_exception_class(globals, module) {
            // Rebuild an Exception from its `:mesg` / `:bt` ivars, then
            // restore any user ivars and the backtrace.
            let mut message: Option<String> = None;
            let mut backtrace: Option<Value> = None;
            let mut user_ivars: Vec<(IdentId, Value)> = Vec::new();
            for (name, val) in &ivars {
                match name.get_name().as_str() {
                    "mesg" => {
                        if let Some(s) = val.is_rstring_inner() {
                            message = Some(String::from_utf8_lossy(s.as_bytes()).into_owned());
                        }
                    }
                    "bt" => {
                        if !val.is_nil() {
                            backtrace = Some(*val);
                        }
                    }
                    _ => user_ivars.push((*name, *val)),
                }
            }
            // A nil `:mesg` means "no explicit message"; CRuby's default
            // message is then the class name.
            let msg = message.unwrap_or_else(|| class_name.clone());
            let exc = Value::new_exception_from_with_class(msg, module.id(), module.id());
            if let Some(bt) = backtrace {
                let set_bt = IdentId::get_id("set_backtrace");
                vm.invoke_method_inner(globals, set_bt, exc, &[bt], None, None)?;
            }
            for (name, val) in user_ivars {
                globals.set_ivar(exc, name, val)?;
            }
            exc
        } else if native_class {
            return Err(MonorubyErr::argumenterr(format!(
                "can't load instance of {} from generic marshal object",
                class_name
            )));
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
        self.finish_user_marshal(vm, globals, class_sym, &bytes, Encoding::Ascii8)
    }

    /// Resolve the `'u'` payload's class, hand the payload string to its
    /// `_load` class method in the given encoding, and register the
    /// result for `'@'` back-references. Shared by the bare `'u'` reader
    /// and the `'I' 'u'` (encoding-wrapped) path.
    fn finish_user_marshal(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
        class_sym: IdentId,
        bytes: &[u8],
        encoding: Encoding,
    ) -> Result<Value> {
        let class_name = class_sym.get_name();
        let module = resolve_class_path(globals, &class_name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
        })?;
        // Hand the payload string to `_load` in its recorded encoding.
        let payload = Value::string_from_inner(RStringInner::from_encoding(bytes, encoding));
        let load_id = IdentId::get_id("_load");
        let result =
            vm.invoke_method_inner(globals, load_id, module.as_val(), &[payload], None, None)?;
        // Register the reconstructed object so later `'@'` links resolve.
        self.objects.push(result);
        Ok(result)
    }

    /// Read an ivar block (`:E T`/`:E F`/`:encoding "name"` plus any user
    /// ivars) following a String/Regexp/userdef payload inside an `'I'`
    /// wrapper. Returns the encoding it denotes and the user ivars (the
    /// non-encoding pairs) so the caller can apply them to the value.
    fn read_encoding_ivars(
        &mut self,
        vm: &mut Executor,
        globals: &mut Globals,
    ) -> Result<(Encoding, Vec<(IdentId, Value)>)> {
        let ivar_count = self.read_fixnum()? as usize;
        let mut encoding = Encoding::Ascii8;
        let mut user_ivars = Vec::new();
        for _ in 0..ivar_count {
            let sym = self.read_symbol()?;
            let sym_name = sym.get_name();
            if sym_name == "E" {
                match self.read_byte()? {
                    b'T' => encoding = Encoding::Utf8,
                    b'F' => encoding = Encoding::UsAscii,
                    _ => {
                        self.pos -= 1;
                        let enc_val = self.read_value(vm, globals)?;
                        if let Some(s) = enc_val.is_rstring_inner() {
                            let enc_name = std::str::from_utf8(s.as_bytes()).unwrap_or("");
                            encoding =
                                Encoding::try_from_str(enc_name).unwrap_or(Encoding::Ascii8);
                        }
                    }
                }
            } else if sym_name == "encoding" {
                let enc_val = self.read_value(vm, globals)?;
                if let Some(s) = enc_val.is_rstring_inner() {
                    let enc_name = std::str::from_utf8(s.as_bytes()).unwrap_or("");
                    encoding = Encoding::try_from_str(enc_name).unwrap_or(Encoding::Ascii8);
                }
            } else {
                let val = self.read_value(vm, globals)?;
                user_ivars.push((sym, val));
            }
        }
        Ok((encoding, user_ivars))
    }

    /// Read a user-marshal object ('U' tag, TYPE_USRMARSHAL).
    /// Format: `symbol(class_name) + inner-value`.
    /// On load CRuby allocates a fresh instance of the class and
    /// then drives `instance.marshal_load(value)` — `instance` is
    /// returned (the `marshal_load` return value is ignored).
    fn read_usr_marshal(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let class_name = class_sym.get_name();
        let module = resolve_class_path(globals, &class_name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
        })?;
        // Rational / Complex dump via #marshal_dump but are immutable
        // numerics with no Ruby-level allocator or #marshal_load; rebuild
        // them through their Kernel constructor (`Rational(n, d)` /
        // `Complex(r, i)`) from the two-element payload array.
        if class_name == "Rational" || class_name == "Complex" {
            let idx = self.objects.len();
            self.objects.push(Value::nil()); // reserve the link slot
            let value = self.read_value(vm, globals)?;
            let parts = value.try_array_ty().ok_or_else(|| {
                MonorubyErr::argumenterr(format!("dump format error for {}", class_name))
            })?;
            let ctor = IdentId::get_id(&class_name);
            let args: Vec<Value> = parts.iter().copied().collect();
            let recv = globals.main_object;
            let result = vm.invoke_method_inner(globals, ctor, recv, &args, None, None)?;
            self.objects[idx] = result;
            return Ok(result);
        }
        // Allocate without running initialize (CRuby `allocate`
        // shape). `call_alloc_func` raises TypeError if the class
        // never declared an allocator — surface that verbatim.
        let instance = crate::builtins::class::call_alloc_func(globals, module.id())?;
        // Register the object *before* reading its payload so a
        // self-reference inside the payload links back to it (this
        // mirrors the dump side, which reserves the slot first).
        self.objects.push(instance);
        let value = self.read_value(vm, globals)?;
        // Drive `instance.marshal_load(value)`. The return value is
        // discarded; the spec checks the receiver's resulting state.
        let marshal_load_id = IdentId::get_id("marshal_load");
        vm.invoke_method_inner(globals, marshal_load_id, instance, &[value], None, None)?;
        Ok(instance)
    }

    /// Read a wrapped C-data object ('d' tag, TYPE_DATA).
    /// Format: `symbol(class_name) + inner-value`.
    /// On load CRuby allocates a fresh instance of the class and
    /// drives `instance._load_data(value)`. The class must define
    /// `_load_data`; otherwise CRuby raises `TypeError`.
    fn read_data(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let value = self.read_value(vm, globals)?;
        let class_name = class_sym.get_name();
        let module = resolve_class_path(globals, &class_name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
        })?;
        // `_load_data` must be defined (CRuby raises TypeError with
        // this exact message when it is missing).
        let load_data_id = IdentId::get_id("_load_data");
        if globals
            .check_method_for_class(module.id(), load_data_id)
            .is_none()
        {
            return Err(MonorubyErr::typeerr(format!(
                "class {} needs to have instance method `_load_data'",
                class_name
            )));
        }
        let instance = crate::builtins::class::call_alloc_func(globals, module.id())?;
        vm.invoke_method_inner(globals, load_data_id, instance, &[value], None, None)?;
        Ok(instance)
    }

    /// Read a user-class wrapper ('C' tag).
    /// Format: symbol(class_name) + inner value.
    /// The inner is a built-in value (String, Array, Hash, …) that was
    /// produced by an instance of a subclass; on load we reconstruct
    /// the inner natively and then swap its class to the named subclass.
    fn read_user_class(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        // Read the base value without freezing it (under `freeze: true`)
        // so its class can still be swapped; the outer `read_value`
        // freezes the final wrapped result.
        let mut inner = self.read_value_inner(vm, globals)?;
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
        // Read without freezing so `extend` can still modify the singleton
        // class; the outer `read_value` freezes the final result.
        let inner = self.read_value_inner(vm, globals)?;
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

    /// Read a Struct subclass instance ('S' tag, TYPE_STRUCT).
    ///
    /// Format: `symbol(class_name) + member_count(varint) +
    /// (member_sym + value)*`.
    ///
    /// The class name is the fully-qualified path (e.g.
    /// `Struct::Useful`); we resolve it by walking `::` segments
    /// from `Object`. CRuby raises `TypeError` if the declared
    /// member symbols don't match the class's actual `/members`,
    /// so we mirror that.
    fn read_struct(&mut self, vm: &mut Executor, globals: &mut Globals) -> Result<Value> {
        let class_sym = self.read_symbol()?;
        let member_count = self.read_fixnum()? as usize;
        // Reserve the object slot up front so any backrefs from member
        // values resolve to the (still-empty) instance.
        let obj_idx = self.objects.len();
        self.objects.push(Value::nil());

        let class_name = class_sym.get_name();
        let module = resolve_class_path(globals, &class_name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", class_name))
        })?;

        // The declared member list lives on the class (or an
        // ancestor — direct `class A < SomeStruct` subclasses
        // inherit `/members` from their `Struct.new`-produced
        // superclass). Walking the chain matches the existing
        // `Struct#get_members` lookup. Absence ⇒ not a Struct.
        let members = lookup_struct_members(globals, module).ok_or_else(|| {
            MonorubyErr::typeerr(format!("{} is not a Struct", class_name))
        })?;

        // CRuby's struct loader enforces that the marshalled member
        // names exactly match the class's declared members (same
        // order, same symbols). Mismatch → TypeError.
        if members.len() != member_count {
            return Err(MonorubyErr::typeerr(format!(
                "struct {} not compatible (struct size differs)",
                class_name
            )));
        }
        let class_id = module.id();
        let mut instance = Value::struct_object(class_id, member_count);
        for i in 0..member_count {
            let member_sym = self.read_symbol()?;
            let val = self.read_value(vm, globals)?;
            // SAFETY: `members` was populated by `Struct.new`, which
            // validates each entry is a Symbol; CRuby blocks Ruby-
            // level mutation of `/`-prefixed ivars (`NameError`), so
            // the slot is guaranteed to be `Value::symbol(...)`.
            let expected_sym = members.get(i).unwrap().try_symbol().unwrap();
            if expected_sym != member_sym {
                return Err(MonorubyErr::typeerr(format!(
                    "struct {} not compatible (:{} for :{})",
                    class_name,
                    member_sym.get_name(),
                    expected_sym.get_name(),
                )));
            }
            instance.set_struct_slot(i, val);
        }
        self.objects[obj_idx] = instance;
        Ok(instance)
    }

    /// Read a 'c' (TYPE_CLASS) or 'm' (TYPE_MODULE) payload: a
    /// length-prefixed UTF-8 name string.
    fn read_class_or_module_name(&mut self) -> Result<String> {
        let len = self.read_fixnum()? as usize;
        let bytes = self.read_bytes(len)?;
        std::str::from_utf8(bytes)
            .map(|s| s.to_string())
            .map_err(|_| MonorubyErr::argumenterr("invalid class/module name in marshal data"))
    }

    /// Read a 'c' (TYPE_CLASS) reference. The payload names a class
    /// by its qualified path; the constant must exist and must be a
    /// class (not a module). Mirrors CRuby's `r_object0` TYPE_CLASS
    /// branch (`ArgumentError` on missing constant, `TypeError` if
    /// the resolved constant is a Module).
    fn read_class_ref(&mut self, globals: &mut Globals) -> Result<Value> {
        let name = self.read_class_or_module_name()?;
        let module = resolve_class_path(globals, &name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", name))
        })?;
        if module.as_val().ty() == Some(ObjTy::MODULE) {
            return Err(MonorubyErr::typeerr(format!("{} does not refer to class", name)));
        }
        let val = module.as_val();
        self.objects.push(val);
        Ok(val)
    }

    /// Read a 'm' (TYPE_MODULE) reference — mirror of `read_class_ref`
    /// but rejects Class with `TypeError`.
    fn read_module_ref(&mut self, globals: &mut Globals) -> Result<Value> {
        let name = self.read_class_or_module_name()?;
        let module = resolve_class_path(globals, &name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", name))
        })?;
        if module.as_val().ty() != Some(ObjTy::MODULE) {
            return Err(MonorubyErr::typeerr(format!("{} does not refer to module", name)));
        }
        let val = module.as_val();
        self.objects.push(val);
        Ok(val)
    }

    /// Read a 'M' (TYPE_MODULE_OLD) reference — legacy CRuby 1.8 tag
    /// that accepts either a class or a module.
    fn read_class_or_module_ref(&mut self, globals: &mut Globals) -> Result<Value> {
        let name = self.read_class_or_module_name()?;
        let module = resolve_class_path(globals, &name).ok_or_else(|| {
            MonorubyErr::argumenterr(format!("undefined class/module {}", name))
        })?;
        let val = module.as_val();
        self.objects.push(val);
        Ok(val)
    }
}

/// Walk a `::`-separated constant path starting at `Object`.
/// Returns `None` if any segment is missing or not a class/module.
fn resolve_class_path(globals: &Globals, path: &str) -> Option<Module> {
    let mut current_class_id = OBJECT_CLASS;
    let mut last: Option<Module> = None;
    for segment in path.split("::") {
        let name = IdentId::get_id(segment);
        let val = globals
            .get_constant(current_class_id, name)
            .and_then(|s| s.loaded_value())?;
        let module = val.is_class_or_module()?;
        current_class_id = module.id();
        last = Some(module);
    }
    last
}

/// Walk a Struct subclass's ancestor chain looking for the
/// `/members` ivar (an Array of Symbols set by `Struct.new`).
/// Returns `None` if the class isn't a Struct — i.e. neither it
/// nor any ancestor below `STRUCT_CLASS` carries `/members`.
///
/// Mirrors `super::struct_class::get_members`, which a follow-up
/// can consolidate once the marshal module gets access to it.
/// Return true if `class` is `Exception` or one of its subclasses.
fn is_exception_class(_globals: &Globals, class: Module) -> bool {
    let mut cur = Some(class);
    while let Some(c) = cur {
        if c.id() == EXCEPTION_CLASS {
            return true;
        }
        cur = c.superclass();
    }
    false
}

fn lookup_struct_members(globals: &Globals, mut class: Module) -> Option<Array> {
    loop {
        if let Some(v) = globals
            .store
            .get_ivar(class.as_val(), IdentId::get_id("/members"))
            && let Some(arr) = v.try_array_ty()
        {
            return Some(arr);
        }
        match class.superclass() {
            Some(s) if s.id() != STRUCT_CLASS => class = s,
            _ => return None,
        }
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

/// Append the single encoding instance variable that CRuby attaches to a
/// String / Regexp payload inside an 'I' (ivar) wrapper:
///
/// - UTF-8              → `1 ivar` + `:E` + true
/// - US-ASCII/ASCII-8BIT → `1 ivar` + `:E` + false
/// - other              → `1 ivar` + `:encoding` + `"<name>"` (raw string)
///
/// The String path never calls this for ASCII-8BIT (a binary string is
/// written without an ivar wrapper); the Regexp path does, because a
/// Regexp always carries its encoding.
fn marshal_write_string_encoding_ivar(
    buf: &mut Vec<u8>,
    enc: Encoding,
    symbols: &mut Vec<IdentId>,
) {
    marshal_write_fixnum(buf, 1); // 1 ivar
    marshal_write_encoding_ivar_pair(buf, enc, symbols);
}

/// Write just the encoding instance-variable *pair* (symbol + value),
/// without the leading ivar count — used when other user ivars share the
/// same `I` block.
fn marshal_write_encoding_ivar_pair(buf: &mut Vec<u8>, enc: Encoding, symbols: &mut Vec<IdentId>) {
    match enc {
        Encoding::Utf8 | Encoding::UsAscii | Encoding::Ascii8 => {
            marshal_write_symbol(buf, IdentId::get_id("E"), symbols);
            buf.push(if enc == Encoding::Utf8 { b'T' } else { b'F' });
        }
        _ => {
            marshal_write_symbol(buf, IdentId::get_id("encoding"), symbols);
            let name = enc.name().as_bytes();
            buf.push(b'"');
            marshal_write_fixnum(buf, name.len() as i32);
            buf.extend_from_slice(name);
        }
    }
}

/// Write a trailing ivar block — the ivar count followed by each
/// (symbol, dumped value) pair — inside an already-opened `'I'` wrapper.
fn marshal_write_ivar_block(
    buf: &mut Vec<u8>,
    ivars: &[(IdentId, Value)],
    vm: &mut Executor,
    globals: &mut Globals,
    symbols: &mut Vec<IdentId>,
    objects: &mut Vec<u64>,
) -> Result<()> {
    marshal_write_fixnum(buf, ivars.len() as i32);
    for (name, val) in ivars {
        marshal_write_symbol(buf, *name, symbols);
        marshal_dump_value(buf, *val, vm, globals, symbols, objects)?;
    }
    Ok(())
}

/// Collect the modules `extend`ed onto `obj` — the iclasses sitting
/// between its singleton class and its real class, nearest-first (which
/// is the order CRuby emits the `'e'` tags in).
fn marshal_extended_modules(globals: &Globals, obj: Value) -> Vec<ClassId> {
    let mut mods = Vec::new();
    let klass = globals.store[obj.class()].get_module();
    if klass.is_singleton().is_some() {
        let mut cur = klass.superclass();
        while let Some(c) = cur {
            if c.is_iclass() {
                mods.push(c.id());
                cur = c.superclass();
            } else {
                break;
            }
        }
    }
    mods
}

/// Emit the `'e'` (extended-module) and `'C'` (user-subclass) wrappers
/// that precede a built-in object's body. `base_class` is the built-in
/// class the tag is expected to be (`Array`, `Hash`, `String`, `Regexp`);
/// a real class other than that triggers the `'C'` tag. The `'I'` (ivar)
/// wrapper, when present, must already have been written by the caller —
/// it is the outermost of the three.
fn marshal_write_extended_and_class(
    buf: &mut Vec<u8>,
    globals: &Globals,
    obj: Value,
    base_class: ClassId,
    symbols: &mut Vec<IdentId>,
) {
    for m in marshal_extended_modules(globals, obj) {
        buf.push(b'e');
        let name = globals.get_class_name(m);
        marshal_write_symbol(buf, IdentId::get_id(&name), symbols);
    }
    let real = obj.real_class(&globals.store).id();
    if real != base_class {
        buf.push(b'C');
        let name = globals.get_class_name(real);
        marshal_write_symbol(buf, IdentId::get_id(&name), symbols);
    }
}

/// Emit an object back-reference ('@' + link index) if `obj` has
/// already been written, otherwise register it in the link table and
/// return `false` so the caller writes the object body.
///
/// CRuby's Marshal keeps a per-dump table of every *non-immediate*
/// object it has serialized; a second reference to the same object
/// (by identity) is written as a `TYPE_LINK` (`@`) back-reference.
/// Only nil / true / false / Fixnum / Symbol are exempt — every other
/// value (String, Bignum, Float, Array, Hash, user object, …) takes a
/// link slot in dump order. `Value::id()` returns the raw tagged bits,
/// which is exactly CRuby's identity key: pointer bits for heap values,
/// value bits for flonums.
fn marshal_emit_link(buf: &mut Vec<u8>, obj: Value, objects: &mut Vec<u64>) -> bool {
    let obj_id = obj.id();
    if let Some(idx) = objects.iter().position(|&x| x == obj_id) {
        buf.push(b'@');
        marshal_write_fixnum(buf, idx as i32);
        true
    } else {
        objects.push(obj_id);
        false
    }
}

/// If `obj` defines the user serialization protocol `#marshal_dump`
/// (preferred) or `#_dump`, serialize it via the matching tag
/// (`'U'` / `'u'`), register its link slot, and return `true`. Returns
/// `false` when neither is defined so the caller falls back to the
/// built-in encoding. The caller must have already resolved any
/// back-reference for `obj`; `obj_id` is `obj.id()`.
fn marshal_try_user_protocol(
    buf: &mut Vec<u8>,
    obj: Value,
    obj_id: u64,
    vm: &mut Executor,
    globals: &mut Globals,
    symbols: &mut Vec<IdentId>,
    objects: &mut Vec<u64>,
) -> Result<bool> {
    let marshal_dump_id = IdentId::get_id("marshal_dump");
    let dump_id = IdentId::get_id("_dump");
    if globals.check_method(obj, marshal_dump_id).is_some() {
        // Use the *real* class (skipping any singleton/iclass) so an
        // extended object reports e.g. `UserDefined`, matching Ruby-level
        // `#class`, not `#<Class:#<…>>`.
        let class_name = obj.get_real_class_name(&globals.store);
        if class_name.starts_with("#<") {
            return Err(MonorubyErr::typeerr(format!(
                "can't dump anonymous class {}",
                class_name
            )));
        }
        let class_name_id = IdentId::get_id(&class_name);
        // 'U' (TYPE_USRMARSHAL): the object takes its link slot *before*
        // its #marshal_dump payload is serialized.
        objects.push(obj_id);
        let payload = vm.invoke_method_inner(globals, marshal_dump_id, obj, &[], None, None)?;
        buf.push(b'U');
        marshal_write_symbol(buf, class_name_id, symbols);
        marshal_dump_value(buf, payload, vm, globals, symbols, objects)?;
        return Ok(true);
    }
    if globals.check_method(obj, dump_id).is_some() {
        let class_name = obj.get_real_class_name(&globals.store);
        if class_name.starts_with("#<") {
            return Err(MonorubyErr::typeerr(format!(
                "can't dump anonymous class {}",
                class_name
            )));
        }
        let class_name_id = IdentId::get_id(&class_name);
        // #_dump receives the recursion-depth limit (unlimited = -1 here)
        // and must return a String.
        let payload =
            vm.invoke_method_inner(globals, dump_id, obj, &[Value::integer(-1)], None, None)?;
        let s = payload
            .is_rstring_inner()
            .ok_or_else(|| MonorubyErr::typeerr("_dump() must return string"))?;
        let bytes = s.as_bytes().to_vec();
        let enc = s.encoding();
        // 'u' (TYPE_USERDEF): the object's own link slot follows any
        // objects embedded in the returned string.
        objects.push(obj_id);
        if enc == Encoding::Ascii8 {
            buf.push(b'u');
            marshal_write_symbol(buf, class_name_id, symbols);
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(&bytes);
        } else {
            // Wrap with 'I' to carry the payload string's encoding.
            buf.push(b'I');
            buf.push(b'u');
            marshal_write_symbol(buf, class_name_id, symbols);
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(&bytes);
            marshal_write_string_encoding_ivar(buf, enc, symbols);
        }
        return Ok(true);
    }
    Ok(false)
}

fn marshal_dump_value(
    buf: &mut Vec<u8>,
    obj: Value,
    vm: &mut Executor,
    globals: &mut Globals,
    symbols: &mut Vec<IdentId>,
    objects: &mut Vec<u64>,
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
            if marshal_emit_link(buf, obj, objects) {
                return Ok(());
            }
            marshal_write_bignum(buf, n);
        }
        RV::Float(f) => {
            if marshal_emit_link(buf, obj, objects) {
                return Ok(());
            }
            marshal_write_float(buf, f);
        }
        RV::Symbol(id) => {
            marshal_write_symbol(buf, id, symbols);
        }
        RV::String(s) => {
            if marshal_emit_link(buf, obj, objects) {
                return Ok(());
            }
            let bytes = s.as_bytes().to_vec();
            let enc = s.encoding();
            let ivars = globals.get_ivars(obj);
            // A binary string needs no encoding ivar; any other encoding
            // does. User ivars share the same `I` block.
            let has_enc_ivar = enc != Encoding::Ascii8;
            let ivar_count = has_enc_ivar as usize + ivars.len();
            let has_i = ivar_count > 0;
            if has_i {
                buf.push(b'I');
            }
            marshal_write_extended_and_class(buf, globals, obj, STRING_CLASS, symbols);
            buf.push(b'"');
            marshal_write_fixnum(buf, bytes.len() as i32);
            buf.extend_from_slice(&bytes);
            if has_i {
                marshal_write_fixnum(buf, ivar_count as i32);
                if has_enc_ivar {
                    marshal_write_encoding_ivar_pair(buf, enc, symbols);
                }
                for (name, val) in ivars {
                    marshal_write_symbol(buf, name, symbols);
                    marshal_dump_value(buf, val, vm, globals, symbols, objects)?;
                }
            }
        }
        // Complex / Rational are their own RV variants but are dumped via
        // the #marshal_dump protocol ('U' tag), so route them through the
        // same user-protocol path as plain objects.
        RV::Complex(_) | RV::Rational(_) => {
            let obj_id = obj.id();
            if let Some(idx) = objects.iter().position(|&x| x == obj_id) {
                buf.push(b'@');
                marshal_write_fixnum(buf, idx as i32);
                return Ok(());
            }
            if !marshal_try_user_protocol(buf, obj, obj_id, vm, globals, symbols, objects)? {
                return Err(MonorubyErr::typeerr(format!(
                    "no _dump_data is defined for class {}",
                    globals.get_class_name(obj.class())
                )));
            }
        }
        RV::Object(_rv) => {
            let obj_id = obj.id();
            // Already dumped this object? → emit a back-reference.
            if let Some(idx) = objects.iter().position(|&x| x == obj_id) {
                buf.push(b'@');
                marshal_write_fixnum(buf, idx as i32);
                return Ok(());
            }
            // User-defined serialization protocols ('U'/'u') take
            // precedence over the built-in container/object encodings.
            if marshal_try_user_protocol(buf, obj, obj_id, vm, globals, symbols, objects)? {
                return Ok(());
            }
            // Register the object in the link table *before* writing its
            // body so that self-referential / cyclic structures resolve
            // to a back-reference instead of recursing forever.
            objects.push(obj_id);
            let r = (|| -> Result<()> {
                // Check for Array and Hash via ty()
                match obj.ty() {
                    Some(ObjTy::ARRAY) => {
                        // Snapshot the elements so re-entrant user code
                        // invoked while dumping can't invalidate the
                        // borrow into the live array.
                        let elems: Vec<Value> =
                            obj.as_array_inner().iter().copied().collect();
                        let ivars = globals.get_ivars(obj);
                        let has_ivars = !ivars.is_empty();
                        if has_ivars {
                            buf.push(b'I');
                        }
                        marshal_write_extended_and_class(
                            buf, globals, obj, ARRAY_CLASS, symbols,
                        );
                        buf.push(b'[');
                        marshal_write_fixnum(buf, elems.len() as i32);
                        for elem in elems {
                            marshal_dump_value(buf, elem, vm, globals, symbols, objects)?;
                        }
                        if has_ivars {
                            marshal_write_ivar_block(
                                buf, &ivars, vm, globals, symbols, objects,
                            )?;
                        }
                    }
                    Some(ObjTy::CLASS) | Some(ObjTy::MODULE) => {
                        // Class ⇒ 'c', Module ⇒ 'm', payload is the
                        // qualified name as a length-prefixed string.
                        // CRuby raises TypeError on anonymous classes;
                        // monoruby renders them as `#<Class:0x...>`,
                        // which is not a constant path.
                        let is_module = obj.ty() == Some(ObjTy::MODULE);
                        let class_id = obj.as_class_id();
                        let class_name = globals.get_class_name(class_id);
                        if class_name.starts_with("#<") {
                            return Err(MonorubyErr::typeerr(format!(
                                "can't dump anonymous {}",
                                if is_module { "module" } else { "class" }
                            )));
                        }
                        let tag = if is_module { b'm' } else { b'c' };
                        let name_bytes = class_name.as_bytes();
                        buf.push(tag);
                        marshal_write_fixnum(buf, name_bytes.len() as i32);
                        buf.extend_from_slice(name_bytes);
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
                        marshal_dump_value(buf, begin, vm, globals, symbols, objects)?;
                        marshal_write_symbol(buf, IdentId::get_id("end"), symbols);
                        marshal_dump_value(buf, end, vm, globals, symbols, objects)?;
                        marshal_write_symbol(buf, IdentId::get_id("excl"), symbols);
                        buf.push(if excl { b'T' } else { b'F' });
                    }
                    Some(ObjTy::HASH) => {
                        // Snapshot the pairs to avoid holding a borrow into
                        // the live hash across re-entrant user code.
                        let pairs: Vec<(Value, Value)> =
                            obj.as_hashmap_inner().iter().collect();
                        let ivars = globals.get_ivars(obj);
                        let has_ivars = !ivars.is_empty();
                        if has_ivars {
                            buf.push(b'I');
                        }
                        marshal_write_extended_and_class(
                            buf, globals, obj, HASH_CLASS, symbols,
                        );
                        buf.push(b'{');
                        marshal_write_fixnum(buf, pairs.len() as i32);
                        for (k, v) in pairs {
                            marshal_dump_value(buf, k, vm, globals, symbols, objects)?;
                            marshal_dump_value(buf, v, vm, globals, symbols, objects)?;
                        }
                        if has_ivars {
                            marshal_write_ivar_block(
                                buf, &ivars, vm, globals, symbols, objects,
                            )?;
                        }
                    }
                    Some(ObjTy::OBJECT) => {
                        // User-defined object with instance variables: 'o'
                        // tag. The class is carried by the tag's symbol
                        // (real class, skipping the singleton), and any
                        // `extend`ed modules become 'e' wrappers.
                        let class_name = obj.get_real_class_name(&globals.store);
                        let class_name_id = IdentId::get_id(&class_name);
                        let ivars = globals.get_ivars(obj);
                        for m in marshal_extended_modules(globals, obj) {
                            buf.push(b'e');
                            let name = globals.get_class_name(m);
                            marshal_write_symbol(buf, IdentId::get_id(&name), symbols);
                        }
                        buf.push(b'o');
                        marshal_write_symbol(buf, class_name_id, symbols);
                        marshal_write_fixnum(buf, ivars.len() as i32);
                        for (name, val) in ivars {
                            marshal_write_symbol(buf, name, symbols);
                            marshal_dump_value(buf, val, vm, globals, symbols, objects)?;
                        }
                    }
                    Some(ObjTy::STRUCT) => {
                        // Struct subclass instance: 'S' tag.
                        // Format: symbol(class_name) + member_count +
                        //         (member_sym + value)*
                        let class_id = obj.class();
                        let class_name = globals.get_class_name(class_id);
                        // CRuby raises TypeError on anonymous classes;
                        // `get_class_name` renders anonymous classes
                        // as `#<Class:0x...>`, which is not a valid
                        // constant path.
                        if class_name.starts_with("#<") {
                            return Err(MonorubyErr::typeerr(
                                "can't dump anonymous class Struct",
                            ));
                        }
                        let class_name_id = IdentId::get_id(&class_name);
                        let class_module = globals.store[class_id].get_module();
                        // SAFETY: STRUCT-typed values are only produced
                        // via `Struct.new` (which stores `/members` on
                        // the class or an ancestor); CRuby blocks
                        // Ruby-level mutation of `/`-prefixed ivars
                        // (`NameError`), so some ancestor in the chain
                        // is guaranteed to carry the members array of
                        // Symbols.
                        let members =
                            lookup_struct_members(globals, class_module).unwrap();
                        let inner = obj.as_struct();
                        buf.push(b'S');
                        marshal_write_symbol(buf, class_name_id, symbols);
                        marshal_write_fixnum(buf, members.len() as i32);
                        for (i, m) in members.iter().enumerate() {
                            let sym = m.try_symbol().unwrap();
                            marshal_write_symbol(buf, sym, symbols);
                            let val = inner.try_get(i).unwrap_or(Value::nil());
                            marshal_dump_value(buf, val, vm, globals, symbols, objects)?;
                        }
                    }
                    Some(ObjTy::REGEXP) => {
                        // Regexp: 'I' + optional 'e'/'C' wrappers + '/' +
                        // int(len) + source + option byte, then an ivar
                        // block carrying the encoding ivar plus any user
                        // ivars. A Regexp always needs the 'I' wrapper.
                        let re = obj.as_regexp_inner();
                        let src = re.source_bytes().to_vec();
                        // CRuby's dump byte uses the Ruby-level option
                        // bits: i=1, x=2, m=4, FIXEDENCODING=0x10,
                        // NOENCODING=0x20 — which `option()` already
                        // reports (onigmo's i/x/m happen to share those
                        // low bits). Mask off any internal-only bits.
                        let opt = (re.option() & 0x37) as u8;
                        let enc = re.declared_encoding();
                        let ivars = globals.get_ivars(obj);
                        buf.push(b'I');
                        marshal_write_extended_and_class(
                            buf, globals, obj, REGEXP_CLASS, symbols,
                        );
                        buf.push(b'/');
                        marshal_write_fixnum(buf, src.len() as i32);
                        buf.extend_from_slice(&src);
                        buf.push(opt);
                        // ivar block: encoding ivar + any user ivars.
                        marshal_write_fixnum(buf, (1 + ivars.len()) as i32);
                        marshal_write_encoding_ivar_pair(buf, enc, symbols);
                        for (name, val) in ivars {
                            marshal_write_symbol(buf, name, symbols);
                            marshal_dump_value(buf, val, vm, globals, symbols, objects)?;
                        }
                    }
                    Some(ObjTy::EXCEPTION) => {
                        // Exception: generic 'o' object whose first two
                        // ivars are the special `:mesg` (message, or nil
                        // when it was never given) and `:bt` (backtrace,
                        // nil unless set), followed by any user ivars.
                        let class_name = obj.get_real_class_name(&globals.store);
                        if class_name.starts_with("#<") {
                            return Err(MonorubyErr::typeerr(format!(
                                "can't dump anonymous class {}",
                                class_name
                            )));
                        }
                        let class_name_id = IdentId::get_id(&class_name);
                        let msg = obj
                            .is_exception()
                            .map(|e| e.message().to_string())
                            .unwrap_or_default();
                        let user_ivars = globals.get_ivars(obj);
                        // #backtrace returns nil for a never-raised
                        // exception, else an array of location strings.
                        let bt = vm.invoke_method_inner(
                            globals,
                            IdentId::get_id("backtrace"),
                            obj,
                            &[],
                            None,
                            None,
                        )?;
                        buf.push(b'o');
                        marshal_write_symbol(buf, class_name_id, symbols);
                        marshal_write_fixnum(buf, (2 + user_ivars.len()) as i32);
                        // :mesg — CRuby stores nil until a message is
                        // explicitly given; monoruby always materializes
                        // the default (the class name), so treat a message
                        // equal to the class name as "unset".
                        marshal_write_symbol(buf, IdentId::get_id("mesg"), symbols);
                        if msg == class_name {
                            buf.push(b'0'); // nil
                        } else {
                            let msg_val = Value::string_from_str(&msg);
                            marshal_dump_value(buf, msg_val, vm, globals, symbols, objects)?;
                        }
                        // :bt
                        marshal_write_symbol(buf, IdentId::get_id("bt"), symbols);
                        marshal_dump_value(buf, bt, vm, globals, symbols, objects)?;
                        for (name, val) in user_ivars {
                            marshal_write_symbol(buf, name, symbols);
                            marshal_dump_value(buf, val, vm, globals, symbols, objects)?;
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

    /// 'S' (TYPE_STRUCT) round-trip and the CRuby-shaped errors:
    /// member-count mismatch and member-name mismatch both raise
    /// `TypeError`. Also exercises the qualified-path lookup
    /// (`Struct::Useful`) via `Struct.new("Name", ...)`.
    #[test]
    fn marshal_struct() {
        // Round-trip a named struct via `Struct.new("Name", ...)`.
        // The qualified path `Struct::Useful` round-trips through
        // the symbol payload and is resolved by the path walker.
        run_test_with_prelude(
            r##"
            s = Struct::MStructA.new(1, "two")
            t = Marshal.load(Marshal.dump(s))
            [t.class.name, t.a, t.b]
            "##,
            r##"
            Struct.new("MStructA", :a, :b)
            "##,
        );
        // Round-trip a top-level struct constant. monoruby's
        // `get_class_name` returns the bare name, which the path
        // walker resolves directly off Object.
        run_test_with_prelude(
            r##"
            s = MStructB.new(:sym, [1, 2, 3])
            t = Marshal.load(Marshal.dump(s))
            [t.class.name, t.x, t.y]
            "##,
            r##"
            MStructB = Struct.new(:x, :y)
            "##,
        );
        // Wrong member count ⇒ TypeError.
        run_test_error(
            r##"
            MStructC = Struct.new(:a, :b)
            bytes = Marshal.dump(MStructC.new(1, 2))
            Object.send(:remove_const, :MStructC)
            MStructC = Struct.new(:a, :b, :c)
            Marshal.load(bytes)
            "##,
        );
        // Member name mismatch ⇒ TypeError.
        run_test_error(
            r##"
            MStructD = Struct.new(:a, :b)
            bytes = Marshal.dump(MStructD.new(1, 2))
            Object.send(:remove_const, :MStructD)
            MStructD = Struct.new(:a, :z)
            Marshal.load(bytes)
            "##,
        );
        // Class name in marshal data refers to a constant that
        // exists but is not a Struct ⇒ TypeError.
        run_test_error(
            r##"
            MStructE = Struct.new(:a)
            bytes = Marshal.dump(MStructE.new(1))
            Object.send(:remove_const, :MStructE)
            MStructE = 42
            Marshal.load(bytes)
            "##,
        );
        // Anonymous Struct dump ⇒ TypeError (no constant path to
        // serialize). Covers the `class_name.starts_with("#<")` arm
        // on the dump side.
        run_test_error(
            r##"
            anon = Struct.new(:a)
            Marshal.dump(anon.new(1))
            "##,
        );
        // Class name in marshal data does not resolve to any
        // constant ⇒ ArgumentError. Hand-crafted 'S' payload naming
        // an undefined `MStructMissing`.
        run_test_error(
            r##"
            bytes = "\x04\x08S:\x14MStructMissing\x06:\x06ai\x06"
            Marshal.load(bytes)
            "##,
        );
        // Class resolves to a regular (non-Struct) class without
        // `/members` ⇒ TypeError "is not a Struct". Covers the
        // `lookup_struct_members` miss arm. Hand-crafted bytes
        // (symbol length 17 ⇒ marshal_int 0x16).
        run_test_error(
            r##"
            class MStructNotAStruct; end
            bytes = "\x04\x08S:\x16MStructNotAStruct\x06:\x06ai\x06"
            Marshal.load(bytes)
            "##,
        );
        // Round-trip a *direct* subclass of a `Struct.new`-produced
        // class (`/members` lives on the anonymous superclass).
        // Regression guard: a non-walking lookup raised `TypeError:
        // ... is not a Struct` on load and panicked on dump.
        // The superclass is hoisted to a constant so the JIT
        // warm-up reruns don't trip `superclass mismatch`.
        run_test(
            r##"
            MStructBase ||= Struct.new(:p, :q)
            class MStructInherited < MStructBase; end unless defined?(MStructInherited)
            s = MStructInherited.new("a", "b")
            t = Marshal.load(Marshal.dump(s))
            [t.class.name, t.p, t.q]
            "##,
        );
    }

    /// 'd' (TYPE_DATA): allocate the named class and drive
    /// `instance._load_data(value)`. The class must define
    /// `_load_data` (else `TypeError`). CRuby only emits this tag
    /// for `T_DATA` objects, so the load path is exercised with
    /// hand-crafted bytes (monoruby-only assertions).
    #[test]
    fn marshal_load_data() {
        // 'd' + symbol("MDataOk") (len 7 ⇒ 0x0c) + Fixnum 1.
        let v = run_test_no_result_check(
            r##"
            class MDataOk
              def _load_data(v); @v = v; end
            end
            obj = Marshal.load("\x04\x08d:\x0cMDataO" + "k" + "i\x06")
            raise "bad class" unless obj.class.name == "MDataOk"
            raise "bad value" unless obj.instance_variable_get(:@v) == 1
            obj.instance_variable_get(:@v)
            "##,
        );
        // _load_data received Fixnum 1.
        assert_eq!(v.try_fixnum(), Some(1));
        // Missing `_load_data` ⇒ TypeError.
        run_test_error(
            r##"
            class MDataNo; end
            Marshal.load("\x04\x08d:\x0cMDataN" + "o" + "i\x06")
            "##,
        );
        // Class name not resolvable ⇒ ArgumentError "undefined
        // class/module" (the resolve_class_path miss arm).
        run_test_error(
            r##"
            Marshal.load("\x04\x08d:\x12MDataNeverDef" + "i\x06")
            "##,
        );
    }

    /// 'c' (TYPE_CLASS), 'm' (TYPE_MODULE) and 'M' (TYPE_MODULE_OLD)
    /// references: round-trip yields the same object (`.equal?`) and
    /// the tag/type mismatch arms both raise `TypeError`.
    #[test]
    fn marshal_class_and_module() {
        // Round-trip a class via 'c'.
        run_test(
            r##"
            bytes = Marshal.dump(String)
            t = Marshal.load(bytes)
            [t.equal?(String), bytes.bytes[2].chr]
            "##,
        );
        // Round-trip a module via 'm'.
        run_test(
            r##"
            bytes = Marshal.dump(Enumerable)
            t = Marshal.load(bytes)
            [t.equal?(Enumerable), bytes.bytes[2].chr]
            "##,
        );
        // 'M' (TYPE_MODULE_OLD) accepts both classes and modules.
        // Hand-crafted bytes since CRuby no longer emits this tag.
        run_test(
            r##"
            # 'M' + length(6 ⇒ marshal_int 0x0b) + "String"
            class_bytes  = "\x04\x08M\x0bString"
            # 'M' + length(10 ⇒ marshal_int 0x0f) + "Enumerable"
            module_bytes = "\x04\x08M\x0fEnumerable"
            [Marshal.load(class_bytes).equal?(String),
             Marshal.load(module_bytes).equal?(Enumerable)]
            "##,
        );
        // 'c' naming a Module ⇒ TypeError.
        run_test_error(
            r##"
            Marshal.load(Marshal.dump(Enumerable).sub("m", "c"))
            "##,
        );
        // 'm' naming a Class ⇒ TypeError.
        run_test_error(
            r##"
            Marshal.load(Marshal.dump(String).sub("c", "m"))
            "##,
        );
        // 'c' naming a constant that does not exist ⇒ ArgumentError.
        run_test_error(
            r##"
            # 'c' + length(13 ⇒ marshal_int 0x12) + "MMissingClass"
            Marshal.load("\x04\x08c\x12MMissingClass")
            "##,
        );
        // 'm' naming a missing constant ⇒ ArgumentError. Same shape
        // as the 'c' case, just under the module reader.
        run_test_error(
            r##"
            # 'm' + length(13 ⇒ marshal_int 0x12) + "MMissingModul"
            Marshal.load("\x04\x08m\x12MMissingModul")
            "##,
        );
        // 'M' (TYPE_MODULE_OLD) naming a missing constant ⇒ ArgumentError.
        run_test_error(
            r##"
            # 'M' + length(13 ⇒ marshal_int 0x12) + "MMissingThing"
            Marshal.load("\x04\x08M\x12MMissingThing")
            "##,
        );
        // Dump of an anonymous class ⇒ TypeError.
        run_test_error(
            r##"
            Marshal.dump(Class.new)
            "##,
        );
        // Dump of an anonymous module ⇒ TypeError.
        run_test_error(
            r##"
            Marshal.dump(Module.new)
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
    fn marshal_dump_regexp() {
        // Regexp now round-trips through the '/' dump tag.
        run_test(r#"Marshal.dump(/foo/im).bytes"#);
        run_test(
            r#"
            r = Marshal.load(Marshal.dump(/foo/im))
            [r.source, r.options]
            "#,
        );
        run_test(r#"Marshal.load(Marshal.dump(/foo/n)).source"#);
    }

    #[test]
    fn marshal_dump_cyclic_array() {
        // CRuby resolves self-references through the object link table,
        // so a recursive array round-trips instead of raising.
        run_test(
            r#"
            a = []
            a << a
            b = Marshal.load(Marshal.dump(a))
            b.equal?(b[0])
            "#,
        );
    }

    #[test]
    fn marshal_dump_cyclic_hash() {
        run_test(
            r#"
            h = {}
            h[:self] = h
            g = Marshal.load(Marshal.dump(h))
            g.equal?(g[:self])
            "#,
        );
    }

    #[test]
    fn marshal_dump_marshal_dump_protocol() {
        // #marshal_dump / #marshal_load round-trip through the 'U' tag.
        run_test(
            r#"
            class MDProto
              attr_reader :x
              def initialize(x); @x = x; end
              def marshal_dump; [@x, "extra"]; end
              def marshal_load(a); @x = a[0]; end
            end
            Marshal.load(Marshal.dump(MDProto.new(42))).x
            "#,
        );
        // #marshal_dump takes precedence and shares a link slot on reuse.
        run_test(
            r#"
            class MDProto2
              def initialize(x); @x = x; end
              def marshal_dump; @x; end
              def marshal_load(a); @x = a; end
            end
            m = MDProto2.new(1)
            Marshal.dump([m, m]).bytes
            "#,
        );
    }

    #[test]
    fn marshal_dump_dump_protocol() {
        // #_dump / ._load round-trip through the 'u' tag.
        run_test(
            r#"
            class UDProto
              attr_reader :s
              def initialize(s); @s = s; end
              def _dump(level); @s.to_s; end
              def self._load(str); UDProto.new(str.to_i); end
            end
            Marshal.load(Marshal.dump(UDProto.new(7))).s
            "#,
        );
        run_test(
            r#"
            class UDProto2
              def initialize(s); @s = s; end
              def _dump(level); @s; end
              def self._load(str); UDProto2.new(str); end
            end
            u = UDProto2.new("hi")
            Marshal.dump([u, u]).bytes
            "#,
        );
        // A #_dump that returns a non-String raises TypeError.
        run_test_error(
            r#"
            class BadDump
              def _dump(level); 123; end
            end
            Marshal.dump(BadDump.new)
            "#,
        );
    }

    #[test]
    fn marshal_load_freeze() {
        // `freeze: true` deep-freezes reconstructed objects but leaves
        // classes/modules mutable.
        run_test(r#"Marshal.load(Marshal.dump("foo"), freeze: true).frozen?"#);
        run_test(r#"Marshal.load(Marshal.dump([1, 2, 3]), freeze: true).frozen?"#);
        run_test(r#"Marshal.load(Marshal.dump({foo: 42}), freeze: true).frozen?"#);
        run_test(
            r#"
            o = Object.new
            a = Marshal.load(Marshal.dump([o]), freeze: true)
            [a.frozen?, a[0].frozen?]
            "#,
        );
        run_test(r#"Marshal.load(Marshal.dump(String), freeze: true).frozen?"#);
        run_test(r#"Marshal.load(Marshal.dump("bar")).frozen?"#);
    }

    #[test]
    fn marshal_load_float_mantissa_extension() {
        // CRuby appends a legacy '\0' + mantissa blob after the decimal
        // text; the load path must parse the textual prefix and ignore
        // the trailing raw bytes.
        run_test(r#"Marshal.load("\x04\bf\v1.3\x00\xcc\xcd")"#);
        run_test(r#"Marshal.load(Marshal.dump(1.3)) == 1.3"#);
    }

    #[test]
    fn marshal_time_roundtrip() {
        // Time is serialized via its private #_dump / ._load hooks; the
        // core wall-clock fields must survive a round-trip.
        run_test(
            r#"
            t = Time.utc(2020, 1, 2, 3, 4, 5)
            r = Marshal.load(Marshal.dump(t))
            [r.year, r.month, r.day, r.hour, r.min, r.sec, r.utc?]
            "#,
        );
    }

    #[test]
    fn marshal_nested_class_roundtrip() {
        // A '`o`'/'`u`'/'`U`'-tagged object whose class lives under a
        // namespace must resolve via the full `::` path on load.
        run_test(
            r#"
            module MShip
              class Widget
                attr_reader :n
                def initialize(n); @n = n; end
              end
            end
            Marshal.load(Marshal.dump(MShip::Widget.new(3))).n
            "#,
        );
        run_test(
            r#"
            module MShip2
              class Blob
                def _dump(l); "z"; end
                def self._load(s); Blob.new; end
              end
            end
            Marshal.load(Marshal.dump(MShip2::Blob.new)).class.to_s
            "#,
        );
    }

    #[test]
    fn marshal_builtin_ivars() {
        // Array / Hash / String carrying user instance variables dump
        // with an 'I' wrapper and round-trip.
        run_test(
            r#"
            a = [1, 2]
            a.instance_variable_set(:@foo, 5)
            Marshal.dump(a).bytes
            "#,
        );
        run_test(
            r#"
            a = [1, 2]
            a.instance_variable_set(:@foo, 5)
            r = Marshal.load(Marshal.dump(a))
            [r, r.instance_variable_get(:@foo)]
            "#,
        );
        run_test(
            r#"
            h = {a: 1}
            h.instance_variable_set(:@foo, 5)
            r = Marshal.load(Marshal.dump(h))
            [r, r.instance_variable_get(:@foo)]
            "#,
        );
        run_test(
            r#"
            s = "str"
            s.instance_variable_set(:@foo, 5)
            r = Marshal.load(Marshal.dump(s))
            [r, r.instance_variable_get(:@foo)]
            "#,
        );
    }

    #[test]
    fn marshal_builtin_subclass() {
        // A subclass of a built-in dumps with the 'C' tag and reloads as
        // the subclass.
        run_test(
            r#"
            class MyArr < Array; end
            Marshal.dump(MyArr.new([1, 2])).bytes
            "#,
        );
        run_test(
            r#"
            class MyArr2 < Array; end
            r = Marshal.load(Marshal.dump(MyArr2.new([1, 2])))
            [r.class.to_s, r.to_a]
            "#,
        );
        run_test(
            r#"
            class MyStr < String; end
            r = Marshal.load(Marshal.dump(MyStr.new("hi")))
            [r.class.to_s, r.to_s]
            "#,
        );
    }

    #[test]
    fn marshal_extended_object() {
        // An object extended with a module dumps with the 'e' tag and
        // the extension survives a round-trip.
        run_test(
            r#"
            module MExt; end
            a = [9]
            a.extend(MExt)
            Marshal.dump(a).bytes
            "#,
        );
        run_test(
            r#"
            module MExt2; end
            a = [9]
            a.extend(MExt2)
            r = Marshal.load(Marshal.dump(a))
            [r, r.singleton_class.include?(MExt2)]
            "#,
        );
    }

    #[test]
    fn marshal_freeze_subclass_with_ivars() {
        // freeze: true must freeze a built-in subclass instance that also
        // carries instance variables (the ivars are applied before the
        // freeze takes effect).
        run_test(
            r#"
            class US < String; end
            s = US.new("x")
            s.instance_variable_set(:@foo, "bar")
            r = Marshal.load(Marshal.dump(s), freeze: true)
            [r.class.to_s, r.frozen?, r.instance_variable_get(:@foo)]
            "#,
        );
    }

    #[test]
    fn marshal_exception() {
        // Exceptions serialize as generic 'o' objects with :mesg / :bt
        // ivars (plus any user ivars) and round-trip.
        run_test(r#"Marshal.dump(Exception.new).bytes"#);
        run_test(r#"Marshal.dump(Exception.new("foo")).bytes"#);
        run_test(
            r#"
            e = RuntimeError.new("boom")
            r = Marshal.load(Marshal.dump(e))
            [r.class.to_s, r.message]
            "#,
        );
        run_test(
            r#"
            e = Exception.new("x")
            e.instance_variable_set(:@ivar, 7)
            Marshal.dump(e).bytes
            "#,
        );
        run_test(
            r#"
            e = Exception.new
            r = Marshal.load(Marshal.dump([e, e]))
            r[0].equal?(r[1])
            "#,
        );
        // Anonymous exception subclass cannot be dumped.
        run_test_error(r#"Marshal.dump(Class.new(Exception).new)"#);
    }

    #[test]
    fn marshal_rational_complex() {
        // Rational / Complex serialize via the 'U' (marshal_dump) tag and
        // round-trip through their Kernel constructors.
        run_test(r#"Marshal.dump(Rational(2, 3)).bytes"#);
        run_test(r#"Marshal.dump(Complex(2, 5)).bytes"#);
        run_test(r#"Marshal.load(Marshal.dump(Rational(2, 3)))"#);
        run_test(r#"Marshal.load(Marshal.dump(Complex(2, 5)))"#);
        run_test(
            r#"
            r = Rational(2, 3)
            Marshal.load(Marshal.dump([r, r]))
            "#,
        );
    }

    #[test]
    fn marshal_dump_string_encoding() {
        // US-ASCII strings carry a `:E false` ivar; UTF-8 carries
        // `:E true`; binary strings carry no ivar.
        run_test(r#"Marshal.dump("abc".force_encoding("US-ASCII")).bytes"#);
        run_test(r#"Marshal.dump("abc".force_encoding("BINARY")).bytes"#);
        run_test(r#"Marshal.dump("abc").bytes"#);
        run_test(
            r#"
            s = Marshal.load(Marshal.dump("abc".force_encoding("US-ASCII")))
            s.encoding.to_s
            "#,
        );
    }

    #[test]
    fn marshal_dump_object_links() {
        // Repeated non-immediate objects share one link slot.
        run_test(
            r#"
            s = "string"
            Marshal.dump([s, s]).bytes
            "#,
        );
        run_test(
            r#"
            s = "abc"
            a = Marshal.load(Marshal.dump([s, s]))
            a[0].equal?(a[1])
            "#,
        );
        run_test(
            r#"
            a = [1]
            arr = Marshal.load(Marshal.dump([a, a, a]))
            [arr[0].equal?(arr[1]), arr[1].equal?(arr[2])]
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
