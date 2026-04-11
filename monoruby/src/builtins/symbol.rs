use super::*;
use std::sync::OnceLock;

//
// Symbol class
//

///
/// FuncId of the shared native body used by `Symbol#to_proc`.
///
/// Stored once at init so that `Proc#call` can detect symbol-to-proc procs
/// for block-forwarding fast path.
///
static SYMBOL_TO_PROC_BODY_FID: OnceLock<FuncId> = OnceLock::new();

pub(crate) fn symbol_to_proc_body_fid() -> Option<FuncId> {
    SYMBOL_TO_PROC_BODY_FID.get().copied()
}

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("Symbol", SYMBOL_CLASS, None);
    globals.define_builtin_class_func(SYMBOL_CLASS, "allocate", super::class::undef_allocate, 0);
    globals.define_builtin_class_func(SYMBOL_CLASS, "all_symbols", all_symbols, 0);
    globals.define_builtin_func(SYMBOL_CLASS, "<=>", cmp, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "===", eq, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "==", eq, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "!=", ne, 1);
    globals.define_builtin_func(SYMBOL_CLASS, "to_s", sym_to_s, 0);
    globals.define_builtin_func(SYMBOL_CLASS, "name", sym_name, 0);
    globals.define_builtin_func(SYMBOL_CLASS, "inspect", sym_inspect, 0);
    globals.define_builtin_func(SYMBOL_CLASS, "to_proc", sym_to_proc, 0);
    // Register the shared body function used by Symbol#to_proc. It is
    // installed on SYMBOL_CLASS with an internal name so that Proc dispatch
    // can invoke it through the normal block-invoker path, but ordinary Ruby
    // code has no reason to call it directly.
    let body_fid = globals.define_builtin_func_with(
        SYMBOL_CLASS,
        "__monoruby_symbol_to_proc_body__",
        symbol_to_proc_body,
        1,
        1,
        true,
    );
    let _ = SYMBOL_TO_PROC_BODY_FID.set(body_fid);
    // Symbol.new is undefined (raises NoMethodError).
    let meta = globals.store.get_metaclass(SYMBOL_CLASS).id();
    globals
        .undef_method_for_class(meta, IdentId::get_id("new"))
        .unwrap();
}

///
/// ### Symbol#to_s
///
/// - to_s -> String
///
/// Returns the name of the symbol as a string.
/// ASCII-only symbols return a US-ASCII encoded string.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/to_s.html]
#[monoruby_builtin]
fn sym_to_s(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let sym = self_val.as_symbol();
    let ident_name = sym.get_ident_name_clone();
    let (bytes, enc) = match &ident_name {
        IdentName::Utf8(s) => {
            let enc = if s.is_ascii() {
                Encoding::UsAscii
            } else {
                Encoding::Utf8
            };
            (s.as_bytes(), enc)
        }
        IdentName::Bytes(b) => (b.as_slice(), Encoding::Ascii8),
    };
    let inner = RStringInner::from_encoding(bytes, enc);
    let result = Value::string_from_inner(inner);
    emit_to_s_chilled_warning(vm, globals, self_val, &ident_name)?;
    Ok(result)
}

/// Emit the "string returned by :foo.to_s will be frozen in the future"
/// deprecation warning when `Warning[:deprecated]` is enabled. This is
/// approximate: CRuby fires the warning on mutation of the chilled string,
/// but monoruby does not track chilled strings, so we fire it eagerly.
fn emit_to_s_chilled_warning(
    vm: &mut Executor,
    globals: &mut Globals,
    self_val: Value,
    ident_name: &IdentName,
) -> Result<()> {
    // Check Warning[:deprecated]
    let warning_val = match globals
        .store
        .get_constant_noautoload(OBJECT_CLASS, IdentId::get_id("Warning"))
    {
        Some(v) => v,
        None => return Ok(()),
    };
    let dep_sym = Value::symbol(IdentId::get_id("deprecated"));
    let dep = vm.invoke_method_inner(
        globals,
        IdentId::get_id("[]"),
        warning_val,
        &[dep_sym],
        None,
        None,
    )?;
    if dep.is_nil() || dep == Value::bool(false) {
        return Ok(());
    }

    // Build the inspect form of self
    let inspect = if is_simple_symbol(ident_name) {
        let mut res = String::from(":");
        match ident_name {
            IdentName::Utf8(name) => res.push_str(name),
            IdentName::Bytes(bytes) => res.push_str(&String::from_utf8_lossy(bytes)),
        }
        res
    } else {
        let (bytes, enc) = match ident_name {
            IdentName::Utf8(s) => {
                let enc = if s.is_ascii() {
                    Encoding::UsAscii
                } else {
                    Encoding::Utf8
                };
                (s.as_bytes(), enc)
            }
            IdentName::Bytes(b) => (b.as_slice(), Encoding::Ascii8),
        };
        let inner = RStringInner::from_encoding(bytes, enc);
        let mut res = String::from(":\"");
        res.push_str(&inner.inspect());
        res.push('"');
        res
    };
    let _ = self_val;

    let msg = format!(
        "warning: string returned by {}.to_s will be frozen in the future\n",
        inspect
    );
    let stderr = globals
        .get_gvar(IdentId::get_id("$stderr"))
        .unwrap_or(Value::nil());
    if stderr.is_nil() {
        return Ok(());
    }
    let msg_val = Value::string(msg);
    vm.invoke_method_inner(
        globals,
        IdentId::get_id("write"),
        stderr,
        &[msg_val],
        None,
        None,
    )?;
    Ok(())
}

///
/// ### Symbol#name
///
/// - name -> String
///
/// Returns a frozen string corresponding to the symbol's name.
/// Returns the same object for the same symbol.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/name.html]
#[monoruby_builtin]
fn sym_name(_: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sym = lfp.self_val().as_symbol();
    if let Some(&v) = globals.symbol_names.get(&sym) {
        return Ok(v);
    }
    let ident_name = sym.get_ident_name_clone();
    let (bytes, enc) = match &ident_name {
        IdentName::Utf8(s) => {
            let enc = if s.is_ascii() {
                Encoding::UsAscii
            } else {
                Encoding::Utf8
            };
            (s.as_bytes(), enc)
        }
        IdentName::Bytes(b) => (b.as_slice(), Encoding::Ascii8),
    };
    let inner = RStringInner::from_encoding(bytes, enc);
    let mut v = Value::string_from_inner(inner);
    v.set_frozen();
    globals.symbol_names.insert(sym, v);
    Ok(v)
}

///
/// ### Symbol#inspect
///
/// - inspect -> String
///
/// Returns a string representation of the symbol as a symbol literal.
/// Names that are not valid simple identifiers / operators / global-ivar-cvar
/// forms are wrapped in `:"..."` with string-style escaping.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/inspect.html]
#[monoruby_builtin]
fn sym_inspect(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sym = lfp.self_val().as_symbol();
    let ident_name = sym.get_ident_name_clone();
    let s = if is_simple_symbol(&ident_name) {
        let mut res = String::from(":");
        match &ident_name {
            IdentName::Utf8(name) => res.push_str(name),
            IdentName::Bytes(bytes) => {
                res.push_str(&String::from_utf8_lossy(bytes));
            }
        }
        res
    } else {
        let (bytes, enc) = match &ident_name {
            IdentName::Utf8(s) => {
                let enc = if s.is_ascii() {
                    Encoding::UsAscii
                } else {
                    Encoding::Utf8
                };
                (s.as_bytes(), enc)
            }
            IdentName::Bytes(b) => (b.as_slice(), Encoding::Ascii8),
        };
        let inner = RStringInner::from_encoding(bytes, enc);
        let mut res = String::from(":\"");
        res.push_str(&inner.inspect());
        res.push('"');
        res
    };
    Ok(Value::string(s))
}

///
/// ### Symbol#to_proc
///
/// - to_proc -> Proc
///
/// Returns a Proc object whose parameters are `[[:req], [:rest]]` and whose
/// `source_location` is `nil`, matching CRuby's C-level implementation.
/// When invoked with `(recv, *args, &blk)`, it calls `recv.public_send(self,
/// *args, &blk)`.
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/to_proc.html]
#[monoruby_builtin]
fn sym_to_proc(
    _: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    pc: BytecodePtr,
) -> Result<Value> {
    let self_val = lfp.self_val();
    let body_fid = SYMBOL_TO_PROC_BODY_FID
        .get()
        .copied()
        .expect("symbol_to_proc body function not initialized");
    let outer_lfp = Lfp::heap_frame(self_val, globals[body_fid].meta());
    let proc = Proc::from_parts(outer_lfp, body_fid, pc);
    Ok(proc.into())
}

///
/// Shared body for `Symbol#to_proc`-created procs.
///
/// Invoked via the block-invoker path (for `&:sym` usage in e.g. `map(&:to_s)`).
/// `lfp.self_val()` carries the symbol (from the proc's outer_lfp), `arg(0)` is
/// the receiver, and `arg(1)` is the rest array.
///
#[monoruby_builtin]
fn symbol_to_proc_body(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let symbol_val = lfp.self_val();
    let symbol = symbol_val
        .try_symbol()
        .expect("symbol_to_proc_body invoked with non-symbol self");
    let recv = lfp.arg(0);
    let rest_val = lfp.arg(1);
    let rest_array = rest_val.as_array();
    let rest: Vec<Value> = rest_array.iter().cloned().collect();
    // public_send semantics: reject private and protected methods.
    let class_id = recv.class();
    if let Some(entry) = globals.check_method_for_class(class_id, symbol) {
        match entry.visibility() {
            Visibility::Private => {
                return Err(MonorubyErr::private_method_called(
                    globals, symbol, recv,
                ));
            }
            Visibility::Protected => {
                return Err(MonorubyErr::protected_method_called(
                    globals, symbol, recv,
                ));
            }
            _ => {}
        }
    }
    let bh = lfp.block();
    vm.invoke_method_inner(globals, symbol, recv, &rest, bh, None)
}

/// Test whether a symbol name can be rendered without quotes.
fn is_simple_symbol(name: &IdentName) -> bool {
    let s = match name.as_str() {
        Some(s) => s,
        None => return false,
    };
    if s.is_empty() {
        return false;
    }
    if is_operator_symbol(s) {
        return true;
    }
    if let Some(rest) = s.strip_prefix("@@") {
        return is_plain_identifier(rest);
    }
    if let Some(rest) = s.strip_prefix('@') {
        return is_plain_identifier(rest);
    }
    if let Some(rest) = s.strip_prefix('$') {
        return is_global_var_tail(rest);
    }
    is_method_identifier(s)
}

fn is_operator_symbol(s: &str) -> bool {
    matches!(
        s,
        "|" | "^"
            | "&"
            | "<=>"
            | "=="
            | "==="
            | "=~"
            | ">"
            | ">="
            | "<"
            | "<="
            | "<<"
            | ">>"
            | "+"
            | "-"
            | "*"
            | "/"
            | "%"
            | "**"
            | "~"
            | "+@"
            | "-@"
            | "[]"
            | "[]="
            | "`"
            | "!"
            | "!="
            | "!~"
    )
}

fn is_ident_start_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphabetic() || (c as u32) >= 0x80
}

fn is_ident_cont_char(c: char) -> bool {
    c == '_' || c.is_ascii_alphanumeric() || (c as u32) >= 0x80
}

fn is_plain_identifier(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if is_ident_start_char(c) => {}
        _ => return false,
    }
    chars.all(is_ident_cont_char)
}

fn is_method_identifier(s: &str) -> bool {
    let bytes = s.as_bytes();
    let body_end = match bytes.last() {
        Some(b'!') | Some(b'?') | Some(b'=') => bytes.len() - 1,
        _ => bytes.len(),
    };
    if body_end == 0 {
        return false;
    }
    is_plain_identifier(&s[..body_end])
}

fn is_global_var_tail(rest: &str) -> bool {
    if rest.is_empty() {
        return false;
    }
    // $1234: digits only
    if rest.bytes().all(|b| b.is_ascii_digit()) {
        return true;
    }
    // $-X: dash + exactly one identifier-ish char
    if let Some(after_dash) = rest.strip_prefix('-') {
        let mut chars = after_dash.chars();
        if let (Some(c), None) = (chars.next(), chars.next()) {
            return is_ident_cont_char(c);
        }
        return false;
    }
    // Single special char globals like $~, $*, $$, $?, $!, ...
    if rest.len() == 1 {
        let c = rest.as_bytes()[0];
        if matches!(
            c,
            b'~' | b'*'
                | b'$'
                | b'?'
                | b'!'
                | b'@'
                | b'/'
                | b'\\'
                | b';'
                | b','
                | b'.'
                | b'<'
                | b'>'
                | b':'
                | b'"'
                | b'&'
                | b'\''
                | b'`'
                | b'+'
                | b'='
        ) {
            return true;
        }
    }
    // $name: plain identifier, no !/?/= suffix allowed
    is_plain_identifier(rest)
}

///
/// ### Symbol.all_symbols
///
/// - Symbol.all_symbols -> [Symbol]
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/s/all_symbols.html]
#[monoruby_builtin]
fn all_symbols(_: &mut Executor, _: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let symbols: Vec<Value> = IdentId::all_symbols()
        .into_iter()
        .map(Value::symbol)
        .collect();
    Ok(Value::array_from_vec(symbols))
}

///
/// ### Symbol#<=>
///
/// - self <=> other -> -1 | 0 | 1 | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/=3c=3d=3e.html]
#[monoruby_builtin]
fn cmp(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::from_ord(lhs.compare(&rhs))),
        (_, None) => Ok(Value::nil()),
    }
}

///
/// ### Symbol#==
///
/// - self == other -> true | false
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/=3d=3d.html]
#[monoruby_builtin]
fn eq(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::bool(lhs == rhs)),
        (_, None) => Ok(Value::bool(false)),
    }
}

#[monoruby_builtin]
fn ne(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let lhs = lfp.self_val();
    let rhs = lfp.arg(0);
    match (lhs.as_symbol(), rhs.try_symbol()) {
        (lhs, Some(rhs)) => Ok(Value::bool(lhs != rhs)),
        (_, None) => Ok(Value::bool(true)),
    }
}

#[cfg(test)]
mod tests {
    use crate::tests::*;
    #[test]
    fn symbol_cmp() {
        run_binop_tests2(
            &[":aaa", ":xxx", ":妖精"],
            &["<=>", "===", "==", "!="],
            &[":aaa", ":xxx", "nil", "3"],
        );
    }

    #[test]
    fn symbol_match() {
        run_test(r#":hello.match?(/ell/)"#);
        run_test(r#":hello.match?(/xyz/)"#);
        run_test(r#":hello.match?(/\A[a-z]+\z/)"#);
    }

    #[test]
    fn symbol_methods() {
        run_test(r#":hello.upcase"#);
        run_test(r#":hello.downcase"#);
        run_test(r#":hello.capitalize"#);
        run_test(r#":hElLo.swapcase"#);
        run_test(r#":hello.size"#);
        run_test(r#":hello.length"#);
        run_test(r#":hello.empty?"#);
        run_test(r#":"".empty?"#);
        run_test(r#":hello.start_with?("he")"#);
        run_test(r#":hello.end_with?("lo")"#);
        run_test(r#":hello.succ"#);
        run_test(r#":hello.next"#);
        run_test(r#":hello.id2name"#);
        run_test(r#":hello.name"#);
        run_test(r#":hello.name.frozen?"#);
        run_test(r#":hello.intern"#);
        run_test(r#":hello.to_sym"#);
        run_test(r#":hello =~ /ell/"#);
        run_test(r#":hello[1]"#);
        run_test(r#":hello[1,3]"#);
        run_test(r#":abc.casecmp(:ABC)"#);
        run_test(r#":abc.casecmp(:abd)"#);
        run_test(r#":abc.casecmp?(:ABC)"#);
        run_test(r#":abc.casecmp("abc")"#);
    }

    #[test]
    fn symbol_all_symbols() {
        run_test(r#"Symbol.all_symbols.is_a?(Array)"#);
        run_test(r#"Symbol.all_symbols.all? { |s| s.is_a?(Symbol) }"#);
        run_test(r#"Symbol.all_symbols.include?(:to_s)"#);
    }

    #[test]
    fn symbol_to_proc() {
        run_test(
            r#"
        :to_i.to_proc.call("42")
        "#,
        );
        run_test(
            r#"
        :to_i.to_proc["ff", 16]
        "#,
        );
        run_test(
            r#"
        (1..3).collect(&:to_s)
        "#,
        );
    }

    #[test]
    fn symbol_to_s_encoding() {
        run_test(r#":hello.to_s.encoding.to_s"#);
        run_test(r#":"日本語".to_s.encoding.to_s"#);
    }

    #[test]
    fn symbol_match_with_matchdata() {
        run_test(r#":hello.match(/ell/).class"#);
        run_test(r#":hello.match(/ell/)[0]"#);
        run_test(r#":hello.match(/xyz/)"#);
    }

    #[test]
    fn symbol_to_proc_public_send() {
        // to_proc should use public_send and forward blocks
        run_test(r#"[1, 2, 3].map(&:to_s)"#);
        run_test(r#"["a", "b", "c"].map(&:upcase)"#);
    }

    #[test]
    fn symbol_comparable() {
        // Comparable methods derived from <=>
        run_test(r#":abc < :abd"#);
        run_test(r#":abd > :abc"#);
        run_test(r#":abc >= :abc"#);
        run_test(r#":abc <= :abc"#);
        run_test(r#":bbb.between?(:aaa, :ccc)"#);
        run_test(r#":aaa.between?(:bbb, :ccc)"#);
        run_test(r#":ddd.clamp(:bbb, :ccc)"#);
        run_test(r#":aaa.clamp(:bbb, :ccc)"#);
        run_test(r#":bbb.clamp(:aaa, :ccc)"#);
    }

    #[test]
    fn symbol_name_identity() {
        // Symbol#name returns the same frozen String object each time
        run_test(r#":hello.name.equal?(:hello.name)"#);
        run_test(r#":hello.name.frozen?"#);
        run_test(r#":world.name.equal?(:world.name)"#);
    }

    #[test]
    fn symbol_new_error() {
        // Symbol.new raises NoMethodError, Symbol.allocate raises TypeError
        run_test_error(r#"Symbol.new"#);
        run_test_error(r#"Symbol.allocate"#);
    }

    #[test]
    fn symbol_global_var_literal() {
        // :$name symbol literals
        run_test(r#":$ruby"#);
        run_test(r#":$0"#);
        run_test(r#":$~"#);
        run_test(r#":$&"#);
        run_test(r#":$'"#);
        run_test(r#":$+"#);
        // :$-w style
        run_test(r#":$-w"#);
        // :@name and :@@name
        run_test(r#":@ruby"#);
        run_test(r#":@@ruby"#);
    }

    #[test]
    fn symbol_binary() {
        // Binary (ASCII-8BIT) string can be converted to symbol and back
        run_test(r#""\xC3".b.to_sym.to_s.encoding.to_s"#);
        run_test(r#""\xC3".b.to_sym.to_s == "\xC3".b"#);
        run_test(r#""\xC3".b.to_sym == "\xC3".b.to_sym"#);
        // Binary and UTF-8 symbols with same bytes are distinct
        run_test_no_result_check(r#""\xC3\xA3".to_sym != "\xC3\xA3".b.to_sym"#);
    }
}
