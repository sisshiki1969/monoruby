use super::*;

//
// Symbol class
//

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
/// Returns the name of the symbol as a string. ASCII-only symbols return a
/// US-ASCII encoded string. The returned String is "chilled": it behaves as
/// mutable but emits a one-shot deprecation warning on first mutation
/// (matching CRuby's `rb_sym_to_s`).
///
/// [https://docs.ruby-lang.org/ja/latest/method/Symbol/i/to_s.html]
#[monoruby_builtin]
fn sym_to_s(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let sym = lfp.self_val().as_symbol();
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
    let mut result = Value::string_from_inner(inner);
    result.set_chilled();
    Ok(result)
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
    Ok(Value::string(inspect_symbol(sym)))
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
    let body_fid = SYMBOL_TO_PROC_BODY_FUNCID;
    let outer_lfp = Lfp::heap_frame(self_val, globals[body_fid].meta());
    let proc = Proc::from_parts(outer_lfp, body_fid, pc);
    Ok(proc.into())
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

    #[test]
    fn symbol_inspect_unquoted() {
        // Plain identifiers
        run_test(r#":hello.inspect"#);
        run_test(r#":Fred.inspect"#);
        run_test(r#":_abc.inspect"#);
        run_test(r#":fred?.inspect"#);
        run_test(r#":fred!.inspect"#);
        run_test(r#":BAD!.inspect"#);
        run_test(r#":_BAD!.inspect"#);
        // Global / ivar / cvar
        run_test(r#":$ruby.inspect"#);
        run_test(r#":@ruby.inspect"#);
        run_test(r#":@@ruby.inspect"#);
        run_test(r#":$-w.inspect"#);
        // Special single-char globals
        run_test(r#":$+.inspect"#);
        run_test(r#":$~.inspect"#);
        run_test(r#":$?.inspect"#);
        run_test(r#":$!.inspect"#);
        // $digits
        run_test(r#":$0.inspect"#);
        run_test(r#":$1234.inspect"#);
        // Operators
        run_test(r#":+.inspect"#);
        run_test(r#":-.inspect"#);
        run_test(r#":*.inspect"#);
        run_test(r#":**.inspect"#);
        run_test(r#":+@.inspect"#);
        run_test(r#":-@.inspect"#);
        run_test(r#":<=>.inspect"#);
        run_test(r#":==.inspect"#);
        run_test(r#":===.inspect"#);
        run_test(r#":=~.inspect"#);
        run_test(r#":[].inspect"#);
        run_test(r#":[]=.inspect"#);
        run_test(r#":"<<".inspect"#);
        run_test(r#":">>".inspect"#);
        // Non-ASCII letters are valid identifier chars
        run_test(r#":"ê".inspect"#);
        run_test(r#":"日本語".inspect"#);
    }

    #[test]
    fn symbol_inspect_quoted() {
        // $ followed by non-simple content
        run_test(r#":"$ruby!".inspect"#);
        run_test(r#":"@ruby!".inspect"#);
        run_test(r#":"@@ruby!".inspect"#);
        run_test(r#":"$-ww".inspect"#);
        run_test(r#":"$".inspect"#);
        // Non-identifier, non-operator sequences
        run_test(r#":"foo bar".inspect"#);
        run_test(r#":"9".inspect"#);
        run_test(r#":"*foo".inspect"#);
        run_test(r#":"foo ".inspect"#);
        run_test(r#":" foo".inspect"#);
        run_test(r#":" ".inspect"#);
        run_test(r#":"&&".inspect"#);
        run_test(r#":"||".inspect"#);
        run_test(r#":"|||".inspect"#);
        run_test(r#":"++".inspect"#);
        run_test(r#":":".inspect"#);
        run_test(r#":"::".inspect"#);
        run_test(r#":",".inspect"#);
        run_test(r#":".".inspect"#);
        run_test(r#":"..".inspect"#);
        run_test(r#":"...".inspect"#);
        run_test(r#":";".inspect"#);
        run_test(r#":"=".inspect"#);
        run_test(r#":"=>".inspect"#);
        run_test(r#":"?".inspect"#);
        run_test(r#":"@".inspect"#);
        // Escaped characters inside quoted form
        run_test(r#":"\"".inspect"#);
        run_test(r#":"\"\"".inspect"#);
        run_test(r#":"'".inspect"#);
        // Binary symbol gets quoted with escape
        run_test(r#""foo\xA4".b.to_sym.inspect"#);
    }

    #[test]
    fn symbol_to_proc_metadata() {
        // Arity is -2 (one required + rest)
        run_test(r#":to_i.to_proc.arity"#);
        // Parameters is [[:req], [:rest]] with no names (native builtin body)
        run_test(r#":to_i.to_proc.parameters"#);
        // Source location is nil (not an ISeq)
        run_test(r#":to_i.to_proc.source_location"#);
        // It is a lambda
        run_test(r#":to_i.to_proc.lambda?"#);
        // It is a Proc
        run_test(r#":to_i.to_proc.is_a?(Proc)"#);
    }

    #[test]
    fn symbol_to_proc_block_forwarding() {
        // A block passed to Proc#call on a Symbol-derived proc must be
        // forwarded to the underlying method.
        run_test(
            r#"
            klass = Class.new do
              def m
                yield
              end
            end
            :m.to_proc.call(klass.new) { :value }
            "#,
        );
    }

    #[test]
    fn symbol_to_proc_no_receiver_raises() {
        // Proc#call with no receiver argument raises ArgumentError.
        run_test_error(r#":object_id.to_proc.call"#);
    }

    #[test]
    fn symbol_to_s_chilled_basics() {
        // Symbol#to_s returns a mutable string (chilled, not frozen).
        run_test(r#":hello.to_s.frozen?"#);
        // dup clears the chilled bit so mutation is silent.
        run_test(r#"s = :hello.to_s.dup; s << "X"; s"#);
        // Each call returns a fresh string instance.
        run_test(r#":hello.to_s.equal?(:hello.to_s)"#);
        // Mutation succeeds (warning or not) and the string changes.
        run_test(
            r#"
            Warning[:deprecated] = false
            s = :bad!.to_s
            s.upcase!
            s
            "#,
        );
    }

    #[test]
    fn symbol_to_s_chilled_dup_silent() {
        // With Warning[:deprecated]=true and a StringIO'd $stderr, dup'd
        // chilled strings must not emit a warning on mutation.
        run_test_once(
            r#"
            require 'stringio'
            old_stderr = $stderr
            old_dep = Warning[:deprecated]
            begin
              $stderr = StringIO.new
              Warning[:deprecated] = true
              :hello.to_s.dup << "X"
              $stderr.string
            ensure
              $stderr = old_stderr
              Warning[:deprecated] = old_dep
            end
            "#,
        );
    }

    #[test]
    fn symbol_to_s_chilled_suppressed() {
        // Warning[:deprecated]=false suppresses the mutation warning.
        run_test_once(
            r#"
            require 'stringio'
            old_stderr = $stderr
            old_dep = Warning[:deprecated]
            begin
              $stderr = StringIO.new
              Warning[:deprecated] = false
              :bad!.to_s.upcase!
              $stderr.string
            ensure
              $stderr = old_stderr
              Warning[:deprecated] = old_dep
            end
            "#,
        );
    }
}
