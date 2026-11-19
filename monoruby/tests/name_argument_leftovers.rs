extern crate monoruby;
use monoruby::tests::*;

// Issue #1995: what #1987 left of the name arguments — a name that is
// not UTF-8 text keeps its bytes in NameError messages and class names,
// `autoload`, `Binding#local_variable_*`, `Thread#[]` / `#key?` /
// `#fetch` read their names as CRuby does, `method_missing` wants a
// Symbol, `define_method` keeps a UTF-16 name's encoding, Struct member
// names go through `rb_id_attrset`, a constant name may start with an
// uppercase letter outside ASCII in any encoding with a case table, a
// BOM-carrying UTF-16 / UTF-32 source is named as itself in a
// conversion error, and `Marshal.load` reports as `path2class` does.

const PRELUDE: &str = r##"
    def t; r = yield; r = r.b if r.is_a?(String); r.inspect.b.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message.b}".gsub(/0x\h+/, "0x"); end
    class Foo; X = 1; end
"##;

#[test]
fn issue_probe() {
    run_test_once(&format!(
        r##"{PRELUDE}
        bin = "A\xE9b".b; euc = "A\xA4\xA2".dup.force_encoding("EUC-JP"); u = "a\xE9b".dup.force_encoding("US-ASCII")
        o = Object.new
        r = []
        r << t {{ o.method(bin) }}
        r << t {{ o.public_send(euc) }}
        r << t {{ Foo.instance_method(bin) }}
        r << t {{ Foo.class_variable_get(bin) }}
        r << t {{ Foo.const_get(bin) }}
        r << t {{ Foo.const_get(euc) }}
        r << t {{ Foo.send(:private_constant, bin) }}
        r << t {{ Foo.send(:undef_method, euc) }}
        r << t {{ Foo.autoload("x", "y") }}
        r << t {{ Foo.autoload(u, "y") }}
        r << t {{ binding.local_variable_get("@") }}
        r << t {{ binding.local_variable_get(bin) }}
        r << t {{ binding.local_variable_get(u) }}
        r << t {{ binding.local_variable_set(bin, 1) }}
        r << t {{ binding.local_variable_defined?(u) }}
        r << t {{ Thread.current[u] }}
        r << t {{ Thread.current.key?(u) }}
        r << t {{ o.send(:method_missing, "foo") }}
        r << t {{ o.send(:method_missing) }}
        r << t {{ Class.new {{ define_method("A".encode("UTF-16LE")) {{ }} }}.instance_methods(false).map {{ |m| [m.to_s.b, m.encoding.to_s] }} }}
        r << t {{ Struct.new(:"") }}
        r << t {{ Struct.new(:"@") }}
        r << t {{ Struct.new(:a, u) }}
        r << t {{ Object.const_set("\xC9x".dup.force_encoding("ISO-8859-1"), 1) }}
        r << t {{ Object.const_defined?("\xC9x".dup.force_encoding("ISO-8859-1")) }}
        r << t {{ Struct.new("\xC9".dup.force_encoding("ISO-8859-1")).name }}
        r << t {{ Object.const_get("Nope::x") }}
        r << t {{ Object.const_get("Foo::X::Y") }}
        r << t {{ Object.const_defined?("Foo::X::Y") }}
        r << t {{ Object.const_defined?("Nope::x") }}
        r << t {{ "\xFE\xFF\x00a\x00".dup.force_encoding("UTF-16").encode("UTF-8") }}
        r << t {{ Marshal.load("\x04\bS:\bFoo\x06:\x06ai\x06".b) }}
        r << t {{ Marshal.load("\x04\bX".b) }}
        module Wd; end
        Wd.const_set("B\xA4\xA2".dup.force_encoding("EUC-JP"), Module.new)
        Wd.const_get("B\xA4\xA2".dup.force_encoding("EUC-JP")).const_set(:Z, 5)
        r << t {{ Wd.const_get("B\xA4\xA2::Z".dup.force_encoding("EUC-JP")) }}
        r << t {{ Wd.const_defined?("B\xA4\xA2::Z".dup.force_encoding("EUC-JP")) }}
        r
        "##
    ));
}

#[test]
fn nameerror_keeps_name_bytes() {
    // The message's encoding is the name's, as CRuby formats the name
    // into it; `#name` stays the Symbol.
    run_test_with_prelude(
        r##"
        euc = "zz\xA4\xA2".dup.force_encoding("EUC-JP")
        o = Object.new
        [
          (begin; o.public_send(euc); rescue NoMethodError => e; [e.message.b, e.message.encoding.to_s, e.name.to_s.b]; end),
          (begin; o.public_send(euc.to_sym); rescue NoMethodError => e; [e.message.b, e.message.encoding.to_s, e.name.to_s.b]; end),
          (begin; o.__send__(euc.to_sym); rescue NoMethodError => e; [e.message.b, e.message.encoding.to_s]; end),
          (begin; Foo.send(:undef_method, euc.to_sym); rescue NameError => e; [e.message.b, e.message.encoding.to_s]; end),
          (begin; Foo.instance_method(euc); rescue NameError => e; [e.message.b, e.message.encoding.to_s]; end),
          (begin; Foo.const_get("Q\xA4\xA2".dup.force_encoding("EUC-JP")); rescue NameError => e; [e.message.b, e.message.encoding.to_s, e.receiver]; end),
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn constant_name_first_character() {
    // Which first byte makes "<b>x" a constant name, per encoding: the
    // single-byte encodings with case tables, the JIS families' full-width
    // capitals, and nothing in the Chinese / Korean sets or BINARY.
    run_test_once(
        r##"
        encs = %w[ISO-8859-1 ISO-8859-2 ISO-8859-5 ISO-8859-7 ISO-8859-10 ISO-8859-14 KOI8-R KOI8-U
                  Windows-1250 Windows-1251 Windows-1252 Windows-1253 TIS-620 ASCII-8BIT IBM437 macRoman]
        rows = encs.map do |en|
          ok = (0x80..0xFF).select { |b| (Module.new.const_defined?((b.chr + "x").force_encoding(en)); true) rescue false }
          [en, ok]
        end
        probes = %w[Ａ ａ Α α Ж ж あ É é ǅ Ⓐ]
        mb = %w[UTF-8 EUC-JP Shift_JIS Windows-31J GBK EUC-KR Big5].map do |en|
          [en, probes.map { |c|
            s = begin; (c + "x").encode(en); rescue; nil; end
            next "-" unless s
            begin; Module.new.const_defined?(s); "C"; rescue NameError; "n"; end
          }.join]
        end
        rest = ["A€", "Aé?", "Aé!", "ǅx"].map { |n| (Module.new.const_defined?(n); true) rescue $!.class }
        [rows, mb, rest]
        "##,
    );
}

#[test]
fn binding_local_variable_names() {
    run_test_once(&format!(
        r##"{PRELUDE}
        b = binding
        zz = 1
        n = ->(e) {{ e.respond_to?(:name) ? e.name.class : nil }}
        [
          t {{ b.local_variable_get("a\xE9b".b) }},
          t {{ b.local_variable_get("zq\xE9b".b) }},
          t {{ b.local_variable_defined?("zq\xE9c".b) }},
          t {{ b.local_variable_get("Été") }},
          t {{ b.local_variable_get("été_unknown") }},
          t {{ b.local_variable_get(:foo?) }},
          t {{ b.local_variable_get("foo?") }},
          t {{ b.local_variable_get(:@x) }},
          t {{ b.local_variable_defined?("@x") }},
          t {{ b.local_variable_defined?(:Foo) }},
          t {{ b.local_variable_get("_1") }},
          t {{ b.local_variable_get("a\u0000") }},
          t {{ b.local_variable_get(3) }},
          t {{ b.local_variable_get("zz") }},
          t {{ b.local_variable_set("zz\xA4\xA2".dup.force_encoding("EUC-JP"), 3); b.local_variable_get("zz\xA4\xA2".dup.force_encoding("EUC-JP")) }},
          (begin; b.local_variable_get("zq\xE9b".b); rescue NameError => e; [e.name.class, e.receiver.class]; end),
          (begin; b.local_variable_get("_1"); rescue NameError => e; [e.name, e.receiver.class]; end),
          (begin; b.local_variable_get("@zq1995_unseen"); rescue NameError => e; [e.name.class, e.receiver.class]; end),
        ]
        "##
    ));
}

#[test]
fn thread_key_names() {
    // `#[]`, `#key?` and `#fetch` read the key through `rb_check_id`;
    // `#[]=` and the thread variables intern it as `to_sym` does.
    run_test_once(&format!(
        r##"{PRELUDE}
        u = "a\xE9b".dup.force_encoding("US-ASCII"); v = "z\xE9".dup.force_encoding("UTF-8")
        th = Thread.current
        [
          t {{ th[u] = 1 }},
          t {{ th[v] = 1 }},
          t {{ th.fetch(u, 2) }},
          t {{ th.thread_variable_get(u) }},
          t {{ th.thread_variable_set(u, 1) }},
          t {{ th.thread_variable?(u) }},
          t {{ th[u] }},
          t {{ th.key?(v) }},
          t {{ th[3] }},
          t {{ th["fresh_key_#{{rand(1)}}"] }},
        ]
        "##
    ));
}

#[test]
fn struct_member_setters() {
    run_test_once(&format!(
        r##"{PRELUDE}
        names = [:"", :"@", :"@@", :"$", :"@x", :"@@x", :"$x", :X, :x?, :x!, :"x=", :"a b", :"1", :+, :-, :[], :[]=,
                 :"!", :"!=", :<=>, :"=", :"==", :`, :+@, :"x y=", :"?", :"::", :"x?!=", :"@x?=", :"$1=", :"$-w=",
                 :"1a=", :"@1=", :"$x?=", :"x==", :"Foo?=", :"a=b=", :"===", :"..", :"&.", :"!@", :"_", :"_="]
        r = names.map do |n|
          [n.to_s, t {{ s = Struct.new(n); [s.members.map(&:to_s), s.instance_methods(false).map(&:to_s).sort] }}]
        end
        r << t {{ Struct.new(:a, "b").members }}
        r << t {{ Struct.new(:a, "a") }}
        r << t {{ Struct.new(:a, 3) }}
        r << t {{ Struct.new(:a, "a\xE9".dup.force_encoding("UTF-8")) }}
        r
        "##
    ));
}

#[test]
fn non_utf8_class_names() {
    run_test_once(
        r##"
        module Wd; end
        m = Module.new
        Wd.const_set("B\xA4\xA2".dup.force_encoding("EUC-JP"), m)
        m::Z = Class.new
        s = Struct.new("\xC9".dup.force_encoding("ISO-8859-1"))
        [m.name.b, m.name.encoding.to_s, m.inspect.b, m.to_s.b, m::Z.name.b, m::Z.name.encoding.to_s,
         s.name.b, s.name.encoding.to_s, s.inspect.b,
         (begin; m.foo; rescue => e; e.message.b; end)]
        "##,
    );
}

#[test]
fn marshal_load_class_paths() {
    run_test_once(&format!(
        r##"{PRELUDE}
        class MNot; end; module MMod; end; MNum = 42; class MCls; end
        [
          t {{ Marshal.load("\x04\bS:\tMNot\x06:\x06ai\x06".b) }},
          t {{ Marshal.load("\x04\bS:\tMMod\x06:\x06ai\x06".b) }},
          t {{ Marshal.load("\x04\bS:\tMNum\x06:\x06ai\x06".b) }},
          t {{ Marshal.load("\x04\bS:\tMNop\x06:\x06ai\x06".b) }},
          t {{ Marshal.load("\x04\bS:\fInteger\x06:\x06ai\x06".b) }},
          t {{ Marshal.load("\x04\bo:\tMMod\x00".b) }},
          t {{ Marshal.load("\x04\bo:\tMNum\x00".b) }},
          t {{ Marshal.load("\x04\bo:\x0eMCls::Foo\x00".b) }},
          t {{ Marshal.load("\x04\b\x01".b) }},
          t {{ Marshal.load("\x04\bz".b) }},
          t {{ Marshal.load("\x04\bc\tMMod".b) }},
          t {{ Marshal.load("\x04\bm\tMCls".b) }},
          t {{ Marshal.load("\x04\bc\tMNum".b) }},
          t {{ Marshal.load("\x04\bM\tMNum".b) }},
          t {{ Marshal.load("\x04\bc\x0eMCls::Foo".b) }},
        ]
        "##
    ));
}

#[test]
fn dummy_wide_source_errors() {
    // The error is about the dummy `UTF-16` / `UTF-32` the string is in,
    // not the byte order its BOM selected.
    run_test_once(
        r##"
        def t; yield.inspect; rescue Encoding::InvalidByteSequenceError => e; [e.class, e.message, e.source_encoding_name, e.source_encoding.to_s, e.destination_encoding_name, e.error_bytes.b, e.readagain_bytes&.b]; rescue Encoding::UndefinedConversionError => e; [e.class, e.message, e.source_encoding_name, e.destination_encoding_name]; end
        [
          t { "\xFE\xFF\x00a\x00".dup.force_encoding("UTF-16").encode("UTF-8") },
          t { "\xFF\xFE\x61\x00\x00".dup.force_encoding("UTF-16").encode("UTF-8") },
          t { "\xFE\xFF\xD8\x00\x00a".dup.force_encoding("UTF-16").encode("UTF-8") },
          t { "\x00\x00\xFE\xFF\x00\x00\x00a\x00".dup.force_encoding("UTF-32").encode("UTF-8") },
          t { "\x00\x00\xFE\xFF\x00\x11\x00\x00".dup.force_encoding("UTF-32").encode("UTF-8") },
          t { "\xFE\xFF\x00a\x00".dup.force_encoding("UTF-16").encode("EUC-JP") },
          t { "\xFE\xFF\x30\x00".dup.force_encoding("UTF-16").encode("ISO-8859-1") },
          t { "\xFE\xFF\x00a".dup.force_encoding("UTF-16").encode("UTF-8") },
        ]
        "##,
    );
}

#[test]
fn autoload_constant_names() {
    // The message shows the name escaped (`QUOTE_ID`) when it is not
    // printable ASCII; a US-ASCII String with 8-bit bytes is interned
    // as `to_sym` interns it.
    run_test_once(&format!(
        r##"{PRELUDE}
        class AutoFoo; end
        [
          t {{ AutoFoo.autoload("a\xE9b".b, "y") }},
          t {{ AutoFoo.autoload("a\xA4\xA2".dup.force_encoding("EUC-JP"), "y") }},
          t {{ AutoFoo.autoload("a\u0001", "y") }},
          t {{ AutoFoo.autoload("a\xE9".dup.force_encoding("UTF-8"), "y") }},
          t {{ AutoFoo.autoload(:"a b", "y") }},
          t {{ AutoFoo.autoload("a\xE9b".dup.force_encoding("US-ASCII"), "y") }},
          t {{ AutoFoo.autoload("A\xE9b".dup.force_encoding("US-ASCII"), "y") }},
          t {{ AutoFoo.autoload?("a\xE9b".dup.force_encoding("US-ASCII")) }},
          t {{ AutoFoo.autoload(3, "y") }},
        ]
        "##
    ));
}
