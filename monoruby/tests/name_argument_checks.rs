extern crate monoruby;
use monoruby::tests::*;

// Issue #1987: a name handed to a reflection method goes through CRuby's
// `rb_check_id`: a String whose bytes are broken in its (ASCII-compatible)
// encoding is an EncodingError before the method's own name check, and the
// name check itself sees the string's characters in its own encoding.

const PRELUDE: &str = r##"
    def t; r = yield; r.is_a?(Class) ? "class" : r; rescue Exception => e; "#{e.class}: #{e.message.b}"; end
    class Foo; X = 1; end
    class NameConv; def initialize(v) = @v = v; def to_str = @v; end
"##;

#[test]
fn name_issue_probe() {
    run_test_with_prelude(
        r##"
        u = "a\xE9b".dup.force_encoding("US-ASCII")
        [
          t { Object.new.instance_variable_get(u) },
          t { Object.new.instance_variable_set(u, 1) },
          t { Object.new.instance_variable_defined?(u) },
          t { Object.const_get(u) },
          t { Object.const_defined?(u) },
          t { Struct.new(u) },
          t { Marshal.load(Marshal.dump(u.to_sym)) },
        ]
        "##,
        PRELUDE,
    );
}

const NAMES: &str = r##"
    names = {
      u: "a\xE9b".dup.force_encoding("US-ASCII"), iv_u: "@a\xE9b".dup.force_encoding("US-ASCII"), c_u: "A\xE9b".dup.force_encoding("US-ASCII"),
      utf8b: "@a\xE9b", cutf8b: "A\xE9b", eucb: "@a\xA4".dup.force_encoding("EUC-JP"), ceucb: "A\xA4".dup.force_encoding("EUC-JP"),
      euc: "@a\xA4\xA2".dup.force_encoding("EUC-JP"), ceuc: "A\xA4\xA2".dup.force_encoding("EUC-JP"),
      sjis: "@\x82\xA0".dup.force_encoding("Shift_JIS"), csjis: "A\x82\xA0".dup.force_encoding("Shift_JIS"),
      latin: "@\xE9".dup.force_encoding("ISO-8859-1"), clatin: "A\xE9".dup.force_encoding("ISO-8859-1"),
      bin: "@a\xE9b".b, cbin: "A\xE9b".b,
      ok: "@a", cok: "X", bad: "a", cbad: "aqq", empty: "", at: "@", atdigit: "@1", cls: "@@a\xE9b".dup.force_encoding("US-ASCII"),
      u8ok: "@\u3042", cu8: "\u00C9x", cu8lower: "\u3042", sym: :@x, csym: :X, nil: nil, int: 1,
      pathb: "Object::A\xE9".dup.force_encoding("US-ASCII"), path2: "A\xE9::B".dup.force_encoding("US-ASCII"), path: "Foo::X", pathbad: "Foo::x",
    }
"##;

#[test]
fn instance_variable_names_are_checked() {
    run_test_with_prelude(
        r##"
        names.map do |k, n|
          o = Object.new
          [k, t { o.instance_variable_get(n) }, t { o.instance_variable_set(n, 1) }, t { o.instance_variable_defined?(n) },
           t { o.remove_instance_variable(n) }, t { o.instance_variables.map { |v| v.to_s.b } }]
        end
        "##,
        &format!("{PRELUDE}{NAMES}"),
    );
}

#[test]
fn constant_path_names_are_checked() {
    run_test_with_prelude(
        r##"
        skip = %i[cbin ceuc csjis clatin]
        names.map do |k, n|
          [k, (skip.include?(k) ? nil : t { Object.const_get(n) }), t { Object.const_defined?(n) }, t { Object.const_source_location(n).class }]
        end
        "##,
        &format!("{PRELUDE}{NAMES}"),
    );
}

#[test]
fn constant_path_conversion_and_encoding() {
    run_test_with_prelude(
        r##"
        conv = NameConv
        [
          t { Object.const_get("X".encode("UTF-16LE")) },
          t { Object.const_defined?("X".encode("UTF-16LE")) },
          t { Object.const_source_location("X".encode("UTF-32BE")) },
          t { Object.const_get("X".dup.force_encoding("UTF-7")) },
          t { Object.const_get(conv.new("Foo::X")) },
          t { Object.const_get(conv.new("A\xE9b".dup.force_encoding("US-ASCII"))) },
          t { Object.const_get(conv.new(1)) },
          t { Object.const_defined?(conv.new("Foo::X")) },
          t { Object.const_get(Object.new) },
        ]
        "##,
        PRELUDE,
    );
}

#[test]
fn struct_class_name_is_checked() {
    run_test_with_prelude(
        r##"
        conv = NameConv
        skip = %i[int sym csym]
        res = names.reject { |k, _| skip.include?(k) }.map do |k, n|
          [k, t { Struct.new(n) }, t { Struct.new(n, :a) }]
        end
        res << t { Struct.new(conv.new("a")) }
        res << t { Struct.new(conv.new("A\xE9".dup.force_encoding("US-ASCII"))) }
        res << t { Struct.new(conv.new(1)) }
        res << t { Struct.new(1) }
        res << t { Struct.new(nil, :a).new(1).a }
        res << t { Struct.new("NameArgChecksStruct", :a).name }
        res
        "##,
        &format!("{PRELUDE}{NAMES}"),
    );
}

#[test]
fn marshal_load_checks_symbol_bytes() {
    run_test_with_prelude(
        r##"
        [
          "\x04\bI:\ba\xE9b\x06:\x06EF",
          "\x04\bI:\ba\xE9b\x06:\x06ET",
          "\x04\b:\ba\xE9b",
          "\x04\bI:\aa\xA4\x06:\rencoding\"\vEUC-JP",
          "\x04\b[\x06I\"\x06a\x06:\x06ET",
          "\x04\bo:\vObject\x06I:\b@\xE9b\x06:\x06ETi\x06",
          "\x04\bi\x06",
          "\x04\bI:\ba\xA4\xA2\x06:\rencoding\"\vEUC-JP",
          "\x04\bI:\ba\xE9b\x06:\rencoding\"\x0FISO-8859-1",
          "\x04\b[\aI:\ba\xE9b\x06:\x06ET;\x00",
          "\x04\b{\x06I:\ba\xE9b\x06:\x06EFi\x06",
        ].map do |d|
          t { v = Marshal.load(d.b); [v.inspect.b, (v.encoding.name if v.is_a?(Symbol))] }
        end
        "##,
        PRELUDE,
    );
}
