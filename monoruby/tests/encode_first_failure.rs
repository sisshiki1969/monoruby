extern crate monoruby;
use monoruby::tests::*;

// A conversion that fails two ways at once — a source cell its own
// encoding has no character for, and a character the destination has
// no cell for — reports whichever comes first in the string, in
// `String#encode` as in the converter (#1611).

#[test]
fn the_earlier_failure_is_the_one_reported() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        res = []
        tis = [0xE3, 0x81].pack("C*").force_encoding("TIS-620")
        res << t.() { tis.encode("EUC-JP") }
        res << t.() { [0x81, 0xE3].pack("C*").force_encoding("TIS-620").encode("EUC-JP") }
        res << t.() { tis.encode("EUC-JP", undef: :replace) }
        res << t.() { tis.encode("EUC-JP", invalid: :replace) }
        res << t.() { tis.encode("UTF-8") }
        res << t.() { tis.encode("UTF-16LE") }
        res << t.() { Encoding::Converter.new("TIS-620", "EUC-JP").convert(tis.dup) }
        cp = [0xB0, 0xA1, 0xA2, 0xE8].pack("C*").force_encoding("CP949")
        res << t.() { cp.encode("EUC-JP") }
        res << t.() { [0xA2, 0xE8, 0xB0, 0xA1].pack("C*").force_encoding("CP949").encode("EUC-JP") }
        res << t.() { cp.encode("EUC-JP", undef: :replace) }
        res << t.() { cp.encode("Shift_JIS") }
        res << t.() { cp.encode("UTF-8") }
        res << t.() { "\xa4\xa2\xf5\xa1".force_encoding("EUC-JP").encode("Shift_JIS") }
        res << t.() { "\xa9\xa1\xa4\xa2\xf5\xa1".force_encoding("EUC-JP").encode("ISO-8859-1") }
        res << t.() { "a\xa4\xa2\xf5\xa1".force_encoding("EUC-JP").encode("ISO-8859-1") }
        res << t.() { "\xa4\x40\xf9\xd6".force_encoding("Big5").encode("EUC-JP") }
        res << t.() { "\xf9\xd6\xa4\x40".force_encoding("Big5").encode("EUC-JP") }
        res << t.() { "\x81\x40\xa1\xa1".force_encoding("Windows-31J").encode("EUC-JP") }
        res << t.() { "\xa4\xa2\x8e\xb1\xf5\xa1".force_encoding("EUC-JP").encode("ISO-2022-JP") }
        res << t.() { "\xa4\xa2\xff\xf5\xa1".force_encoding("EUC-JP").encode("Shift_JIS") }
        res
        "##,
    );
}
