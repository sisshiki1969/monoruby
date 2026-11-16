extern crate monoruby;
use monoruby::tests::*;

// Issue #1977: `String#to_sym` interns a US-ASCII string with 8-bit
// bytes as a US-ASCII Symbol (`rb_str_intern` skips the validity check
// for a string that is already US-ASCII), while a *name* argument
// (`rb_check_id`: `send`, `respond_to?`, `method`, ...) refuses it, and
// a broken string of any other ASCII-compatible encoding is refused by
// both.

#[test]
fn usascii_8bit_to_sym() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        u = "a\xE9b".force_encoding("US-ASCII")
        v = "\xC3\xA9".force_encoding("US-ASCII")
        [
          t { u.to_sym.class },
          t { u.to_sym.encoding.to_s },
          t { u.to_sym =~ /b/ },
          t { u.to_sym.inspect },
          t { u.to_sym.to_s.bytes },
          t { u.to_sym.to_s.encoding.to_s },
          t { u.to_sym.name.encoding.to_s },
          t { u.to_sym == u.dup.force_encoding("BINARY").to_sym },
          t { u.to_sym.equal?("a\xE9b".force_encoding("US-ASCII").intern) },
          t { v.to_sym == :"é" },
          t { [v.to_sym.inspect, v.to_sym.encoding.to_s] },
          t { v.to_sym.size },
          t { u.to_sym.length },
          t { u.to_sym.to_proc.class },
          t { u.to_sym.start_with?("a") },
          t { u.to_sym.start_with?(/a/) },
          t { u.to_sym.match?(/b/) },
          t { u.to_sym[0] },
          t { u.to_sym.upcase.inspect },
          t { u.to_sym.succ.inspect },
          t { u.to_sym <=> :a },
          t { [u.to_sym].inspect },
          t { {u.to_sym => 1}.inspect },
          t { Object.new.respond_to?(u) },
          t { Object.new.send(u) },
          t { Object.new.method(u) },
          t { "\xD8".force_encoding("UTF-16LE").to_sym.inspect },
          t { "\xD8\x00\x00".force_encoding("UTF-16LE").to_sym.inspect },
          t { "\x00\xD8".force_encoding("UTF-16LE").to_sym.inspect },
          t { "\xFF".force_encoding("UTF-8").to_sym.inspect },
          t { "\xFF".force_encoding("EUC-JP").to_sym.inspect },
          t { "\xFF".force_encoding("ISO-8859-1").to_sym.inspect },
          t { "\xFF".force_encoding("BINARY").to_sym.inspect },
          t { "\xFF".force_encoding("UTF-32LE").to_sym.inspect },
          t { "\xFF".force_encoding("UTF-16").to_sym.inspect },
          t { "\xFF".force_encoding("UTF-7").to_sym.inspect },
          t { "a".force_encoding("UTF-7").to_sym.encoding.to_s },
          t { "\xFF".force_encoding("CESU-8").to_sym.inspect },
          t { u.to_sym.to_s.valid_encoding? },
          t { u.to_sym.hash == u.to_sym.hash },
          t { %s{a}.encoding.to_s },
          t { define_singleton_method(u) { 1 }; [singleton_methods.last.encoding.to_s, send(u)] },
          t { "#{u.to_sym}".encoding.to_s },
          t { u.to_sym.to_s + "é" }
        ]
        "##,
    );
}
