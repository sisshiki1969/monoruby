extern crate monoruby;
use monoruby::tests::*;

// `String#<<` with a String argument has a JIT inline fast path
// (`emit_string_shl` with a `Str`/`Both` hint): a heap String argument of
// the receiver's exact payload-free encoding is byte-copied into spare
// capacity, a shared receiver is detached in place via
// `runtime::str_detach` and retried, and everything else (encoding
// mismatch, growth, frozen, non-String) falls back to the native builtin.
// `run_test` runs each snippet 25x, so the JITted body (test-mode
// thresholds: >=5 calls / >=15 loop iters) is what executes the hot
// appends; every result — bytes, encoding, and the *cached* code range as
// observed through `valid_encoding?` / `ascii_only?` — is compared against
// the CRuby oracle.

#[test]
fn string_shl_str_utf8_ascii_only() {
    // Same-encoding UTF-8 appends of pure-ASCII pieces: the fast path
    // keeps the SevenBit code-range cache.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 200
          s << "abcde"
          i += 1
        end
        [s.bytesize, s.encoding.name, s.ascii_only?, s.valid_encoding?, s[0, 10], s[-5, 5]]
        "##,
    );
}

#[test]
fn string_shl_str_utf8_multibyte() {
    // Multibyte pieces (the str_concat benchmark's TEST_STR shape): the
    // code-range fold must answer Valid, never SevenBit.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 200
          s << "sssssséé"
          i += 1
        end
        [s.bytesize, s.length, s.encoding.name, s.ascii_only?, s.valid_encoding?, s[-3, 3]]
        "##,
    );
}

#[test]
fn string_shl_str_seven_bit_then_multibyte() {
    // A SevenBit receiver degrades exactly when the multibyte piece
    // arrives, and `length` (chars) must see it.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 60
          s << "abc"
          i += 1
        end
        pre = [s.ascii_only?, s.valid_encoding?]
        i = 0
        while i < 60
          s << "é"
          i += 1
        end
        [pre, s.ascii_only?, s.valid_encoding?, s.bytesize, s.length]
        "##,
    );
}

#[test]
fn string_shl_str_broken_piece() {
    // A Broken piece must fold the receiver's cache to (at best) Unknown —
    // `valid_encoding?` re-classifies and must answer false, and appending
    // more valid pieces afterwards must not resurrect validity.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        broken = "\xff".dup.force_encoding(Encoding::UTF_8)
        i = 0
        while i < 30
          s << "ok"
          i += 1
        end
        s << broken
        i = 0
        while i < 30
          s << "ok"
          i += 1
        end
        [s.valid_encoding?, s.ascii_only?, s.bytesize]
        "##,
    );
}

#[test]
fn string_shl_str_binary_receiver() {
    // ASCII-8BIT << ASCII-8BIT with high bytes: same-encoding fast path in
    // a non-UTF-8 encoding.
    run_test(
        r##"
        s = String.new(encoding: Encoding::BINARY)
        piece = "\x80\xfeok".dup.force_encoding(Encoding::BINARY)
        i = 0
        while i < 100
          s << piece
          i += 1
        end
        [s.bytesize, s.encoding.name, s.valid_encoding?, s.ascii_only?, s.getbyte(0), s.getbyte(-1)]
        "##,
    );
}

#[test]
fn string_shl_str_encoding_mismatch() {
    // UTF-8 receiver << ASCII-8BIT piece and the reverse: the inline gate
    // (different encoding tags) must route to the builtin's negotiation —
    // including the empty-7-bit-receiver upgrade and the incompatibility
    // error.
    run_test(
        r##"
        res = []
        i = 0
        while i < 30
          s = String.new(encoding: Encoding::BINARY)
          s << "sé"           # empty binary + non-ASCII UTF-8 -> upgrades to UTF-8
          s << "sé"           # now same-encoding
          res << [s.encoding.name, s.bytesize, s.valid_encoding?]
          t = "abc".dup       # UTF-8
          t << "x".b          # 7-bit binary piece -> stays UTF-8
          res << [t.encoding.name, t, t.ascii_only?]
          i += 1
        end
        res.uniq
        "##,
    );
    run_test_error(
        r##"
        s = "文字".dup
        s << "\x90\x80".b    # non-ASCII binary into non-7-bit UTF-8: incompatible
        "##,
    );
}

#[test]
fn string_shl_str_shared_receiver() {
    // A `dup` of a spilled literal shares the literal's buffer
    // (copy-on-write). The inline path must detach the receiver in place
    // (str_detach) and append into the private copy — the literal and
    // every other sharer must keep their bytes.
    run_test(
        r##"
        lit = "0123456789012345678901234567890123456789"  # > STRING_INLINE_CAP, spilled
        res = []
        i = 0
        while i < 40
          a = lit.dup
          b = lit.dup
          a << "XY"
          res << [a.bytesize, b == lit, b.bytesize, lit.bytesize, a[-2, 2], b[-2, 2]]
          i += 1
        end
        res.uniq << lit
        "##,
    );
}

#[test]
fn string_shl_str_shared_argument() {
    // The *argument* may stay shared: reads go through its ptr/len overlay.
    run_test(
        r##"
        lit = "abcdefghijabcdefghijabcdefghijabcdefghij"
        piece = lit.dup
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 50
          s << piece
          i += 1
        end
        [s.bytesize, s[0, 10], piece == lit]
        "##,
    );
}

#[test]
fn string_shl_str_self_append() {
    // `s << s`: source [0, len) and destination [len, 2*len) never overlap,
    // and the length is snapshotted before the copy.
    run_test(
        r##"
        res = []
        i = 0
        while i < 20
          s = "ab".dup
          5.times { s << s }
          res << [s.bytesize, s == "ab" * 32]
          i += 1
        end
        res.uniq
        "##,
    );
}

#[test]
fn string_shl_str_frozen_receiver() {
    // A frozen receiver must raise FrozenError out of the fallback (the
    // inline path tests the header bit first).
    run_test_error(
        r##"
        s = "frozen".freeze
        s << "x"
        "##,
    );
    run_test_error(
        r##"
        i = 0
        s = "warm".dup
        while i < 30
          s << "x"
          i += 1
        end
        s.freeze
        s << "boom"
        "##,
    );
}

#[test]
fn string_shl_str_capacity_growth() {
    // Repeated growth: the inline capacity check must hand every
    // reallocation to the builtin and resume inline afterwards. 8 KiB of
    // 16-byte pieces crosses the inline->spill boundary and many
    // doublings.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 512
          s << "0123456789abcdef"
          i += 1
        end
        [s.bytesize, s[0, 16] == s[-16, 16], s.ascii_only?]
        "##,
    );
}

#[test]
fn string_shl_str_mixed_fixnum_and_string() {
    // One call site fed both Fixnum bytes and Strings: the `Both` hint's
    // runtime tag dispatch must pick the right path per iteration.
    run_test(
        r##"
        s = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 100
          x = i.even? ? 65 : "bc"
          s << x
          i += 1
        end
        [s.bytesize, s[0, 6], s.ascii_only?, s.valid_encoding?]
        "##,
    );
}

#[test]
fn string_shl_str_subclass_and_coercion() {
    // A String-subclass receiver resolves to the same builtin; a non-String
    // argument with #to_str coerces via the fallback.
    run_test(
        r##"
        class MyStr < String; end
        class Coerced
          def to_str = "[c]"
        end
        s = MyStr.new
        c = Coerced.new
        i = 0
        while i < 30
          s << "ab"
          s << c
          i += 1
        end
        [s.class.name, s.bytesize, s[0, 5], s.encoding.name]
        "##,
    );
}

#[test]
fn string_shl_str_return_value_identity() {
    // `<<` answers the receiver itself, so chained appends mutate one
    // object.
    run_test(
        r##"
        s = "x".dup
        i = 0
        while i < 30
          t = (s << "y")
          raise "not self" unless t.equal?(s)
          i += 1
        end
        [s.bytesize, s[0, 3]]
        "##,
    );
}

#[test]
fn string_shl_str_empty_piece() {
    // Empty argument: zero bytes copied, length unchanged, cache intact.
    run_test(
        r##"
        s = "seed".dup
        e = String.new(encoding: Encoding::UTF_8)
        i = 0
        while i < 30
          s << e
          i += 1
        end
        [s, s.bytesize, s.ascii_only?, s.valid_encoding?]
        "##,
    );
}

#[test]
fn string_shl_str_ascii_compatible_mixed_encodings() {
    // The erubi buffer shape: a binary `String.new` receiver fed 7-bit
    // UTF-8 pieces. `Encoding.compatible?` keeps the receiver's encoding
    // (both sides 7-bit -> first wins; receiver Valid + 7-bit piece ->
    // non-7-bit side wins), so the relaxed inline gate may byte-copy —
    // and must never take that shortcut once a non-ASCII piece flipped
    // the receiver.
    run_test(
        r##"
        buf = String.new
        i = 0
        while i < 200
          buf << "<td>"
          buf << i.to_s
          buf << "</td>\n"
          i += 1
        end
        [buf.bytesize, buf.encoding.name, buf.ascii_only?, buf.valid_encoding?, buf[0, 12]]
        "##,
    );
    run_test(
        r##"
        buf = "x".b          # ASCII-8BIT, SevenBit
        buf << "é"           # non-7-bit UTF-8 piece: negotiation -> UTF-8
        pre = buf.encoding.name
        i = 0
        while i < 60
          buf << "ok"        # 7-bit piece into the (now Valid UTF-8) receiver
          i += 1
        end
        [pre, buf.encoding.name, buf.bytesize, buf.valid_encoding?, buf.ascii_only?]
        "##,
    );
    // US-ASCII receiver + 7-bit UTF-8 pieces keeps US-ASCII.
    run_test(
        r##"
        buf = "seed".encode("US-ASCII")
        i = 0
        while i < 60
          buf << "ab"
          i += 1
        end
        [buf.encoding.name, buf.bytesize, buf.ascii_only?]
        "##,
    );
}

#[test]
fn string_shl_str_exotic_encoding_fallback() {
    // Payload-carrying encodings (ISO-8859-n) are outside the inline gate:
    // the discriminant byte alone cannot distinguish ISO-8859-1 from
    // ISO-8859-15, so these must take the builtin (and still work).
    run_test(
        r##"
        s = "abc".dup.force_encoding("ISO-8859-1")
        t = "def".dup.force_encoding("ISO-8859-1")
        i = 0
        while i < 30
          s << t
          i += 1
        end
        [s.bytesize, s.encoding.name, s.valid_encoding?]
        "##,
    );
}
#[test]
fn setbyte_on_shared_string_detaches() {
    // The write must land in a detached buffer: the dup source, a second
    // sharer, and the frozen literal all keep their original bytes.
    run_test(
        r#"
        src = "hello world"
        out = []
        20.times do |k|
          a = src.dup
          b = src.dup
          a.setbyte(0, 72 + (k & 1))
          out << [a.getbyte(0), b.getbyte(0), src.getbyte(0)]
        end
        out.uniq
        "#,
    );
}

#[test]
fn setbyte_xor_loop_shape() {
    // The ruby-bench ruby-xor kernel: every call dups a frozen literal and
    // rewrites it byte-by-byte through the inline fast path.
    run_test(
        r#"
        def bxor!(a, b)
          l = a.bytesize
          lb = b.bytesize
          l = lb if lb < l
          i = 0
          while i < l
            a.setbyte(i, a.getbyte(i) ^ b.getbyte(i))
            i = i.succ
          end
          a
        end
        s = "this is a long string with no useful contents".freeze
        t = "this is also a long string with no useful cont".freeze
        r = nil
        30.times { r = bxor!(s.dup, t) }
        r.bytes
        "#,
    );
}

#[test]
fn setbyte_edge_cases_still_deopt_correctly() {
    // Frozen receivers, negative indices, out-of-range indices and the
    // code-range downgrade all still behave like CRuby after the
    // detach-and-retry rewrite (errors go through the deopt path).
    run_test(
        r#"
        r = []
        20.times do
          s = "abcdef".dup
          s.setbyte(-2, 90)
          r << s
          begin; "xyz".freeze.setbyte(0, 65); rescue => e; r << e.class; end
          begin; s2 = "abc".dup; s2.setbyte(5, 65); rescue => e; r << e.class << e.message; end
          begin; s3 = "abc".dup; s3.setbyte(-4, 65); rescue => e; r << e.class; end
          u = "plain".dup
          u.setbyte(0, 0xE3)
          r << u.valid_encoding? << u.bytesize
        end
        r.uniq
        "#,
    );
}
