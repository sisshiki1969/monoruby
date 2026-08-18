extern crate monoruby;
use monoruby::tests::*;

// `String#<<` with a Fixnum-byte or String argument has a JIT inline fast
// path (`emit_string_shl`, hint `Fixnum`/`Str`/`Both` per `string_shl_gen`'s
// argument-class proof): a byte or a heap String piece of a compatible
// payload-free encoding is stored straight into spare capacity, a shared
// receiver is detached in place via `runtime::str_detach` and retried, and
// everything else (encoding mismatch, growth, frozen, out-of-range,
// non-String) falls back to the native builtin inside the emitted code.
//
// Shape note: the top-level main script is *never* JIT-compiled — only
// methods and blocks are — so every hot append loop below lives in a `def`
// driven well past the method-compile threshold (20 calls; `run_test` also
// re-runs each snippet 25x). A bare top-level `while` loop would silently
// test only the VM/builtin path. Every result — bytes, encoding, and the
// *cached* code range as observed through `valid_encoding?` /
// `ascii_only?` — is compared against the CRuby oracle.

#[test]
fn string_shl_str_utf8_ascii_only() {
    // Same-encoding UTF-8 appends of pure-ASCII pieces: the fast path
    // keeps the SevenBit code-range cache.
    run_test(
        r##"
        def app_ascii(s, n)
          i = 0
          while i < n
            s << "abcde"
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = app_ascii(String.new(encoding: Encoding::UTF_8), 40)
        end
        [r.bytesize, r.encoding.name, r.ascii_only?, r.valid_encoding?, r[0, 10], r[-5, 5]]
        "##,
    );
}

#[test]
fn string_shl_str_utf8_multibyte() {
    // Multibyte pieces (the str_concat benchmark's TEST_STR shape): the
    // code-range fold must answer Valid, never SevenBit.
    run_test(
        r##"
        def app_mb(s, n)
          i = 0
          while i < n
            s << "sssssséé"
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = app_mb(String.new(encoding: Encoding::UTF_8), 40)
        end
        [r.bytesize, r.length, r.encoding.name, r.ascii_only?, r.valid_encoding?, r[-3, 3]]
        "##,
    );
}

#[test]
fn string_shl_str_seven_bit_then_multibyte() {
    // A SevenBit receiver degrades exactly when the multibyte piece
    // arrives, and `length` (chars) must see it.
    run_test(
        r##"
        def app_piece(s, piece, n)
          i = 0
          while i < n
            s << piece
            i += 1
          end
          s
        end
        r = nil
        25.times do
          s = String.new(encoding: Encoding::UTF_8)
          app_piece(s, "abc", 20)
          pre = [s.ascii_only?, s.valid_encoding?]
          app_piece(s, "é", 20)
          r = [pre, s.ascii_only?, s.valid_encoding?, s.bytesize, s.length]
        end
        r
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
        def app_ok(s, n)
          i = 0
          while i < n
            s << "ok"
            i += 1
          end
          s
        end
        broken = "\xff".dup.force_encoding(Encoding::UTF_8)
        r = nil
        25.times do
          s = String.new(encoding: Encoding::UTF_8)
          app_ok(s, 15)
          s << broken
          app_ok(s, 15)
          r = [s.valid_encoding?, s.ascii_only?, s.bytesize]
        end
        r
        "##,
    );
}

#[test]
fn string_shl_str_binary_receiver() {
    // ASCII-8BIT << ASCII-8BIT with high bytes: same-encoding fast path in
    // a non-UTF-8 encoding.
    run_test(
        r##"
        def app_bin(s, piece, n)
          i = 0
          while i < n
            s << piece
            i += 1
          end
          s
        end
        piece = "\x80\xfeok".dup.force_encoding(Encoding::BINARY)
        r = nil
        25.times do
          r = app_bin(String.new(encoding: Encoding::BINARY), piece, 40)
        end
        [r.bytesize, r.encoding.name, r.valid_encoding?, r.ascii_only?, r.getbyte(0), r.getbyte(-1)]
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
        def mism()
          s = String.new(encoding: Encoding::BINARY)
          s << "sé"           # empty binary + non-ASCII UTF-8 -> upgrades to UTF-8
          s << "sé"           # now same-encoding
          t = "abc".dup       # UTF-8
          t << "x".b          # 7-bit binary piece -> stays UTF-8
          [[s.encoding.name, s.bytesize, s.valid_encoding?],
           [t.encoding.name, t, t.ascii_only?]]
        end
        res = []
        30.times { res.concat(mism()) }
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
        def shared_app(lit)
          a = lit.dup
          b = lit.dup
          a << "XY"
          [a.bytesize, b == lit, b.bytesize, lit.bytesize, a[-2, 2], b[-2, 2]]
        end
        lit = "0123456789012345678901234567890123456789"  # > STRING_INLINE_CAP, spilled
        res = []
        30.times { res << shared_app(lit) }
        res.uniq << lit
        "##,
    );
}

#[test]
fn string_shl_str_shared_argument() {
    // The *argument* may stay shared: reads go through its ptr/len overlay.
    run_test(
        r##"
        def app_piece2(s, piece, n)
          i = 0
          while i < n
            s << piece
            i += 1
          end
          s
        end
        lit = "abcdefghijabcdefghijabcdefghijabcdefghij"
        piece = lit.dup
        r = nil
        25.times do
          r = app_piece2(String.new(encoding: Encoding::UTF_8), piece, 30)
        end
        [r.bytesize, r[0, 10], piece == lit]
        "##,
    );
}

#[test]
fn string_shl_str_self_append() {
    // `s << s`: source [0, len) and destination [len, 2*len) never overlap,
    // and the length is snapshotted before the copy.
    run_test(
        r##"
        def dbl(s, n)
          i = 0
          while i < n
            s << s
            i += 1
          end
          s
        end
        res = []
        25.times do
          s = dbl("ab".dup, 5)
          res << [s.bytesize, s == "ab" * 32]
        end
        res.uniq
        "##,
    );
}

#[test]
fn string_shl_str_frozen_receiver() {
    // A frozen receiver must raise FrozenError out of the fallback (the
    // inline path tests the header bit first) — including from the
    // JIT-compiled call site, exercised via the warmed-up method below.
    run_test(
        r##"
        def fapp(s)
          s << "x"
        end
        r = []
        25.times { fapp("warm".dup) }
        begin
          fapp("frozen".freeze)
        rescue FrozenError => e
          r << e.class.to_s
        end
        r
        "##,
    );
    run_test_error(
        r##"
        s = "frozen".freeze
        s << "x"
        "##,
    );
}

#[test]
fn string_shl_str_capacity_growth() {
    // Repeated growth: the inline capacity check must hand every
    // reallocation to the builtin and resume inline afterwards. 60
    // 16-byte pieces cross the inline->spill boundary and several
    // doublings on every call.
    run_test(
        r##"
        def grow(s, n)
          i = 0
          while i < n
            s << "0123456789abcdef"
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = grow(String.new(encoding: Encoding::UTF_8), 60)
        end
        [r.bytesize, r[0, 16] == r[-16, 16], r.ascii_only?]
        "##,
    );
}

#[test]
fn string_shl_fixnum_byte_ascii() {
    // Fixnum-hint call site (`s << <ascii byte>`): the byte store must keep
    // the SevenBit cache, work in the inline buffer, across the
    // inline->spill boundary and in the spilled buffer.
    run_test(
        r##"
        def bapp(s, n)
          i = 0
          while i < n
            s << 65
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = bapp(String.new(encoding: Encoding::UTF_8), 60)
        end
        [r.bytesize, r.ascii_only?, r.valid_encoding?, r[0, 3], r[-1, 1]]
        "##,
    );
}

#[test]
fn string_shl_fixnum_byte_high_binary() {
    // A high byte (0x80..=0xff) appends raw only into ASCII-8BIT, and must
    // degrade the code-range cache to Unknown (ascii_only? -> false).
    run_test(
        r##"
        def happ(s, n)
          i = 0
          while i < n
            s << 0xfe
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = happ(String.new(encoding: Encoding::BINARY), 40)
        end
        [r.bytesize, r.getbyte(0), r.encoding.name, r.ascii_only?, r.valid_encoding?]
        "##,
    );
}

#[test]
fn string_shl_fixnum_codepoint_fallback() {
    // Out-of-byte-range Integers (codepoints) and high bytes into UTF-8 are
    // outside the inline gate: the builtin must encode them (and a negative
    // codepoint must raise).
    run_test(
        r##"
        def cpapp(s, n)
          i = 0
          while i < n
            s << 233          # é: multi-byte encode via the fallback
            s << 0x1F600      # 😀: 4-byte encode
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = cpapp(String.new(encoding: Encoding::UTF_8), 10)
        end
        [r.bytesize, r.length, r.valid_encoding?, r.ascii_only?, r[0, 2]]
        "##,
    );
    run_test_error("''.dup << -1");
}

#[test]
fn string_shl_str_mixed_fixnum_and_string() {
    // One call site fed both Fixnum bytes and Strings: the `Both` hint's
    // runtime tag dispatch must pick the right path per iteration.
    run_test(
        r##"
        def mixapp(s, n)
          i = 0
          while i < n
            x = i.even? ? 65 : "bc"
            s << x
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = mixapp(String.new(encoding: Encoding::UTF_8), 100)
        end
        [r.bytesize, r[0, 6], r.ascii_only?, r.valid_encoding?]
        "##,
    );
}

#[test]
fn string_shl_str_subclass_and_coercion() {
    // A String-subclass receiver resolves to the same builtin; a non-String
    // argument with #to_str coerces via the fallback (its class proof is
    // neither Integer nor String, so this is a `Both`-hint site).
    run_test(
        r##"
        class MyStr < String; end
        class Coerced
          def to_str = "[c]"
        end
        def capp(s, c, n)
          i = 0
          while i < n
            s << "ab"
            s << c
            i += 1
          end
          s
        end
        r = nil
        25.times do
          r = capp(MyStr.new, Coerced.new, 15)
        end
        [r.class.name, r.bytesize, r[0, 5], r.encoding.name]
        "##,
    );
}

#[test]
fn string_shl_gen_declined_sites() {
    // Call shapes `string_shl_gen` must decline: a splat argument (not a
    // simple 1-positional call) and a receiver whose class the call site
    // cannot prove (String/Array polymorphic dispatch). Both still answer
    // through the ordinary builtin.
    run_test(
        r##"
        def sapp(s, a)
          s.<<(*a)
        end
        def papp(o)
          o << "x"
        end
        r = []
        25.times do
          r << sapp("q".dup, ["y"]).to_s
          r << papp("s".dup).to_s
          r << papp([1]).size
        end
        r.uniq
        "##,
    );
}

#[test]
fn string_shl_str_return_value_identity() {
    // `<<` answers the receiver itself, so chained appends mutate one
    // object.
    run_test(
        r##"
        def idapp(s, n)
          i = 0
          while i < n
            t = (s << "y")
            raise "not self" unless t.equal?(s)
            i += 1
          end
          s
        end
        r = nil
        25.times { r = idapp("x".dup, 20) }
        [r.bytesize, r[0, 3]]
        "##,
    );
}

#[test]
fn string_shl_str_empty_piece() {
    // Empty argument: zero bytes copied, length unchanged, cache intact.
    run_test(
        r##"
        def eapp(s, e, n)
          i = 0
          while i < n
            s << e
            i += 1
          end
          s
        end
        e = String.new(encoding: Encoding::UTF_8)
        r = nil
        25.times { r = eapp("seed".dup, e, 20) }
        [r, r.bytesize, r.ascii_only?, r.valid_encoding?]
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
        def erubi(buf, n)
          i = 0
          while i < n
            buf << "<td>"
            buf << i.to_s
            buf << "</td>\n"
            i += 1
          end
          buf
        end
        r = nil
        25.times { r = erubi(String.new, 40) }
        [r.bytesize, r.encoding.name, r.ascii_only?, r.valid_encoding?, r[0, 12]]
        "##,
    );
    run_test(
        r##"
        def app_ok2(buf, n)
          i = 0
          while i < n
            buf << "ok"        # 7-bit piece into the (now Valid UTF-8) receiver
            i += 1
          end
          buf
        end
        r = nil
        25.times do
          buf = "x".b          # ASCII-8BIT, SevenBit
          buf << "é"           # non-7-bit UTF-8 piece: negotiation -> UTF-8
          pre = buf.encoding.name
          app_ok2(buf, 20)
          r = [pre, buf.encoding.name, buf.bytesize, buf.valid_encoding?, buf.ascii_only?]
        end
        r
        "##,
    );
    // US-ASCII receiver + 7-bit UTF-8 pieces keeps US-ASCII.
    run_test(
        r##"
        def app_us(buf, n)
          i = 0
          while i < n
            buf << "ab"
            i += 1
          end
          buf
        end
        r = nil
        25.times { r = app_us("seed".encode("US-ASCII"), 20) }
        [r.encoding.name, r.bytesize, r.ascii_only?]
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
        def iso_app(s, t, n)
          i = 0
          while i < n
            s << t
            i += 1
          end
          s
        end
        t = "def".dup.force_encoding("ISO-8859-1")
        r = nil
        25.times do
          r = iso_app("abc".dup.force_encoding("ISO-8859-1"), t, 15)
        end
        [r.bytesize, r.encoding.name, r.valid_encoding?]
        "##,
    );
}

#[test]
fn force_encoding_coercible_name() {
    // `String#force_encoding` / `String.new(encoding:)` accept anything
    // `#to_str`-coercible as the encoding name (`value_to_encoding`'s
    // coercion arm).
    run_test(
        r##"
        class EncName
          def to_str = "US-ASCII"
        end
        [
          "abc".dup.force_encoding(EncName.new).encoding.name,
          String.new(encoding: EncName.new).encoding.name,
        ]
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
