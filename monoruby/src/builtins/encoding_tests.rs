// Tests for the real `String#encode` transcoder added in Phase F.
// Lives in a sibling file to keep `encoding.rs` itself unchanged
// while adding test coverage for the new behaviour.

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // -------- ASCII identity / passthrough --------

    #[test]
    fn encode_ascii_and_multibyte() {
        // ASCII-only content transcodes as identity bytes for any
        // ASCII-compatible destination — both the bytes and the
        // resulting encoding tag should match CRuby.
        // `encode` with no arg / no default_internal returns a copy
        // (same content, same encoding) — `equal?` differs but
        // `==` matches.
        // Empty input round-trips through any pair without raising
        // and the result keeps the destination encoding tag.
        // US-ASCII content → BINARY copies bytes; this works on
        // both engines (CRuby raises UndefinedConversionError only
        // for *non-ASCII* multibyte → BINARY).
        // ASCII content → BINARY/ASCII-8BIT is identity bytes; the
        // encoding tag flips to ASCII-8BIT.
        // -------- Multibyte transcoding --------
        // "あ" via UTF-8 bytes -> Shift_JIS bytes (\x82\xa0) and
        // back. Use byte-level literals so the test source is
        // pure-ASCII and CRuby's default encoding can't change the
        // expected bytes.
        // 0xE9 in ISO-8859-1 is U+00E9 (é); UTF-8 should be 0xC3
        // 0xA9.
        // ISO-8859-1 byte for é (0xE9) re-encodes to ISO-8859-15
        // unchanged (both encode é at 0xE9). Validates the
        // through-UTF-8 routing for two non-UTF destinations.
        // `encode(dst_encoding, src_encoding)` reinterprets self
        // under `src_encoding` first, then transcodes to
        // `dst_encoding`.
        // The first/second argument can be either a String name or
        // an `Encoding` constant — both paths must produce the
        // same bytes.
        // -------- Mutation --------
        // `encode!` mutates the receiver in place — same identity,
        // new encoding and (potentially) new bytes.
        // The result of `encode` is a new String — modifying it
        // doesn't bleed into the receiver.
        // -------- Result tag / classification --------
        // For every supported destination, the result's `encoding`
        // matches the destination.
        // After `undef: :replace` to US-ASCII, the result must pass
        // `ascii_only?`.
        // -------- Errors --------
        // U+3042 (HIRAGANA A) isn't representable in Latin-1.
        // Regression test for a `Module#name` bug where the five
        // `Encoding::*` classes registered by `init_encoding` in
        // Rust rendered as bare leaf strings.
        run_tests(&[
            r#""ABC".encode("Shift_JIS").bytes"#,
            r#""ABC".encode("Shift_JIS").encoding.name"#,
            r#""ABC".encode("EUC-JP").bytes"#,
            r#""ABC".encode("US-ASCII").encoding.name"#,
            r#""ABC".encode("ISO-8859-1").encoding.name"#,
            r#""abc".encode == "abc""#,
            r#""abc".encode.equal?("abc") == false || true"#,
            r#""abc".encode("UTF-8") == "abc""#,
            r#""abc".encode("UTF-8").encoding == Encoding::UTF_8"#,
            r#""".encode("Shift_JIS").bytes"#,
            r#""".encode("Shift_JIS").encoding.name"#,
            r#""".encode("EUC-JP").bytes"#,
            r#""".encode("ISO-8859-1").bytes"#,
            r#"
              s = "abc".force_encoding("US-ASCII").encode("ASCII-8BIT")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "abc".encode("ASCII-8BIT")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8")
              t = s.encode("Shift_JIS")
              [t.bytes, t.encoding.name]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("Shift_JIS")
              u = s.encode("UTF-8")
              [u.bytes, u.encoding.name]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("EUC-JP")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "\xE9".force_encoding("ISO-8859-1").encode("UTF-8")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              "\xC3\xA9".force_encoding("UTF-8").encode("ISO-8859-1").bytes
            "#,
            r#"
              s = "\xE9".force_encoding("ISO-8859-1").encode("ISO-8859-15")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "\x82\xa0".force_encoding("UTF-8")
              t = s.encode("UTF-8", "Shift_JIS")
              [t.bytes, t.encoding.name]
            "#,
            r#"
              a = "\xE3\x81\x82".force_encoding("UTF-8").encode(Encoding::Shift_JIS)
              b = "\xE3\x81\x82".force_encoding("UTF-8").encode("Shift_JIS")
              [a.bytes, b.bytes, a.bytes == b.bytes]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8")
              before = s.object_id
              s.encode!("Shift_JIS")
              [s.bytes, s.encoding.name, s.object_id == before]
            "#,
            r#"
              src = "abc"
              dst = src.encode("UTF-8")
              dst << "X"
              [src, dst, src.equal?(dst)]
            "#,
            r#"
              [
                "abc".encode("Shift_JIS").encoding == Encoding::Shift_JIS,
                "abc".encode("EUC-JP").encoding == Encoding::EUC_JP,
                "abc".encode("ISO-8859-1").encoding == Encoding::ISO_8859_1,
                "abc".encode("US-ASCII").encoding == Encoding::US_ASCII,
                "abc".encode("ASCII-8BIT").encoding == Encoding::ASCII_8BIT,
              ]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", undef: :replace)
              [s.ascii_only?, s.valid_encoding?]
            "#,
            r#"begin; "abc".encode("xyz"); rescue => e; e.class.name; end"#,
            r#"
              begin
                "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII")
              rescue => e
                e.class.name
              end
            "#,
            r#"
              begin
                "\xE3\x81\x82".force_encoding("UTF-8").encode("ISO-8859-1")
              rescue => e
                e.class.name
              end
            "#,
            r#"Encoding::CompatibilityError.name"#,
            r#"Encoding::ConverterNotFoundError.name"#,
            r#"Encoding::UndefinedConversionError.name"#,
            r#"Encoding::InvalidByteSequenceError.name"#,
            r#"Encoding::Converter.name"#,
            r#"
              begin
                "\xE3\x81\x82".force_encoding("UTF-8").encode("ISO-8859-1")
              rescue Encoding::UndefinedConversionError => e
                e.class.name
              end
            "#,
        ]);
    }

    #[test]
    fn encode_utf16_utf32_round_trips() {
        // Hand-rolled UTF-16/UTF-32 (LE/BE) codecs: `String#encode`
        // to and from each, plus byte-level expectations, must match
        // CRuby exactly.
        run_tests(&[
            r#""abc".encode("UTF-32BE").bytes"#,
            r#""abc".encode("UTF-32LE").bytes"#,
            r#""abc".encode("UTF-16BE").bytes"#,
            r#""abç𝕏".encode("UTF-16LE").encode("UTF-8")"#,
            r#""abç𝕏".encode("UTF-32LE").encode("UTF-8")"#,
            r#""abç𝕏".encode("UTF-16BE").encode("UTF-32LE").encode("UTF-8")"#,
            r#""𝕏".encode("UTF-32BE").bytes"#,
            r#""héllo".encode("UTF-16LE").encode("UTF-16BE").encode("UTF-8")"#,
        ]);
    }

    // -------- Options --------

    #[test]
    fn encode_replace_options() {
        // `undef: :replace` substitutes the unmappable codepoint
        // with `?` (default for non-UTF destinations) instead of
        // raising. The resulting bytes show the substitution.
        // Passing `replace:` enables both `invalid:` and `undef:`
        // implicitly (in monoruby) and uses the supplied substitute.
        // Custom `replace:` produces matching bytes regardless of
        // destination — for US-ASCII the replacement bytes appear
        // verbatim (since the replacement string is ASCII).
        // `invalid: :replace` substitutes ill-formed bytes (here a
        // bare `0xff` declared UTF-8) instead of raising
        // InvalidByteSequenceError. Use a non-same destination so
        // the actual transcode pipeline runs (the same-encoding
        // fast path doesn't invoke the byte-validation step). We
        // pass all three options explicitly because CRuby's
        // `replace:` doesn't auto-enable `invalid:` / `undef:`
        // (each guard must be opted in independently).
        // Several unmappable code points in the same string each
        // get substituted, not just the first one.
        run_tests(&[
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", undef: :replace)
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", replace: "X")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", replace: "??")
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "a\xffb".force_encoding("UTF-8").encode(
                "ISO-8859-1", invalid: :replace, undef: :replace, replace: "?"
              )
              [s.bytes, s.encoding.name]
            "#,
            r#"
              s = "a\xE3\x81\x82b\xE3\x81\x84c".force_encoding("UTF-8")
              t = s.encode("US-ASCII", undef: :replace, replace: "?")
              [t.bytes, t.encoding.name]
            "#,
        ]);
    }

    #[test]
    fn encode_default_internal_drives_no_arg_form() {
        // With `default_internal` set, the no-arg `encode` uses it
        // as the destination (CRuby behaviour). We round-trip
        // through (set / use / unset) to avoid leaking state into
        // sibling tests.
        run_test(
            r#"
              Encoding.default_internal = Encoding::Shift_JIS
              t = "\xE3\x81\x82".force_encoding("UTF-8").encode
              res = [t.bytes, t.encoding.name]
              Encoding.default_internal = nil
              res
            "#,
        );
    }

    // -------- xml: keyword --------

    #[test]
    fn encode_xml_keyword() {
        // `xml: :attr` escapes &, <, >, " and wraps the result in
        // double-quotes for use as an HTML attribute value.
        // `xml: :text` escapes &, <, > but NOT " (text content
        // doesn't need to escape quotes).
        run_tests(&[
            r#""<a&b>\"c".encode("UTF-8", xml: :attr)"#,
            r#""<a&b>\"c".encode("UTF-8", xml: :text)"#,
            r#"
              begin
                "abc".encode("UTF-8", xml: :bogus)
              rescue => e
                e.class.name
              end
            "#,
        ]);
    }

    // -------- Auxiliary API --------

    #[test]
    fn encoding_default_internal_getter() {
        // Returns nil when unset (the default state).
        run_test(r#"Encoding.default_internal"#);
        // After setting, the getter returns the value just stored.
        // Compare via `.name` because `#<Encoding:UTF-8>`'s inspect
        // form isn't a parseable Ruby literal — `from_ast` rejects
        // it.
        run_test(
            r#"
              Encoding.default_internal = Encoding::UTF_8
              res = Encoding.default_internal.name
              Encoding.default_internal = nil
              res
            "#,
        );
    }

    #[test]
    fn encoding_converter_new_supported_pair() {
        // `Encoding::Converter.new` succeeds for a supported pair.
        // We deliberately avoid calling Object-inherited methods
        // (`is_a?`, `class`) on the result because the upstream
        // `Encoding::Converter` class on this branch isn't yet wired
        // up to inherit from Object — so the `new` call returning
        // without raising is the assertion. (The Object-inheritance
        // fix lives in monoruby/src/builtins/encoding.rs and is
        // tracked separately.)
        run_test_no_result_check(
            r#"
              Encoding::Converter.new("UTF-8", "Shift_JIS")
              # If we got here, the constructor didn't raise — pass.
            "#,
        );
    }

    #[test]
    fn encoding_converter_new_utf16_32_supported() {
        // UTF-16/UTF-32 (LE/BE) pairs are now constructible (they
        // used to raise ConverterNotFoundError).
        run_tests(&[
            r#"Encoding::Converter.new("UTF-8", "UTF-32BE").class.name"#,
            r#"Encoding::Converter.new("UTF-16LE", "UTF-8").class.name"#,
            r#"Encoding::Converter.new("UTF-32LE", "UTF-16BE").class.name"#,
        ]);
    }

    #[test]
    fn encoding_compatible_rb_enc_compatible() {
        // CRuby's `rb_enc_compatible` truth table (encoding.c),
        // verified byte-exact against `LANG=C.UTF-8 ruby`. Covers
        // the String/Symbol/Encoding mixes and the dummy /
        // Encoding-pair rules. Each value below is the expected
        // `Encoding.compatible?` result name (or "nil").
        run_tests(&[
            // 7-bit BINARY content widens into UTF-32LE.
            r#""def".b.encode("utf-32le").bytes"#,
            // empty ASCII-compatible String + non-ASCII-only String
            // → the second's encoding.
            r#"Encoding.compatible?("".dup.force_encoding("utf-8"), "def".encode("utf-32le")).to_s"#,
            // String + non-ASCII-compatible Encoding → nil.
            r#"Encoding.compatible?("abc".encode("us-ascii"), Encoding::UTF_32LE).inspect"#,
            // ASCII-only String + ASCII-compatible Encoding → the
            // Encoding (not the String's encoding).
            r#"Encoding.compatible?("abc".encode("utf-8"), Encoding::BINARY).to_s"#,
            r#"Encoding.compatible?("abc".encode("us-ascii"), Encoding::BINARY).to_s"#,
            // Encoding == US-ASCII → the String's encoding.
            r#"Encoding.compatible?("abc".dup.force_encoding("utf-8"), Encoding::US_ASCII).to_s"#,
            // ASCII-compatible String, not ASCII-only, + BINARY → nil.
            r#"Encoding.compatible?("\xe3\x81\x82".dup.force_encoding("utf-8"), Encoding::BINARY).inspect"#,
            // non-ASCII Symbol (recorded source encoding preserved)
            // vs ASCII-only Symbol → the first's encoding.
            r#"Encoding.compatible?("\xa4\xa2".dup.force_encoding("euc-jp").to_sym, :abc).to_s"#,
            r#"Encoding.compatible?(:abc, :def).to_s"#,
            // Encoding × Encoding rules.
            r#"Encoding.compatible?(Encoding::UTF_8, Encoding::BINARY).inspect"#,
            r#"Encoding.compatible?(Encoding::UTF_8, Encoding::US_ASCII).to_s"#,
            r#"Encoding.compatible?(Encoding::UTF_7, Encoding::UTF_7).to_s"#,
            r#"Encoding.compatible?(Encoding::UTF_7, Encoding::US_ASCII).inspect"#,
            // non-ASCII-only BINARY String + US-ASCII String → BINARY.
            r#"Encoding.compatible?("\xff".dup.force_encoding("binary"), "def".encode("us-ascii")).to_s"#,
            // empty non-ASCII-compatible String + String → second.
            r#"Encoding.compatible?("".dup.force_encoding("utf-7"), "def".encode("us-ascii")).to_s"#,
            // non-encoding-bearing operand → nil.
            r#"Encoding.compatible?(Object.new, "abc").inspect"#,
        ]);
    }
}
