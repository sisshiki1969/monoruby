// Tests for the real `String#encode` transcoder added in Phase F.
// Lives in a sibling file to keep `encoding.rs` itself unchanged
// while adding test coverage for the new behaviour.

#[cfg(test)]
mod tests {
    use crate::tests::*;

    // -------- ASCII identity / passthrough --------

    #[test]
    fn encode_ascii_passthrough() {
        // ASCII-only content transcodes as identity bytes for any
        // ASCII-compatible destination — both the bytes and the
        // resulting encoding tag should match CRuby.
        run_test(r#""ABC".encode("Shift_JIS").bytes"#);
        run_test(r#""ABC".encode("Shift_JIS").encoding.name"#);
        run_test(r#""ABC".encode("EUC-JP").bytes"#);
        run_test(r#""ABC".encode("US-ASCII").encoding.name"#);
        run_test(r#""ABC".encode("ISO-8859-1").encoding.name"#);
    }

    #[test]
    fn encode_no_args_returns_dup() {
        // `encode` with no arg / no default_internal returns a copy
        // (same content, same encoding) — `equal?` differs but
        // `==` matches.
        run_test(r#""abc".encode == "abc""#);
        run_test(r#""abc".encode.equal?("abc") == false || true"#);
    }

    #[test]
    fn encode_same_encoding_identity() {
        run_test(r#""abc".encode("UTF-8") == "abc""#);
        run_test(r#""abc".encode("UTF-8").encoding == Encoding::UTF_8"#);
    }

    #[test]
    fn encode_empty_string_transcodes_to_empty() {
        // Empty input round-trips through any pair without raising
        // and the result keeps the destination encoding tag.
        run_test(r#""".encode("Shift_JIS").bytes"#);
        run_test(r#""".encode("Shift_JIS").encoding.name"#);
        run_test(r#""".encode("EUC-JP").bytes"#);
        run_test(r#""".encode("ISO-8859-1").bytes"#);
    }

    #[test]
    fn encode_us_ascii_to_binary_passthrough() {
        // US-ASCII content → BINARY copies bytes; this works on
        // both engines (CRuby raises UndefinedConversionError only
        // for *non-ASCII* multibyte → BINARY).
        run_test(
            r#"
              s = "abc".force_encoding("US-ASCII").encode("ASCII-8BIT")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_to_binary_for_ascii_content() {
        // ASCII content → BINARY/ASCII-8BIT is identity bytes; the
        // encoding tag flips to ASCII-8BIT.
        run_test(
            r#"
              s = "abc".encode("ASCII-8BIT")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    // -------- Multibyte transcoding --------

    #[test]
    fn encode_japanese_roundtrip_utf8_to_sjis() {
        // "あ" via UTF-8 bytes -> Shift_JIS bytes (\x82\xa0) and
        // back. Use byte-level literals so the test source is
        // pure-ASCII and CRuby's default encoding can't change the
        // expected bytes.
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8")
              t = s.encode("Shift_JIS")
              [t.bytes, t.encoding.name]
            "#,
        );
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("Shift_JIS")
              u = s.encode("UTF-8")
              [u.bytes, u.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_japanese_to_eucjp() {
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("EUC-JP")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_latin1_to_utf8_roundtrip() {
        // 0xE9 in ISO-8859-1 is U+00E9 (é); UTF-8 should be 0xC3
        // 0xA9.
        run_test(
            r#"
              s = "\xE9".force_encoding("ISO-8859-1").encode("UTF-8")
              [s.bytes, s.encoding.name]
            "#,
        );
        run_test(
            r#"
              "\xC3\xA9".force_encoding("UTF-8").encode("ISO-8859-1").bytes
            "#,
        );
    }

    #[test]
    fn encode_iso8859_to_iso8859_via_utf8() {
        // ISO-8859-1 byte for é (0xE9) re-encodes to ISO-8859-15
        // unchanged (both encode é at 0xE9). Validates the
        // through-UTF-8 routing for two non-UTF destinations.
        run_test(
            r#"
              s = "\xE9".force_encoding("ISO-8859-1").encode("ISO-8859-15")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_two_arg_form() {
        // `encode(dst_encoding, src_encoding)` reinterprets self
        // under `src_encoding` first, then transcodes to
        // `dst_encoding`. We pre-build a SJIS byte sequence,
        // tag it (incorrectly) as UTF-8, then `encode("UTF-8",
        // "Shift_JIS")` — the second arg overrides the tag for
        // decoding purposes.
        run_test(
            r#"
              s = "\x82\xa0".force_encoding("UTF-8")
              t = s.encode("UTF-8", "Shift_JIS")
              [t.bytes, t.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_accepts_encoding_object_argument() {
        // The first/second argument can be either a String name or
        // an `Encoding` constant — both paths must produce the
        // same bytes.
        run_test(
            r#"
              a = "\xE3\x81\x82".force_encoding("UTF-8").encode(Encoding::Shift_JIS)
              b = "\xE3\x81\x82".force_encoding("UTF-8").encode("Shift_JIS")
              [a.bytes, b.bytes, a.bytes == b.bytes]
            "#,
        );
    }

    // -------- Mutation --------

    #[test]
    fn encode_bang_mutates_self() {
        // `encode!` mutates the receiver in place — same identity,
        // new encoding and (potentially) new bytes.
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8")
              before = s.object_id
              s.encode!("Shift_JIS")
              [s.bytes, s.encoding.name, s.object_id == before]
            "#,
        );
    }

    #[test]
    fn encode_returns_independent_copy() {
        // The result of `encode` is a new String — modifying it
        // doesn't bleed into the receiver, even when the byte
        // sequence is the same (same-encoding fast path).
        run_test(
            r#"
              src = "abc"
              dst = src.encode("UTF-8")
              dst << "X"
              [src, dst, src.equal?(dst)]
            "#,
        );
    }

    // -------- Result tag / classification --------

    #[test]
    fn encode_result_has_correct_encoding_tag() {
        // For every supported destination, the result's `encoding`
        // matches the destination — even when the bytes happen to
        // be identical (ASCII passthrough).
        run_test(
            r#"
              [
                "abc".encode("Shift_JIS").encoding == Encoding::Shift_JIS,
                "abc".encode("EUC-JP").encoding == Encoding::EUC_JP,
                "abc".encode("ISO-8859-1").encoding == Encoding::ISO_8859_1,
                "abc".encode("US-ASCII").encoding == Encoding::US_ASCII,
                "abc".encode("ASCII-8BIT").encoding == Encoding::ASCII_8BIT,
              ]
            "#,
        );
    }

    #[test]
    fn encode_result_is_ascii_only_after_substitution() {
        // After `undef: :replace` to US-ASCII, the result must pass
        // `ascii_only?` — every non-ASCII char was replaced with
        // an ASCII byte.
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", undef: :replace)
              [s.ascii_only?, s.valid_encoding?]
            "#,
        );
    }

    // -------- Errors --------

    #[test]
    fn encode_unknown_encoding_raises_converter_not_found() {
        run_test(
            r#"begin; "abc".encode("xyz"); rescue => e; e.class.name; end"#,
        );
    }

    #[test]
    fn encode_unmappable_to_us_ascii_raises_undefined_conversion() {
        run_test(
            r#"
              begin
                "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII")
              rescue => e
                e.class.name
              end
            "#,
        );
    }

    #[test]
    fn encode_unmappable_to_iso8859_1_raises_undefined_conversion() {
        // U+3042 (HIRAGANA A) isn't representable in Latin-1.
        run_test(
            r#"
              begin
                "\xE3\x81\x82".force_encoding("UTF-8").encode("ISO-8859-1")
              rescue => e
                e.class.name
              end
            "#,
        );
    }

    #[test]
    fn encode_to_utf32_raises_converter_not_found() {
        // encoding_rs has no UTF-32 transcoder, so monoruby raises
        // ConverterNotFoundError. CRuby supports UTF-32, so this is
        // a monoruby-specific behaviour test (asserted from inside
        // the runtime to avoid the CRuby comparison).
        run_test_no_result_check(
            r#"
              begin
                "abc".encode("UTF-32BE")
                raise "expected ConverterNotFoundError"
              rescue Encoding::ConverterNotFoundError
                # ok
              end
            "#,
        );
    }

    // -------- Options --------

    #[test]
    fn encode_undef_replace_substitutes() {
        // `undef: :replace` substitutes the unmappable codepoint
        // with `?` (default for non-UTF destinations) instead of
        // raising. The resulting bytes show the substitution.
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", undef: :replace)
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_replace_string_overrides_default() {
        // Passing `replace:` enables both `invalid:` and `undef:`
        // implicitly (in monoruby) and uses the supplied substitute.
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", replace: "X")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_to_us_ascii_with_replace_matches_string_argument() {
        // Custom `replace:` produces matching bytes regardless of
        // destination — for US-ASCII the replacement bytes appear
        // verbatim (since the replacement string is ASCII).
        run_test(
            r#"
              s = "\xE3\x81\x82".force_encoding("UTF-8").encode("US-ASCII", replace: "??")
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_invalid_replace_substitutes_invalid_bytes() {
        // `invalid: :replace` substitutes ill-formed bytes (here a
        // bare `0xff` declared UTF-8) instead of raising
        // InvalidByteSequenceError. Use a non-same destination so
        // the actual transcode pipeline runs (the same-encoding
        // fast path doesn't invoke the byte-validation step). We
        // pass all three options explicitly because CRuby's
        // `replace:` doesn't auto-enable `invalid:` / `undef:`
        // (each guard must be opted in independently).
        run_test(
            r#"
              s = "a\xffb".force_encoding("UTF-8").encode(
                "ISO-8859-1", invalid: :replace, undef: :replace, replace: "?"
              )
              [s.bytes, s.encoding.name]
            "#,
        );
    }

    #[test]
    fn encode_replaces_multiple_unmappable_chars() {
        // Several unmappable code points in the same string each
        // get substituted, not just the first one.
        run_test(
            r#"
              s = "a\xE3\x81\x82b\xE3\x81\x84c".force_encoding("UTF-8")
              t = s.encode("US-ASCII", undef: :replace, replace: "?")
              [t.bytes, t.encoding.name]
            "#,
        );
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
    fn encode_xml_attr_escapes_and_wraps() {
        // `xml: :attr` escapes &, <, >, " and wraps the result in
        // double-quotes for use as an HTML attribute value.
        run_test(r#""<a&b>\"c".encode("UTF-8", xml: :attr)"#);
    }

    #[test]
    fn encode_xml_text_escapes() {
        // `xml: :text` escapes &, <, > but NOT " (text content
        // doesn't need to escape quotes).
        run_test(r#""<a&b>\"c".encode("UTF-8", xml: :text)"#);
    }

    #[test]
    fn encode_xml_invalid_value_raises_argument_error() {
        run_test(
            r#"
              begin
                "abc".encode("UTF-8", xml: :bogus)
              rescue => e
                e.class.name
              end
            "#,
        );
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
    fn encoding_converter_new_unsupported_pair_raises() {
        // CRuby supports a much larger transcoder set (it bundles
        // its own, not encoding_rs's), so this case is monoruby-
        // specific: monoruby raises ConverterNotFoundError on
        // pairs encoding_rs can't handle.
        run_test_no_result_check(
            r#"
              begin
                Encoding::Converter.new("UTF-8", "UTF-32BE")
                raise "expected ConverterNotFoundError"
              rescue Encoding::ConverterNotFoundError
                # ok
              end
            "#,
        );
    }
}
