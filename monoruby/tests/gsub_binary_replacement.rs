extern crate monoruby;
use monoruby::tests::*;

// `sub` / `gsub` with a replacement that is a BINARY (or other
// byte-oriented) String carrying non-ASCII bytes (#1378): the bytes are
// spliced as they are, and the result's encoding is settled piece by
// piece the way CRuby's `rb_enc_cr_str_buf_cat` does — a 7-bit receiver
// takes the replacement's encoding, a receiver with its own non-ASCII
// content keeps it, and two non-ASCII pieces in different encodings
// raise `Encoding::CompatibilityError`.

/// Every form of `sub` / `gsub` (a String replacement with and without
/// backreferences, a block, a Hash, a String pattern, the bang forms)
/// over a 7-bit receiver: the replacement's bytes, its encoding.
#[test]
fn binary_replacement_over_a_seven_bit_receiver() {
    run_test_once(
        r##"
        r = ->(s) { [s.bytes, s.encoding.to_s, s.valid_encoding?] }
        bin = "\xff".b
        res = []
        res << r.("a-b".gsub(/-/, bin))
        res << r.("a-b".gsub(/-/) { bin })
        res << r.("a-b".sub(/-/, bin))
        res << r.("a-b".sub(/-/) { bin })
        res << r.("a-b".sub(/-/, "-" => bin))
        res << r.("a-b".gsub(/-/, "-" => bin))
        res << r.("a-b".gsub("-", bin))
        res << r.("a-b".sub("-", bin))
        res << r.("a-b-c".gsub(/-/, bin))
        res << r.("a-b".gsub(/-/, "\xff\xfe".b + "x"))
        res << r.("a-b".gsub(/(-)/, bin + "\\1"))
        res << r.("a-b".gsub(/(-)/, bin + "\\1\\\\"))
        res << r.("a-b".gsub("-", bin + "\\0"))
        res << r.("a-b".sub("-", bin + "\\&"))
        res << r.("a-b".gsub(/-/, "\xff\\".b))
        res << r.("a-b".gsub(/-/, "\\\xff".b))
        res << r.("a-b".gsub(/-/, "é".b))
        res << r.("a-b".gsub(/-/, "\xe3\x81\x82".b))
        res << r.("a-b".gsub(/-/) { "\xff".force_encoding("ASCII-8BIT") + "é".b })
        s = "a-b"; s.gsub!(/-/, bin); res << r.(s)
        s = "a-b"; s.sub!(/-/) { bin }; res << r.(s)
        s = "a-b"; s.sub!(/-/, bin); res << r.(s)
        s = "a-b"; s.gsub!(/-/, "-" => bin); res << r.(s)
        # A 7-bit replacement in another encoding does not move it.
        res << r.("a-b".gsub(/-/, "x".b))
        res << r.("a-b".gsub(/-/, "x".force_encoding("Shift_JIS")))
        # Other byte-oriented encodings, and other 7-bit receivers.
        res << r.("a-b".gsub(/-/, "\xff".force_encoding("Shift_JIS")))
        res << r.("a-b".gsub(/-/, "é".force_encoding("Shift_JIS")))
        res << r.("a-b".gsub(/-/) { "é".force_encoding("Shift_JIS") })
        res << r.("a-b".force_encoding("US-ASCII").gsub(/-/, bin))
        res << r.("a-b".force_encoding("US-ASCII").gsub(/-/, "é"))
        res << r.("a-b".b.gsub(/-/, bin))
        res << r.("a-b".b.gsub(/-/, "é"))
        res << r.("a-b".b.gsub(/-/) { "é" })
        # A `to_str` / `to_s` answering a BINARY String.
        o = Object.new
        def o.to_str; "\xff".b; end
        res << r.("a-b".gsub(/-/, o))
        p = Object.new
        def p.to_s; "\xff".b; end
        res << r.("a-b".gsub(/-/) { p })
        res << r.("a-b".gsub(/-/, "-" => p))
        # The result is a String like any other.
        t = "a-b".gsub(/-/, "\xe3\x81\x82".b)
        res << [t.size, t.force_encoding("UTF-8") == "aあb"]
        u = "a-b".gsub(/-/, bin)
        res << [u.ascii_only?, u.frozen?, $~[0], $~.pre_match.encoding.to_s]
        res << r.(u.gsub(/b/, "c"))
        res << r.(u.gsub(/b/) { "c" })
        res << r.("a-b".gsub(/-/, "é".b) + "x")
        # A broken UTF-8 replacement is spliced as its bytes too.
        res << r.("a-b".gsub(/-/, "\xff"))
        res << r.("a-b".sub(/-/, "\xff"))
        res << r.("a-b".gsub(/-/) { "\xff" })
        res << r.("a-b".gsub(/-/, "-" => "\xff"))
        res
        "##,
    );
}

/// Where the pieces cannot share an encoding: a UTF-8 receiver whose
/// own non-ASCII text survives next to a BINARY replacement, non-ASCII
/// captured text pasted into a BINARY template, two replacements in
/// different encodings, a UTF-16 replacement — and where they can:
/// the receiver's only non-ASCII characters are what gets replaced.
#[test]
fn incompatible_pieces_raise_and_replaced_ones_do_not() {
    run_test_once(
        r##"
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        t = ->(&b) { begin; r.(b.call); rescue => e; [e.class, e.message]; end }
        bin = "\xff".b
        res = []
        res << t.() { "aéb".gsub(/é/, bin) }
        res << t.() { "aéb".sub(/é/, bin) }
        res << t.() { "aéb".gsub(/é/) { bin } }
        res << t.() { "aéb".gsub(/é/, "é" => bin) }
        res << t.() { "aéb".gsub(/a/, bin) }
        res << t.() { "aéb".sub(/b/, bin) }
        res << t.() { "aéb".gsub(/a/) { bin } }
        res << t.() { "aéb".gsub(/a/, "a" => bin) }
        res << t.() { "aéb".gsub(/(é)/, bin + "\\1") }
        res << t.() { "aéb".gsub(/(é)/, "x".b + "\\1") }
        res << t.() { "a-b-c".gsub(/-/).with_index { |m, i| i == 0 ? bin : "é" } }
        res << t.() { "a-b-c".gsub(/-/).with_index { |m, i| i == 0 ? "é" : bin } }
        res << t.() { "a-b".gsub(/-/, "a".encode("UTF-16LE")) }
        res << t.() { "a-b".gsub(/-/, "".encode("UTF-16LE")) }
        res << t.() { "xyz".gsub(/-/, "a".encode("UTF-16LE")) }
        res << t.() { "aéb".gsub(/-/, bin) }
        res << t.() { "aéb".sub(/-/, bin) }
        res
        "##,
    );
}
