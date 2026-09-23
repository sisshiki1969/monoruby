extern crate monoruby;
use monoruby::tests::*;

// `sub` / `gsub` with a replacement in an encoding that is not
// ASCII-compatible (UTF-16 / UTF-32), and the order in which the pieces
// of a result are appended (#1634). CRuby builds a `gsub` result by
// appending to a buffer that starts empty in the receiver's encoding
// (`rb_enc_cr_str_buf_cat`): an empty buffer takes a wide piece's
// encoding, a non-empty one refuses it, naming its own encoding first;
// the expansion of a template with an escape in it is built the same
// way from an empty BINARY buffer (`rb_reg_regsub`); and `sub` splices
// the bytes under the replacement's encoding when nothing but 7-bit text
// surrounds the match (`rb_str_sub_bang`).

/// `gsub`: where the match sits decides. Matched from its first
/// character, a receiver comes back in the replacement's encoding; with
/// text before or after the match, the append that meets the other
/// encoding is refused, and the message names the buffer's encoding
/// first — the receiver's before the match, the replacement's after it.
#[test]
fn a_wide_replacement_is_taken_by_an_empty_result_and_refused_by_a_non_empty_one() {
    run_test_once(
        r##"
        u16 = ->(s) { s.encode("UTF-16LE") }
        u32 = ->(s) { s.encode("UTF-32BE") }
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        t = ->(&b) { begin; r.(b.call); rescue => e; [e.class, e.message]; end }
        bin = "\xff".b
        sjis = "\x82\xa0".force_encoding("Shift_JIS")
        res = []
        res << t.() { "-".gsub(/-/, u16["a"]) }
        res << t.() { "--".gsub(/-/, u16["a"]) }
        res << t.() { "-".gsub("-", u16["a"]) }
        res << t.() { "-".b.gsub(/-/, u16["a"]) }
        res << t.() { "-".force_encoding("US-ASCII").gsub(/-/, u16["a"]) }
        res << t.() { "-".force_encoding("Shift_JIS").gsub(/-/, u16["a"]) }
        res << t.() { sjis.gsub(/./, u16["a"]) }
        res << t.() { "-".gsub(/-/, u32["a"]) }
        res << t.() { "ab".gsub(/a|b/, u16["x"]) }
        res << t.() { "-".gsub(/-/, "x".force_encoding("UTF-16LE")) }
        res << t.() { "-b".gsub(/-/, u16["a"]) }
        res << t.() { "a-".gsub(/-/, u16["a"]) }
        res << t.() { "a-b".gsub(/-/, u16["a"]) }
        res << t.() { "é-".gsub(/-/, u16["a"]) }
        res << t.() { "-é".gsub(/-/, u16["a"]) }
        res << t.() { "a-b".gsub(/-/, "x".force_encoding("UTF-16LE")) }
        # An empty wide replacement is no piece at all.
        res << t.() { "-".gsub(/-/, u16[""]) }
        res << t.() { "-b".gsub(/-/, u16[""]) }
        res << t.() { "-b".gsub(/-/, "".force_encoding("UTF-16LE")) }
        res << t.() { "xyz".gsub(/-/, u16["a"]) }
        # The block and Hash forms append the same way.
        res << t.() { "-".gsub(/-/) { u16["a"] } }
        res << t.() { "-b".gsub(/-/) { u16["a"] } }
        res << t.() { "-".gsub(/-/, "-" => u16["a"]) }
        res << t.() { "-b".gsub(/-/, "-" => u16["a"]) }
        res << t.() { "ab".gsub(/a|b/) { |m| m == "a" ? u16["x"] : "y" } }
        res << t.() { "ab".gsub(/a|b/) { |m| m == "a" ? "y" : u16["x"] } }
        # The bang forms.
        res << t.() { s = "-"; s.gsub!(/-/, u16["a"]); s }
        res << t.() { s = "-"; s.gsub!(/-/) { u16["a"] }; s }
        res << t.() { s = "-b"; s.gsub!(/-/, u16["a"]); s }
        res
        "##,
    );
}

/// A template with an escape is expanded into a buffer that starts
/// empty and BINARY, its stretches in the template's encoding and the
/// captured text in the receiver's: whichever non-ASCII piece comes
/// first settles the buffer, and the message names it first.
#[test]
fn a_template_with_an_escape_is_expanded_piece_by_piece() {
    run_test_once(
        r##"
        u16 = ->(s) { s.encode("UTF-16LE") }
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        t = ->(&b) { begin; r.(b.call); rescue => e; [e.class, e.message]; end }
        bin = "\xff".b
        res = []
        # A captured 7-bit piece leaves the buffer BINARY, and a BINARY
        # 7-bit expansion changes nothing when appended.
        res << t.() { "-".gsub(/(-)/, u16["\\1"]) }
        res << t.() { "-".gsub(/(-)/, u16["\\1\\1"]) }
        res << t.() { "-".gsub(/(-)/, u16["\\\\"]) }
        res << t.() { "-".gsub(/(-)/, u16["\\"]) }
        res << t.() { "aéb".gsub(/(é)/, u16["\\1"]) }
        res << t.() { "aéb".b.gsub(/(b)/, "\\1".encode("UTF-16LE")) }
        # A wide stretch of the template first, then captured UTF-8.
        res << t.() { "-".gsub(/(-)/, u16["x\\1"]) }
        # Captured UTF-8 first, then a wide stretch.
        res << t.() { "é".gsub(/(é)/, u16["\\1x"]) }
        res << t.() { "-".gsub(/(-)/, u16["\\1"] + u16["x"]) }
        # The same order with a BINARY template over a UTF-8 receiver.
        res << t.() { "aéb".gsub(/(é)/, "\\1" + bin) }
        res << t.() { "aéb".gsub(/(é)/, bin + "\\1") }
        res << t.() { "aéb".gsub(/(é)/, "x".b + "\\1") }
        res << t.() { "aéb".gsub(/(é)/, "\\1" + "x".b) }
        res << t.() { "-".gsub(/(-)/, "\\1".encode("UTF-16LE") + "\xff".b.force_encoding("UTF-16LE")) }
        res << t.() { "-".sub(/(-)/, u16["\\1"]) }
        res << t.() { "-".sub(/(-)/, u16["x\\1"]) }
        res << t.() { "é".sub(/(é)/, u16["\\1x"]) }
        res << t.() { "aéb".sub(/(é)/, u16["\\1"]) }
        res << t.() { "aéb".sub(/(é)/, "\\1" + bin) }
        res << t.() { "aéb".sub(/(é)/, bin + "\\1") }
        res
        "##,
    );
}

/// `sub` checks the receiver against the replacement as a whole: when
/// they cannot share an encoding, the bytes before and after the match
/// must be 7-bit, and the result is tagged with the replacement's
/// encoding — a mixed-width string, which is what CRuby makes too.
/// (Its cached code range still says such a string is valid; monoruby
/// classifies the bytes, so `valid_encoding?` is not compared here.)
#[test]
fn sub_splices_a_wide_replacement_into_seven_bit_surroundings() {
    run_test_once(
        r##"
        u16 = ->(s) { s.encode("UTF-16LE") }
        u32 = ->(s) { s.encode("UTF-32BE") }
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        t = ->(&b) { begin; r.(b.call); rescue => e; [e.class, e.message]; end }
        bin = "\xff".b
        res = []
        res << t.() { "-".sub(/-/, u16["a"]) }
        res << t.() { "-x".sub(/-/, u16["a"]) }
        res << t.() { "x-".sub(/-/, u16["a"]) }
        res << t.() { "x-".sub(/-/, u32["a"]) }
        res << t.() { "é-".sub(/-/, u16["a"]) }
        res << t.() { "-é".sub(/-/, u16["a"]) }
        res << t.() { "-".b.sub(/-/, u16["a"]) }
        res << t.() { "-".force_encoding("US-ASCII").sub(/-/, u16["a"]) }
        res << t.() { "-".sub("-", u16["a"]) }
        res << t.() { "-".sub(/-/) { u16["a"] } }
        res << t.() { "ab".sub(/a/) { u16["x"] } }
        res << t.() { "ab".sub(/b/) { u16["x"] } }
        res << t.() { "-".sub(/-/, "-" => u16["a"]) }
        res << t.() { "-".sub(/-/, u16[""]) }
        res << t.() { "-b".sub(/-/, u16[""]) }
        res << t.() { s = "-"; s.sub!(/-/, u16["a"]); s }
        res << t.() { s = "x-"; s.sub!(/-/) { u16["a"] }; s }
        # The same rule for byte-oriented pieces: the receiver's non-ASCII
        # text has to be what the match takes out.
        res << t.() { "aéb".sub(/é/, bin) }
        res << t.() { "aéb".sub(/a/, bin) }
        res << t.() { "a-b".sub(/-/, "\xff".force_encoding("Shift_JIS")) }
        res << t.() { "é-".sub(/-/, "\xff".force_encoding("Shift_JIS")) }
        res << t.() { "".sub(//, u16["a"]) }
        res << t.() { "".sub(//, "x".b) }
        res
        "##,
    );
}

/// A UTF-16 receiver: a 7-bit UTF-8 replacement is a piece of another
/// encoding, taken by an empty result and refused by a non-empty one,
/// for `gsub` and `sub` alike, with a Regexp or a String pattern.
#[test]
fn a_wide_receiver_takes_only_its_own_encoding() {
    run_test_once(
        r##"
        u16 = ->(s) { s.encode("UTF-16LE") }
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        t = ->(&b) { begin; r.(b.call); rescue => e; [e.class, e.message]; end }
        re = Regexp.new(u16["-"])
        reg = Regexp.new(u16["(-)"])
        res = []
        res << t.() { u16["-b"].gsub(re, "x") }
        res << t.() { u16["b-"].gsub(re, "x") }
        res << t.() { u16["-"].gsub(re, "x") }
        res << t.() { u16["-"].gsub(re, u16["x"]) }
        res << t.() { u16["-b"].gsub(re, "") }
        res << t.() { u16["-b"].gsub(re, "é") }
        res << t.() { u16["-"].gsub(re, "é") }
        res << t.() { u16["-b"].gsub(re) { "x" } }
        res << t.() { u16["-"].gsub(re) { "x" } }
        res << t.() { u16["-b"].gsub(reg, "\\1") }
        res << t.() { u16["-b"].gsub(reg, u16["\\1"]) }
        res << t.() { u16["-b"].gsub(reg, "x\\1") }
        res << t.() { u16["-b"].gsub(reg, "\\1x") }
        res << t.() { u16["-b"].sub(re, "x") }
        res << t.() { u16["-"].sub(re, "x") }
        res << t.() { u16["-"].sub(re, "") }
        res << t.() { u16["-"].sub(re, "é") }
        # A String pattern.
        res << t.() { u16["-b"].gsub(u16["-"], "x") }
        res << t.() { u16["-"].gsub(u16["-"], "x") }
        res << t.() { u16["-b"].gsub(u16["-"], u16["x"]) }
        res << t.() { u16["-b"].sub(u16["-"], "x") }
        res << t.() { u16["-"].sub(u16["-"], "x") }
        res << t.() { u16["-b"].sub(u16["-"], "") }
        res
        "##,
    );
}

/// The pieces are appended as the walk goes, so a piece the result
/// cannot take is refused right after the yield that produced it — or
/// after the next one, when it is the stretch of the receiver between
/// two matches that cannot be appended — and the block is not called
/// for the matches after that.
#[test]
fn the_error_comes_after_the_yield_whose_piece_cannot_be_appended() {
    run_test_once(
        r##"
        u16 = ->(s) { s.encode("UTF-16LE") }
        bin = "\xff".b
        c = ->(&b) { n = 0; err = nil; begin; b.call(-> { n += 1 }); rescue => e; err = e.message; end; [n, err] }
        res = []
        res << c.() { |tick| "-b-".gsub(/-/) { tick.(); u16["a"] } }
        res << c.() { |tick| "a-b-".gsub(/-/) { tick.(); u16["a"] } }
        res << c.() { |tick| "-b-".gsub(/-/) { tick.() == 1 ? "x" : bin } }
        res << c.() { |tick| "-b-".gsub(/-/) { tick.() == 1 ? "é" : bin } }
        res << c.() { |tick| "-é-".gsub(/-/) { tick.(); bin } }
        res << c.() { |tick| "-b-c-".gsub(/-/) { tick.() == 3 ? bin : "é" } }
        res << c.() { |tick| "-b-".gsub(/-/, Hash.new { |h, k| tick.(); u16["a"] }) }
        res << c.() { |tick| "-b-".gsub(/-/, Hash.new { |h, k| tick.() == 1 ? "é" : bin }) }
        res << c.() { |tick| "b-b-".gsub(/-/) { tick.() == 1 ? "x" : "é".b } }
        res << c.() { |tick| "-b-".gsub(/-/) { tick.(); "é".b } }
        res << c.() { |tick| "-b-c".gsub(/-/) { tick.(); "é".b } }
        res
        "##,
    );
}

/// Inside the block form the walk runs over a snapshot of the receiver,
/// and `$~` is cut from that snapshot: the captures, the whole match and
/// the pre-match carry the receiver's encoding, so what the block builds
/// out of `$1` appends as a piece of that encoding and the result keeps
/// it (a UTF8-MAC receiver stayed UTF8-MAC only by a re-tag before).
#[test]
fn backrefs_inside_the_block_carry_the_receivers_encoding() {
    run_test_once(
        r##"
        r = ->(s) { [s.bytes, s.encoding.to_s] }
        res = []
        s = "eé".force_encoding("UTF8-MAC")
        seen = []
        res << r.(s.gsub(/(.)/) { seen << [$1.encoding.to_s, $~[0].encoding.to_s, $~.pre_match.encoding.to_s, $~.post_match.encoding.to_s]; $1 * 2 })
        res << seen
        res << r.(s.gsub(/(.)/) { |m| m * 2 })
        res << r.(s.gsub(/(.)/, "." => "x") { })
        seen = []
        res << r.("ab".force_encoding("Shift_JIS").gsub(/(.)/) { seen << $1.encoding.to_s; $1 })
        res << seen
        res << r.("ab".force_encoding("Shift_JIS").gsub(/(.)/) { "x" })
        seen = []
        res << r.("a\x82\xa0b".force_encoding("Shift_JIS").gsub(/(.)/) { seen << [$1.encoding.to_s, $1.bytes]; $1 })
        res << seen
        h = Hash.new { |hash, k| seen << [k.encoding.to_s, $~[0].encoding.to_s]; k }
        seen = []
        res << r.("ab".force_encoding("Shift_JIS").gsub(/(.)/, h))
        res << seen
        res
        "##,
    );
}
