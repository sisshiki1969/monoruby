extern crate monoruby;
use monoruby::tests::*;

// Issue #1986: on a UTF-16 string a character is one or two code units,
// so a Range (or start/length) subscript walks characters rather than
// multiplying by a fixed width; `tr` and friends work on scalar values in
// the receiver's own encoding; and a dummy encoding (BOM-carrying UTF-16 /
// UTF-32, UTF-7, ISO-2022-JP, …) cannot back a Regexp.

const PRELUDE: &str = r##"
    def t; r = yield; r.is_a?(String) ? [r.b, r.encoding.name] : r; rescue Exception => e; "#{e.class}: #{e.message.b}"; end
"##;

#[test]
fn wide_issue_probe() {
    run_test_with_prelude(
        r##"
        res = []
        %w[UTF-16LE UTF-32BE].each do |e|
          s = "aéb\u{1F600}éa".encode(e)
          hi = "é".encode(e)
          res << [e, t { s[2..-1].bytesize }, t { s[2..].encode("UTF-8") }, t { s[-4..-1].bytesize }, t { s[3..-1].bytesize },
             t { s.slice(2..-1).bytesize }, t { s.partition(hi).map(&:bytesize) },
             t { s.tr(hi, "z".encode(e)).encode("UTF-8") }, t { s.tr("a".encode(e), "z".encode(e)).encode("UTF-8") },
             t { s.tr_s(hi, "z".encode(e)).bytesize }]
        end
        %w[UTF-16 UTF-32].each do |e|
          s = "aéb".encode(e)
          res << [e, t { s.scan("".encode(e)).size }, t { Regexp.new("b".encode(e)) }]
        end
        res
        "##,
        PRELUDE,
    );
}

#[test]
fn utf16_subscripts_walk_characters() {
    run_test_with_prelude(
        r##"
        res = []
        %w[UTF-16LE UTF-16BE UTF-32LE UTF-32BE].each do |e|
          s = "aéb\u{1F600}éa\u{1F601}\u{1F601}zz".encode(e)
          en = ->(x) { x.encode(e) }
          [[0..-1],[2..-1],[3..-1],[4..-1],[-4..-1],[1...4],[3..3],[3...3],[2,10],[3,1],[3,2],[4,1],[-1,1],[-3,2],[11..],[12..],[..3],[3],[-1],[6],[7],[10,5],[11,5]].each do |a|
            res << [a, t { s[*a] }, t { s.slice(*a) }, t { s.dup.slice!(*a) }]
          end
          res << t { x = s.dup; x[2..3] = en["Q"]; x }
          res << t { x = s.dup; x[3, 1] = en["Q"]; x }
          res << t { x = s.dup; x[3] = en["Q"]; x }
          res << [t { s.partition(en["é"]).map(&:b) }, t { s.partition(en["\u{1F600}"]).map(&:b) }]
          res << [t { s.index(en["\u{1F601}"]) }, t { s.index(en["z"]) }, t { s.index(en["é"], 2) }]
          res << [t { s.chars.size }, t { s.length }, t { s.reverse }, t { s.each_char.to_a.last }]
          res << [t { s.dup.insert(4, en["Q"]) }, t { s.chop }, t { s.chr }]
          res << [t { s.ljust(14, en["*"]) }, t { s.center(15, en["*"]) }, t { s.rjust(14, en["*"]) }]
          res << [t { s.sub(en["b"], en["Q"]) } , t { s.delete_prefix(en["a"]) }, t { s.delete_suffix(en["z"]) }]
        end
        res
        "##,
        PRELUDE,
    );
}

#[test]
fn wide_tr_family_is_per_scalar() {
    run_test_with_prelude(
        r##"
        res = []
        %w[UTF-16LE UTF-16BE UTF-32LE UTF-32BE].each do |e|
          s = "aéb\u{1F600}éa\u{1F601}\u{1F601}zz".encode(e)
          en = ->(x) { x.encode(e) }
          res << [t { s.tr(en["é"], en["z"]) }, t { s.tr(en["a"], en["z"]) }, t { s.tr_s(en["é"], en["z"]) }]
          res << [t { s.tr(en["a-c"], en["A-C"]) }, t { s.tr(en["^a-c"], en["*"]) }, t { s.tr(en["\u{1F600}-\u{1F601}"], en["x"]) }]
          res << [t { s.tr(en["z"], en["\u{1F602}"]) }, t { s.tr_s(en["z"], en["\u{1F602}"]) }, t { s.tr(en["a"], en[""]) }]
          res << [t { s.tr(en["a-b"], en["\u3042-\u3043"]) }, t { s.tr_s(en["\u{1F601}z"], en["q"]) }]
          res << [t { s.dup.tr!(en["q"], en["z"]) }, t { s.dup.tr!(en["a"], en["z"]) }, t { s.dup.tr_s!(en["z"], en["y"]) }]
          res << [t { s.delete(en["a-c"]) }, t { s.delete(en["\u{1F600}"]) }, t { s.squeeze }, t { s.squeeze(en["z"]) }, t { s.squeeze(en["\u{1F601}"]) }]
          res << [t { s.count(en["a-c"]) }, t { s.count(en["^a"]) }, t { s.count(en["\u{1F600}-\u{1F601}"]) }]
          res << [t { s.tr("a", "z") }, t { s.delete("a") }, t { s.count("a") }, t { s.squeeze("z") }]
          res << t { s.tr(en["c-a"], en["z"]) }
        end
        c = "aé\u{1F600}b\u{1F600}c".encode("CESU-8")
        res << [t { c.tr("\u{1F600}".encode("CESU-8"), "x") }, t { c.tr("a-c", "A-C") }, t { c.delete("\u{1F600}".encode("CESU-8")) }]
        res
        "##,
        PRELUDE,
    );
}

#[test]
fn dummy_wide_encoding_strings() {
    run_test_with_prelude(
        r##"
        res = []
        %w[UTF-16 UTF-32].each do |e|
          res << ["".encode(e).bytes, "a".encode(e).bytes, "".encode(e, "UTF-8").bytes, "".dup.force_encoding("UTF-16LE").encode(e).bytes]
          s = "aéb".encode(e)
          em = "".dup.force_encoding(e)
          b = "b".encode(e)
          res << [t { s.scan("".encode(e)).size }, t { s.scan(em).size }, t { s.scan(b).size }, t { s.split(b) }, t { s.split(em).size }]
          res << [t { s.index(b) }, t { s.include?(b) }, t { s.partition(b) }, t { s.start_with?(b) }, t { s[b] }, t { s.sub(b, b) }, t { s.gsub(b, b) }]
          res << [t { s.tr(b, b) }, t { s.delete(b) }, t { s.count(b) }, t { s[1..-1] }, t { s.length }, t { s.chars.size }, t { s.reverse }]
          res << [b.inspect, b.dump, Regexp.escape(b).b]
        end
        res
        "##,
        PRELUDE,
    );
}

#[test]
fn regexp_rejects_dummy_encodings() {
    run_test_with_prelude(
        r##"
        res = []
        %w[UTF-7 UTF-16 UTF-32 ISO-2022-JP CP50221 CP50220 ISO-2022-JP-2 ISO-2022-JP-KDDI].each do |e|
          x = "b".dup.force_encoding(e)
          s = "abc".dup.force_encoding(e)
          res << [e, t { Regexp.new(x) }, t { Regexp.new(x, Regexp::IGNORECASE) }, t { /#{x}/ }, t { /a#{x}/i }, t { /#{x}#{x}/ },
            t { /#{x}/n.source }, t { Regexp.new(x, Regexp::NOENCODING).source }, t { Regexp.new(x, "n").source },
            t { Regexp.new(x, Regexp::FIXEDENCODING) },
            t { s.match?(x) }, t { s.match(x) }, t { s =~ Regexp.new("b") }, t { s.sub(/b/, "x") }]
        end
        res << t { Regexp.new("\e$B$\"\e(B".dup.force_encoding("ISO-2022-JP")) }
        res << t { Regexp.new("+AOk-".dup.force_encoding("UTF-7")) }
        res
        "##,
        PRELUDE,
    );
}

#[test]
fn regexp_union_encoding_negotiation() {
    run_test_with_prelude(
        r##"
        def u; r = yield; r.is_a?(Regexp) ? [r.source.b, r.encoding.name, r.fixed_encoding?] : r; rescue Exception => e; "#{e.class}: #{e.message.b}"; end
        u16 = "b".encode("UTF-16LE"); u32 = "c".encode("UTF-32BE")
        euc = "\xA4\xA2".dup.force_encoding("EUC-JP"); sj = "\x82\xA0".dup.force_encoding("Shift_JIS")
        u7 = "b".dup.force_encoding("UTF-7"); d16 = "b".encode("UTF-16")
        cases = {
          a_u16: ["a", u16], u16_a: [u16, "a"], e_u16: ["é", u16], u16_e: [u16, "é"], u16_u32: [u16, u32], u16_u16: [u16, u16],
          re_u16: [/a/, u16], u16_re: [u16, /a/], reu_u16: [/é/, u16], u16_reu: [u16, /é/],
          sym_euc: [:é, euc], euc_sym: [euc, :é], sym_a: [:a, euc], sym_sym: [:é, :a], symeuc: [euc.to_sym], symeuc_sj: [euc.to_sym, sj],
          a_sym16: ["a", u16.to_sym], sym16: [u16.to_sym], sym16_16: [u16.to_sym, u16], sym_u: [:é], sym_au: [:a, "é"],
          euc_sj: [euc, sj], sj_euc: [sj, euc], a_euc_a: ["a", euc, "a"], bin: ["\xff".b, "a"], bin_u: ["\xff".b, "é"], u_bin: ["é", "\xff".b],
          a_u16_e: ["a", u16, "é"], e_a_u16: ["é", "a", u16], u16_e_a: [u16, "é", "a"],
          ren_u16: [/a/n, u16], reeuc_u16: [Regexp.new(euc), u16], u16_reeuc: [u16, Regexp.new(euc)],
          a_u7: ["a", u7], u7_a: [u7, "a"], u7_re: [u7, /a/], u7_e: [u7, "é"], u7_u7: [u7, u7], u7_u16: [u7, u16], u7: [u7], u7dot: ["b.".dup.force_encoding("UTF-7")],
          a_d16: ["a", d16], d16_a: [d16, "a"], d16_d16: [d16, d16], d16: [d16], empty: [],
        }
        cases.map { |k, a| [k, u { Regexp.union(*a) }, u { Regexp.union(a) }] }
        "##,
        PRELUDE,
    );
}
