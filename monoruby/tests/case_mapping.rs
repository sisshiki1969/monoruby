extern crate monoruby;
use monoruby::tests::*;

// Issue #1955: String case mapping — the option checks, `:turkic` under
// an encoding that has no Turkic letters, the titlecase mapping, and the
// case maps of the single-byte encodings.

#[test]
fn case_option_errors() {
    // `check_case_options` has three messages and one order: more than
    // two options are too many whatever they are, only `:turkic` and
    // `:lithuanian` pair up, and `:fold` is for downcasing.
    run_test_once(
        r##"
        def t; yield; rescue ArgumentError => e; e.message; end
        res = []
        [:upcase, :downcase, :capitalize, :swapcase, :upcase!, :downcase!, :capitalize!, :swapcase!].each do |m|
          [[:bogus], [1], [nil], ["ascii"], [:ascii, :turkic], [:turkic, :ascii], [:fold, :ascii],
           [:ascii, :ascii], [:ascii, :ascii, :ascii], [:turkic, :lithuanian, :ascii], [:fold],
           [:fold, :fold], [:lithuanian, :bogus], [:turkic, :turkic], [:lithuanian, :lithuanian],
           [:turkic, :lithuanian], [:lithuanian, :turkic], [:lithuanian], [:ascii], [:turkic]].each do |o|
            res << t { "aIi".dup.send(m, *o) }
          end
        end
        # Symbol's methods are String's.
        res << t { :a.upcase(:bogus) } << t { :a.swapcase(:fold) } << t { :a.downcase(:ascii, :fold) }
        res
        "##,
    );
}

#[test]
fn case_bang_check_order() {
    // `rb_str_upcase_bang`: the options, then frozenness, then the
    // encoding and the contents.
    run_test_once(
        r##"
        def t; yield; rescue => e; [e.class, e.message]; end
        [
          t { "\xff".freeze.upcase!(:bogus) },
          t { "\xff".freeze.upcase! },
          t { "abc".encode("UTF-16LE").force_encoding("UTF-16").freeze.downcase! },
          t { "abc".encode("UTF-16LE").force_encoding("UTF-16").swapcase! },
          t { "\xff\xfe".force_encoding("EUC-JP").capitalize! },
          t { "é\xff".swapcase },
          t { "a\xff".force_encoding("US-ASCII").upcase(:turkic) },
        ]
        "##,
    );
}

#[test]
fn turkic_belongs_to_the_encoding() {
    // The Unicode encodings have a dotless `ı` and a dotted `İ`, and so
    // do four single-byte ones; under any other encoding `:turkic`
    // changes nothing — it used to write the UTF-8 bytes of `ı` / `İ`
    // into a string of that encoding.
    run_test_once(
        r##"
        res = %w[US-ASCII ASCII-8BIT ISO-8859-1 ISO-8859-2 ISO-8859-3 ISO-8859-9 Windows-1252 Windows-1254
                 Windows-1257 IBM437 KOI8-R EUC-JP Shift_JIS UTF-8 UTF8-MAC].map do |e|
          s = "Ii".dup.force_encoding(e)
          [e, s.upcase(:turkic).bytes, s.downcase(:turkic).bytes, s.capitalize(:turkic).bytes,
           s.swapcase(:turkic).bytes, s.upcase(:turkic, :lithuanian).bytes, s.downcase(:lithuanian, :turkic).bytes,
           s.upcase(:turkic).encoding.to_s, s.dup.downcase!(:turkic)&.bytes, s.dup.upcase!(:turkic)&.valid_encoding?]
        end
        res += %w[UTF-16LE UTF-16BE UTF-32LE UTF-32BE].map do |e|
          s = "Iiİı".encode(e)
          [e, s.upcase(:turkic).bytes, s.downcase(:turkic).bytes, s.swapcase(:turkic).bytes, s.downcase.bytes]
        end
        res << [:I.downcase(:turkic), :i.upcase(:turkic), :"İ".downcase(:turkic), :i.capitalize(:turkic), :Ii.swapcase(:turkic)]
        "##,
    );
}

#[test]
fn capitalize_uses_the_titlecase_mapping() {
    run_test_once(
        r##"
        res = ["ǆ", "ǆa", "Ǆx", "ǅX", "ǳ", "ᾀ", "ᾈA", "ßa", "ß",
               "ŉA", "ﬁX", "ﬃ", "აა", "ᲐᲐ", "և", "ẖ",
               "i̇", "ıa", "aǅ", "aß"].map do |s|
          [s.capitalize.unpack("U*"), s.capitalize(:turkic).unpack("U*"), s.capitalize(:lithuanian).unpack("U*"),
           s.capitalize(:ascii).unpack("U*"), s.dup.capitalize!&.unpack("U*")]
        end
        res << [:"ǆ".capitalize, :"ǆa".capitalize, :"ßx".capitalize]
        res << %w[UTF-16LE UTF-32BE UTF8-MAC].map { |e| "ǆßI".encode(e).capitalize.bytes }
        res
        "##,
    );
}

#[test]
fn swapcase_of_titlecase_letters() {
    run_test_once(
        r##"
        res = ["ǅ", "ǈa", "ǋǋ", "ǲ", "ᾈ", "ᾼ", "ῼx", "Ǆǆ",
               "ß", "İ", "ı", "ა", "Ა", "ŉ", "ﬁ", "aBéÉ"].map do |s|
          [s.swapcase.unpack("U*"), s.swapcase(:turkic).unpack("U*"), s.swapcase(:ascii).unpack("U*"),
           s.dup.swapcase!&.unpack("U*")]
        end
        res << [:"ǅ".swapcase, :"aǅ".swapcase]
        res << %w[UTF-16BE UTF-32LE].map { |e| "ǅi".encode(e).swapcase.bytes }
        res
        "##,
    );
}

#[test]
fn single_byte_encodings_have_their_own_case_maps() {
    // The ISO-8859 family and its Windows cousins map their own letters
    // (`ß` spelled out, the final sigma folded); the IBM / Mac / KOI8
    // code pages and the multibyte encodings map ASCII alone.
    run_test_once(
        r##"
        %w[ISO-8859-1 ISO-8859-2 ISO-8859-5 ISO-8859-7 ISO-8859-9 ISO-8859-15 Windows-1250 Windows-1251
           Windows-1252 Windows-1253 Windows-1257 IBM437 macRoman KOI8-R ISO-8859-6 TIS-620 ASCII-8BIT
           US-ASCII EUC-JP Shift_JIS].map do |e|
          s = "i\xDF\xE9I\xF2\xB5\xC0\xA1".dup.force_encoding(e)
          t = ->(&b) { b.call rescue [$!.class, $!.message] }
          [e, s.valid_encoding?, s.upcase.bytes, s.downcase.bytes, s.capitalize.bytes, s.swapcase.bytes,
           s.downcase(:fold).bytes, s.upcase(:ascii).bytes, s.swapcase(:lithuanian).bytes,
           s.dup.upcase!&.bytes, s.dup.downcase!(:ascii)&.bytes, s.upcase.encoding.to_s,
           t.() { "\xDFx".dup.force_encoding(e).capitalize.bytes },
           t.() { "x\xDF".dup.force_encoding(e).capitalize.bytes }]
        end
        "##,
    );
}

#[test]
fn single_byte_case_tables_match() {
    // Every byte of every table, as a checksum per method: the tables in
    // `builtins/string/byte_case.rs` were read off CRuby, and this is
    // CRuby reading them back.
    run_test_once(
        r##"
        sum = ->(strs) { strs.inject(7) { |a, s| s.each_byte.inject((a * 131 + s.bytesize) % 1000000007) { |x, b| (x * 131 + b) % 1000000007 } } }
        %w[ISO-8859-1 ISO-8859-2 ISO-8859-3 ISO-8859-4 ISO-8859-5 ISO-8859-6 ISO-8859-7 ISO-8859-8
           ISO-8859-9 ISO-8859-10 ISO-8859-11 ISO-8859-13 ISO-8859-14 ISO-8859-15 ISO-8859-16
           Windows-1250 Windows-1251 Windows-1252 Windows-1253 Windows-1254 Windows-1255 Windows-1256
           Windows-1257 Windows-1258 Windows-874 IBM437 IBM852 IBM866 CP850 KOI8-R KOI8-U macRoman
           macCyrillic macGreek macTurkish TIS-620 GB1988 ASCII-8BIT].map do |e|
          chars = (0..0xFF).map { |b| b.chr.force_encoding(e) }
          [e,
           sum.(chars.map(&:upcase)), sum.(chars.map(&:downcase)), sum.(chars.map(&:capitalize)),
           sum.(chars.map(&:swapcase)), sum.(chars.map { |c| c.downcase(:fold) }),
           sum.(chars.map { |c| c.upcase(:turkic) }), sum.(chars.map { |c| c.downcase(:turkic) }),
           sum.(chars.map { |c| c.swapcase(:turkic) }), sum.(chars.map { |c| c.swapcase(:ascii) }),
           sum.(chars.map { |c| (c + c).capitalize })]
        end
        "##,
    );
}

#[test]
fn pattern_argument_errors() {
    // `get_pat`: what is neither a Regexp nor a String (nor answers
    // `to_str`) was supposed to be a Regexp; `=~` is an ordinary call on
    // the operand; `index` / `rindex` take a String by `StringValue`.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}".gsub(/#<Class:0x\h+>/, "C").gsub(/0x\h+/, "XX"); end
        o = Object.new
        bad = Class.new { def to_str = 1 }.new
        str = Class.new { def to_str = "." }.new
        mm = Class.new {
          def method_missing(n, *a) = n == :to_str ? "b" : super
          def respond_to_missing?(n, p = false) = n == :to_str || super
        }.new
        eq = Class.new { def =~(x) = [:called, x] }.new
        eqmm = Class.new { def method_missing(n, *a) = n == :=~ ? [:mm, a] : super }.new
        [1, nil, :sym, o, 1.5, [1], true, false, 2**70, Object, 1..2, bad, str, mm, eq, eqmm].map do |x|
          [t { "a.bc".match?(x) }, t { "a.bc".match(x).to_a }, t { "a.bc" =~ x }, t { "a.bc".scan(x) },
           t { "a.bc".partition(x) }, t { "a.bc".rpartition(x) }, t { "a.bc".sub(x, "_") }, t { "a.bc".gsub(x, "_") },
           t { "a.bc".split(x) }, t { "a.bc".index(x) }, t { "a.bc".rindex(x) }, t { "a.bc".match(x, 1).to_a },
           t { "a.bc".match?(x, 1) }, t { :"a.bc".match?(x) }, t { :"a.bc".match(x).to_a }, t { :"a.bc" =~ x }]
        end
        "##,
    );
}

#[test]
fn index_argument_errors() {
    // `NUM2LONG` on what is no Integer, Range, Regexp or String: nil has
    // a message of its own, and a String with a length is no index.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        [nil, true, 1.9, 2**70, Object, :a].map { |x| [t { "abc"[x] }, t { "abc".slice!(x) }, t { "abc".slice(x) }] } +
          [t { "abc"["b", 1] }, t { "abc".slice!("b", 1) }, t { "abc".slice!("b") }, t { "abc".slice!("z") }]
        "##,
    );
}
