//! Conformance details ruby/spec's `core` suite caught: keywords
//! documented as booleans that were read as truthy, a `try_convert` that
//! swallowed every error, two index conversions that raised the wrong
//! class, a codepoint appended without asking the encoding, a `concat`
//! that read its arguments as it went, a `warn(uplevel:)` that counted
//! block frames out, a redefined basic operation that said nothing, one
//! replaced through a mixin that nothing noticed at all, and an
//! `include` that landed above a `prepend`.

extern crate monoruby;
use monoruby::tests::*;

/// `exception:` and `highlight:` take `true` or `false` and nothing else
/// — `rb_bool_expected`. A truthiness test quietly accepted `0` and
/// `"false"`, which CRuby rejects.
#[test]
fn boolean_keywords_are_strict() {
    run_test(
        r##"
        res = []
        [0, "false", nil, :sym].each do |bad|
          [-> { Integer(1, exception: bad) },
           -> { Float(1, exception: bad) },
           -> { Rational(1, exception: bad) },
           -> { Complex(1, exception: bad) },
           -> { StandardError.new("x").detailed_message(highlight: bad) },
           -> { StandardError.new("x").full_message(highlight: bad) }].each do |probe|
            begin
              probe.call
              res << :no_error
            rescue ArgumentError => e
              res << e.message
            end
          end
        end
        res
        "##,
    );
    // The boolean values themselves still work, and so does leaving the
    // keyword out.
    run_test(
        r##"
        [Integer("1", exception: true), Integer("z", exception: false),
         Float("1.5", exception: true), Float("z", exception: false),
         Rational("1", exception: true), Rational("z", exception: false),
         Complex("1", exception: true), Complex("z", exception: false),
         Integer("1"), Float("1.5"),
         StandardError.new("x").detailed_message(highlight: false),
         StandardError.new("x").detailed_message(highlight: true),
         StandardError.new("x").detailed_message]
        "##,
    );
    // `system` is the exception to the rule: there, `nil` is the default.
    run_test(
        r##"
        res = []
        [1, "true"].each do |bad|
          begin
            system("true", exception: bad)
            res << :no_error
          rescue ArgumentError => e
            res << e.message
          end
        end
        res
        "##,
    );
}

/// `Warning.warn` writes nothing for a category that is switched off, and
/// still writes for one that is on — `rb_warning_s_warn` checks the
/// category before writing anything.
#[test]
fn warning_categories_gate_the_default_warn() {
    run_test_once(
        r##"
        require "stringio"
        def capture
          old = $stderr
          $stderr = StringIO.new
          yield
          $stderr.string
        ensure
          $stderr = old
        end
        res = []
        Warning[:deprecated] = false
        res << capture { Warning.warn("off\n", category: :deprecated) }
        Warning[:deprecated] = true
        res << capture { Warning.warn("on\n", category: :deprecated) }
        Warning[:deprecated] = false
        # No category at all always writes.
        res << capture { Warning.warn("plain\n") }
        # An unknown category, and a non-Symbol one, are errors.
        [:no_such_category, "deprecated"].each do |bad|
          begin
            Warning.warn("x", category: bad)
            res << :no_error
          rescue => e
            res << [e.class.to_s, e.message]
          end
        end
        res
        "##,
    );
}

/// `String.try_convert` answers `nil` only for an object with no
/// `#to_str`. A `#to_str` that returns a non-String is a TypeError, and
/// one that raises propagates — both used to come back as `nil`.
#[test]
fn string_try_convert_reports_a_bad_to_str() {
    run_test(
        r##"
        no_to_str = Object.new
        bad = Object.new
        def bad.to_str = 42
        raiser = Object.new
        def raiser.to_str = raise("boom")
        good = Object.new
        def good.to_str = "converted"
        [String.try_convert(no_to_str),
         String.try_convert("already"),
         String.try_convert(good),
         (begin; String.try_convert(bad); rescue TypeError => e; e.message; end),
         (begin; String.try_convert(raiser); rescue RuntimeError => e; e.message; end)]
        "##,
    );
}

/// An index too large for a machine word is a RangeError, whatever
/// container it indexes — `String#[]` reported it as a conversion error
/// about the class instead.
#[test]
fn out_of_range_indices_raise_range_error() {
    run_test(
        r##"
        big = 2 ** 64
        probes = [-> { "hello"[big] }, -> { "hello"[big, 1] }, -> { "hello"[-big, 1] },
                  -> { "hello".slice(big) }, -> { [1, 2][big] }]
        probes.map do |p|
          begin
            p.call
          rescue RangeError => e
            [e.class.to_s, e.message]
          end
        end
        "##,
    );
    // The ordinary conversion error for a non-Integer index is unchanged.
    run_test_error(r#""hello"[Object.new]"#);
}

/// A `#to_hash` / `#to_i` / `#to_str` that answers the wrong class is
/// reported with CRuby's wording — "can't convert X to Y (X#m gives Z)",
/// not "into".
#[test]
fn conversion_mismatch_says_to_not_into() {
    run_test(
        r##"
        bad = Object.new
        def bad.to_hash = 42
        def bad.to_str = 1.0
        def bad.to_ary = :sym
        # `Integer()` reaches `#to_str` before `#to_i`, so the `to_i`
        # mismatch needs an object with no `to_str` to be seen at all.
        int = Object.new
        def int.to_i = "x"
        probes = [-> { Hash.try_convert(bad) },
                  -> { {}.merge(bad) },
                  -> { {a: 1}.update(bad) },
                  -> { Integer(int) },
                  -> { String.try_convert(bad) },
                  -> { Array.try_convert(bad) }]
        probes.map do |p|
          begin
            p.call
          rescue TypeError => e
            e.message
          end
        end
        "##,
    );
}

/// `String#bytesplice` distinguishes its two argument shapes: a Range
/// boundary out of range is a RangeError naming the range, while the
/// (index, length) form keeps IndexError.
#[test]
fn bytesplice_range_boundaries() {
    run_test(
        r##"
        probes = [-> { "hello".bytesplice(-6...-6, "xxx") },
                  -> { "hello".bytesplice(0..1, "HELLO", -6...-6) },
                  -> { "hello".bytesplice(-6, 1, "xxx") }]
        errs = probes.map do |p|
          begin
            p.call
          rescue => e
            [e.class.to_s, e.message]
          end
        end
        # The shapes that are in range still splice.
        [errs, "hello".bytesplice(0..1, "xx"), "hello".bytesplice(1..2, "HELLO", 0..1),
         "hello".bytesplice(1..2, "HELLO", -5...-5), "hello".bytesplice(0...10, "x")]
        "##,
    );
}

/// `String#<<` with an Integer appends a *codepoint in the receiver's
/// encoding*, which is three different things: US-ASCII widens to
/// BINARY for a byte it cannot name, UTF-8 encodes, and a byte-oriented
/// or multibyte encoding refuses a codepoint it has no sequence for.
#[test]
fn shovelling_a_codepoint_respects_the_encoding() {
    run_test(
        r##"
        res = []
        # US-ASCII: 7-bit stays, 128..255 widens to BINARY, past that is
        # out of char range.
        a = "".encode(Encoding::US_ASCII)
        a << 65
        res << [a.encoding.to_s, a.bytes]
        a << 128
        res << [a.encoding.to_s, a.bytes]
        b = "".encode(Encoding::US_ASCII)
        b << 255
        res << [b.encoding.to_s, b.bytes]
        # UTF-8 encodes the codepoint.
        u = "".encode(Encoding::UTF_8)
        u << 0x203D
        res << [u.encoding.to_s, u.bytes]
        # EUC-JP: a lone lead byte is not a codepoint; the two-byte
        # plane is.
        # EUC-JP: ASCII, the 0x8E half-width-kana pair, the two-byte
        # JIS X 0208 plane, and the three-byte 0x8F plane.
        e = "".encode(Encoding::EUC_JP)
        e << 0x41
        e << 0x8EA1
        e << 0xA1A1
        e << 0x8FA1A1
        res << [e.encoding.to_s, e.bytes]
        # Shift_JIS: ASCII, half-width kana, and a two-byte pair.
        s = "".encode(Encoding::Windows_31J)
        s << 0x41
        s << 0xB1
        s << 0x8140
        res << [s.encoding.to_s, s.bytes]
        # BINARY takes any byte, and so does a byte-oriented encoding
        # monoruby has no codec for.
        n = "".b
        n << 0
        n << 255
        res << [n.encoding.to_s, n.bytes]
        i = "".encode(Encoding::ISO_8859_1)
        i << 0x41
        i << 0xFF
        res << [i.encoding.to_s, i.bytes]
        # The fixed-width Unicode forms encode code units, and refuse a
        # lone surrogate.
        [Encoding::UTF_16LE, Encoding::UTF_16BE,
         Encoding::UTF_32LE, Encoding::UTF_32BE].each do |enc|
          [0x41, 0x203D, 0x10348].each do |cp|
            t = "".encode(enc)
            t << cp
            res << [enc.to_s, cp, t.bytes]
          end
          begin
            "".encode(enc) << 0xD800
            res << :no_error
          rescue RangeError => ex
            res << [enc.to_s, ex.message]
          end
        end
        [["".encode(Encoding::US_ASCII), 256],
         ["".encode(Encoding::US_ASCII), -1],
         ["".encode(Encoding::EUC_JP), 0x81],
         ["".encode(Encoding::EUC_JP), 0x8EFF],
         ["".encode(Encoding::EUC_JP), 0xA100],
         ["".encode(Encoding::EUC_JP), 0x8F41A1],
         ["".encode(Encoding::Windows_31J), 0x80],
         # Past the widest sequence the encoding has at all, the message
         # is "out of char range" rather than "invalid codepoint".
         ["".encode(Encoding::Windows_31J), 0x10000],
         ["".encode(Encoding::EUC_JP), 0x100_0000],
         ["".encode(Encoding::ISO_8859_1), 256],
         ["".encode(Encoding::ISO_8859_5), 0x1234],
         ["".encode(Encoding::UTF_16LE), 0xDFFF],
         ["".encode(Encoding::UTF_32BE), 0x110000],
         ["".b, 256],
         ["".encode(Encoding::UTF_8), 0x110000],
         ["", 2 ** 64]].each do |str, cp|
          begin
            str << cp
            res << :no_error
          rescue RangeError => ex
            res << [ex.class.to_s, ex.message]
          end
        end
        res
        "##,
    );
}

/// `String#concat` with more than one argument gathers the parts first,
/// so an argument that *is* the receiver contributes what it held on
/// entry — `rb_str_concat_multi`. The single-argument and no-argument
/// shapes still go straight through.
#[test]
fn concat_snapshots_its_arguments() {
    run_test(
        r##"
        a = +"hello"
        a.concat a, a
        b = +"hello"
        b.concat b
        c = +"hello "
        d = +"x"
        [a, b, c.concat("wo", "", "rld"), d.concat.equal?(d), d,
         (+"").concat(33, 0x203D), (+"ab").concat("c", 100)]
        "##,
    );
}

/// `warn(uplevel:)` counts frames from the caller of `warn`, and only
/// then walks past a core-library frame. A block or lambda frame is a
/// frame like any other — counting it out was what put
/// `Enumerable#inject`'s "given block not used" on the wrong line.
#[test]
fn warn_uplevel_counts_block_frames() {
    run_test_once(
        r##"
        require "stringio"
        def capture
          old = $stderr
          $stderr = StringIO.new
          yield
          $stderr.string.gsub(__FILE__, "FILE")
        ensure
          $stderr = old
        end
        $VERBOSE = true
        class Numerous
          include Enumerable
          def initialize(*a) = @a = a
          def each(&b) = @a.each(&b)
        end
        def invoke(p) = p.call
        # The warning belongs to the lambda body, not to `invoke`.
        from_enumerable = -> { Numerous.new(1, 2, 3).inject(10, :-) { raise "unused" } }
        from_array = -> { [1, 2, 3].inject(10, :-) { raise "unused" } }
        res = []
        res << capture { res << invoke(from_enumerable) }
        res << capture { res << invoke(from_array) }
        # An explicit uplevel from a plain method chain is unchanged, and
        # one past the top of the stack still gets the bare prefix.
        def warner = warn("plain", uplevel: 1)
        def deep = warn("deep", uplevel: 99)
        res << capture { warner }
        res << capture { deep }
        res
        "##,
    );
}

/// Redefining a basic operation emits CRuby's `:performance` warning —
/// silent unless `Warning[:performance]` is on, and routed through
/// `Warning.warn` so an override sees it.
#[test]
fn redefining_a_basic_op_warns_when_asked() {
    run_test_once(
        r##"
        require "stringio"
        def capture
          old = $stderr
          $stderr = StringIO.new
          yield
          $stderr.string.gsub(__FILE__, "FILE")
        ensure
          $stderr = old
        end
        res = []
        # Off by default.
        res << capture do
          class Integer
            def *(o) = 1
          end
        end
        Warning[:performance] = true
        res << capture do
          class Integer
            def +(o) = 1
          end
        end
        # `define_method` counts too, and the message names the class.
        res << capture { Integer.define_method(:-) { |o| 2 } }
        # A method that is not a basic operation says nothing.
        res << capture do
          class Integer
            def not_an_op = 1
          end
        end
        res
        "##,
    );
}

/// A basic operation replaced through a *mixin* has to retire the fast
/// paths, the same as reopening the class does. The table records the
/// pair as `(Integer, :+)`, but the definition lands in the module, so
/// neither the definition-into-a-module case nor the mix-a-module-in
/// case had anything to mark it — and both tiers kept firing the
/// builtin.
#[test]
fn a_mixin_can_replace_a_basic_op() {
    // The module is mixed in first and gains the method afterwards.
    run_test_once(
        r##"
        module Later; end
        class Integer
          prepend Later
        end
        before = 1 + 2
        Later.module_eval do
          def +(o)
            $called = true
            super(o)
          end
        end
        [before, 1 + 2, $called]
        "##,
    );
    // ... and the other way round: the module already carries it when
    // it is spliced in.
    run_test_once(
        r##"
        module Already
          def *(o)
            $times = true
            super(o)
          end
        end
        before = 3 * 4
        class Integer
          prepend Already
        end
        [before, 3 * 4, $times]
        "##,
    );
    // A hot loop that compiled against the builtin has to come off it
    // too.
    run_test_once(
        r##"
        module Hot; end
        class Integer
          prepend Hot
        end
        def total(n)
          s = 0
          i = 0
          while i < n
            s = s + i
            i += 1
          end
          s
        end
        before = total(1000)
        Hot.module_eval { def +(o) = super(o) * 2 }
        [before, total(4)]
        "##,
    );
}

/// `include` splices below the class's own methods; only `prepend`
/// inserts at the head. With something already prepended, the class's
/// own table lives at its origin iclass, and the include has to start
/// there — starting at the head put the included module ahead of both
/// the prepends and the class itself.
#[test]
fn include_lands_below_a_prepend() {
    run_test(
        r##"
        module Pre
          def who = [:pre, super]
        end
        module Inc
          def who = :inc
          def only_here = :from_inc
        end
        class Base
          def who = :base
        end
        class Base
          prepend Pre
          include Inc
        end
        obj = Base.new
        [Base.ancestors.take(4).map(&:to_s), obj.who, obj.only_here,
         Base.instance_method(:who).owner.to_s]
        "##,
    );
    // The same on a class whose own method is a builtin, and with the
    // include arriving before the prepend.
    run_test(
        r##"
        module A1; def to_s = "a1"; end
        module A2; def to_s = "a2"; end
        class Thing
          include A1
          prepend A2
        end
        [Thing.ancestors.take(4).map(&:to_s), Thing.new.to_s]
        "##,
    );
}
