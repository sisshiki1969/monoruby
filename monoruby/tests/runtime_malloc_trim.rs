extern crate monoruby;
use monoruby::tests::*;

// Paths whose temporary Rust allocations were trimmed for railsbench: the
// results must be unchanged.

#[test]
fn unpack_hex_without_format() {
    run_test(
        r##"
        s = "\x00\x0f\xa5\xff\x10"
        [s.unpack1("H*"), s.unpack1("h*"), s.unpack("H3"), s.unpack("H"),
         s.unpack("H20"), s.unpack("h5"), "".unpack("H*"),
         [s].pack("m"), [s * 20].pack("m"), [s * 20].pack("m0"), [s * 20].pack("m30")]
        "##,
    );
}

#[test]
fn symbol_to_s_and_encodings() {
    run_test(
        r##"
        [:abc.to_s, :abc.to_s.encoding.name, :"é".to_s, :"é".to_s.encoding.name,
         "\xff".b.to_sym.to_s.encoding.name, :abc.name.frozen?,
         Encoding::UTF_8.ascii_compatible?, Encoding::UTF_16LE.ascii_compatible?,
         Encoding::UTF_7.dummy?, Encoding::ISO_2022_JP.dummy?, Encoding::EUC_JP.dummy?]
        "##,
    );
}

#[test]
fn integer_to_s_decimal() {
    run_test(
        r##"
        [0.to_s, 7.to_s, -7.to_s, 1234567890.to_s, -1234567890.to_s,
         4611686018427387903.to_s, -4611686018427387904.to_s,
         4611686018427387904.to_s, 255.to_s(16), -255.to_s(2), 10.to_s.encoding.name]
        "##,
    );
}

#[test]
fn match_data_named_reference() {
    run_test(
        r##"
        m = /(?<a>x)(?<b>y)?(?<a>z)?/.match("xq")
        n = /(?<a>x)(?<a>z)?/.match("xz")
        r = [m[:a], m["a"], m[:b], n[:a], n["a"]]
        begin
          m[:nope]
        rescue IndexError => e
          r << e.message
        end
        r
        "##,
    );
}

#[test]
fn file_join_presized() {
    run_test(
        r##"
        [File.join("a", "b"), File.join("a/", "/b"), File.join("a//", "b"),
         File.join("a", ["b", ["c", "d/"]], "/e"), File.join, File.join([]),
         File.join("", ""), File.join("usr", "", "bin")]
        "##,
    );
}

#[test]
fn hash_promotion_presized() {
    run_test(
        r##"
        h = {a: 1, b: 2, c: 3}
        h[:d] = 4
        (5..20).each { |i| h["k#{i}"] = i }
        g = Hash.new { |hh, k| hh[k] = k.to_s }
        g[:x]
        i = {}.compare_by_identity
        i["a"] = 1
        [h.size, h.to_a.last, h[:d], g, i.size, i.compare_by_identity?]
        "##,
    );
}

#[test]
fn instance_variables_skip_hidden() {
    run_test(
        r##"
        S = Struct.new(:x)
        s = S.new(1)
        s.instance_variable_set(:@y, 2)
        o = Object.new
        o.instance_variable_set(:@a, 1)
        o.instance_variable_set(:@b, 2)
        [s.instance_variables, o.instance_variables]
        "##,
    );
}

#[test]
fn regexp_region_reentrant() {
    run_test(
        r##"
        s = "a1b22c333"
        t = s.gsub(/(\d+)/) { |d| d.gsub(/(\d)/) { "<#{$1}>" } + $1.to_s }
        u = s.sub(/([a-z])(\d)/) { "#{$2}#{$1}" }
        v = s.split(/(\d)/)
        w = s.scan(/([a-z])(\d+)/)
        x = s.gsub(/x/) { "never" }
        [t, u, v, w, x, $~]
        "##,
    );
}

#[test]
fn promoted_frames_and_procs() {
    run_test(
        r##"
        def mk(a, b)
          c = a + b
          [-> { a + b + c }, proc { |x| x + c }]
        end
        l, p = mk(1, 2)
        def outer
          x = 10
          [1, 2].map { |i| -> { i + x } }
        end
        [l.call, p.call(5), outer.map(&:call)]
        "##,
    );
}

#[test]
fn method_missing_args() {
    run_test_with_prelude(
        r##"
        t(C.new, [1, 2])
        "##,
        r##"
        class C
          def method_missing(name, *args, **kw, &blk)
            [name, args, kw, blk&.call]
          end
          def respond_to_missing?(*) = true
        end
        def t(o, a)
          [o.foo, o.bar(1), o.baz(*a, 3), o.qux(1, k: 2), o.quux(*a, z: 1) { 9 }]
        end
        "##,
    );
}

#[test]
fn string_tr_ranges() {
    run_test(
        r##"
        ["hello".tr("a-y", "b-z"), "hello".tr("^l", "*"), "a-b".tr("-", "_"),
         "hello".tr("el", "ip"), "hello".delete("l-o"), "hello".count("a-z")]
        "##,
    );
}
