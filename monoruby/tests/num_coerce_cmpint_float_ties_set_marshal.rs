//! Numeric coercion as CRuby's `do_coerce`, `Comparable` as `rb_cmpint`,
//! the even digit of a `Float#to_s` tie, and `Set` on the Marshal wire
//! (#2001, #2003, #2012, #2022, #1661).
//!
//! - **`coerce`** (#2022). The numeric operators ask `rhs.coerce(lhs)`
//!   with `rb_check_funcall`: one answered through `method_missing`
//!   counts, a redefined `respond_to?` can veto one that exists, and an
//!   answer that is not a two-element Array is `TypeError: coerce must
//!   return [x, y]` — except for `<=>` and the relational operators,
//!   where a nil answer is "does not coerce" (`rb_num_coerce_cmp` /
//!   `rb_num_coerce_relop`).
//! - **`Integer#coerce` / `Float#coerce`** (#2003) convert through
//!   `rb_Float`, so a Rational argument is a pair of Floats;
//!   `divmod` / `Float#div` with an argument that is not a real number go
//!   through `coerce`, and `Float#divmod` is `flodivmod`.
//! - **`Comparable`** (#2001): `>`, `>=`, `<` and `<=` read the answer
//!   of `<=>` as `rb_cmpint` does: nil is the comparison failure, an
//!   Integer its sign, anything else is asked `> 0` and `< 0`.
//! - **`Float#to_s`** (#1661). A double exactly halfway between two
//!   shortest digit strings prints the even one, as `ruby_dtoa` does.
//! - **`Marshal`** (#2012) writes a Set as CRuby 4.0 does — an object
//!   with the one instance variable `@hash` — and reads that back.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

const COERCERS: &str = r#"
  class MM; def method_missing(n, *a); n == :coerce ? [a[0].to_f, 2.0] : super; end; def respond_to_missing?(n, p = false) = n == :coerce; end
  class Bad; def coerce(o) = :nope; end
  class NilC; def coerce(o) = nil; end
  class One; def coerce(o) = [1]; end
  class Veto; def coerce(o) = [o, 2]; def respond_to?(n, p = false) = n != :coerce; end
  class Priv; private def coerce(o) = [o, 2]; end
  class Rz; def coerce(o) = raise("boom"); end
  class NilRes; def coerce(o) = [NilRes2.new, 1]; end
  class NilRes2; %i[< > <= >= <=> + - * / ** % & | ^].each { |m| define_method(m) { |o| nil } }; end
  class NoC; end
  objs = [MM, Bad, NilC, One, Veto, Priv, Rz, NilRes, NoC]
"#;

fn probe(prelude: &str, exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n{prelude}\n[{list}]"));
}

/// The examples of #2022.
#[test]
fn coerce_through_check_funcall() {
    probe(
        COERCERS,
        &[
            "1 + MM.new",
            "1.5 * MM.new",
            "1 < MM.new",
            "2r + MM.new",
            "Complex(1, 2) + MM.new",
            "1 <=> Bad.new",
            "1 > Bad.new",
            "(2**70) <=> Bad.new",
            "1.5 <=> Bad.new",
            "2r <=> Bad.new",
            "1 <=> NilC.new",
            "1.5 <=> NilC.new",
            "1 < NilC.new",
            "1.5 >= NilC.new",
            "1 + NilC.new",
            "1 & NilRes.new",
            "1 | 1.5",
            "1 ^ NoC.new",
        ],
    );
}

/// Every arithmetic and comparison operator of the real number classes
/// against every kind of `coerce`.
#[test]
fn coerce_matrix() {
    run_test_once(&format!(
        r#"
        {HELPER}
        {COERCERS}
        recvs = [1, 1.5, 2**70, 2r]
        ops = %i[+ - * / ** % < > <= >= <=> & | ^ == quo fdiv div divmod modulo pow]
        res = []
        recvs.each do |r|
          objs.each do |oc|
            ops.each do |op|
              next unless r.respond_to?(op)
              res << t.() {{ r.send(op, oc.new) }}
            end
          end
        end
        res
        "#
    ));
}

/// Complex: the operators it has.
#[test]
fn coerce_matrix_complex() {
    run_test_once(&format!(
        r#"
        {HELPER}
        {COERCERS}
        c = Complex(1, 2)
        res = []
        objs.each do |oc|
          %i[+ - * / ** <=> == quo].each do |op|
            res << t.() {{ c.send(op, oc.new) }}
          end
        end
        res
        "#
    ));
}

/// The operators stay compiled: the same sites, warmed up.
#[test]
fn coerce_in_compiled_code() {
    run_test_with_prelude(
        r#"
        res = []
        m = MM.new
        b = Bad.new
        res << add(1, m) << add(1.5, m) << lt(1, m) << lt(1.5, m) << cmp(1, m) << cmp(2r, m)
        res << (cmp(1, b) rescue $!.message) << (lt(1.5, b) rescue $!.message)
        res << (add(1, NoC.new) rescue $!.message) << (lt(1, NoC.new) rescue $!.message)
        res
        "#,
        &format!(
            r#"
            {COERCERS}
            def add(a, b) = a + b
            def lt(a, b) = a < b
            def cmp(a, b) = a <=> b
            "#
        ),
    );
}

/// `rb_int_coerce` / `flo_coerce`: `rb_Float` on what is not an Integer
/// (#2003).
#[test]
fn integer_and_float_coerce() {
    run_test_once(&format!(
        r#"
        {HELPER}
        class TF; def to_f = 2.5; end
        class TFs; def to_f = "x"; end
        class MMf; def method_missing(n, *a) = n == :to_f ? 3.5 : super; def respond_to_missing?(n, p = false) = n == :to_f; end
        res = []
        [nil, true, false, :a, "1.5", " 2 ", "", "abc", "0x1A", "1_000", "1e3", 1r/3, Complex(2, 0),
         Complex(2, 1), Complex(2.0, 0.0), TF.new, TFs.new, MMf.new, Object.new, [1], 2**70, 1.5, 3, -0.0].each do |v|
          res << t.() {{ 3.coerce(v) }} << t.() {{ 1.5.coerce(v) }} << t.() {{ (2**70).coerce(v) }}
        end
        res
        "#
    ));
}

/// The two consequences #2003 names, and exact Rational parts through
/// `Complex#coerce`.
#[test]
fn coerce_consequences() {
    probe(
        "",
        &[
            "(1..2).step(1r/3).last(2)",
            "Complex(1, 2).coerce(1r/3)",
            "(1r/3) / Complex(1, 2)",
            "7.quo(Complex(1, 2))",
            "(2**70).quo(Complex(2, 0))",
        ],
    );
}

/// `div`, `divmod`, `%`, `modulo`, `fdiv`, `quo` of the real number
/// classes with each other, the infinities, NaN and the zeros.
#[test]
fn division_matrix() {
    run_test_once(&format!(
        r#"
        {HELPER}
        vals = [0, 1, -1, 7, -7, 2**70, -(2**70), 0.0, -0.0, 1.5, -2.5, Float::INFINITY, -Float::INFINITY,
                Float::NAN, 1e30, -1e30, 0r, 1r/3, -7r/2, 5r, Rational(2**70, 3), nil, "2", :a]
        res = []
        vals.each do |a|
          next unless a.is_a?(Numeric)
          ops = %i[div divmod % modulo quo]
          ops << :fdiv unless a.is_a?(Integer)
          ops << :remainder if a.is_a?(Rational)
          vals.each do |b|
            ops.each do |op|
              res << t.() {{ a.send(op, b) }}
            end
          end
        end
        res
        "#
    ));
}

/// `Float#divmod` keeps a quotient past the range of an `i64`.
#[test]
fn float_divmod_large_quotient() {
    run_test(
        r#"
        [(2.0**70).divmod(2.0), (2**70).divmod(2.0), 1e30.divmod(7), -1e30.divmod(7), 1e19.divmod(1),
         9.3e18.divmod(1.0), 7.divmod(2.5), -7.5.divmod(2), 7.5.divmod(-2.0), (2.0**70).div(2.0)]
        "#,
    );
}

/// The examples of #2001, every operator against every kind of answer.
#[test]
fn comparable_reads_cmp_as_cmpint() {
    run_test_once(&format!(
        r#"
        {HELPER}
        class A; include Comparable; def initialize(r) = @r = r; def <=>(o) = @r; end
        class Wd; def >(o) = false; def <(o) = true; end
        res = []
        [:>, :>=, :<, :<=].each do |op|
          ["x", Object.new, nil, 1.5, -0.5, Float::NAN, 2**70, -(2**70), 0, 1, -1, 1r, Wd.new, true].each do |r|
            res << t.() {{ A.new(r).send(op, A.new(r)) }}
          end
        end
        # `between?` and `clamp` compare the answer as it is (#2046).
        [:between?, :clamp].each do |op|
          ["x", nil, 1.5, -0.5, 2**70, -(2**70), 0, 1, -1, 1r].each do |r|
            res << t.() {{ v = A.new(r).send(op, A.new(r), A.new(r)); v.is_a?(A) ? :a : v }}
          end
        end
        res << t.() {{ 1.clamp(A.new("x"), A.new("x")) }}
        res << t.() {{ A.new(nil).clamp(1, 2) }} << t.() {{ A.new(nil).between?(1, 2) }}
        res << [1.clamp(nil, 3), 5.clamp(..3), 5.clamp(1..), A.new(0) == A.new(1), A.new(1) == A.new(0)]
        res << [Comparable.private_instance_methods(false), Comparable.instance_methods(false).sort]
        res
        "#
    ));
}

/// The Integer answer is the path that stays compiled.
#[test]
fn comparable_integer_answers() {
    run_test_with_prelude(
        r#"
        a, b, c = V.new(1), V.new(2), V.new(1)
        [a < b, a <= b, a > b, a >= b, a <= c, a >= c, b > a, a == c, a.between?(c, b), b.clamp(a, c).n,
         a.clamp(b, b).n, [b, a, c].sort.map(&:n), [a, b].max.n]
        "#,
        r#"
        class V
          include Comparable
          attr_reader :n
          def initialize(n) = @n = n
          def <=>(o) = n <=> o.n
        end
        "#,
    );
}

/// The values of #1661 and #1912, and their neighbours.
#[test]
fn float_to_s_tie_is_even() {
    run_test(
        r#"
        [23855792383581.3125, 11967817989581.3125, -23855792383581.3125, 0.159824371337890625,
         0.027414321899414062, 0.5, 2.5, 0.125, 0.3125, 4.35, 1e22, 1e23, 5e-324, 9007199254740993.0,
         1.7976931348623157e308, 2.2250738585072014e-308, 0.1, 0.3, 1.0, 100.0, 1e15, 1e16, 123456789.125,
         8.5, 0.0625, 3002399751580331.5, 1.5e-5, 9.5e-5].map { |f| [f.to_s, f.inspect, "x#{f}"] }
        "#,
    );
}

/// A deterministic sample of doubles `n / 2**k`, where about one in
/// eighty is such a tie, and of doubles built to sit on a decimal
/// midpoint.
#[test]
fn float_to_s_sample() {
    run_test_once(
        r#"
        s = 12345
        rnd = -> { s = (s * 6364136223846793005 + 1442695040888963407) & 0xffff_ffff_ffff_ffff; s >> 11 }
        out = []
        6000.times do |i|
          k = rnd.() % 60
          n = rnd.() % (1 << (10 + rnd.() % 50))
          f = n.to_f / (1 << k)
          f = -f if i % 7 == 0
          out << f.to_s
        end
        2000.times do
          bits = (rnd.() << 11) ^ rnd.()
          out << [bits & 0xffff_ffff_ffff_ffff].pack("Q").unpack1("D").to_s
        end
        2000.times do
          d = rnd.() % 14 + 1
          n = rnd.() % (10**d)
          k = rnd.() % 20 + 1
          f = (n * 10 + 5).to_f / 10**(rnd.() % (d + 1) + 1) + (rnd.() % 1000) * (1 << (rnd.() % 40))
          out << f.to_s << (f / (1 << k)).to_s
        end
        ties = out.select { |x| x =~ /\.\d*[02468]\z/ }.size
        [out.size, ties, out.join(",").sum, out.each_slice(40).map { |a| a.join.sum & 0xff }]
        "#,
    );
}

/// CRuby's bytes for a Set, and what it reads back (#2012).
#[test]
fn set_dump_is_an_object_with_a_hash() {
    probe(
        "class MySet < Set; end",
        &[
            "Marshal.dump(Set[1])",
            "Marshal.dump(Set[])",
            "Marshal.dump(Set.new) == Marshal.dump(Set[])",
            "Marshal.dump(Set[1, :b, [2], 1.5, nil])",
            "Marshal.dump(Set[1].compare_by_identity)",
            "Marshal.dump(MySet[1])",
            "s = Set[1]; s.instance_variable_set(:@a, 5); Marshal.dump(s)",
            "s = MySet[1]; s.instance_variable_set(:@a, 5); Marshal.dump(s)",
            "s = Set[1]; s.instance_variable_set(:@hash, 3); Marshal.dump(s)",
            "Marshal.dump(Set[1].freeze)",
            "s = Set[1]; s.extend(Comparable); Marshal.dump(s)",
            "s = Set[1]; Marshal.dump([s, s])",
            "a = [1]; s = Set[a]; Marshal.dump([s, a, s, a])",
            "Marshal.dump(Set[Set[1], Set[2]])",
            "s = Set[1]; def s.foo; end; Marshal.dump(s)",
            "Marshal.dump(Class.new(Set)[1])",
            "Marshal.dump(Set[1, 2], 1)",
            "Marshal.dump(Set[1, 2], 2)",
            "Marshal.dump(Set[1, 2], 3)",
            "Marshal.dump(Set[[1]], 3)",
            "Marshal.dump(Set[[1]], 4)",
            "Marshal.dump(Set[], 1)",
            "Marshal.dump(Set[], 2)",
            "Marshal.dump({ Set[1] => Set[2] })",
        ],
    );
}

#[test]
fn set_load() {
    probe(
        "class MySet < Set; end",
        &[
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hash}\x06i\x06TF".b)"#,
            r#"Marshal.load("\x04\bC:\bSet{\x06i\x06T".b)"#,
            r#"Marshal.load("\x04\bC:\nMySet{\x06i\x06T".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x00".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hashi\x06".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hash[\x00".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hash{\x06i\x06i\x07".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hash}\x06i\x06i\x00F".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hash{\x06i\x06T".b)"#,
            r#"Marshal.load("\x04\bo:\bSet\x07:\n@hash}\x06i\x06TF:\a@ai\n".b).then { [_1, _1.instance_variables] }"#,
            r#"Marshal.load("\x04\bo:\bSet\x07:\a@ai\n:\n@hash}\x06i\x06TF".b).then { [_1, _1.instance_variables] }"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\a@ai\n".b)"#,
            r#"Marshal.load("\x04\bIo:\bSet\x06:\n@hash}\x06i\x06TF\x06:\a@ai\n".b).then { [_1, _1.instance_variables, _1.instance_variable_get(:@a)] }"#,
            r#"Marshal.load("\x04\bo:\bSet\x06:\n@hashC:\tHash}\x06i\x06TF".b).compare_by_identity?"#,
            r#"Marshal.load("\x04\be:\x0FComparableo:\bSet\x06:\n@hash}\x06i\x06TF".b).singleton_class.include?(Comparable)"#,
            r#"Marshal.load("\x04\bo:\nMySet\x06:\n@hash}\x06i\x06TF".b).then { [_1, _1.class] }"#,
        ],
    );
}

#[test]
fn set_round_trip() {
    probe(
        "class MySet < Set; end",
        &[
            "s = Marshal.load(Marshal.dump(Set[1].compare_by_identity)); [s, s.compare_by_identity?]",
            "s = Marshal.load(Marshal.dump(MySet[1, 2])); [s, s.class, s.size, s.include?(2)]",
            "s = Marshal.load(Marshal.dump(Set[1, 2]), freeze: true); [s, s.frozen?]",
            "Marshal.load(Marshal.dump(Set[1, 2].freeze)).frozen?",
            "Marshal.load(Marshal.dump(Set[Set[1], Set[2]]))",
            "s = Set[1]; s << s; Marshal.load(Marshal.dump(s)).size",
            "a = [1]; s = Set[a]; r = Marshal.load(Marshal.dump([s, a, s, a])); [r, r[0].equal?(r[2]), r[1].equal?(r[3]), r[0].first.equal?(r[1])]",
            "Marshal.load(Marshal.dump(Set[1, 2]), ->(o) { o.is_a?(Integer) ? o * 2 : o })",
            "r = []; Marshal.load(Marshal.dump(Set[1, 2]), ->(o) { r << o.class; o }); r",
            "Marshal.load(Marshal.dump({ Set[1] => Set[2] }))",
            "s = Set[1, 2]; s.instance_variable_set(:@a, [s]); r = Marshal.load(Marshal.dump(s)); [r, r.instance_variable_get(:@a)[0].equal?(r)]",
            "Marshal.load(Marshal.dump(Set[*1..300])).size",
        ],
    );
}
