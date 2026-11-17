//! A zero Float divisor of `%` in compiled code, the duplicated-key
//! warning for keyword arguments, `Enumerator::Chain` / `Product` as
//! receivers of `with_index` and friends, and a few neighbours
//! (#2042, #2041, #2038, #2037, the `fdiv` half of #2045).
//!
//! - **`%`** (#2042). `Float#%` and `Integer#%` with a Float divisor are
//!   an inline C call under the JIT; a zero divisor has to leave it for
//!   the builtin, which raises ZeroDivisionError (`flodivmod`). The
//!   remainder itself is `flodivmod`'s too: the divisor is added when
//!   `y * mod < 0`, so a product that underflows leaves `fmod`'s answer.
//! - **Duplicated keys** (#2041). The warning is prism's
//!   `pm_hash_key_static_literals_add`, for the keyword list of a call as
//!   for a hash literal: reported at the overwritten key with the line
//!   of the overwriting one, once per pair, a `**{...}` operand sharing
//!   the set of the list around it.
//! - **Chain / Product** (#2038) are Enumerators by class only:
//!   `with_index { }`, `each_with_index { }`, `with_object { }` raise
//!   TypeError, external iteration is undefined, and an Enumerator made
//!   of a Chain's method is over `chain.each`.
//! - **`0.0` and `-0.0`** are `eql?` and so one Hash key.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

#[test]
fn float_modulo_by_zero_raises_in_compiled_code() {
    // Every site sees non-zero Float operands first, so it is compiled
    // to the inline call before the zero arrives.
    run_test_with_prelude(
        r##"
        rows = []
        40.times do |i|
          z = i == 39 ? 0.0 : 1.5
          zi = i == 39 ? 0 : 2
          nz = i == 39 ? -0.0 : -1.5
          rows = [
            t.() { fm(7.5, z) }, t.() { fm(7.5, nz) }, t.() { fmod(7.5, z) }, t.() { discard(7.5, z) },
            t.() { const_zero(7.5) }, t.() { const_int_zero(7.5) }, t.() { const_div(7.5) }, t.() { const_div(-7.5) },
            t.() { int_recv(z) }, t.() { int_recv(nz) }, t.() { int_discard(z) }, t.() { send_form(7.5, z) },
            t.() { send_form(7, z) }, t.() { fm(7.5, zi) }, t.() { fm(7, z) }, t.() { fm(-7, nz) },
            t.() { fm(7.5, Float::NAN) }, t.() { fm(Float::NAN, z) }, t.() { fm(Float::INFINITY, z) },
          ]
        end
        # The sites keep working after the raise.
        rows + [fm(7.5, 2.0), fm(7, 2.0), int_recv(2.0), discard(1.0, 3.0),
               100.times.sum { |i| fm(i.to_f, 7.0) + fm(i, 3.5) }]
        "##,
        &format!(
            r##"{HELPER}
        def fm(x, y) = x % y
        def fmod(x, y) = x.modulo(y)
        def discard(x, y) = (x % y; 1)
        def const_zero(x) = x % 0.0
        def const_int_zero(x) = x % 0
        def const_div(x) = x % 2.5
        def int_recv(x) = 7 % x
        def int_discard(x) = (7 % x; :ok)
        def send_form(x, y) = x.%(y)
        "##
        ),
    );
}

#[test]
fn float_modulo_by_zero_in_a_loop() {
    run_test_with_prelude(
        r##"
        [
          t.() { i = 0; while i < 200; i += 1; i % (200.0 - i); end; i },
          t.() { i = 0; x = 0.0; while i < 200; i += 1; x += i.to_f % (200.0 - i); end; x },
          t.() { x = 7.5; 30.times.map { x % 0.0 } },
          t.() { 30.times.map { 7 % 0.0 } },
          t.() { 30.times.map { 7.5.divmod(0.0) } },
        ]
        "##,
        HELPER,
    );
}

#[test]
fn float_modulo_is_flodivmod() {
    run_test(
        r##"
        [
          1e-200 % -3e-200, -1e-200 % 3e-200, 5e-324 % -1.0, 1e-320 % -1e-5,
          1 % -3e-200, 3 % -1e-320, (2**70) % -1e-300, 1e-200.modulo(-3e-200),
          -4.0 % 2.0, 4.0 % -2.0, 0.0 % -1.0, -0.0 % 1.0, 5.5 % -2.0, -5.5 % 2.0,
          5.5 % Float::INFINITY, -5.5 % Float::INFINITY, 5.5 % -Float::INFINITY,
          (Float::INFINITY % 2.0).nan?, (5.5 % Float::NAN).nan?,
          1e-200.divmod(-3e-200), 7.5.divmod(-2.0), 7.divmod(-2.5),
        ].map(&:to_s)
        "##,
    );
}

/// `src` evaluated as `dup.rb` from line 10, answering what it wrote to
/// `$stderr`.
const CAP: &str = r##"
  require 'stringio'
  def f(*a, **k) = [a, k]
  def cap(src)
    $stderr = StringIO.new
    eval(src, nil, "dup.rb", 10)
    $stderr.string.lines.map(&:chomp)
  ensure
    $stderr = STDERR
  end
"##;

#[test]
fn duplicated_keyword_warning() {
    run_test_with_prelude(
        r##"
        [
          cap("f(to: 5, by: 2, to: 10)"),
          cap("f(**{to: 5}, by: 2, to: 10)"),
          cap("f(to: 5,\n  by: 2,\n  to:\n    10)"),
          cap(%q[f("a": 1, a: 2)]),
          cap("f(a: 1, **{a: 2})"),
          cap("h = {a: 3}; f(a: 1, **h)"),
          cap("h = {a: 3}; f(a: 1, **h, a: 2)"),
          cap("f(a: 1, a: 2, a: 3)"),
          cap("f(**{a: 1}, **{a: 2})"),
          cap("f(a: 1, **{b: 1, **{a: 5}})"),
          cap("f(a: 1, **{a: 1, a: 2})"),
          cap("f(a: 1, **({a: 2}))"),
          cap("f(a: 1,\n  **{b: 1},\n  a: 3,\n  a: 4)"),
          cap("f(a: 1, **{a: 2}.merge({}))"),
          cap("f(1, a: 1, a: 2, &nil)"),
          cap("f(a: 1, a: 2) { }"),
          cap("f(if: 1, if: 2)"),
          cap(%q[f("a b": 1, "a b": 2)]),
          cap("y = 5; f(y:, y: 6)"),
          cap("pr = ->(**k) { k }; pr.(a: 1, a: 2); pr[b: 1, b: 2]"),
          cap("def g = yield(a: 1, a: 2); g { |**k| k }"),
          cap("def dk1 = f(q: 1, q: 2)"),
          cap("class DK1; def initialize(**k) = k; end; DK1.new(z: 1, z: 2)"),
          cap("[1, a: 1, a: 2]"),
          cap("f(a: 1, b: 2)"),
          cap("f(a: {a: 1}, b: {a: 1})"),
          cap("f(a: f(a: 1), b: f(a: 1))"),
          cap(%q[f(a: 1, "b" => 2, a: 3)]),
          cap("f(1 => 2, 1 => 3)"),
        ]
        "##,
        CAP,
    );
}

#[test]
fn duplicated_hash_key_warning_as_prism_reports_it() {
    run_test_with_prelude(
        r##"
        [
          cap("{a: 1,\n b: 2,\n a: 3}"),
          cap("{a: 1,\n **{a:\n   2}}"),
          cap("{a: 1, a: 2, a: 3}"),
          # Reported once, not once per enclosing literal.
          cap("{a: 1, **{b: 1, b: 2}}"),
          cap("{a: 1, **{b: 1, **{c: 1, c: 2}}}"),
          cap("{a: 1, **({a: 2})}"),
          cap("{a: 1, **{a: 2}.merge({a: 3, a: 4}).merge({})}"),
          # An `ensure` body is compiled once per exit and warns once.
          cap("begin; ensure; {z: 1, z: 2}; end"),
          cap("def dk2; yield; ensure; {y: 1, y: 2}; end; dk2 { }"),
          cap("[1, 2].each { |i| {w: 1, w: 2} }"),
          cap(%q["#{{v: 1, v: 2}}"]),
          # How each kind of key is compared and printed.
          cap(%q[{"あ" => 1, "あ" => 2}]),
          cap(%q[{'a#{x}#$y#z' => 1, 'a#{x}#$y#z' => 2}]),
          cap(%q[{"a\n\t\e\0\x7f\"\\\\" => 1, "a\n\t\e\0\x7f\"\\\\" => 2}]),
          cap(%q[{:"a\nb" => 1, :"a\nb" => 2, :あ => 1, あ: 2, :"foo=" => 1, "foo=": 2, :a => 1, "a": 2, :"a" => 3, "": 1, "": 2}]),
          cap("{1234567.5 => 1, 1234567.5 => 2, 1e100 => 1, 1e100 => 2, 1.5e-7 => 1, 1.5e-7 => 2, 0.0 => 1, -0.0 => 2, 100.0 => 1, 100.0 => 2, 0.1 => 1, 0.1 => 2, 123456.0 => 1, 123456.0 => 2, 1e-4 => 1, 1e-4 => 2, 1e-5 => 1, 1e-5 => 2, 999999.5 => 1, 999999.5 => 2, 0.00012345678 => 1, 0.00012345678 => 2}"),
          cap("{1.0 => 1, 1.00 => 2, 1e0 => 3, 1e20 => 1, 1e20 => 2}"),
          cap("{-1 => 1, -1 => 2, 0x10 => 1, 16 => 2, 1_000 => 1, 1000 => 2, 123456789012345678901234567890 => 1, 123456789012345678901234567890 => 2}"),
          cap("{1r => 1, 1r => 2, 0.75r => 1, 0.75r => 2, -3r => 1, -3r => 2, 2i => 1, 2i => 2, -2i => 1, -2i => 2, 1.5i => 1, 1.5i => 2, 0.0i => 1, 0.0i => 2, -1.5i => 1, -1.5i => 2, 12345678901234567890i => 1, 12345678901234567890i => 2}"),
          cap("{/a b/ix => 1, /a b/ix => 2, /a/ => 1, /a/i => 2, /\\// => 1, /\\// => 2, %r{x/y}m => 1, %r{x/y}m => 2}"),
          cap("{true => 1, true => 2, false => 1, false => 2, nil => 1, nil => 2}"),
          cap("{1 => 1, 1.0 => 2, 1r => 3, '1' => 4, :'1' => 5}"),
          cap("{__LINE__ => 1, 10 => 2, __FILE__ => 1, 'dup.rb' => 2}"),
          cap(%q[{"a" "b" => 1, "ab" => 2, 'ab' => 3, %q(ab) => 4, ?a => 1, "a" => 2}]),
          cap(%q[x = 1; {x => 1, x => 2, "#{x}" => 1, "#{x}" => 2, [1] => 1, [1] => 2, (1) => 1, (1) => 2}]),
        ]
        "##,
        CAP,
    );
}

#[test]
fn zero_and_negative_zero_are_one_hash_key() {
    run_test(
        r##"
        h = {}; h[0.0] = 1; h[-0.0] = 2
        g = {}; g[-0.0] = 1; g[0.0] = 2
        a = 0.0; b = -a
        id = {}.compare_by_identity; id[0.0] = 1; id[-0.0] = 2
        [
          h, g, {0.0 => 1, -0.0 => 2}, {a => 1, b => 2}, id.size,
          0.0.eql?(-0.0), 0.0.hash == -0.0.hash,
          [0.0, -0.0].uniq, [-0.0, 0.0].uniq, [0.0] - [-0.0], [-0.0] - [0.0],
          [0.0] & [-0.0], [0.0] | [-0.0], [0.0, -0.0].tally, [0.0, -0.0].group_by { _1 },
          {0.0 => 1}.key?(-0.0), {-0.0 => 1}[0.0], {-0.0 => 1}.fetch(0.0, :none),
          [[0.0], [-0.0]].uniq, {[0.0] => 1}[[-0.0]], [0.0, 1.5].eql?([-0.0, 1.5]),
          {1.5 => 1}[-1.5], {Float::NAN => 1}[Float::NAN], {1e300 * 10 => 1}[1e300 * 10],
          {2.0**70 => 1}[2.0**70], {-0.0 => 1}[0], {0 => 1}[-0.0],
        ].map(&:inspect)
        "##,
    );
    run_test(
        r##"
        require 'set'
        s = Set[0.0, -0.0]
        [s.size, s.include?(-0.0), Set[-0.0].include?(0.0), Set[-0.0] == Set[0.0]]
        "##,
    );
}

const ENUMS: &str = r##"
  c = [1, 2].each + [3]
  pr = Enumerator.product([1, 2], [3])
"##;

#[test]
fn chain_and_product_are_not_enumerators_by_payload() {
    run_test_with_prelude(
        r##"
        [c, pr].flat_map do |e|
          [
            t.() { e.with_index { } }, t.() { e.with_index(1) { } }, t.() { e.each_with_index { } },
            t.() { e.with_object(1) { } }, t.() { e.each_with_object([]) { |x, a| a << x } },
            t.() { e.each_with_index(1) }, t.() { e.with_object },
            t.() { e.with_index }, t.() { e.with_index(1) }, t.() { e.with_index("x") },
            t.() { e.each_with_index }, t.() { e.with_object(1) }, t.() { e.each_with_object([]) },
            t.() { e.with_index.to_a }, t.() { e.each_with_index.to_a }, t.() { e.with_object(1).to_a },
            t.() { e.each_with_object([]).to_a },
            t.() { e.next }, t.() { e.peek }, t.() { e.next_values }, t.() { e.peek_values }, t.() { e.feed 1 },
            %i[next peek next_values peek_values feed with_index each_with_index with_object rewind size].map { |m| e.respond_to?(m) },
            t.() { e.rewind.equal?(e) }, t.() { e.size },
            t.() { e.each_slice(2).to_a }, t.() { e.each_entry.to_a }, t.() { e.lazy.map { _1 }.first(2) },
            t.() { (e + [9]).to_a }, t.() { e.dup.to_a }, t.() { e.dup.with_index { } }, t.() { e.clone.with_object(1) { } },
            t.() { e.map.with_index { |x, i| [x, i] } }, t.() { e.each.with_index { |x, i| [x, i] }.class },
            t.() { e.to_enum.with_index.to_a }, t.() { e.each.next }, t.() { e.to_enum.take(2) },
            t.() { [7, 8].zip(e) }, t.() { e.lazy.zip(e).first(2) },
            t.() { e.sum { |x| x.is_a?(Array) ? x.sum : x } }, t.() { e.first }, t.() { e.first(2) }, t.() { e.count },
            t.() { e.each_cons(2).to_a }, t.() { e.chain([4]).to_a },
            e.class.instance_methods(false).sort,
            %i[each_with_index with_index with_object each_with_object].map { |m| e.method(m).owner },
            e.class.ancestors.take(3),
            t.() { e.class.instance_method(:next) }, e.class.method_defined?(:next), e.class.public_method_defined?(:peek),
            e.to_s.sub(/0x\h+/, "0x"),
          ]
        end
        "##,
        &format!("{HELPER}{ENUMS}"),
    );
}

#[test]
fn enumerator_of_a_chain_method_is_over_chain_each() {
    run_test_with_prelude(
        r##"
        sub = Class.new(Enumerator::Chain).new([1], [2])
        [
          c.each, c.map, c.each_slice(2), c.to_enum, c.to_enum(:foo, 1), c.enum_for, c.each_entry, c.lazy,
          c.with_index, c.each_with_index, c.with_object(:m), c.select, c.each_cons(2),
          pr.each, pr.map, pr.to_enum, pr.with_index, pr.each_with_index,
          sub.each, sub.with_index,
          [1].lazy.to_s,
        ].map { _1.inspect.gsub(/0x\h+/, "0x") } + [
          t.() { sub.each_with_index { } }, t.() { sub.with_index.to_a }, t.() { sub.next },
          e = c.each_with_index, e.next, e.next, e.peek, (e.next; t.() { e.next }),
        ].map(&:inspect)
        "##,
        &format!("{HELPER}{ENUMS}"),
    );
}

#[test]
fn with_index_adapter_block_is_not_a_lambda() {
    run_test_with_prelude(
        r##"
        o.seen.clear
        o.to_enum(:each).with_index { }
        o.to_enum(:each).with_object(:m) { }
        o.to_enum(:each).each_with_index { }
        o.to_enum(:each).each_with_object(:m) { }
        o.to_enum(:each).with_index(3) { |a, b| }
        o.seen + [proc { }.lambda?, lambda { }.lambda?, :a.to_proc.lambda?, 1.method(:+).to_proc.lambda?]
        "##,
        r##"
        o = Object.new
        def o.seen = (@seen ||= [])
        def o.each(&b) = (seen << [b.arity, b.parameters, b.lambda?])
        "##,
    );
}

#[test]
fn integer_fdiv_converts_the_coerced_answer() {
    run_test_with_prelude(
        r##"
        [nil, 3, 2.5, 3r, "x", :s, TF.new, 2**70, Complex(1, 0), Complex(1, 2), true, Object.new, [1], 1.0 / 0].flat_map do |r|
          [t.() { 1.fdiv(NR.new(r)) }, t.() { (2**70).fdiv(NR.new(r)) }]
        end + [
          t.() { 1.fdiv(nil) }, t.() { 1.fdiv("a") }, t.() { (2**70).fdiv(:a) }, t.() { 1.fdiv(1r) },
          t.() { 3.fdiv(2r) }, t.() { (2**70).fdiv(3r) }, t.() { 7.fdiv(2) }, t.() { 7.fdiv(2.5) },
        ]
        "##,
        &format!(
            r##"{HELPER}
        class NR; def initialize(r) = @r = r; def coerce(o) = [NR2.new(@r), 1]; end
        class NR2; def initialize(r) = @r = r; def fdiv(o) = @r; end
        class TF; def to_f = 2.5; end
        "##
        ),
    );
}
