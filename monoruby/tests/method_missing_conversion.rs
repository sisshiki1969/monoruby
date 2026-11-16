extern crate monoruby;
use monoruby::tests::*;

// Issue #1976: an implicit conversion asks the object through CRuby's
// `rb_check_funcall` — `respond_to?`, then the method, and a
// `method_missing` that answers the conversion's name counts, with or
// without a `respond_to_missing?` beside it.

#[test]
fn conversions_through_method_missing() {
    // Each row runs one conversion site against an object whose only
    // way to answer `to_str` / `to_ary` / `to_int` / `to_hash` / `to_f`
    // / `to_path` is `method_missing`: once with `respond_to_missing?`
    // defined, once without. The answers that are not of the wanted
    // class (`bad`, `nil`) pin the error wording of the two conversion
    // families (`rb_convert_type` / `rb_check_convert_type`).
    run_test_once(
        r##"
        def t; r = yield; r.is_a?(String) && r.encoding != Encoding::UTF_8 ? [r.bytes, r.encoding.to_s] : r; rescue Exception => e; "#{e.class}: #{e.message}"; end
        def mm(name, val, rtm = true)
          c = Class.new {
            define_method(:method_missing) { |n, *a| n == name ? val : super(n, *a) }
            define_method(:respond_to_missing?) { |n, p = false| n == name || super(n, p) } if rtm
          }
          Object.const_set("MM#{name.to_s.capitalize.delete('_')}#{rtm ? 1 : 0}#{$n = $n.to_i + 1}", c)
          c.new
        end
        out = []
        out = []
        [true, false].each do |rtm|
        s = mm(:to_str, "b", rtm)
        a = mm(:to_ary, [1, 2], rtm)
        i = mm(:to_int, 1, rtm)
        h = mm(:to_hash, {a: 1}, rtm)
        f = mm(:to_f, 1.5, rtm)
        pa = mm(:to_path, "/tmp", rtm)
        sy = mm(:to_sym, :abc, rtm)
        bad = mm(:to_str, 1, rtm)
        nilstr = mm(:to_str, nil, rtm)
        tests = {
         "include?" => -> { "abc".include?(s) }, "+" => -> { "a" + s }, "start_with?" => -> { "abc".start_with?(s) },
         "end_with?" => -> { "abc".end_with?(s) },
         "count" => -> { "abc".count(s) }, "delete" => -> { "abc".delete(s) }, "String()" => -> { String(s) },
         "join" => -> { ["a", "c"].join(s) }, "tr" => -> { "abc".tr(s, "x") }, "<=>" => -> { "abc" <=> s },
         "casecmp?" => -> { "abc".casecmp?(s) }, "casecmp" => -> { "abc".casecmp(s) }, "==" => -> { "b" == s }, "===" => -> { "b" === s },
         "eql?" => -> { "b".eql?(s) }, "<<" => -> { "a" << s }, "concat" => -> { "a".concat(s) }, "prepend" => -> { "a".prepend(s) },
         "replace" => -> { "a".dup.replace(s) }, "insert" => -> { "ac".insert(1, s) }, "squeeze" => -> { "abbc".squeeze(s) },
         "delete_prefix" => -> { "bc".delete_prefix(s) }, "delete_suffix" => -> { "ab".delete_suffix(s) },
         "center" => -> { "a".center(5, s) }, "ljust" => -> { "a".ljust(3, s) }, "rjust" => -> { "a".rjust(3, s) },
         "each_line" => -> { "abc".each_line(s).to_a }, "lines" => -> { "abc".lines(s) }, "chomp" => -> { "ab".chomp(s) },
         "tr_s" => -> { "abbc".tr_s(s, "x") }, "unpack" => -> { "b".unpack(mm(:to_str, "a", rtm)) },
         "crypt" => -> { "ab".crypt(mm(:to_str, "ab", rtm)) }, "%" => -> { "%s" % s }, "format" => -> { format(mm(:to_str, "%d", rtm), 1) },
         "between?" => -> { "b".between?("a", s) }, "encode" => -> { "a".encode(mm(:to_str, "UTF-16LE", rtm)).bytes },
         "force_encoding" => -> { "a".force_encoding(mm(:to_str, "BINARY", rtm)).encoding.to_s },
         "unicode_normalize" => -> { "a".byteindex(s) }, "byterindex" => -> { "ab".byterindex(s) },
         "str[]" => -> { "abc"[s] }, "str[]=" => -> { x = "abc"; x[s] = "z"; x }, "slice!" => -> { x = "abc"; x.slice!(s) },
         "str bad" => -> { "abc".include?(bad) }, "str nil" => -> { "abc".include?(nilstr) }, "<=> bad" => -> { "abc" <=> bad },
         "<=> nil" => -> { "abc" <=> nilstr }, "join bad" => -> { ["a"].join(bad) }, "+ nil" => -> { "a" + nilstr },
         "sym ==" => -> { :b.to_proc rescue 1; :abc.start_with?(s) }, "Symbol[]" => -> { :abc[s] },
         "Array +" => -> { [0] + a }, "Array concat" => -> { [0].concat(a) }, "flatten" => -> { [0, a].flatten },
         "Array ==" => -> { [1, 2] == a }, "Array <=>" => -> { [1, 2] <=> a }, "Array &" => -> { [1] & a }, "Array |" => -> { [1] | a },
         "Array -" => -> { [1, 3] - a }, "Array replace" => -> { [0].replace(a) }, "Array()" => -> { Array(a) },
         "zip" => -> { [5, 6].zip(a) }, "product" => -> { [5].product(a) }, "transpose" => -> { [a, [3, 4]].transpose },
         "masgn" => -> { x, y = a; [x, y] }, "splat" => -> { [*a] }, "block arg" => -> { [a].map { |x, y| [x, y] } },
         "Array.new" => -> { Array.new(a) }, "assoc" => -> { [a].assoc(1) }, "to_h" => -> { [a].to_h }, "Hash[]" => -> { Hash[[a]] },
         "Array.try_convert" => -> { Array.try_convert(a) }, "String.try_convert" => -> { String.try_convert(s) },
         "Hash.try_convert" => -> { Hash.try_convert(h) }, "first" => -> { [1, 2, 3].first(i) }, "Array#[]" => -> { [1, 2, 3][i] },
         "Array#*" => -> { [1] * i }, "Array#* str" => -> { [1, 2] * s }, "str *" => -> { "a" * i }, "Integer +" => -> { 1 + i },
         "Integer()" => -> { Integer(i) }, "Float()" => -> { Float(f) }, "Math.sqrt" => -> { Math.sqrt(f) }, "sleep" => -> { 1.0 + f rescue "x" },
         "Array#pack" => -> { [i].pack("C").bytes }, "chr" => -> { "abc"[i] }, "times" => -> { "abc".center(5, "x" * i) },
         "rotate" => -> { [1, 2, 3].rotate(i) }, "round" => -> { 1.234.round(i) }, "<<int" => -> { 1 << i }, "Integer#[]" => -> { 5[i] },
         "Hash merge" => -> { {b: 2}.merge(h) }, "Hash update" => -> { {b: 2}.update(h) }, "Hash replace" => -> { {b: 2}.replace(h) },
         "Hash()" => -> { Hash(h) }, "Hash ==" => -> { {a: 1} == h }, "**" => -> { {**h} }, "kw call" => -> { ->(**k) { k }.(**h) },
         "Hash[] h" => -> { Hash[h] }, "Hash.new" => -> { {a: 1}.any?; {a: 2}.merge!(h) },
         "File.join" => -> { File.join("a", s) }, "File.basename" => -> { File.basename(pa) }, "File.exist?" => -> { File.exist?(pa) },
         "File.expand_path" => -> { File.expand_path(pa) }, "Dir.exist?" => -> { Dir.exist?(pa) }, "File.basename s" => -> { File.basename(mm(:to_str, "/x/y", rtm)) },
         "send" => -> { "abc".send(s) rescue $!.class }, "respond_to?" => -> { "abc".respond_to?(mm(:to_str, "size", rtm)) },
         "instance_variable_get" => -> { Object.new.instance_variable_get(mm(:to_str, "@a", rtm)) },
         "const_get" => -> { Object.const_get(mm(:to_str, "String", rtm)) }, "method" => -> { "a".method(mm(:to_str, "size", rtm)).name },
         "define_method" => -> { Class.new { define_method(mm(:to_str, "zz", rtm)) { 1 } }.new.zz },
         "Regexp.new" => -> { Regexp.new(s).source }, "Regexp.union" => -> { Regexp.union(s, "c").source }, "Regexp.escape" => -> { Regexp.escape(s) },
         "=~" => -> { /b/ =~ s }, "match" => -> { /b/.match(s).to_a }, "match?" => -> { /b/.match?(s) }, "Regexp ===" => -> { /b/ === s },
         "Integer(str)" => -> { Integer(mm(:to_str, "12", rtm)) }, "Float(str)" => -> { Float(mm(:to_str, "1.5", rtm)) },
         "Rational" => -> { Rational(mm(:to_str, "1/2", rtm)) }, "Complex" => -> { Complex(mm(:to_str, "1+2i", rtm)) },
         "eval" => -> { eval(mm(:to_str, "1+1", rtm)) }, "Symbol <=>" => -> { :a <=> s }, "Comparable" => -> { "a".clamp("a", s) },
         "Encoding.find" => -> { Encoding.find(mm(:to_str, "utf-8", rtm)).to_s }, "Time.at" => -> { Time.at(i).to_i },
         "Integer#pow" => -> { 2.pow(i) }, "Integer#digits" => -> { 12.digits(10 * i) },
         "rand" => -> { rand(i) }, "Kernel#Array hash" => -> { Array(h) }, "puts" => -> { $stdout.sync; [a].flatten(i) },
         "Struct" => -> { Struct.new(:a).new(1)[mm(:to_str, "a", rtm)] }, "Struct int" => -> { Struct.new(:a, :b).new(1, 2)[i] },
         "sprintf %d" => -> { "%d" % [i] }, "sprintf %f" => -> { "%.1f" % [f] }, "ENV" => -> { ENV[mm(:to_str, "HOME", rtm)].class },
         "Array#fill" => -> { [1, 2, 3].fill(0, i) }, "Array#insert" => -> { [1, 2].insert(i, 9) }, "String#[]i,i" => -> { "abc"[i, i] },
         "to_sym send" => -> { "a".public_send(mm(:to_str, "upcase", rtm)) }, "Symbol ==" => -> { :b == s },
         "start_with multi" => -> { "abc".start_with?("x", mm(:to_str, "a", rtm)) }, "partition" => -> { "abc".partition(s) },
         "rpartition" => -> { "abc".rpartition(s) }, "split" => -> { "abc".split(s) }, "sub" => -> { "abc".sub(s, "x") }, "gsub hash" => -> { "abc".gsub(s, s) },
         "sub repl" => -> { "abc".sub("b", s) }, "index" => -> { "abc".index(s) }, "scan" => -> { "abc".scan(s) }, "sum" => -> { ["a"].sum(s) rescue $!.class },
         "IO.read" => -> { File.read(mm(:to_path, "/etc/passwd", rtm)).class }, "File.open str" => -> { File.open(mm(:to_str, "/etc/passwd", rtm)) { |x| x.class } },
         "require" => -> { require(mm(:to_str, "set", rtm)) }, "Dir.glob" => -> { Dir.glob(mm(:to_str, "/etc/passw?", rtm)) },
         "String#%ary" => -> { "%d %d" % a }, "Array#dig" => -> { [[1, 2]].dig(0, i) }, "values_at" => -> { [1, 2, 3].values_at(i) },
         "Array#sample" => -> { [1].sample(i) }, "Array#first bad" => -> { [1].first(mm(:to_int, "x", rtm)) }, "to_ary bad" => -> { [1] + mm(:to_ary, 1, rtm) },
         "to_ary nil" => -> { [1] + mm(:to_ary, nil, rtm) }, "flatten nil" => -> { [mm(:to_ary, nil, rtm)].flatten.size }, "flatten bad" => -> { [mm(:to_ary, 1, rtm)].flatten.size },
         "to_hash bad" => -> { {}.merge(mm(:to_hash, 1, rtm)) }, "to_hash nil" => -> { {}.merge(mm(:to_hash, nil, rtm)) }, "to_int nil" => -> { [1].first(mm(:to_int, nil, rtm)) },
         "to_f bad" => -> { Math.sqrt(mm(:to_f, "x", rtm)) }, "Float + f" => -> { 1.0 + f rescue $!.class }, "Integer * f" => -> { Float(mm(:to_f, 3, rtm)) },
         "to_sym" => -> { "abc".respond_to?(sy) rescue $!.class }, "intern send" => -> { 1.send(mm(:to_str, "+", rtm), 2) },
        }
        tests.each { |k, v| out << "#{rtm ? 'rtm' : 'mm '} #{k}: #{t(&v).inspect}".gsub(/0x\h+/, "0x").gsub("#<", "#(") }
        end
        out

        "##,
    );
}

#[test]
fn destructuring_through_method_missing() {
    // The three hot conversion probes — block parameters, multiple
    // assignment and splat — once the loop is compiled.
    run_test(
        r##"
        class MMAry
          def initialize(v) = @v = v
          def method_missing(n, *a) = n == :to_ary || n == :to_a ? @v : super
        end
        class MMAryR < MMAry
          def respond_to_missing?(n, p = false) = n == :to_ary || n == :to_a || super
        end
        class Plain; end
        res = []
        [MMAry, MMAryR].each do |k|
          o = k.new([1, 2])
          10.times do
            a, b = o
            res << [a, b] << [o].map { |x, y| [x, y] } << [*o].size
            res << [[o, 3]].each_with_index.map { |(p, q), i| [q, i] }
          end
        end
        pl = Plain.new
        10.times do
          a, b = pl
          res << b << [pl].map { |x, y| y } << [*pl].size
        end
        res.uniq.size
        "##,
    );
}

#[test]
fn regexp_operand_conversion() {
    // `reg_operand`: a Symbol is its name, nil is no match, anything
    // else is `rb_str_to_str`.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message}"; end
        [1, nil, :b, 1.5, [], Object].map do |x|
          [t { /b/ =~ x }, t { /b/.match(x).to_a }, t { /b/.match?(x) }, t { /b/ === x },
           t { /b/.match(x, 0).to_a }, t { /b/.match?(x, 0) }]
        end
        "##,
    );
}

#[test]
fn kernel_string_and_hash() {
    // `rb_String` asks `to_str` and then `to_s`; `rb_Hash` takes nil
    // and `[]` as `{}` and asks `to_hash` only.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; "#{e.class}: #{e.message.gsub(/0x\h+/, "0x").gsub("#<", "#(")}"; end
        class ToS; def to_s = "s"; end
        class BadToS; def to_s = 1; end
        class ToH; def to_hash = {a: 1}; end
        class BadToH; def to_hash = 1; end
        bo = BasicObject.new
        [
          t { String(1) }, t { String(nil) }, t { String(ToS.new) }, t { String(BadToS.new) },
          t { String(bo) }, t { String(:a) },
          t { Hash(nil) }, t { Hash([]) }, t { Hash({b: 2}) }, t { Hash(ToH.new) }, t { Hash(BadToH.new) },
          t { Hash([1]) }, t { Hash(1) }, t { Hash(bo) }, t { Hash("") },
        ]
        "##,
    );
}

#[test]
fn conversion_miss_memo_follows_definitions() {
    // A probe that found nothing is remembered per class
    // (`Store::no_conversion`); a method, a `method_missing`, a
    // `respond_to_missing?` or a singleton method defined afterwards
    // has to be seen by the next one.
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; e.class; end
        class Late; end
        o = Late.new
        probe = -> { a, b = o; [[o].flatten.size, b, [o].map { |x, y| y }, [*o].size, t { "a" + o }, t { [1, 2].first(o) }] }
        res = [probe.call, probe.call]
        class Late; def method_missing(n, *a) = n == :to_ary ? [1, 2] : super; end
        res << probe.call
        class Late; def respond_to_missing?(n, p = false) = false; end
        res << probe.call
        class Late
          remove_method :method_missing
          remove_method :respond_to_missing?
          def to_ary = [3, 4]
          def to_str = "s"
        end
        res << probe.call
        class Late; remove_method :to_ary; remove_method :to_str; end
        res << probe.call
        def o.to_int = 1
        res << probe.call
        # A second instance of the class has no singleton method.
        o = Late.new
        res << probe.call
        module LateConv; def to_ary = [5, 6]; end
        o.extend(LateConv)
        res << probe.call
        res
        "##,
    );
}
