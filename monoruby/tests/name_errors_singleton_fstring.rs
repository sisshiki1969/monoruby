extern crate monoruby;
use monoruby::tests::*;

// Issue #1972 (3): the reflection methods that take a constant, class
// variable or attribute name raise CRuby's NameError — the message, and
// `#name` / `#receiver` (the name as it was given: a String stays a
// String unless a Symbol of that name exists).
#[test]
fn name_reflection_errors() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def attrs; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; [e.class, e.message, (e.name rescue :no_name), (e.receiver rescue :no_receiver)].inspect.gsub(/0x\h+/, "0x"); end
        class C; X = 1; end
        class TS; def to_str = "nope"; end
        ts = TS.new
        [:nope, "nope", :"a b", "A::", "", :"", "nope=", :X?, "@zq1972", :"@@x", "$zq1972", ts, "C::nope", "X::y", 1, nil, "λ"].each do |n|
          row "--- #{n.inspect.gsub(/0x\h+/, "0x")}"
          row attrs { C.send(:remove_const, n) }
          row attrs { C.send(:remove_class_variable, n) }
          row attrs { C.const_get(n) }
          row attrs { C.const_set(n, 1) }
          row attrs { C.const_defined?(n) }
          row attrs { C.const_source_location(n) }
          row attrs { C.send(:private_constant, n) }
          row attrs { C.send(:public_constant, n) }
          row attrs { C.send(:deprecate_constant, n) }
          row attrs { C.autoload(n, "zz") }
          row attrs { C.autoload?(n) }
          row attrs { C.class_variable_get(n) }
          row attrs { C.class_variable_set(n, 1) }
          row attrs { C.class_variable_defined?(n) }
          row attrs { C.send(:attr_accessor, n) }
          row attrs { C.send(:attr_reader, n) }
          row attrs { C.send(:attr_writer, n) }
          row attrs { C.send(:attr, n) }
          row attrs { C.send(:attr, n, true) }
          row attrs { Struct.new(n) } unless n == :""
          row attrs { Object.const_missing(n) }
        end
        $o
        "##,
    );
}

// Issue #1972 (2): a Range or a Complex gets a (frozen) singleton class
// like any other frozen object; only Integer, Float and Symbol have none.
#[test]
fn range_and_complex_have_singleton_classes() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end
        module M; def mm = 1; end
        [(1..2), Complex(1, 2), Rational(1, 2), 1.0e300, 2**70, "lit".freeze, :sym, 1, 1.5, nil, (1..2).step(1), Object.new.freeze].each do |o|
          row t { o.singleton_class }
          row t { o.singleton_class.frozen? }
          row t { def o.foo; end }
          row t { o.extend(M) }
          row t { o.define_singleton_method(:x) {} }
          row t { o.singleton_methods.sort }
          row t { class << o; self; end }
          row t { class << o; def bar; end; end }
          row t { o.instance_eval { def baz; end } }
          row t { (o.singleton_class.instance_methods(false) - [:===, :nil?]).sort }
          row t { o.class }
        end
        r = (1..3)
        r.singleton_class
        row t { r.include?(2) }
        row t { r.to_a }
        row t { (r === 2) }
        row t { case 2 when r then 1 end }
        row t { r.each { } }
        row t { r.sum }
        row t { r.map { |x| x * 2 } }
        row t { [1,2,3,4][r] }
        row t { "abcdef"[r] }
        row t { r == (1..3) }
        row t { r.hash == (1..3).hash }
        row t { r.eql?(1..3) }
        row t { r.dup.singleton_class.equal?(r.singleton_class) }
        row t { Marshal.load(Marshal.dump(r)) }
        c = Complex(1, 2); c.singleton_class
        row t { c + 1 }
        row t { c * c }
        row t { c == Complex(1, 2) }
        row t { 1 + c }
        row t { c.abs }
        30.times { r.include?(2); c + 1; [1,2,3,4][r] }
        row t { c.real }
        $o
        "##,
    );
}

// Issue #1972 (6): `Enumerable#take` / `#first` allocate their result up
// front in CRuby (`rb_ary_new2`), so a count no allocator can serve is a
// NoMemoryError; `Array#*` with such a count no longer aborts.
#[test]
fn result_array_too_large_to_allocate() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect[0, 60]; rescue Exception => e; "#{e.class}: #{e.message}"; end
        row t { [1,2,3].each_entry.take(2**60 - 1) }
        row t { [1,2,3].each_entry.take(2**40) }
        row t { [1,2,3].each_entry.take(2**36) }
        row t { [1,2,3].each_entry.take(2**33) }
        row t { [1,2,3].each_entry.first(2**60 - 1) }
        row t { [1,2,3].each_entry.first(2**40) }
        row t { [1,2,3].take(2**60 - 1) }
        row t { [1,2,3].first(2**60 - 1) }
        row t { (1..3).take(2**60 - 1) }
        row t { (1..3).first(2**60 - 1) }
        row t { [1,2,3].each_slice(2**60 - 1).to_a }
        row t { [1,2,3].lazy.take(2**60 - 1).to_a }
        row t { [1] * (2**40) }
        $o
        "##,
    );
}

// `Array#*` (`rb_ary_times`): a product past `ARY_MAX_SIZE` is
// "argument too big" and one the allocator refuses a NoMemoryError,
// whether the count is an Integer or arrives through `to_int`.
#[test]
fn array_times_too_big() {
    run_test2(
        r##"
        $o = []
        def t; yield.inspect[0, 60]; rescue Exception => e; "#{e.class}: #{e.message}"; end
        huge = Object.new
        def huge.to_int = 2**62
        big = Object.new
        def big.to_int = 2**40
        $o << t { [1, 2] * (2**62) }
        $o << t { [1, 2] * huge }
        $o << t { [] * (2**62) }
        $o << t { [] * huge }
        $o << t { [1] * big }
        $o << t { [1] * 3 }
        $o
        "##,
    );
}

// Issue #1972 (5): the copy a Hash makes of an unfrozen String key is the
// fstring of its content (`rb_hash_key_str` -> `rb_fstring`), the same
// object `-str` and a frozen literal answer.
#[test]
fn hash_key_copy_is_the_fstring() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; yield.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
        row t { h = {"k".dup => 1}; h.keys[0].equal?(-"k") }
        row t { h = {}; k = "abc".dup; h[k] = 1; h.keys[0].equal?(-"abc") }
        row t { h = {}; h["x1".dup] = 1; g = {}; g["x1".dup] = 2; h.keys[0].equal?(g.keys[0]) }
        row t { a = -"zz1".dup; h = {}; h["zz1".dup] = 1; h.keys[0].equal?(a) }
        row t { h = {}; k = "fr".dup.freeze; h[k] = 1; [h.keys[0].equal?(k), k.equal?(-"fr")] }
        row t { h = {}.compare_by_identity; k = "id".dup; h[k] = 1; [h.keys[0].equal?(k), h.keys[0].frozen?] }
        row t { h = {}; k = "bin\xff".b; h[k] = 1; [h.keys[0].encoding, h.keys[0].equal?(-"bin\xff".b), h.keys[0].equal?(-"bin\xff")] }
        row t { h = {}; k = "sing".dup; def k.foo = 1; h[k] = 1; [h.keys[0].equal?(-"sing"), h.keys[0].frozen?] }
        row t { h = {}; k = "iv".dup; k.instance_variable_set(:@a, 1); h[k] = 1; [h.keys[0].equal?(-"iv"), h.keys[0].frozen?, h.keys[0].instance_variables] }
        row t { c = Class.new(String); h = {}; k = c.new("sub"); h[k] = 1; [h.keys[0].equal?(k), h.keys[0].frozen?] }
        row t { h = {}; 1000.times { |i| h["key#{i}"] = i }; GC.start; h = nil; GC.start; g = {}; g["key5".dup] = 1; g.keys[0] }
        row t { h = {"lit" => 1}; h.keys[0].equal?(-"lit") }
        row t { h = Hash.new; h.store("st".dup, 1); h.keys[0].equal?(-"st") }
        row t { h = {a: 1}.merge("mg".dup => 2); h.keys[1].equal?(-"mg") }
        row t { h = {}; h["s".dup] = 1; h.keys[0].singleton_class rescue "#{$!.class}: #{$!.message}" }
        row t { :sym.name.equal?(-"sym") }
        row t { h = {}; k = "e".dup.force_encoding("EUC-JP"); h[k] = 1; [h.keys[0].equal?(-"e"), h.keys[0].encoding] }
        $o
        "##,
    );
}

// The table behind it holds its Strings weakly: keys are made, dropped,
// collected and made again, and one that outlives its Hash stays the
// fstring of its content.
#[test]
fn hash_key_fstrings_are_collected() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        # churn: keys made, dropped, collected, remade
        res = []
        20.times do |round|
          h = {}
          2000.times { |i| h["key-#{round % 3}-#{i}"] = i }
          res << h.size
          ids = h.keys.first(3).map { |k| k.equal?(-k.dup) }
          res << ids
          h = nil
          GC.start if round.odd?
        end
        rowp res.uniq
        # a key that outlives its Hash through another reference
        keep = nil
        h = {}; h["survivor".dup] = 1; keep = h.keys[0]; h = nil
        GC.start
        g = {}; g["survivor".dup] = 2
        rowp g.keys[0].equal?(keep), keep.equal?(-"survivor")
        # long strings (heap buffers)
        big = "x" * 5000
        h = {}; h[big.dup] = 1
        k1 = h.keys[0]
        rowp k1.equal?(-big), k1.size
        h = nil; k1 = nil; GC.start; GC.start
        h = {}; h[big.dup] = 1; rowp h.keys[0].size
        # many threads storing the same keys
        ths = 4.times.map { |t| Thread.new { hh = {}; 500.times { |i| hh["t#{i}"] = t }; hh.keys } }
        ks = ths.map(&:value)
        rowp ks.map(&:size), ks[0].zip(ks[1]).all? { |a, b| a.equal?(b) }
        # interned after the Hash made it: the same object, and it survives the Hash
        h = {}; h["later".dup] = 1; a = -"later".dup; rowp a.equal?(h.keys[0]); h = nil; GC.start; rowp a, (-"later".dup).equal?(a)
        100.times { |i| s = "tmp#{i}"; hh = {s => 1}; hh = nil }
        GC.start
        rowp :done
        $o
        "##,
    );
}
