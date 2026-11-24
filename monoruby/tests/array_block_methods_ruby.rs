//! `Array#select` (`filter`, `find_all`), `#reject`, `#partition` and
//! `#flat_map` (`collect_concat`) are written in Ruby (builtins/array.rb),
//! so that the JIT compiles the block into the loop. Whatever the block is
//! and whatever it does to the receiver, they answer as CRuby's do.

extern crate monoruby;
use monoruby::tests::*;

/// The results, the aliases, and the block-less forms: the Enumerator's
/// name, its size and what chaining it gives.
#[test]
fn array_block_methods_results() {
    run_test_once(
        r##"
        def t
          a = [1, 2, 3, 4, 5, 6]
          r = []
          r << a.select { |x| x.even? } << a.filter { |x| x > 2 } << a.find_all { |x| x < 3 }
          r << a.reject { |x| x.even? } << a.partition { |x| x.odd? }
          r << a.flat_map { |x| [x, [x]] } << a.collect_concat { |x| x }
          r << a.flat_map { |x| x.even? ? [x] : x }
          r << [].select { } << [].reject { } << [].partition { } << [].flat_map { }
          r << [nil, false, 1].select { |x| x } << [nil, false, 1].reject { |x| x }
          r << [nil, false, 1].partition { |x| x } << [nil, false, [nil]].flat_map { |x| x }
          # the block-less forms
          r << [3, 1].select.inspect << [3, 1].filter.inspect << [3, 1].find_all.inspect
          r << [3, 1].reject.inspect << [3, 1].partition.inspect
          r << [3, 1].flat_map.inspect << [3, 1].collect_concat.inspect
          r << a.select.size << a.reject.size << a.partition.size << a.flat_map.size
          r << a.select.with_index { |x, i| i.even? } << a.reject.with_index { |x, i| i.even? }
          r << a.partition.with_index { |x, i| i.even? } << a.flat_map.with_index { |x, i| [x, i] }
          r << a.select.each { |x| x == 3 } << a.filter.with_object([]) { |x, m| m << x }.size
          # fresh, unfrozen Arrays, also from a frozen receiver and a subclass
          s = a.select { true }
          r << s.equal?(a) << s.frozen? << a.dup.freeze.select { |x| x > 4 }
          m = Class.new(Array).new([1, 2, 3])
          r << m.select { true }.class << m.reject { false }.class
          r << m.partition { true }.map(&:class) << m.flat_map { |x| [x] }.class
          r << a.method(:select).arity << a.method(:filter).name << a.method(:reject).arity
          r << (Array.instance_method(:select) == Array.instance_method(:filter))
          r << (begin; a.select(1) { }; rescue ArgumentError => e; e.message; end)
          r << (begin; a.flat_map(1) { }; rescue ArgumentError => e; e.message; end)
          big = (1..3000).to_a
          r << big.select(&:even?).size << big.reject(&:even?).sum
          r << big.partition { |x| x % 3 == 0 }.map(&:size)
          r << big.flat_map { |x| [x, x] }.size << big.flat_map { |x| big[0, 3] }.sum
          r
        end
        res = nil
        30.times { res = t }
        res
        "##,
    );
}

/// A block that changes the receiver while it is walked: every step reads
/// the receiver again, and `select` alone reads the element a second time
/// after the block, as `rb_ary_select` does (#2186).
#[test]
fn array_block_methods_receiver_changed_by_the_block() {
    run_test_once(
        r##"
        def t
          r = []
          b = [1, 2, 3]; r << b.select { |x| b[0] = 9 if x == 1; true }
          b = [1, 2, 3]; r << b.select { |x| b.clear; true }
          b = [1, 2, 3]; r << b.filter { |x| b[1] = :x if x == 2; true }
          b = [1, 2, 3]; r << b.find_all { |x| b[2] = "s" if x == 1; x != 2 }
          b = [1, 2, 3]; r << b.select { |x| b << x if b.size < 6; x.odd? }
          b = [1, 2, 3]; r << b.select { |x| b.shift; true } << b
          b = [1, 2, 3]; r << b.reject { |x| b[0] = 9 if x == 1; false }
          b = [1, 2, 3]; r << b.reject { |x| b.clear; false }
          b = [1, 2, 3]; r << b.reject { |x| b.unshift(0) if b.size < 5; x == 1 }
          b = [1, 2, 3]; r << b.partition { |x| b.pop if x == 1; true }
          b = [1, 2, 3]; r << b.partition { |x| b[2] = 7 if x == 1; x > 2 }
          b = [1, 2, 3]; r << b.flat_map { |x| b << 4 if b.size < 5; [x] }
          b = [1, 2, 3]; r << b.flat_map { |x| b.clear; [x, x, x] }
          b = [[1], [2], [3]]; r << b.flat_map { |x| x << 0; x }
          # an element only the result still holds, with collections in between
          b = Array.new(40) { |i| "s#{i}" }
          r << b.select { |x| b.shift; GC.start if x == "s8"; true }.size
          b = Array.new(40) { |i| "s#{i}" }
          r << b.reject { |x| b.pop; GC.start if x == "s8"; false }
          b = Array.new(40) { |i| "s#{i}" }
          r << b.partition { |x| b.pop; GC.start if x == "s8"; x.size == 2 }
          b = Array.new(40) { |i| ["s#{i}", "t#{i}", "u#{i}"] }
          r << b.flat_map { |x| b.pop; GC.start if x[0] == "s8"; x }
          r
        end
        res = nil
        30.times { res = t }
        res
        "##,
    );
}

/// What `flat_map` does with the block's value: an Array of any length is
/// spliced, an Array subclass too, `to_ary` is asked once (through
/// `respond_to_missing?` / `method_missing` as well), a `nil` from it keeps
/// the object and anything else is a TypeError.
#[test]
fn array_flat_map_block_values() {
    run_test_once(
        r##"
        class FmSub < Array; end
        class FmAry; def initialize(v) = @v = v; def to_ary = [@v, @v]; end
        class FmBad; def to_ary = 1; end
        class FmNil; def to_ary = nil; end
        class FmMissing
          def respond_to_missing?(n, p) = n == :to_ary
          def method_missing(n, *a) = n == :to_ary ? [7, 8, 9] : super
        end
        class FmLong < Array
          def size = 1
          def [](i) = :overridden
        end
        def t
          r = []
          a = [1, 2, 3]
          r << a.flat_map { |x| [] } << a.flat_map { |x| [x] } << a.flat_map { |x| [x, -x] }
          r << a.flat_map { |x| [x, x, x] } << a.flat_map { |x| Array.new(x) { |i| i } }
          r << a.flat_map { |x| (1..20).to_a }.size << a.flat_map { |x| a } << a.flat_map { |x| [a] }
          r << a.flat_map { |x| FmSub.new([x, x]) } << a.flat_map { |x| FmSub.new([x, x, x]) }.class
          r << a.flat_map { |x| FmLong.new([x, x, x]) }
          r << a.flat_map { |x| FmAry.new(x) } << a.flat_map { |x| FmMissing.new }
          r << a.flat_map { |x| FmNil.new }.map(&:class)
          r << (begin; a.flat_map { FmBad.new }; rescue TypeError => e; e.message; end)
          r << a.flat_map { |x| x } << a.flat_map { |x| nil } << a.flat_map { |x| {x => 1} }
          r << a.flat_map { |x| x.to_s } << a.flat_map { |x| x..x } << a.flat_map { |x| [[x, [x]]] }
          r << [[1, [2]], [3]].flat_map { |x| x } << [[1, 2], [3, 4]].flat_map { |x, y| [y, x] }
          r << a.flat_map { |x| x.odd? ? [x, x, x, x] : :s }
          r << a.collect_concat { |x| [x] * x } << a.flat_map(&:digits) << [[1, 2], [3]].flat_map(&:itself)
          r
        end
        res = nil
        30.times { res = t }
        res
        "##,
    );
}

/// Leaving the block early, and every kind of block: a literal, a Symbol,
/// a Proc, a lambda, a Method, blocks with several parameters.
#[test]
fn array_block_methods_control_flow_and_block_kinds() {
    run_test_once(
        r##"
        def ret_sel(a) = a.select { |x| return x if x == 4; true }
        def ret_rej(a) = a.reject { |x| return [x] if x == 2; true }
        def ret_par(a) = a.partition { |x| return x if x == 4; true }
        def ret_fm(a) = a.flat_map { |x| return :out if x == 5; [x] }
        def ens(a)
          log = []
          begin
            a.select { |x| raise "boom#{x}" if x == 3; log << x; true }
          rescue => e
            log << e.message
          ensure
            log << :ensure
          end
          log
        end
        def t
          a = [1, 2, 3, 4, 5, 6]
          r = []
          r << a.select { |x| break x if x == 3; true } << a.reject { |x| break :b if x == 3 }
          r << a.partition { |x| break [x] if x == 6 } << a.flat_map { |x| break x * 2 if x == 2; [x] }
          r << a.select { |x| next false if x == 2; true } << a.flat_map { |x| next [0] if x == 2; [x] }
          r << a.reject { |x| next true if x > 2 } << a.partition { |x| next if x.odd?; 1 }
          r << ret_sel(a) << ret_rej(a) << ret_par(a) << ret_fm(a) << ens(a)
          r << (catch(:t) { a.reject { |x| throw :t, x if x == 5 } })
          r << (begin; a.partition { |x| raise ArgumentError, "p" if x == 2 }; rescue => e; [e.class, e.message]; end)
          r << (begin; a.flat_map { |x| x.nope }; rescue NoMethodError => e; e.name; end)
          # Symbols, Procs, lambdas, Methods
          r << a.select(&:odd?) << a.reject(&:even?) << a.partition(&:even?) << a.filter(&:zero?)
          r << %w[a bb ccc].select(&:frozen?).size << %w[a bb ccc].flat_map(&:chars)
          pr = proc { |x, y| y.nil? }
          r << [[1, 2], [3]].select(&pr) << [[1, 2], [3]].reject(&pr) << [[1, nil], [3, 4]].partition(&pr)
          r << [[1, 2], [3, 4]].select { |x, y| y == 4 } << [[1, 2], [3, 4]].partition { |x, y| x == 1 }
          r << [[1, [2, 3]]].select { |x, (y, z)| z == 3 } << [[1, 2]].reject { |x, *y| y == [2] }
          l = lambda { |x| x > 2 }
          r << a.select(&l) << a.reject(&l) << a.partition(&l) << a.flat_map(&lambda { |x| [x, l.(x)] })
          r << a.select(&2.method(:<)) << a.reject(&4.method(:>)) << a.partition(&3.method(:==))
          r << a.flat_map(&[].method(:push)).size
          # one call site reached with different blocks and receivers
          [[:odd?, a], [:even?, a], [:empty?, ["", "x"]], [:nil?, [nil, 1]]].each do |s, recv|
            r << recv.select(&s) << recv.reject(&s) << recv.partition(&s)
          end
          lim = 3
          r << a.select { |x| x > lim } << a.reject { |x| lim += 1 if x == 1; x > lim } << lim
          r << a.select { |x| a.select { |y| y > x }.size.odd? } << a.flat_map { |x| a.reject { |y| y < 5 } }
          r << [1.5, 2.5, 3.5].select { |x| x > 2.0 } << [1.5, 2.5].flat_map { |x| [x * 2.0, x] }
          r
        end
        res = nil
        30.times { res = t }
        res
        "##,
    );
}

/// `Array#__concat_to`, the private primitive `flat_map` appends with: it
/// takes two Arrays, the same one twice included, and nothing else. CRuby
/// has no such method, so the script gives it one that answers the same.
#[test]
fn array_concat_to_primitive() {
    run_test_once(
        r##"
        unless [].respond_to?(:__concat_to, true)
          class Array
            private def __concat_to(dst, src)
              raise TypeError, "__concat_to: Array expected" unless dst.instance_of?(Array) || Array === dst
              raise TypeError, "__concat_to: Array expected" unless Array === src
              dst.concat(src.dup)
            end
          end
        end
        r = []
        a = [1, 2]
        r << [].send(:__concat_to, a, [3, 4, 5]) << a << [].send(:__concat_to, a, a) << a.size
        r << [].send(:__concat_to, [], []) << [].send(:__concat_to, [1], Class.new(Array).new([2]))
        r << (begin; [].send(:__concat_to, 1, [2]); rescue TypeError => e; e.message; end)
        r << (begin; [].send(:__concat_to, [1], :s); rescue TypeError => e; e.message; end)
        r << (begin; [].send(:__concat_to, [1].freeze, [2]); rescue FrozenError => e; e.class; end)
        r
        "##,
    );
}
