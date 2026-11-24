//! A call that passes a Symbol literal as its block (`a.map(&:even?)`)
//! is specialized for it: the callee is compiled into the caller knowing
//! the block is `:even?`, and each `yield x` in it becomes the call
//! `x.even?` behind a class guard, with the generic yield as the arm for
//! any other receiver. Whatever the Symbol names and whatever the
//! receivers turn out to be, the result is what the symbol proc gives.

extern crate monoruby;
use monoruby::tests::*;

/// Every kind of method a Symbol can name (inlined builtins, attribute
/// readers, Ruby methods with optional / rest / keyword parameters,
/// `super`, Struct members, `Proc#call`), on one class and on many, and
/// the errors: private, protected and missing methods, a wrong arity, a
/// method that yields, no receiver.
#[test]
fn symbol_literal_block_methods() {
    run_test_once(
        r##"
        class Pt
          attr_reader :x
          attr_writer :w
          def initialize(x) = @x = x
          def dbl = @x * 2
          def opt(a = 10) = @x + a
          def rest(*r) = [@x, r]
          def kw(k: 3) = @x + k
          def need(a) = a
          def blk = block_given?
          def yl = yield
          def sup = super
          def to_s = "Pt#{@x}"
          private def priv = 1
          protected def prot = 2
          def ==(o) = o.is_a?(Pt) && o.x == @x
        end
        class Pt2 < Pt
          def dbl = super + 1
          def x = super * 10
        end
        S = Struct.new(:a, :b)
        def each2(a)
          i = 0
          while i < a.size
            yield a[i]
            i += 1
          end
          a
        end
        def map2(a)
          r = []
          each2(a) { |e| r << yield(e) }
          r
        end
        def two(a) = a.map { |e| yield e, 1 }
        def none = yield
        def given = block_given?
        def res = [yield(1), yield(2)]
        def tryit
          yield
        rescue => e
          [e.class, e.message]
        end
        def run
          pts = [Pt.new(1), Pt.new(2), Pt.new(3)]
          mixed = [Pt.new(1), Pt2.new(2), 3, "s", :sym, nil, 2.5, [1, 2], { a: 1 }, true, false, 1..2, 10**30]
          out = nil
          30.times do
            out = []
            out << pts.map(&:x) << pts.map(&:dbl) << pts.map(&:opt) << pts.map(&:rest) << pts.map(&:kw)
            out << pts.map(&:blk) << pts.map(&:to_s) << pts.map(&:itself).size << pts.map(&:class)
            out << [1, 2, 3].map(&:even?) << [1, 2, 3].map(&:succ) << [1, 2, 3].map(&:-@) << [1, 2, 3].map(&:to_s)
            out << [1.5, 2.5].map(&:floor) << ["a", "bc"].map(&:size) << ["a", "bc"].map(&:upcase) << [:a, :b].map(&:to_proc).size
            out << [[1, 2], [3]].map(&:size) << [[1, 2], [3]].map(&:first) << [[1, 2], [3]].map(&:sum)
            out << [nil, 1].map(&:nil?) << [nil, 1].map(&:!) << [true, false, nil].map(&:to_s) << [true, false].map(&:!)
            out << mixed.map(&:class) << mixed.map(&:to_s) << mixed.drop(2).map(&:inspect) << mixed.map(&:frozen?)
            out << [S.new(1, 2), S.new(3, 4)].map(&:a) << [S.new(1, 2)].map(&:to_a)
            out << [Pt, Pt2].map(&:name) << [Pt, Pt2].map { |c| c.new(4) }.map(&:x) << [Pt, Pt2].map(&:superclass)
            out << [-> { 1 }, proc { 2 }].map(&:call) << [-> { 1 }, proc { 2 }].map(&:lambda?)
            out << each2(pts, &:x).size << map2(pts, &:dbl) << map2([Pt2.new(5), Pt.new(5)], &:dbl) << res(&:succ)
            out << given(&:x) << pts.each(&:x).size << pts.select(&:x).size << pts.reject(&:nil?).size
            out << [3, 1, 2].sort_by(&:-@) << [3, 1, 2].max_by(&:itself) << (1..4).map(&:odd?) << { a: 1 }.map(&:last)
            out << [1, 2, 3, 4].partition(&:even?) << [[1], [2, 3]].flat_map(&:itself)
            out << [1, 2, 3].inject(&:+) << [[1, 2], [3, 4]].map(&:reverse)
            out << tryit { pts.map(&:priv) } << tryit { pts.map(&:prot) } << tryit { pts.map(&:nope) } << tryit { pts.map(&:need) }
            out << tryit { pts.map(&:w=) } << tryit { pts.map(&:yl) } << tryit { [Pt.new(1)].map(&:sup) } << tryit { none(&:x) }
            out << tryit { two([1, 2], &:+) } << tryit { two([[1]], &:push) } << tryit { [1, nil].map(&:succ) }
            out << tryit { [BasicObject.new].map(&:class) } << tryit { [1, 2].map(&:puts) } << tryit { [0].map(&:fdiv) }
            out << [2**62 - 1, 2**62].map(&:succ) << [4611686018427387903].map(&:succ) << [1, 2.0, 3r].map(&:abs)
          end
          out
        end
        run
        "##,
    );
}

/// What the called method sees and does: its caller's line, `binding`,
/// recursion, an allocation and a GC in the middle, `ensure`, an
/// exception and a `throw` through the specialized frame, and a nested
/// `&:sym` call.
#[test]
fn symbol_literal_block_frames() {
    run_test_once(
        r##"
        class Pt
          def initialize(x) = @x = x
          def line = caller_locations(1, 1)[0].lineno
          def meth = __method__
          def bnd = binding.local_variables
          def raise_it = raise(ArgumentError, "boom #{@x}")
          def thr = throw(:done, @x)
          def gc = (GC.start if @x == 2; @x.to_s * 3)
          def alloc = [@x, "s#{@x}", { k: @x }]
          def deep(n = 3) = n == 0 ? @x : deep(n - 1)
          def ens
            yield if block_given?
            @x
          ensure
            $log << @x
          end
          def fib = @x < 2 ? @x : Pt.new(@x - 1).fib + Pt.new(@x - 2).fib
          def nested = [Pt.new(@x + 1), Pt.new(@x + 2)].map(&:meth)
          def ivs = instance_variables
          def frz = frozen?
        end
        def each2(a)
          i = 0
          while i < a.size
            yield a[i]
            i += 1
          end
        end
        def m(a) = a.map(&:line)
        def run
          pts = [Pt.new(1), Pt.new(2), Pt.new(3)]
          out = nil
          $log = []
          30.times do
            $log.clear
            out = []
            out << m(pts).uniq.size << pts.map(&:meth) << pts.map(&:bnd) << pts.map(&:gc) << pts.map(&:alloc)
            out << pts.map(&:deep) << pts.map(&:ens) << $log.dup << pts.map(&:nested) << pts.map(&:ivs) << pts.map(&:frz)
            out << [Pt.new(10), Pt.new(12)].map(&:fib)
            out << (begin; pts.map(&:raise_it); rescue => e; [e.class, e.message, e.backtrace.size > 1]; end)
            out << catch(:done) { pts.map(&:thr) }
            out << (begin; each2(pts, &:raise_it); rescue => e; e.message; end)
            out << [[1, [2, 3]], [4, [5]]].map(&:flatten) << [[1, 2]].each(&:freeze).map(&:frozen?)
            x = 0
            each2([1, 2, 3]) { |e| x += e }
            out << x
          end
          out
        end
        run
        "##,
    );
}

/// The compiled call follows the program: a redefinition, a visibility
/// change, `method_missing` giving way to a real method, a singleton
/// method on one receiver, a subclass override, `undef_method`, a module
/// included and another prepended after the compile.
#[test]
fn symbol_literal_block_redefinition() {
    run_test_once(
        r##"
        class Pt
          attr_reader :x
          def initialize(x) = @x = x
          def v = @x
          def method_missing(n, *a) = n == :ghost ? [:mm, @x] : super
          def respond_to_missing?(n, p = false) = n == :ghost || super
        end
        def each2(a)
          i = 0
          while i < a.size
            yield a[i]
            i += 1
          end
        end
        def col(a, &b)
          r = []
          each2(a) { |e| r << b.call(e) }
          r
        end
        def go(a) = a.map(&:v)
        def gg(a) = a.map(&:ghost)
        def gs(a)
          r = []
          each2(a, &:v)
          a.each { |e| r << e.v }
          r
        end
        pts = [Pt.new(1), Pt.new(2)]
        res = []
        out = []
        30.times { out = [go(pts), gg(pts), gs(pts)] }
        res << out
        class Pt; def v = @x + 100; end
        30.times { out = [go(pts), gg(pts), gs(pts)] }
        res << out
        class Pt; private :v; end
        r = (go(pts) rescue [$!.class, $!.message])
        res << r
        class Pt; public :v; def ghost = :real; end
        30.times { out = [go(pts), gg(pts)] }
        res << out
        o = Pt.new(9)
        def o.v = :single
        30.times { out = go([Pt.new(1), o, Pt.new(3)]) }
        res << out
        class Sub < Pt; def v = super.to_s; end
        30.times { out = go([Sub.new(1), Pt.new(2), Sub.new(3)]) }
        res << out
        class Pt; undef_method :v; end
        r = (go(pts) rescue [$!.class, $!.message])
        res << r
        class Integer; def v = self + 1; end
        30.times { out = go([1, 2]) }
        res << out
        30.times { out = go([1, 2.5, Sub.new(1)]) rescue out = [$!.class] }
        res << out
        module Ext; def tw = x * 2; end
        class Pt; include Ext; end
        def g2(a) = a.map(&:tw)
        30.times { out = g2(pts) }
        res << out
        module Pre; def tw = super + 1; end
        class Pt; prepend Pre; end
        30.times { out = g2(pts) }
        res << out
        def g3(a) = a.map(&:size)
        30.times { out = g3(["a", "bc"]) }
        res << out
        class String; def size = :redef; end
        30.times { out = g3(["a", "bc"]) }
        res << out
        class Pt; protected :tw; end
        r = (g2(pts) rescue [$!.class, $!.message])
        res << r
        class Pt; public :tw; end
        res << g2(pts)
        res
        "##,
    );
}

/// `private :name` after a call of `name` was compiled: the method is the
/// one the unit resolved, and the call is no longer allowed (#2182). At a
/// call written in the method, in an inlined block, and through `&:name`.
#[test]
fn visibility_change_after_compile() {
    run_test_once(
        r##"
        class Pt
          def v = 1
          def w = @w
          def initialize = @w = 2
        end
        def go(o) = o.v
        def gw(o) = o.w
        def gl(o)
          r = 0
          3.times { r = o.v }
          r
        end
        def gm(a) = a.map { |e| e.v }
        def gs(a) = a.map(&:v)
        def all(o) = [(go(o) rescue $!.class), (gw(o) rescue $!.class), (gl(o) rescue $!.class), (gm([o]) rescue $!.class), (gs([o]) rescue $!.class)]
        o = Pt.new
        res = []
        30.times { all(o) }
        class Pt; private :v, :w; end
        res << all(o) << all(o)
        class Pt; public :v, :w; end
        30.times { all(o) }
        res << all(o)
        class Sub < Pt; private :v; end
        s = Sub.new
        res << all(s) << all(o)
        class Pt; protected :v; end
        res << (gs([o]) rescue $!.class) << (gs([o]) rescue $!.class)
        res
        "##,
    );
}

/// What is yielded: an Array, Hash, String or Range literal built for the
/// yield, an unboxed Float, a local, and a yield whose result replaces
/// its own argument.
#[test]
fn symbol_literal_block_yielded_values() {
    run_test_once(
        r##"
        def pair(a, b) = yield([a, b])
        def trip(a) = yield([a, a + 1, a + 2])
        def fl(a) = yield(a * 1.5)
        def hs(a) = yield({ k: a })
        def st(a) = yield("s#{a}")
        def rg(a) = yield(a..a + 2)
        def nested(a) = a.map { |e| yield [e, e] }
        def loc(a)
          x = a + 1
          y = yield x
          [x, y]
        end
        def same(a)
          a = yield a
          a
        end
        def run
          out = nil
          30.times do |i|
            out = []
            out << pair(i, 2, &:first) << pair(i, 2, &:size) << pair(i, 2, &:sum) << pair(i, 2, &:itself) << pair(1, 2, &:frozen?)
            out << trip(i, &:last) << trip(i, &:max) << trip(i, &:reverse)
            out << fl(i, &:floor) << fl(i, &:to_i) << fl(i, &:-@) << fl(i, &:itself) << fl(i, &:class) << fl(3, &:nan?)
            out << hs(i, &:size) << hs(i, &:keys) << hs(i, &:to_a) << st(i, &:size) << st(i, &:upcase) << rg(i, &:to_a) << rg(i, &:sum)
            out << nested([1, 2], &:first) << nested([1, 2], &:itself) << loc(i, &:succ) << same(i, &:to_s) << same(i, &:itself)
          end
          out
        end
        run
        "##,
    );
}
