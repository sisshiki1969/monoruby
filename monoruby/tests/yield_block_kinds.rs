//! A `yield` the JIT compiles without knowing the block (the generic
//! `Yield`) resolves it at run time: a Proc and a bare Symbol held by the
//! method's own frame are read inline, a block taking exactly the values
//! yielded gets them copied inline, and everything else goes to the
//! runtime helpers. Each kind of block, each way of binding, and the
//! errors must behave the same on every path.

extern crate monoruby;
use monoruby::tests::*;

const PRELUDE: &str = r##"
    def y0 = yield
    def y1(x) = yield(x)
    def y2(x, y) = yield(x, y)
    def ys(*a) = yield(*a)
    def ykw(x) = yield(x, k: 1)
    def loopy(n)
      i = 0
      s = []
      while i < n
        s << yield(i)
        i += 1
      end
      s
    end
    def try
      yield
    rescue StandardError => e
      [e.class, e.message.sub(/0x\h+/, "XX")]
    end
"##;

/// The shared methods plus a test's own definitions.
fn prelude(extra: &str) -> String {
    format!("{PRELUDE}{extra}")
}

/// One yield site, every kind of block in turn: the site must not keep
/// what it learned from the previous kind.
#[test]
fn one_site_every_kind_of_block() {
    run_test_with_prelude(
        r##"
        pr = proc { |x| x * 2 }
        la = lambda { |x| x + 1 }
        [
          loopy(3, &:succ),
          loopy(3, &pr),
          loopy(3, &la),
          loopy(3, &1.method(:+)),
          loopy(3, &:succ.to_proc),
          loopy(3) { |x| x - 1 },
          loopy(3, &:to_s),
          loopy(3, &:itself),
        ]
        "##,
        PRELUDE,
    );
}

/// A Proc binds loosely and a lambda strictly, whatever the yield site
/// knows about its own arguments.
#[test]
fn proc_and_lambda_arity() {
    run_test_with_prelude(
        r##"
        [
          y2(3, 4, &proc { |a, b| [a, b] }),
          y2(3, 4, &proc { |a| a }),
          y2(3, 4, &proc { |a, b, c| [a, b, c] }),
          y1([5, 6], &proc { |a, b| [b, a] }),
          y1([5, 6], &proc { |a| a }),
          y1([5, 6], &lambda { |a| a }),
          y1(7, &proc { |a, b = 9, *c, k: 3| [a, b, c, k] }),
          y1(7, &proc { |*a| a }),
          y1(7, &proc { |a, **k| [a, k] }),
          y1(7, &proc { |a, k: 2, **r| [a, k, r] }),
          y0(&proc { |a| a }),
          y0(&proc { 1 }),
          ys(1, 2, 3, &proc { |*a| a }),
          ys(1, 2, 3, &proc { |a, b| [a, b] }),
          ykw(1, &proc { |x, k:| [x, k] }),
          ykw(1, &proc { |x, **r| [x, r] }),
          try { y1(3, &lambda { |a, b| a }) },
          try { y2(3, 4, &lambda { |a| a }) },
          try { y1(3, &lambda { |a, k:| a }) },
          try { y0(&lambda { |a| a }) },
        ]
        "##,
        PRELUDE,
    );
}

/// `yield x` to `&:sym` is `x.sym`: public only, `method_missing` for the
/// rest, the remaining values as arguments.
#[test]
fn symbol_block() {
    run_test_with_prelude(
        r##"
        [
          y1(3, &:succ),
          y1([3, 1, 2], &:sort),
          y1([1, 2], &:sum),
          y2(1, 2, &:+),
          y2(10, 3, &:divmod),
          ys(1, 2, &:+),
          y1(Foo.new, &:pub),
          try { y1(Foo.new, &:priv) },
          try { y1(Foo.new, &:prot) },
          try { y1(3, &:nope) },
          try { y0(&:succ) },
          try { ys(1, 2, 3, &:+) },
          y1(Bar.new, &:zork),
          y2(Bar.new, 5, &:zork),
          y1(BasicObject.new, &:__id__).class,
          [1, "a", :b, nil, 2.5, true, false].map(&:to_s),
          [[1, 2], [3, 4]].each_with_index.map(&:first),
        ]
        "##,
        &prelude(
            r##"
        class Foo
          def pub = 1
          private def priv = 2
          protected def prot = 3
        end
        class Bar
          def method_missing(name, *args) = [name, args]
          def respond_to_missing?(*) = true
        end
        "##,
        ),
    );
}

/// A method redefined between yields is seen by the next one.
#[test]
fn symbol_block_sees_a_redefinition() {
    run_test_with_prelude(
        r##"
        class Baz; def tag = :one; end
        b = Baz.new
        res = [y1(b, &:tag)]
        class Baz; def tag = :two; end
        res << y1(b, &:tag)
        class Baz; private :tag; end
        res << try { y1(b, &:tag) }
        class Baz; public :tag; end
        res << y1(b, &:tag)
        res
        "##,
        PRELUDE,
    );
}

/// `&:sym` written where a refinement is active is a Proc that looks the
/// method up through it, not the bare Symbol: the yield site must not
/// call the unrefined method (or miss the refined one) for it.
#[test]
fn refined_symbol_block() {
    run_test_once(
        r##"
        module ShoutY
          refine String do
            def shout = upcase + "!"
            def to_s = "refined"
          end
        end
        def y1(x) = yield(x)
        def loopy(n)
          i = 0
          s = []
          while i < n
            s << yield(i)
            i += 1
          end
          s
        end
        using ShoutY
        res = nil
        40.times do
          res = [
            y1("a", &:shout),
            y1("b", &:to_s),
            y1(3, &:to_s),
            loopy(3, &:to_s),
            (y1(3, &:shout) rescue $!.class),
            (y1("c", &:shout.to_proc) rescue $!.class),
          ]
        end
        res
        "##,
    );
}

/// No block, from a method body and from a block inside it.
#[test]
fn no_block_given() {
    run_test_with_prelude(
        r##"
        [try { y1(3) }, try { y0 }, try { ys(1, 2) }, try { inblk }]
        "##,
        &prelude(
            r##"
        def inblk = [1].map { |x| yield(x) }
        "##,
        ),
    );
}

/// The method's block is reached from blocks inside it at any depth, and
/// from a `define_method` body.
#[test]
fn yield_from_a_block() {
    run_test_with_prelude(
        r##"
        pr = proc { |x| x * 2 }
        [
          inblk(&:succ), inblk(&pr), inblk { |x| x + 5 },
          inblk2(&:succ), inblk2(&pr), inblk2 { |x| x + 5 },
          ie(&:succ), ie(&pr), ie { |x| x + 5 },
          Maker.new.dm(4),
        ]
        "##,
        &prelude(
            r##"
        def inblk = [1, 2].map { |x| yield(x) }
        def inblk2 = [1, 2].map { |x| [3].map { |y| yield(x + y) } }
        def ie = 5.instance_exec { yield(self) }
        class Maker
          def self.make = define_method(:dm) { |x| yield(x) }
        end
        Maker.make(&:succ)
        "##,
        ),
    );
}

/// The frame of the method and of the block's home move to the heap
/// between two yields (a `binding`, a `proc`), and the second yield still
/// finds the block and its variables.
#[test]
fn frames_captured_between_yields() {
    run_test_with_prelude(
        r##"
        pr = proc { |x| x * 2 }
        [cap(&:succ), cap(&pr), cap { |x| x * 10 }, mk(100), home, O.go]
        "##,
        &prelude(
            r##"
        def cap
          a = yield(1)
          b = binding
          keep = proc { a }
          c = yield(2)
          [a, c, b.local_variable_get(:a), keep.call]
        end
        def mk(n) = loopy(2, &proc { |x| x + n })
        def home
          k = 10
          r = cap { |x| h = binding; k += x; h.local_variable_get(:k) }
          [r, k]
        end
        O = Object.new
        def O.v = 42
        def O.go = loopy(2, &proc { |x| x + v })
        "##,
        ),
    );
}

/// `break`, `next`, a lambda's `return` and an exception leave a yielded
/// block the way they do an inlined one.
#[test]
fn leaving_the_block() {
    run_test_with_prelude(
        r##"
        boom = proc { |x| raise "boom#{x}" if x == 1; x }
        [brk, nxt, ret, try { loopy(3, &boom) }, loopy(3, &proc { |x| "s" * (x + 1) })]
        "##,
        &prelude(
            r##"
        def brk = loopy(5) { |x| break x * 100 if x == 2; x }
        def nxt = loopy(3) { |x| next 7 if x == 1; x }
        def ret = loopy(3, &lambda { |x| return x * 3 })
        "##,
        ),
    );
}

/// More values than fit the inline copy, and a block whose parameters
/// are bound by the helper at the same site.
#[test]
fn many_arguments() {
    run_test_with_prelude(
        r##"
        [
          y9(&proc { |a, b, c, d, e, f, g, h, i| [a, i] }),
          y9(&proc { |a, *r| [a, r] }),
          y9(&proc { |a| a }),
          y8(&proc { |a, b, c, d, e, f, g, h| [a, h] }),
          y8(&proc { |a, b, c, d, e, f, g, h, i| [a, h, i] }),
          y8(&lambda { |a, b, c, d, e, f, g, h| [a, h] }),
          try { y8(&lambda { |a| a }) },
          try { y9(&:+.to_proc) },
        ]
        "##,
        &prelude(
            r##"
        def y9 = yield(1, 2, 3, 4, 5, 6, 7, 8, 9)
        def y8 = yield(1, 2, 3, 4, 5, 6, 7, 8)
        "##,
        ),
    );
}
