extern crate monoruby;
use monoruby::tests::*;

// `JitContext::inline_class_new` emits `Foo.new` as the caller's own
// instructions instead of entering the Ruby `Class#new` trampoline. It has
// two legs — fold the `initialize` away, or expand it into ivar stores —
// and declines everything else (a native `initialize`, a body too big to
// expand), which then runs through the Ruby `Class#new`. Both sides are
// covered here. Every case below loops past the JIT thresholds so the
// compiled form, not the interpreter, is compared.

const LOOP: usize = if cfg!(feature = "gc-stress") { 20 } else { 300 };

#[test]
fn class_new_declined_native_initialize() {
    // A custom `alloc_func` plus a native `initialize`: `inline_class_new`
    // declines, so the construction runs through the Ruby `Class#new`.
    run_test(&format!(
        r#"
        r = []
        {LOOP}.times do
          r = [
            String.new,
            String.new("ab"),
            String.new("ab", capacity: 64),
            Hash.new,
            Hash.new(7)[:absent],
            Array.new(3),
            Array.new(3, :v),
            Array.new([1, 2]),
            Array.new(2) {{ |i| i + 1 }},
          ]
        end
        r
        "#
    ));
}

#[test]
fn class_new_declined_iseq_initialize() {
    // A Ruby `initialize` that is neither trivial nor a plain ivar-store
    // body, so `inline_class_new` declines and the Ruby `Class#new` runs
    // it — including one declaring an optional keyword the call site does
    // not pass, and one taking a rest parameter.
    run_test(&format!(
        r#"
        class CNBig
          def initialize(a)
            @a = a
            @b = a.to_s
            @c = @b.size + 1
          end
          def to_a = [@a, @b, @c]
        end
        class CNKw
          def initialize(a, b: 10)
            @v = [a, b]
          end
          def to_a = @v
        end
        class CNRest
          def initialize(*a)
            @v = a.reverse
          end
          def to_a = @v
        end
        r = []
        {LOOP}.times do
          r = [CNBig.new(12).to_a, CNKw.new(1).to_a, CNRest.new(1, 2, 3).to_a]
        end
        r
        "#
    ));
}

#[test]
fn class_new_subclasses_and_struct() {
    run_test(&format!(
        r#"
        class CNStr < String; end
        class CNAry < Array; end
        class CNHash < Hash; end
        CNPoint = Struct.new(:x, :y) unless defined?(CNPoint)
        r = []
        {LOOP}.times do
          r = [CNStr.new("s"), CNAry.new(2, 0), CNHash.new[:k], CNPoint.new(1, 2).to_a]
        end
        r
        "#
    ));
}

#[test]
fn class_new_deeply_nested_site() {
    // Past the specialization depth limit the trampoline is not inlined
    // into the caller's unit, which used to cost an empty rest Array per
    // construction. The construction must be identical either way.
    run_test(&format!(
        r#"
        class CNDeep
          def initialize(a)
            @a = a * 2
          end
          def a = @a
        end
        def cn_d4(n) = cn_d3(n)
        def cn_d3(n) = cn_d2(n)
        def cn_d2(n) = cn_d1(n)
        def cn_d1(n)
          acc = []
          n.times {{ |i| acc << [String.new("x"), CNDeep.new(i).a] }}
          acc.last
        end
        r = []
        {LOOP}.times {{ r = cn_d4(4) }}
        r
        "#
    ));
}

#[test]
fn class_new_error_paths() {
    // Every decline and every raise still comes from a real frame.
    run_test(&format!(
        r#"
        class CNOne; def initialize(a); @a = a; end; end
        class CNRaise
          def initialize(a)
            raise ArgumentError, "no" if a.negative?
            @a = a
          end
          def a = @a
        end
        r = []
        {LOOP}.times do
          r = []
          begin; Object.new(1); rescue ArgumentError => e; r << e.class; end
          begin; CNOne.new; rescue ArgumentError => e; r << e.class; end
          begin; CNOne.new(1, 2); rescue ArgumentError => e; r << e.class; end
          begin; String.new(1); rescue TypeError => e; r << e.class; end
          begin; Array.new(-1); rescue ArgumentError => e; r << e.class; end
          begin; CNRaise.new(-1); rescue ArgumentError => e; r << e.message; end
          r << CNRaise.new(3).a
          o = CNOne.new(1)
          begin; o.singleton_class.new; rescue TypeError => e; r << e.class; end
        end
        r
        "#
    ));
}

#[test]
fn class_new_redefined_initialize() {
    // The class-version guard the whole inline rides on: redefining
    // `initialize` mid-loop must be observed by the compiled site.
    //
    // Every construction below goes through ONE call site, inside a method
    // that is already hot and compiled when the redefinition lands. That is
    // what makes this a test: a second, separate site would simply compile
    // afresh against the new `initialize` and pass no matter what the first
    // one kept. Here the site's own inline-cache entry
    // (`#<Class:CNVer>#new -> Class#new`) is still true after the
    // redefinition, so `salvage_method_unit` only rejects the unit because
    // `inline_class_new` records the `initialize` resolution it baked in.
    //
    // Both legs are covered, and the last two change the resolution from
    // an *inherited* `initialize` to an own one:
    //   CNVer  — redefined ivar-store body      (expand leg)
    //   CNAdd  — none, then defined             (fold leg -> expand leg)
    //   CNSub  — inherited, then overridden     (expand leg)
    //   CNStrS — String's native one, then own  (declined either way)
    //
    // `run_test_once`, not `run_test`: the redefinitions persist across a
    // repeated run in the same process, so the second run's `before` would
    // already see them. The inner loops warm the JIT within the one run.
    run_test_once(&format!(
        r#"
        class CNVer; def initialize(a); @a = a; end; def a = @a; end
        class CNAdd; def a = @a; end
        class CNBase; def initialize(a); @a = a; end; def a = @a; end
        class CNSub < CNBase; end
        class CNStrS < String; end

        def cn_v = CNVer.new(1).a
        def cn_a = CNAdd.new.a
        def cn_s = CNSub.new(1).a
        def cn_t = CNStrS.new("ab").size

        r = []
        {LOOP}.times {{ r << [cn_v, cn_a, cn_s, cn_t] }}
        before = r.last

        class CNVer; def initialize(a); @a = a + 100; end; end
        class CNAdd; def initialize; @a = 7; end; end
        class CNSub; def initialize(a); @a = a + 50; end; end
        class CNStrS; def initialize(s); super(s + "zz"); end; end

        {LOOP}.times {{ r << [cn_v, cn_a, cn_s, cn_t] }}
        [before, r.last, r.size]
        "#
    ));
}

#[test]
fn class_new_define_method_initialize() {
    // A `define_method` `initialize` is not one `send` can enter directly,
    // so the site keeps the trampoline — and keeps working.
    run_test(&format!(
        r#"
        class CNDm
          define_method(:initialize) {{ |a| @a = a * 3 }}
          def a = @a
        end
        r = []
        {LOOP}.times {{ r = CNDm.new(4).a }}
        r
        "#
    ));
}
