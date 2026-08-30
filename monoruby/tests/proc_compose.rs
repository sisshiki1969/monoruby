extern crate monoruby;
use monoruby::tests::*;

#[test]
fn proc_compose_forward() {
    run_test(r#"succ = proc { |s| s.succ }; upcase = proc { |s| s.upcase }; (succ >> upcase).call("Ruby")"#);
    run_test("f = proc { |x| x * x }; g = proc { |x| x + x }; [(f >> g).call(2), (g >> f).call(2)]");
    run_test("mul = proc { |n, m| n * m }; inc = proc { |n| n + 1 }; (mul >> inc).call(2, 3)");
}

#[test]
fn proc_compose_backward() {
    run_test(r#"succ = proc { |s| s.succ }; upcase = proc { |s| s.upcase }; (succ << upcase).call("Ruby")"#);
    run_test("f = proc { |x| x * x }; g = proc { |x| x + x }; [(f << g).call(2), (g << f).call(2)]");
    run_test("inc = proc { |n| n + 1 }; mul = proc { |n, m| n * m }; (inc << mul).call(2, 3)");
}

#[test]
fn proc_compose_lambda_ness() {
    // `>>` follows self; `<<` follows the argument (first invoked).
    run_test(
        r#"
        f = -> x { x * x }
        g = proc { |x| x + x }
        l = -> x { x }
        [
          (g >> g).lambda?, (g >> f).lambda?, (f >> g).lambda?,
          (f << g).lambda?, (f << l).lambda?, (g << f).lambda?
        ]
        "#,
    );
}

#[test]
fn proc_compose_accepts_callable_object() {
    run_test(
        r#"
        inc = proc { |n| n + 1 }
        d = Object.new
        def d.call(n); n * 2; end
        [(inc >> d).call(3), (inc << d).call(3)]
        "#,
    );
}

#[test]
fn proc_compose_type_error() {
    run_test_error("proc { |x| x }.>>(Object.new)");
    run_test_error("proc { |x| x }.<<(42)");
}

#[test]
fn proc_compose_passes_block_to_first() {
    run_test(
        r#"
        rec = []
        one = proc { |&arg| arg.call(:one) if arg }
        two = proc { |&arg| arg.call(:two) if arg }
        (one >> two).call { |x| rec << x }
        (one << two).call { |x| rec << x }
        rec
        "#,
    );
}

/// `&:sym` shares one outer frame per symbol (`Globals::symbol_proc_frame`)
/// instead of building one per yield. The frame is reachable only from that
/// cache, so a collection between two uses of the same symbol proc must not
/// reclaim it — and two different symbols must not share one.
#[test]
fn symbol_proc_frames_survive_gc_and_stay_per_symbol() {
    run_test(
        r##"
        a = ["one", "two", "three"]
        first = a.map(&:size)
        GC.start
        second = a.map(&:upcase)
        GC.start
        third = a.map(&:size)
        [first, second, third, [1, 2].inject(&:+), :size.to_proc.call("abcd")]
        "##,
    );
}

/// `Symbol#to_proc` answers one Proc per symbol, as CRuby does, so the
/// object identity is observable: state put on the proc through one
/// `to_proc` is there through the next, and two symbols stay apart.
#[test]
fn symbol_to_proc_is_one_proc_per_symbol() {
    run_test(
        r##"
        p1 = :to_s.to_proc
        [p1.equal?(:to_s.to_proc), p1 == :to_s.to_proc,
         p1.equal?(:to_i.to_proc), p1 == :to_i.to_proc,
         :to_s.to_proc.call(12), [1, 2].map(&:to_s),
         :to_s.to_proc.arity, :to_s.to_proc.lambda?,
         :to_s.to_proc.source_location]
        "##,
    );
    // Mutating the shared proc is visible through the next `to_proc` —
    // run once, since the mutation outlives the snippet.
    run_test_once(
        r##"
        p1 = :to_s.to_proc
        p1.instance_variable_set(:@tag, :seen)
        tag = :to_s.to_proc.instance_variable_get(:@tag)
        p1.freeze
        [tag, :to_s.to_proc.frozen?, :to_s.to_proc.call(12), :to_i.to_proc.frozen?]
        "##,
    );
    // The cache holds the only reference to the Proc, so a collection
    // between two uses must not reclaim it.
    run_test(
        r##"
        p1 = :size.to_proc.call("abcd")
        GC.start
        p2 = :size.to_proc.call("ab")
        GC.start
        [p1, p2, :size.to_proc.equal?(:size.to_proc)]
        "##,
    );
    // A symbol proc has no Ruby frame to bind to, and its one frame is
    // shared — CRuby calls it a C-level proc and raises.
    run_test(
        r##"
        begin
          :to_s.to_proc.binding
        rescue => e
          [e.class, e.message]
        end
        "##,
    );
}
