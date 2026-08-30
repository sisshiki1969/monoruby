//! The `&:sym` dispatch resolves through a one-entry inline cache
//! (`Globals::cached_symbol_method`). Everything it must still get right:
//! the method it finds, and every reason a cached entry has to be retired.

extern crate monoruby;
use monoruby::tests::*;

/// An array of several classes thrashes the one-entry cache, so each
/// element must still reach its own method.
#[test]
fn polymorphic_receivers_each_reach_their_own_method() {
    run_test(
        r##"
        class A; def tag = :a; end
        class B; def tag = :b; end
        class C < A; def tag = :c; end
        [[A.new, B.new, C.new, A.new].map(&:tag),
         [1, 2.0, "3", :four, nil, true, false].map(&:to_s),
         [[1, 2], {a: 1}, (1..2)].map(&:size)]
        "##,
    );
}

/// `true` and `false` share one inline-cache class in the VM; the symbol
/// cache must not, or `false` gets the method resolved for `true`.
#[test]
fn true_and_false_do_not_share_a_cache_entry() {
    run_test(
        r##"
        class TrueClass; def which = :from_true; end
        class FalseClass; def which = :from_false; end
        [[true, false, true].map(&:which), [false, true].map(&:which)]
        "##,
    );
}

/// A definition made *after* a symbol proc has run must be seen: the
/// entry is keyed by the class version, which every definition bumps.
#[test]
fn a_redefinition_retires_the_entry() {
    run_test_once(
        r##"
        class R; def v = 1; end
        first = [R.new].map(&:v)
        class R; def v = 2; end
        second = [R.new].map(&:v)
        # A method added to a superclass, and one removed.
        class Base; end
        class Sub < Base; end
        added = begin
          [Sub.new].map(&:hello)
        rescue NoMethodError
          :missing
        end
        class Base; def hello = :hi; end
        after_add = [Sub.new].map(&:hello)
        class R; undef_method :v; end
        after_undef = begin
          [R.new].map(&:v)
        rescue NoMethodError
          :gone
        end
        [first, second, added, after_add, after_undef]
        "##,
    );
}

/// `&:sym` is `public_send`: a private or protected method is not called,
/// and making a public one private has to retire the cached entry.
#[test]
fn visibility_is_enforced_and_tracked() {
    run_test_once(
        r##"
        class V
          def open_door = :ok
          private def secret = :no
          protected def guarded = :no
        end
        priv = begin; [V.new].map(&:secret); rescue NoMethodError => e; :private; end
        prot = begin; [V.new].map(&:guarded); rescue NoMethodError => e; :protected; end
        before = [V.new].map(&:open_door)
        class V; private :open_door; end
        after = begin; [V.new].map(&:open_door); rescue NoMethodError => e; :now_private; end
        class V; public :open_door; end
        again = [V.new].map(&:open_door)
        [priv, prot, before, after, again]
        "##,
    );
}

/// A receiver with no such method reaches `method_missing`, and one
/// without that raises — neither is cached, and both must survive a
/// successful call to the same symbol on another class.
#[test]
fn missing_methods_are_not_cached() {
    run_test(
        r##"
        class MM
          def method_missing(name, *args) = [:mm, name, args]
          def respond_to_missing?(*) = true
        end
        class Real; def poke(x = nil) = [:real, x]; end
        [[Real.new, MM.new, Real.new].map(&:poke),
         [MM.new].map(&:poke),
         (begin; [Object.new].map(&:poke); rescue NoMethodError; :raised; end),
         [Real.new].map(&:poke)]
        "##,
    );
}

/// The cached entry carries the method, not the arguments: the same
/// symbol proc reused with arguments, keywords and a block still passes
/// them through.
#[test]
fn arguments_survive_the_cache() {
    run_test(
        r##"
        class Args
          def call_me(a, b = 2, *rest, kw: 9, &blk)
            [a, b, rest, kw, blk ? blk.call : nil]
          end
        end
        o = Args.new
        p1 = :call_me.to_proc
        [p1.call(o, 1), p1.call(o, 1, 5, 6), p1.call(o, 1, 2, kw: 3),
         p1.call(o, 1) { :block }, [1, 2].inject(&:+), [[1, 2]].map(&:first)]
        "##,
    );
}

/// `String` is `Comparable`, as in CRuby — `between?` and `clamp` come
/// from it, while String's own `<=>` / `==` and the native ordering
/// operators keep answering.
#[test]
fn string_is_comparable() {
    run_test(
        r##"
        [String.include?(Comparable), String.ancestors.map(&:to_s),
         "b".between?("a", "c"), "b".between?("c", "d"),
         "b".clamp("a", "c"), "b".clamp("c", "d"), "b".clamp("c".."d"),
         ("a" < "b"), ("a" <= "a"), ("b" > "a"), ("b" >= "c"),
         ("a" == "a"), ("a" == 1), ("a" <=> "b"), ("a" <=> 1),
         "b".clamp("a"..), "b".clamp(.."a")]
        "##,
    );
    run_test_error(r#""b".clamp("c", "a")"#);
    run_test_error(r#""b".between?("a", 1)"#);
    // A String subclass inherits Comparable through String.
    run_test(
        r##"
        class Sub < String; end
        [Sub.new("b").between?("a", "c"), Sub.ancestors.include?(Comparable)]
        "##,
    );
}
