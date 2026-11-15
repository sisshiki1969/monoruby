extern crate monoruby;
use monoruby::tests::*;

// Issue #1921: `"#{x}"` formatted an Integer, nil, a Symbol, a Float, true
// or false itself, whatever `to_s` had become. CRuby's `objtostring` takes
// its shortcuts only while the class's `to_s` is still the builtin one.

#[test]
fn interpolation_calls_a_redefined_to_s() {
    run_test_once(
        r##"
        class Integer; def to_s(*) = "INT"; end
        class NilClass; def to_s = "NIL"; end
        class Symbol; def to_s = "SYM"; end
        class Float; def to_s = "FLT"; end
        class TrueClass; def to_s = "TRUE"; end
        class FalseClass; def to_s = "FALSE"; end
        class Rational; def to_s = "RAT"; end
        class Complex; def to_s = "CPX"; end
        class Array; def to_s = "ARY"; end
        x = 1
        [
          "#{x}", "#{nil}", "#{:a}", "#{1.5}", "#{true}", "#{false}",
          "#{[1]}", "#{2**70}", "#{1e300}", "#{1r}", "#{1i}",
          "#{x}#{x}-#{nil}", "a#{x}", :"s#{x}", /a#{x}b/.source,
          "str stays #{"itself"}",
        ]
        "##,
    );
}

#[test]
fn interpolation_follows_to_s_through_the_jit() {
    // The same call site, compiled, before and after each change of
    // `Integer#to_s`: a definition, a prepended module, its removal, an
    // alias back to the builtin (which is the builtin again), a change of
    // visibility (which leaves the method what it was).
    run_test_once(
        r##"
        def f(x) = "v=#{x}"
        def warm = 60.times.map { |i| f(i) }.last
        res = [warm]
        module ToSWrap; def to_s = "M(#{super})"; end
        Integer.prepend(ToSWrap)
        res << warm << "#{2**70}"
        ToSWrap.send(:remove_method, :to_s)
        res << warm
        class Integer
          alias_method :orig_to_s, :to_s
          def to_s = "<#{orig_to_s}>"
        end
        res << warm
        class Integer; alias_method :to_s, :orig_to_s; end
        res << warm
        class Integer; private :to_s; end
        res << warm
        class Integer; public :to_s; end
        res << warm
        res
        "##,
    );
}

#[test]
fn interpolation_to_s_removed_or_not_a_string() {
    run_test_once(
        r##"
        res = []
        class Integer
          alias_method :orig_to_s, :to_s
          def to_s = 42
        end
        # A `to_s` that does not answer a String: `rb_any_to_s` of the
        # receiver.
        res << "v=#{10}".sub(/0x\h+/, "0xX")
        class Integer; alias_method :to_s, :orig_to_s; end
        res << "v=#{11}"
        class Float; undef_method :to_s; end
        res << begin
          "#{1.5}"
        rescue NoMethodError => e
          [e.class, e.message]
        end
        # Without `NilClass#to_s`, `Kernel#to_s` answers.
        class NilClass; remove_method :to_s; end
        res << "#{nil}".sub(/0x\h+/, "0xX")
        def nil.to_s = "nil!"
        res << "#{nil}"
        res
        "##,
    );
}

#[test]
fn interpolation_with_one_to_s_redefined() {
    // One class has lost its builtin `to_s`; the others keep being
    // formatted directly, with the encodings their `to_s` gives.
    run_test_once(
        r##"
        class NilClass; def to_s = "NIL"; end
        def f(a, b, c) = "#{a}|#{b}|#{c}|#{nil}|#{:s}|#{1.5}|#{true}|#{2**80}"
        r = nil
        100.times { |i| r = f(i, "s", :x) }
        o = Object.new
        def o.to_s = "obj"
        [
          r, r.encoding.name,
          ["#{1}", "#{:a}", "#{nil}", "#{1.5}#{2}"].map { |s| s.encoding.name },
          /#{1}#{nil}#{:a}x/.source,
          "a#{o}#{1}#{o}#{nil}b#{2}",
        ]
        "##,
    );
}

#[test]
fn interpolation_sees_a_to_s_defined_while_it_runs() {
    // Each operand is converted in turn, so the `to_s` of one can change
    // what the next one answers.
    run_test_once(
        r##"
        o = Object.new
        def o.to_s
          Integer.class_eval { def to_s = "late" }
          "o"
        end
        "#{1}#{o}#{2}"
        "##,
    );
}

#[test]
fn interpolation_refined_to_s_still_wins() {
    run_test_once(
        r##"
        module RefineIntToS
          refine Integer do
            def to_s = "refined"
          end
        end
        using RefineIntToS
        ["#{3}", "#{3}#{4}"]
        "##,
    );
}
