//! `TrueClass`/`FalseClass` operators on polymorphic-on-true/false JIT
//! sites.
//!
//! The JIT profile tags every boolean receiver `BOOL_CLASS`, whose method
//! lookup answers only while TrueClass and FalseClass resolve the name to
//! one FuncId (bool_class.rs registers `&`/`|`/`^` that way). boolean.rb's
//! ruby/spec identity alias used to re-point false's `^` at `|`, breaking
//! that unification — every `(a < 0) ^ (b < 0)` site then looped through
//! `MethodNotFound` recompiles and deopted on every execution (dewasm's
//! `i32_div_s`/`i64_div_s` sign tests, ~700 recompiles in a minute of
//! DOOM). The alias now points `|` at `^` instead (`false ^ x` and
//! `false | x` are both `!!x`, and the spec's identity check still
//! holds), keeping `^` unified; a divergent bool operator (`|`, or a
//! user redefinition on one class) falls back to the generic dispatch in
//! the JIT instead of a non-converging recompile.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn bool_xor_stays_unified() {
    run_test(
        r##"
        def g(a, b) = (a < 0) ^ (b < 0)
        r = []
        60.times { |i| r = [g(i - 30, 7), g(i - 30, -7)] }
        r << (true ^ true) << (true ^ false) << (false ^ true) << (false ^ false)
        r << (true ^ nil) << (false ^ nil) << (true ^ "x") << (false ^ 0)
        r
        "##,
    );
}

#[test]
fn divergent_bool_or_takes_generic_dispatch() {
    // `|` is the divergent pair now (true keeps its own; false's aliases
    // to `^`): a hot polymorphic-on-true/false site must neither storm
    // nor change semantics.
    run_test(
        r##"
        def h(a, b) = (a < 0) | (b < 0)
        def k(a, b) = (a < 0) & (b < 0)
        r = []
        60.times { |i| r = [h(i - 30, 7), k(i - 30, -7)] }
        r << (true | false) << (false | nil) << (false | 3) << (true | nil)
        r << (true & 1) << (false & 1) << (true & nil) << (false & nil)
        r << (false.method(:^) == false.method(:|)) << (true.method(:^) == true.method(:|))
        r
        "##,
    );
}
