//! A basic operator redefined by Ruby code that a frameless runtime helper
//! ran (a user-defined operator behind the generic operator helper, a
//! `#to_s` of string interpolation, a `#hash` of a Hash literal, ...):
//! the compiled caller must not go on with the inlined old operator
//! (#2198). The eviction's chain walk cannot convert the frame that is
//! inside the helper, so the frame checks after the call.
//!
//! Each test redefines `Integer#+` for good, so each is one run.
extern crate monoruby;
use monoruby::tests::*;

// The `==` site sees six receiver classes, so it is compiled as a call of
// the generic operator helper.
#[test]
fn generic_binop_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def ==(o)
            Integer.class_eval { def +(o) = 999 } if @redef
            true
          end
        end
        def foo(k, x, a) = (r = (k == x); [r, a + 1])
        ks = [nil, :a, "s", K.new(false), 1.5, [1]]
        120.times { |i| foo(ks[i % 6], 1, 1) }
        foo(K.new(true), 1, 1)
        "##,
    );
}

// The comparison feeds a branch.
#[test]
fn generic_cmp_before_a_branch_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def <(o)
            Integer.class_eval { def +(o) = 999 } if @redef
            true
          end
        end
        def foo(k, x, a)
          if k < x
            a + 1
          else
            a + 2
          end
        end
        ks = [1, 1.5, "s", K.new(false), 2r, 3]
        120.times { |i| foo(ks[i % 6], ks[i % 6] == "s" ? "t" : 1, 1) }
        foo(K.new(true), 1, 1)
        "##,
    );
}

#[test]
fn interpolation_to_s_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def to_s
            Integer.class_eval { def +(o) = 999 } if @redef
            "k"
          end
        end
        def foo(k, a) = (r = "#{k}"; [r, a + 1])
        60.times { foo(K.new(false), 1) }
        foo(K.new(true), 1)
        "##,
    );
}

#[test]
fn splat_to_a_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def to_a
            Integer.class_eval { def +(o) = 999 } if @redef
            [1]
          end
        end
        def foo(k, a) = (r = [*k]; [r, a + 1])
        60.times { foo(K.new(false), 1) }
        foo(K.new(true), 1)
        "##,
    );
}

#[test]
fn masgn_to_ary_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def to_ary
            Integer.class_eval { def +(o) = 999 } if @redef
            [1, 2]
          end
        end
        def foo(k, a) = (x, y = k; [x, y, a + 1])
        60.times { foo(K.new(false), 1) }
        foo(K.new(true), 1)
        "##,
    );
}

#[test]
fn range_literal_cmp_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def <=>(o)
            Integer.class_eval { def +(o) = 999 } if @redef
            0
          end
        end
        def foo(k, a) = (r = (k..k); [r.class, a + 1])
        60.times { foo(K.new(false), 1) }
        foo(K.new(true), 1)
        "##,
    );
}

#[test]
fn hash_literal_key_hash_redefines_a_basic_op() {
    run_test_once(
        r##"
        class K
          def initialize(redef) = @redef = redef
          def hash
            Integer.class_eval { def +(o) = 999 } if @redef
            1
          end
        end
        def foo(k, a) = (h = {k => 1}; [h.size, a + 1])
        60.times { foo(K.new(false), 1) }
        foo(K.new(true), 1)
        "##,
    );
}

// A redefinition the compiled body does not depend on moves the check's
// counter too: the body deopts at the check, then recompiles and keeps
// answering right.
#[test]
fn unrelated_redefinition_after_a_generic_binop() {
    run_test_once(
        r##"
        class K
          def ==(o) = true
        end
        def foo(k, x, a) = (r = (k == x); [r, a - 1])
        ks = [nil, :a, "s", K.new, 1.5, [1]]
        120.times { |i| foo(ks[i % 6], 1, 1) }
        class Float
          def *(o) = 0
        end
        res = []
        120.times { |i| res << foo(ks[i % 6], 1, 1) }
        [res.uniq, 1.5 * 2]
        "##,
    );
}
