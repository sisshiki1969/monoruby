//! The set of `(class, method)` pairs whose builtin definition the VM and
//! the JIT are allowed to assume — monoruby's answer to CRuby's
//! `vm_opt_method_defs`.
//!
//! Both tiers compute `1 + 2`, `a < b`, `!x`, `ary[i]` … without consulting
//! the method table. That is only sound while nobody has replaced the
//! builtin. This table names every pair that licenses such a fast path, so
//! `Store::insert_method` / `remove_method` can notice the moment one is
//! replaced and fall back (`Store::set_bop_redefine`).
//!
//! **The membership test is on the pair, not on the entry being
//! overwritten.** That distinction is the whole point: `Integer#!`,
//! `Integer#+@`, `Integer#~`, `NilClass#==` and friends have no entry in
//! their own class at all (they are inherited from `Object` /
//! `BasicObject`), so `def !` inside `class Integer` is an *insert*, not an
//! overwrite — and an "was the old entry a basic op?" test can never fire
//! for them, however many methods are flagged. CRuby has the same shape for
//! the same reason.
//!
//! See `doc/bop_redefinition.md`.

use super::*;

/// Every `(class, method)` pair with a lookup-free fast path.
///
/// Each group names the code that licenses it, so a new fast path (or a
/// deleted one) can be matched against this list by reading. The pairs were
/// derived from those implementations and then confirmed one by one against
/// CRuby 4.0.2 — see the differential sweep in `doc/bop_redefinition.md`
/// §1.4 and the `basic_op_coverage_*` tests in `builtins/module.rs`.
pub(crate) const BASIC_OP_DEFS: &[(ClassId, &str)] = &[
    // ---- arithmetic: `binop_values` (executor/op/binary_ops.rs) computes
    // Integer / Float directly through `RealKind`, and Complex through its
    // own arms. The JIT additionally folds and inlines the Integer / Float
    // cases with no guard (`jitgen/compile/binary_op.rs`).
    (INTEGER_CLASS, "+"),
    (INTEGER_CLASS, "-"),
    (INTEGER_CLASS, "*"),
    (INTEGER_CLASS, "/"),
    (INTEGER_CLASS, "%"),
    (INTEGER_CLASS, "**"),
    (INTEGER_CLASS, "<<"),
    (INTEGER_CLASS, ">>"),
    (INTEGER_CLASS, "&"),
    (INTEGER_CLASS, "|"),
    (INTEGER_CLASS, "^"),
    (FLOAT_CLASS, "+"),
    (FLOAT_CLASS, "-"),
    (FLOAT_CLASS, "*"),
    (FLOAT_CLASS, "/"),
    (FLOAT_CLASS, "%"),
    (FLOAT_CLASS, "**"),
    (COMPLEX_CLASS, "+"),
    (COMPLEX_CLASS, "-"),
    (COMPLEX_CLASS, "*"),
    (COMPLEX_CLASS, "%"),
    // ---- relational: `cmp_values!` (executor/op.rs) covers Integer and
    // Float only; every other receiver already dispatches.
    (INTEGER_CLASS, "<"),
    (INTEGER_CLASS, "<="),
    (INTEGER_CLASS, ">"),
    (INTEGER_CLASS, ">="),
    (FLOAT_CLASS, "<"),
    (FLOAT_CLASS, "<="),
    (FLOAT_CLASS, ">"),
    (FLOAT_CLASS, ">="),
    // ---- equality: `eq_values_vis` / `ne_values` / `cmp_teq_values_impl`
    // answer directly for nil, booleans, Integer, Float, Symbol and String.
    (INTEGER_CLASS, "=="),
    (INTEGER_CLASS, "!="),
    (INTEGER_CLASS, "==="),
    (FLOAT_CLASS, "=="),
    (FLOAT_CLASS, "!="),
    (FLOAT_CLASS, "==="),
    (STRING_CLASS, "=="),
    (STRING_CLASS, "!="),
    (SYMBOL_CLASS, "=="),
    (SYMBOL_CLASS, "==="),
    (NIL_CLASS, "=="),
    (NIL_CLASS, "==="),
    (TRUE_CLASS, "=="),
    (TRUE_CLASS, "==="),
    (FALSE_CLASS, "=="),
    (FALSE_CLASS, "==="),
    // ---- unary: `neg_value` / `pos_value` / `bitnot_value` (Integer,
    // Float, Complex) and `not_value`, whose truthiness answer covers every
    // immediate plus String and Complex — a heap object of any other class
    // dispatches, so `!` needs no entry for user classes.
    (INTEGER_CLASS, "-@"),
    (INTEGER_CLASS, "+@"),
    (INTEGER_CLASS, "~"),
    (FLOAT_CLASS, "-@"),
    (FLOAT_CLASS, "+@"),
    (COMPLEX_CLASS, "-@"),
    (INTEGER_CLASS, "!"),
    (FLOAT_CLASS, "!"),
    (STRING_CLASS, "!"),
    (SYMBOL_CLASS, "!"),
    (COMPLEX_CLASS, "!"),
    (NIL_CLASS, "!"),
    (TRUE_CLASS, "!"),
    (FALSE_CLASS, "!"),
    // ---- indexing: `runtime::get_index` answers Array and Hash receivers
    // inline, and `runtime::set_index` answers `Array#[]=` with a fixnum
    // index (`Hash#[]=` already dispatches, so it needs no entry). Unlike
    // the dispatch-table ops these are plain Rust helpers, so they consult
    // [`BasicOpTable::redefined`] directly instead of being swapped out.
    (ARRAY_CLASS, "[]"),
    (HASH_CLASS, "[]"),
    (ARRAY_CLASS, "[]="),
];

/// Interned [`BASIC_OP_DEFS`], plus the bootstrap latch.
pub(crate) struct BasicOpTable {
    set: HashSet<(ClassId, IdentId)>,
    /// False until the builtins — Rust *and* the Ruby ones in
    /// `builtins/*.rb` — have finished defining themselves. Their own
    /// definitions land in this very table, so an armed lookup during
    /// bootstrap would report the interpreter monkey-patching itself and
    /// disable every optimization before the user's first line runs.
    armed: bool,
    /// Set once any pair in the table has been replaced — the one-test
    /// gate every Rust-side fast path opens with, so a program that
    /// redefines nothing pays a single bool per helper call.
    redefined: bool,
    /// *Which* pairs were replaced. Only consulted once `redefined` is
    /// true, i.e. never in a program that leaves the builtins alone, so a
    /// hash probe here costs nothing in the case that matters.
    redefined_set: HashSet<(ClassId, IdentId)>,
    /// Whether any *Integer* pair was replaced. The VM's assembly fast
    /// paths are fixnum-only — every one of them opens with
    /// `guard_rdi_rsi_fixnum` / `guard_rdi_fixnum` and drops non-fixnum
    /// operands into a Rust helper — so redefining `Float#+`, `String#==`
    /// or `Array#[]` leaves the assembly correct as written. Only an
    /// Integer redefinition invalidates it, and only then is the
    /// dispatch-table swap needed. This is what keeps an unrelated
    /// `Float#+` from costing the whole process its VM fast paths.
    integer_redefined: bool,
}

impl BasicOpTable {
    pub(crate) fn new() -> Self {
        Self {
            set: BASIC_OP_DEFS
                .iter()
                .map(|(class_id, name)| (*class_id, IdentId::get_id(name)))
                .collect(),
            armed: false,
            redefined: false,
            redefined_set: HashSet::default(),
            integer_redefined: false,
        }
    }

    /// Start reporting. Called once, after startup.rb and the gems have
    /// loaded and before user code runs.
    pub(crate) fn arm(&mut self) {
        self.armed = true;
    }

    /// Whether replacing `class_id#name` invalidates a fast path.
    pub(crate) fn contains(&self, class_id: ClassId, name: IdentId) -> bool {
        self.armed && self.set.contains(&(class_id, name))
    }

    /// Record that `class_id#name` was replaced.
    pub(crate) fn mark_redefined(&mut self, class_id: ClassId, name: IdentId) {
        self.redefined = true;
        self.redefined_set.insert((class_id, name));
        if class_id == INTEGER_CLASS {
            self.integer_redefined = true;
        }
    }

    /// Whether *anything* has been replaced. The cheap gate.
    pub(crate) fn redefined(&self) -> bool {
        self.redefined
    }

    /// Whether this exact pair has been replaced.
    pub(crate) fn redefined_pair(&self, class_id: ClassId, name: IdentId) -> bool {
        self.redefined && self.redefined_set.contains(&(class_id, name))
    }

    /// Whether the VM's fixnum assembly is still valid. See
    /// [`Self::integer_redefined`].
    pub(crate) fn integer_redefined(&self) -> bool {
        self.integer_redefined
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tests::*;

    #[test]
    fn basic_op_defs_have_no_duplicates() {
        let mut seen = std::collections::HashSet::new();
        for (class_id, name) in BASIC_OP_DEFS {
            assert!(
                seen.insert((*class_id, *name)),
                "duplicate entry in BASIC_OP_DEFS: {class_id:?}#{name}"
            );
        }
    }

    /// Every pair in the table, redefined and then exercised through
    /// operator syntax — the form that takes the fast path. `send` would
    /// pass even with no table at all, since it goes through the method
    /// table. One process per case: a redefinition is global and permanent,
    /// so sharing one would let the first case mask the rest.
    #[test]
    fn basic_op_redefinition_is_honored_per_pair() {
        let cases: &[(&str, &str, &str)] = &[
            // (class, `def` header, expression under test)
            ("Integer", "+(o)", "3 + 4"),
            ("Integer", "-(o)", "3 - 4"),
            ("Integer", "*(o)", "3 * 4"),
            ("Integer", "/(o)", "3 / 4"),
            ("Integer", "%(o)", "3 % 4"),
            ("Integer", "**(o)", "3 ** 4"),
            ("Integer", "<<(o)", "3 << 4"),
            ("Integer", ">>(o)", "3 >> 4"),
            ("Integer", "&(o)", "3 & 4"),
            ("Integer", "|(o)", "3 | 4"),
            ("Integer", "^(o)", "3 ^ 4"),
            ("Integer", "<(o)", "3 < 4"),
            ("Integer", "<=(o)", "3 <= 4"),
            ("Integer", ">(o)", "3 > 4"),
            ("Integer", ">=(o)", "3 >= 4"),
            ("Integer", "==(o)", "3 == 4"),
            ("Integer", "!=(o)", "3 != 4"),
            ("Integer", "===(o)", "3 === 4"),
            ("Integer", "-@", "-(3)"),
            ("Integer", "+@", "+(3)"),
            ("Integer", "~", "~(3)"),
            ("Integer", "!", "!(3)"),
            ("Float", "+(o)", "3.0 + 4.0"),
            ("Float", "-(o)", "3.0 - 4.0"),
            ("Float", "*(o)", "3.0 * 4.0"),
            ("Float", "/(o)", "3.0 / 4.0"),
            ("Float", "%(o)", "3.0 % 4.0"),
            ("Float", "**(o)", "3.0 ** 4.0"),
            ("Float", "<(o)", "3.0 < 4.0"),
            ("Float", "<=(o)", "3.0 <= 4.0"),
            ("Float", ">(o)", "3.0 > 4.0"),
            ("Float", ">=(o)", "3.0 >= 4.0"),
            ("Float", "==(o)", "3.0 == 4.0"),
            ("Float", "!=(o)", "3.0 != 4.0"),
            ("Float", "===(o)", "3.0 === 4.0"),
            ("Float", "-@", "-(3.0)"),
            ("Float", "+@", "+(3.0)"),
            ("Float", "!", "!(3.0)"),
            ("String", "==(o)", r#""a" == "b""#),
            ("String", "!=(o)", r#""a" != "b""#),
            ("String", "!", r#"!("a")"#),
            ("Symbol", "==(o)", ":a == :b"),
            ("Symbol", "===(o)", ":a === :b"),
            ("Symbol", "!", "!(:a)"),
            ("NilClass", "==(o)", "nil == nil"),
            ("NilClass", "===(o)", "nil === nil"),
            ("NilClass", "!", "!(nil)"),
            ("TrueClass", "==(o)", "true == true"),
            ("TrueClass", "===(o)", "true === true"),
            ("TrueClass", "!", "!(true)"),
            ("FalseClass", "==(o)", "false == false"),
            ("FalseClass", "===(o)", "false === false"),
            ("FalseClass", "!", "!(false)"),
            ("Complex", "+(o)", "Complex(1,2) + Complex(3,4)"),
            ("Complex", "-(o)", "Complex(1,2) - Complex(3,4)"),
            ("Complex", "*(o)", "Complex(1,2) * Complex(3,4)"),
            ("Complex", "%(o)", "Complex(1,2) % Complex(3,4)"),
            ("Complex", "-@", "-(Complex(1,2))"),
            ("Complex", "!", "!(Complex(1,2))"),
            ("Array", "[](i)", "[1,2][0]"),
            ("Hash", "[](k)", "({1=>2})[1]"),
        ];
        // Keeps the table and this list from drifting apart: a new entry in
        // `BASIC_OP_DEFS` fails here until it is exercised. The one entry
        // not covered above is `Array#[]=`, whose own test follows (an
        // assignment expression cannot show which `[]=` ran).
        assert_eq!(
            cases.len() + 1,
            BASIC_OP_DEFS.len(),
            "every BASIC_OP_DEFS entry needs a case here"
        );
        for (class, header, expr) in cases {
            run_test_once(&format!(
                "class {class}; def {header}; :OVERRIDDEN; end; end; {expr}"
            ));
        }
    }

    /// `Array#[]=` is in the table but cannot be observed through the
    /// expression's value (an assignment always evaluates to its RHS), so
    /// it gets its own case that inspects the receiver instead.
    #[test]
    fn basic_op_index_assign_redefinition_is_honored() {
        run_test_once(
            r#"
            class Array; def []=(i, v); nil; end; end
            a = [1, 2]
            a[0] = 9
            a
            "#,
        );
    }

    /// Removal invalidates a fast path just as replacement does. This is
    /// the case the old "was the displaced entry a basic op?" test could
    /// not see at all — `remove_method` never displaces anything.
    #[test]
    fn basic_op_removal_is_honored() {
        run_test_error("class Integer; remove_method(:+); end; 1 + 2");
        run_test_error("class Integer; undef_method(:+); end; 1 + 2");
    }

    /// Defining an operator that the class does not own — it is inherited
    /// from `Object` / `BasicObject` — still shadows a fast path. Insert,
    /// not overwrite: the shape the pair-keyed table exists for.
    #[test]
    fn basic_op_shadowing_an_inherited_operator_is_honored() {
        run_test_once("class Integer; def !; :OVERRIDDEN; end; end; !(3)");
        run_test_once("class NilClass; def ==(o); :OVERRIDDEN; end; end; nil == nil");
        run_test_once("class Integer; def ~; :OVERRIDDEN; end; end; ~(3)");
    }

    /// A redefinition on one class must not cost the *other* classes their
    /// fast paths. This is the property Step 2a exists for: before it, any
    /// entry in the table tripped a process-wide fallback.
    #[test]
    fn redefining_one_class_leaves_the_others_alone() {
        run_test_once(
            r#"
            class Float; def +(o); :OVERRIDDEN; end; end
            # Integer arithmetic, comparison and indexing must still be the
            # builtins, and Float#+ must be the override.
            [1 + 2, 3 * 4, 1 < 2, [7, 8][1], ({1 => 2})[1], 1.0 + 2.0]
            "#,
        );
        run_test_once(
            r#"
            class String; def ==(o); :OVERRIDDEN; end; end
            [1 == 1, :a == :a, nil == nil, 1.0 == 1.0, "a" == "a"]
            "#,
        );
    }

    /// A user class is not in the table and must keep dispatching normally
    /// — no fast path ever assumed its operators, so redefining them must
    /// not trip the global fallback.
    #[test]
    fn user_class_operators_are_untouched() {
        run_test(
            r#"
            class BopUser
              def +(o); :plus; end
              def <(o); :lt; end
              def !; :bang; end
              def [](i); :aref; end
            end
            u = BopUser.new
            [u + 1, u < 1, !u, u[0], 1 + 2, 1 < 2, !1, [9][0]]
            "#,
        );
    }
}
