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
    // ---- ordering: `Executor::compare_values_inner` (executor/op.rs)
    // answers `<=>` for these classes without a lookup — the numeric arms
    // compare through `RV`, and `Array#sort` / `#max` / `#min` and every
    // other `Comparable`-style builtin that goes through it inherits the
    // fast path. `Array#sort`'s homogeneous specialization
    // (`Executor::sort`) rests on the same pairs.
    (INTEGER_CLASS, "<=>"),
    (FLOAT_CLASS, "<=>"),
    (STRING_CLASS, "<=>"),
    (SYMBOL_CLASS, "<=>"),
    (NIL_CLASS, "<=>"),
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
    /// Just the *names* in [`BASIC_OP_DEFS`], any class. Used to tell an
    /// operator apart from a conversion performed on the caller's behalf
    /// when resolving under refinements — see
    /// `Executor::basic_op_refinements`.
    names: HashSet<IdentId>,
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
    ///
    /// Union of the two sets below: this is "the fast path for this pair is
    /// no longer unconditionally sound", which is what every runtime guard
    /// wants to know.
    redefined_set: HashSet<(ClassId, IdentId)>,
    /// The subset replaced by an ordinary global redefinition. No scope
    /// escapes one, so the JIT must never inline these.
    globally_redefined_set: HashSet<(ClassId, IdentId)>,
    /// The subset invalidated *only* by a refinement. A refinement binds
    /// lexically, so a body being compiled under a set that does not refine
    /// the pair may still inline it — see
    /// `JitContext::assume_basic_op`. The VM's assembly guard has no call
    /// site to ask about and stays coarse.
    refined_set: HashSet<(ClassId, IdentId)>,
    /// Pairs replaced since the last drain, awaiting CRuby's
    /// `:performance` warning ("Redefining 'Integer#+' disables
    /// interpreter and JIT optimizations"). Marking happens deep inside
    /// `Store`, which has no executor to run `Warning.warn` with, so the
    /// pair waits here until `Executor::invoke_method_added` — the one
    /// place every `def` / `define_method` / `alias` / `attr_*` passes
    /// through with both in hand.
    pending_warnings: Vec<(ClassId, IdentId)>,
}

impl BasicOpTable {
    pub(crate) fn new() -> Self {
        Self {
            set: BASIC_OP_DEFS
                .iter()
                .map(|(class_id, name)| (*class_id, IdentId::get_id(name)))
                .collect(),
            names: BASIC_OP_DEFS
                .iter()
                .map(|(_, name)| IdentId::get_id(name))
                .collect(),
            armed: false,
            redefined: false,
            redefined_set: HashSet::default(),
            globally_redefined_set: HashSet::default(),
            refined_set: HashSet::default(),
            pending_warnings: Vec::new(),
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

    /// Whether `name` is an operator some class gets a fast path for.
    pub(crate) fn is_basic_op_name(&self, name: IdentId) -> bool {
        self.names.contains(&name)
    }

    /// Record that `class_id#name` was replaced outright.
    pub(crate) fn mark_redefined(&mut self, class_id: ClassId, name: IdentId) {
        self.redefined = true;
        self.redefined_set.insert((class_id, name));
        self.globally_redefined_set.insert((class_id, name));
        self.pending_warnings.push((class_id, name));
    }

    /// Record that `class_id#name` was replaced *by a refinement*. Same
    /// consequence for every unconditional fast path, but a compiling scope
    /// that does not activate the refinement can still inline it.
    pub(crate) fn mark_refined(&mut self, class_id: ClassId, name: IdentId) {
        self.redefined = true;
        self.redefined_set.insert((class_id, name));
        self.refined_set.insert((class_id, name));
    }

    /// Whether this pair was replaced by something no scope escapes.
    pub(crate) fn globally_redefined_pair(&self, class_id: ClassId, name: IdentId) -> bool {
        self.redefined && self.globally_redefined_set.contains(&(class_id, name))
    }

    /// Whether some refinement replaces this pair.
    pub(crate) fn refined_pair(&self, class_id: ClassId, name: IdentId) -> bool {
        self.redefined && self.refined_set.contains(&(class_id, name))
    }

    /// Whether *anything* has been replaced. The cheap gate.
    pub(crate) fn redefined(&self) -> bool {
        self.redefined
    }

    /// Whether this exact pair has been replaced.
    pub(crate) fn redefined_pair(&self, class_id: ClassId, name: IdentId) -> bool {
        self.redefined && self.redefined_set.contains(&(class_id, name))
    }

    /// Take the pairs replaced since the last call, for the
    /// `:performance` warning. Empty in every program that leaves the
    /// builtins alone.
    pub(crate) fn take_pending_warnings(&mut self) -> Vec<(ClassId, IdentId)> {
        std::mem::take(&mut self.pending_warnings)
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
            ("Integer", "<=>(o)", "1 <=> 2"),
            ("Float", "<=>(o)", "1.0 <=> 2.0"),
            ("String", "<=>(o)", r#""a" <=> "b""#),
            ("Symbol", "<=>(o)", ":a <=> :b"),
            ("NilClass", "<=>(o)", "nil <=> nil"),
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

    /// The `<=>` pairs license more than the operator itself: every
    /// builtin that orders values goes through
    /// `Executor::compare_values_inner`, and `Array#sort` additionally
    /// specializes a homogeneous array (`sort_homogeneous`). A
    /// redefinition has to reach both — before the pairs were tracked,
    /// `[3,1,2].sort` kept comparing fixnums directly and returned a
    /// sorted array where CRuby raises (the redefined `<=>` returns a
    /// Symbol, which is not a valid comparison result).
    #[test]
    fn sort_honors_redefined_cmp() {
        run_test_once(
            r#"
            class Integer; def <=>(o); :OVERRIDDEN; end; end
            begin
              [3, 1, 2].sort
            rescue => e
              e.class
            end
            "#,
        );
        run_test_once(
            r#"
            class String; def <=>(o); :OVERRIDDEN; end; end
            begin
              ["c", "a", "b"].sort
            rescue => e
              e.class
            end
            "#,
        );
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

    /// The classic way to redefine an operator without losing it: alias the
    /// original, then call the alias from the replacement. The alias holds
    /// the *builtin* entry, so reaching the implementation through it must
    /// not consult the basic-op flag — otherwise the implementation
    /// re-dispatches the operator's name, lands back on the replacement, and
    /// recurses until the stack runs out. Every operator whose builtin body
    /// is one of the shared `op::*_values` helpers is covered here.
    #[test]
    fn aliasing_an_operator_before_redefining_it_does_not_recurse() {
        for (class, op, expr) in [
            ("Integer", "+", "3 + 4"),
            ("Integer", "-", "3 - 4"),
            ("Integer", "*", "3 * 4"),
            ("Integer", "/", "12 / 4"),
            ("Integer", "%", "13 % 4"),
            ("Integer", "**", "3 ** 4"),
            ("Integer", "<<", "3 << 4"),
            ("Integer", ">>", "48 >> 4"),
            ("Integer", "<", "3 < 4"),
            ("Integer", "<=", "3 <= 4"),
            ("Integer", ">", "3 > 4"),
            ("Integer", ">=", "3 >= 4"),
            ("Float", "+", "3.0 + 4.0"),
            ("Float", "-", "3.0 - 4.0"),
            ("Float", "*", "3.0 * 4.0"),
            ("Float", "/", "3.0 / 4.0"),
            ("Float", "==", "3.0 == 4.0"),
            ("Float", "<", "3.0 < 4.0"),
            ("Float", ">=", "3.0 >= 4.0"),
        ] {
            run_test_once(&format!(
                "class {class}; alias __orig :{op}; def {op}(o); __orig(o); end; end; {expr}"
            ));
        }
        // Unary operators take no argument, so they need their own shape.
        for (class, op, expr) in [
            ("Integer", "-@", "-(3)"),
            ("Float", "-@", "-(3.0)"),
            ("Integer", "~", "~(3)"),
        ] {
            run_test_once(&format!(
                "class {class}; alias __orig :{op}; def {op}; __orig; end; end; {expr}"
            ));
        }
    }

    /// Redefining an operator that a *compiled* method inlined has to reach
    /// that method: its JIT body computed the operation with no runtime
    /// check, so the body must be thrown away and the on-stack frames
    /// evicted. `fib(20)` and `loopy(2000)` are both past the test-mode JIT
    /// thresholds on their own, so the redefinition lands on warm compiled
    /// code. `run_test_once`, not `run_test`: aliasing `+` a second time in
    /// the same process would capture the replacement and recurse (CRuby
    /// does the same).
    #[test]
    fn redefining_an_operator_a_compiled_method_inlined() {
        run_test_once(
            r#"
            def fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
            def loopy(n); s = 0; i = 0; while i < n; s = s + i; i = i + 1; end; s; end
            res = [fib(20), loopy(2000)]
            class Integer
              alias __plus +
              def +(o) = __plus(o)
            end
            res << fib(20) << loopy(2000)
        "#,
        );
    }

    /// The other half: a redefinition of an operator the compiled body never
    /// inlined must leave that body alone. Only observable as behaviour in
    /// that the results stay right, but the property under test is that
    /// `evict_jit_code_for_bop` finds nothing to evict here.
    #[test]
    fn redefining_an_unrelated_operator_leaves_compiled_code_alone() {
        run_test(
            r#"
            def fib(n) = n < 2 ? n : fib(n-1) + fib(n-2)
            res = [fib(20)]
            class Float; def *(o) = 0.0; end
            class Array; def [](i) = nil; end
            res << fib(20) << (2.0 * 3.0) << [1, 2][0]
        "#,
        );
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
