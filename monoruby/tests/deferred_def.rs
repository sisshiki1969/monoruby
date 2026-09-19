//! `def` bodies the parse hands on unlowered.
//!
//! A method body is lowered where its bytecode is generated, not where
//! the file is parsed, so the whole file's method bodies never exist as
//! monoruby AST at once (see `crate::ast::deferred`). The lowering
//! resumes on a fresh `Lowerer` seeded only with the prism scope level
//! and the pattern-match temp counter — the claim being that a method
//! body needs nothing else from the scope it was written in.
//!
//! These tests are that claim. Each exercises a construct whose lowering
//! reads enclosing-scope state, so a body lowered in the wrong scope
//! would answer differently or fail to compile: anonymous argument
//! forwarding (`anon_rest_levels`), the pattern-match desugar's hidden
//! locals (`pm_temp`), `__FILE__` / `__LINE__` (the per-file context),
//! and the nesting cases where one deferred body is lowered from inside
//! another.
//!
//! The remaining piece of enclosing state, `scope_wraps` — the closure
//! scope synthesized around an `END { }` body — is covered by
//! `begin_end.rs`, whose effects only show at process exit.
extern crate monoruby;
use monoruby::tests::*;

#[test]
fn anonymous_forwarding_in_a_deferred_body() {
    // `*` and `**` are locals bound by this `def`'s own parameter list,
    // and `fwd`'s forwarding call resolves them by counting scopes from
    // the binder. Lowered in a scope one level off, the block form below
    // would read the wrong frame.
    run_test(
        r##"
        def inner(*a, **k, &b) = [a, k, b ? b.call : nil]
        def fwd(*, **, &)
          inner(*, **, &)
        end
        def through_block(*)
          [1].map { inner(*) }
        end
        [fwd(1, 2, x: 3) { "blk" }, through_block(7, 8)]
        "##,
    );
}

#[test]
fn pattern_matching_in_a_deferred_body() {
    // The desugar synthesizes `%pm<n>` locals; the counter is seeded from
    // the definition site so two bodies cannot name the same slot in a
    // scope they share.
    run_test(
        r##"
        def pat(x)
          case x
          in [1, *rest] then ["arr", rest]
          in {a: Integer => n} then ["hash", n]
          in String => s if s.size > 2 then ["str", s]
          else "none"
          end
        end
        def destructure(v)
          v => {k: Integer => got}
          got
        end
        [pat([1, 2, 3]), pat({a: 9}), pat("abcd"), pat(:z), destructure({k: 3})]
        "##,
    );
}

#[test]
fn file_and_line_in_a_deferred_body() {
    // `__LINE__` is inlined by the lowerer, which now runs long after the
    // parse: the line offset has to come from the deferred context rather
    // than from whatever is being lowered at the time.
    run_test_once(
        r##"
        def where = __LINE__
        def where_in_block = [1].map { __LINE__ }
        [where, where_in_block, __FILE__.class]
        "##,
    );
}

#[test]
fn nested_deferred_bodies() {
    // The inner `def` is deferred again while the outer one is being
    // lowered from inside bytecodegen, and compiled from inside the
    // outer's own compilation.
    run_test(
        r##"
        def deep
          def mid
            def leaf = "leaf"
            leaf
          end
          mid
        end
        class Holder
          class << self
            def sing(n)
              acc = n
              [1, 2].each { |i| acc += i }
              acc
            end
          end
          def inst(a, b = 2, *c, d:, e: 5, &blk)
            x = a + b
            [1, 2].each { |i| x += i }
            (->(n) { n * x }).call(d) + e + c.size
          end
        end
        [deep, Holder.sing(10), Holder.new.inst(1, d: 10)]
        "##,
    );
}

#[test]
fn def_in_an_eval_and_a_binding() {
    // Both parse with prism scopes seeded from the caller, and both defer
    // their bodies like an ordinary file parse.
    run_test_once(
        r##"
        a = eval("def in_eval(v); w = v * 2; [1].each { w += 1 }; w; end; in_eval(5)")
        def with_binding
          q = 41
          binding
        end
        b = with_binding.eval("def from_binding(z) = z + 1; from_binding(q)")
        [a, b]
        "##,
    );
}
