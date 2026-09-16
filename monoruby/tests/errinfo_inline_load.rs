//! A protected region saves `$!` at its entry, and the JIT now reads that
//! save straight off the `Executor` instead of through the generic
//! hooked-global runtime call.
//!
//! ```text
//! :00001 %2 = $(errinfo)
//!       mov  rdi,rbx / mov rsi,r12 / mov edx,0x6a      before
//!       movabs rax,<runtime::get_global_var> / call rax
//!
//!       mov  rax,QWORD PTR [rbx+0x198]                 after
//! ```
//!
//! `$!` is a plain `Value` field of the `Executor` and `$(errinfo)`'s hook
//! is `Some(vm.errinfo())`, so the two are the same read — but the call
//! also carried an FP save set, and it ran once per invocation of *every*
//! method carrying a `begin`..`ensure` or `begin`..`rescue`.
//!
//! The load is only equivalent because `rbx` is the *current* `Executor`,
//! which is what `$!` is per: CRuby keeps errinfo per execution context,
//! and so does monoruby (`Executor::errinfo`). These tests make the
//! protected region hot enough to be compiled and then pin the semantics
//! the inline read has to preserve — including across a Fiber and a
//! Thread, which have `Executor`s of their own.
extern crate monoruby;
use monoruby::tests::*;

/// The region-entry save and its restore, with a non-nil `$!` in flight
/// across a hot protected region.
#[test]
fn errinfo_is_restored_across_a_hot_protected_region() {
    run_test(
        r#"
        def inner
          begin
            :body
          ensure
            nil
          end
        end
        log = []
        40.times do
          begin
            raise "outer"
          rescue => e
            inner
            log << [$!.message, $!.equal?(e)]
          end
          log << $!.inspect
        end
        log.uniq
        "#,
    );
}

/// A `rescue` nested inside the hot region sets `$!` and the region's own
/// save must put it back on the way out.
#[test]
fn a_nested_rescue_inside_a_hot_region_restores_the_outer_errinfo() {
    run_test(
        r#"
        def inner
          begin
            begin
              raise "inner"
            rescue
              $!.message
            end
          ensure
            nil
          end
        end
        log = []
        40.times do
          begin
            raise "outer"
          rescue
            log << [inner, $!.message]
          end
        end
        log.uniq
        "#,
    );
}

/// `$!` is per execution context. A Fiber has its own `Executor`, so a hot
/// protected region running inside one must read that Fiber's `$!`, not
/// the resuming thread's.
#[test]
fn errinfo_stays_per_fiber_across_a_hot_region() {
    run_test(
        r#"
        def inner
          begin
            $!.nil? ? :nil : $!.message
          ensure
            nil
          end
        end
        out = []
        40.times do
          f = Fiber.new do
            out << inner
            begin
              raise "in fiber"
            rescue
              out << inner
            end
            Fiber.yield :done
          end
          begin
            raise "in main"
          rescue
            out << inner
            f.resume
            out << inner
          end
        end
        out.uniq
        "#,
    );
}

/// The same for a Thread.
#[test]
fn errinfo_stays_per_thread_across_a_hot_region() {
    run_test(
        r#"
        def inner
          begin
            $!.nil? ? :nil : $!.message
          ensure
            nil
          end
        end
        out = []
        20.times do
          begin
            raise "in main"
          rescue
            t = Thread.new { out << inner }
            t.join
            out << inner
          end
        end
        out.uniq
        "#,
    );
}

/// A `begin`..`rescue` with no `ensure` carries the same entry save, and
/// the exception object reaching the clause must be the one raised.
#[test]
fn a_hot_rescue_only_region_binds_errinfo_to_its_own_exception() {
    run_test(
        r#"
        def inner(n)
          begin
            raise ArgumentError, "e#{n}"
          rescue => e
            [e.message, $!.equal?(e), e.class.name]
          end
        end
        r = []
        40.times { |i| r << inner(i % 3) }
        [r.uniq.sort_by(&:first), $!.inspect]
        "#,
    );
}
