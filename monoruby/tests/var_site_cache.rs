//! The per-site caches of `LoadGvar` / `StoreGvar` / `LoadCvar` /
//! `StoreCvar` (the instruction's second word): each case warms a site
//! past the JIT threshold, changes what the name resolves to, and reads
//! through the same site again.

use monoruby::tests::*;

#[test]
fn cvar_cache_follows_writes() {
    run_test_once(
        r#"
        class CvA; @@x = 1; def x; @@x; end; def setx(v); @@x = v; end; end
        class CvB < CvA; def y; @@x; end; def sety(v); @@x = v; end; end
        b = CvB.new
        res = []
        30.times { |i| res << b.y; res << b.x; b.sety(i) if i % 7 == 0; b.setx(i * 3) if i % 11 == 0 }
        res << CvA.class_variable_get(:@@x)
        res
        "#,
    );
}

#[test]
fn cvar_cache_new_definition_and_include() {
    // A variable defined later, and a module bringing one in. (No class
    // body runs after the warm-up: entering one sets its lexical scope,
    // which would move the state on its own.)
    run_test_once(
        r#"
        class CvC; def z; @@z; rescue NameError => e; e.message; end; end
        c = CvC.new
        res = []
        30.times { res << c.z }
        CvC.class_variable_set(:@@z, 5)
        30.times { res << c.z }
        module CvN; @@w = :n; end
        class CvD0; @@w = :d0; end
        class CvD < CvD0; def w; @@w; rescue RuntimeError => e; e.message; end; end
        d = CvD.new
        30.times { res << d.w }
        CvD.include(CvN)
        30.times { res << d.w }
        res
        "#,
    );
}

#[test]
fn cvar_cache_overtaken() {
    // A superclass gaining the variable its subclass already had.
    run_test_once(
        r#"
        class CvP; end
        class CvQ < CvP; @@o = 1; def o; @@o; rescue RuntimeError => e; e.message; end; end
        q = CvQ.new
        res = []
        30.times { res << q.o }
        CvP.class_variable_set(:@@o, 2)
        30.times { res << q.o }
        res
        "#,
    );
}

#[test]
fn cvar_cache_class_variable_set_and_singleton_scope() {
    run_test_once(
        r#"
        class CvE; def self.r; @@e; rescue NameError => e; e.message; end; def r; @@e; rescue NameError => e; e.message; end; end
        res = []
        30.times { res << CvE.r; res << CvE.new.r }
        CvE.class_variable_set(:@@e, 7)
        30.times { |i| res << CvE.r; res << CvE.new.r; CvE.class_variable_set(:@@e, i) }
        res
        "#,
    );
}

#[test]
fn cvar_cache_remove() {
    // CRuby 4.0.9 reads a freed slot here (a crash), so the expectation is
    // spelled out instead of asked of a live ruby.
    let v = run_test_no_result_check(
        r#"
        class CvR; @@x = 1; def x; @@x; end; end
        class CvS < CvR; end
        s = CvS.new
        30.times { s.x }
        CvR.remove_class_variable(:@@x)
        a = (s.x rescue $!.class)
        CvR.class_variable_set(:@@x, 42)
        b = 30.times.map { s.x }.uniq
        a == NameError && b == [42]
        "#,
    );
    assert!(v.as_bool());
}

#[test]
fn gvar_cache_alias_and_trace() {
    run_test_once(
        r#"
        $gc_g = 1
        def gc_rg; $gc_g; end
        def gc_wg(v); $gc_g = v; end
        def gc_rh; $gc_h; end
        def gc_wh(v); $gc_h = v; end
        res = []
        30.times { |i| res << gc_rg; gc_wg(i); res << gc_rh }
        alias $gc_h $gc_g
        30.times { |i| res << gc_rh; gc_wh(i * 2); res << gc_rg }
        trace = []
        trace_var(:$gc_g) { |v| trace << v }
        30.times { |i| gc_wg(i) }
        untrace_var(:$gc_g)
        30.times { |i| gc_wh(-i) }
        res << trace << gc_rg
        res
        "#,
    );
}

#[test]
fn gvar_cache_hooked() {
    // Hooked and special globals keep their hooks behind a filled cache.
    run_test_once(
        r#"
        def gc_rs; $/; end
        def gc_ws(v); $/ = v; end
        def gc_m(s); s =~ /(b)/; $1; end
        res = []
        30.times { res << gc_rs; res << gc_m("abc") }
        res << (gc_ws(1) rescue $!.class)
        res << gc_rs
        res
        "#,
    );
}

#[test]
fn errinfo_restore_in_jit() {
    // A protected region saves `$!` on entry and restores it on exit.
    run_test_once(
        r#"
        def ei
          begin
            raise "inner"
          rescue
            $!.message
          ensure
            1
          end
        end
        def eo
          raise "outer"
        rescue
          [ei, $!.message]
        end
        res = []
        30.times { res << ei; res << $!.inspect; res << eo; res << $!.inspect }
        res
        "#,
    );
}
