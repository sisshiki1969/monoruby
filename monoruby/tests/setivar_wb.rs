extern crate monoruby;
use monoruby::tests::*;

// JIT ivar stores and the generational-GC write barrier
// (`emit_write_barrier_rdi` / the shared `JitModule::write_barrier` stub).
//
// A store whose value the abstract state proves immediate (Fixnum, Symbol,
// nil, booleans) elides the barrier entirely; a possibly-heap value keeps
// the two fast tests plus a call into the shared out-of-line stub. These
// tests drive both shapes hot enough to JIT (methods, not top-level loops
// — the main script is never JIT-compiled), promote the parent object to
// the old generation with explicit `GC.start`s, and then verify the stored
// children survive another collection — i.e. the remembered set still
// learns about old-parent→young-child edges, and the elided-barrier stores
// really never needed one. Run under the `gc-stress` feature for the
// per-safepoint version of the same checks.

#[test]
fn setivar_immediate_elides_barrier() {
    // Fixnum / Symbol / nil / bool stores into an old-generation object:
    // the barrier is elided at compile time, which must be invisible to
    // the collector (immediates are not heap edges).
    run_test(
        r##"
        class P
          attr_reader :a, :b, :c, :d
          def fill(i)
            @a = i
            @b = :sym
            @c = nil
            @d = (i % 2 == 0)
          end
        end
        p = P.new
        GC.start
        GC.start   # promote p to the old generation
        i = 0
        while i < 200
          p.fill(i)
          i += 1
        end
        GC.start
        [p.a, p.b, p.c, p.d]
        "##,
    );
}

#[test]
fn setivar_heap_child_into_old_parent() {
    // Fresh (young) strings stored into an old-generation parent from
    // JIT-compiled code: the outlined write-barrier stub must record the
    // parent in the remembered set, or the next minor GC frees the
    // children while the parent still points at them.
    run_test(
        r##"
        class P
          attr_reader :a, :b
          def fill(i)
            @a = "s#{i}"
            @b = [i, i + 1]
          end
        end
        p = P.new
        GC.start
        GC.start   # promote p to the old generation
        i = 0
        while i < 200
          p.fill(i)
          i += 1
        end
        GC.start
        [p.a, p.b]
        "##,
    );
}

#[test]
fn setivar_attr_writer_barrier() {
    // The attr_writer inline path (`attr_writer`'s StoreIVarInline /
    // StoreIVarHeap) with both an immediate and a heap argument.
    run_test(
        r##"
        class P
          attr_accessor :x, :y
        end
        p = P.new
        GC.start
        GC.start
        i = 0
        while i < 200
          p.x = i          # immediate: barrier elided
          p.y = "v#{i}"    # heap child: barrier via the shared stub
          i += 1
        end
        GC.start
        [p.x, p.y]
        "##,
    );
}

#[test]
fn setivar_heap_table_stores() {
    // More ivars than the inline slots: the heap var-table store path
    // (StoreSelfIVarHeap / StoreIVarHeap) with mixed immediate and heap
    // values.
    run_test(
        r##"
        class P
          def fill(i)
            @a0 = i; @a1 = i; @a2 = i; @a3 = i
            @a4 = i; @a5 = i; @a6 = i; @a7 = i
            @a8 = "h#{i}"    # spilled to the heap table on object layout
            @a9 = :sym
          end
          def get = [@a0, @a5, @a7, @a8, @a9]
        end
        p = P.new
        GC.start
        GC.start
        i = 0
        while i < 200
          p.fill(i)
          i += 1
        end
        GC.start
        p.get
        "##,
    );
}

#[test]
fn setivar_unproven_value_keeps_barrier() {
    // A store site whose value class the state cannot prove (alternating
    // immediate / heap) must keep the runtime-tag-tested barrier.
    run_test(
        r##"
        class P
          attr_reader :v
          def put(x)
            @v = x
          end
        end
        p = P.new
        GC.start
        GC.start
        r = []
        i = 0
        while i < 200
          p.put(i.even? ? i : "s#{i}")
          i += 1
        end
        GC.start
        [p.v]
        "##,
    );
}

#[test]
fn setivar_float_value_keeps_barrier() {
    // Guarded::Float is not proof of an immediate: a non-flonum f64 boxes
    // to a heap Float RValue on the way into the ivar.
    run_test(
        r##"
        class P
          attr_reader :f
          def put(x)
            @f = x * 1.0e300   # exponent out of flonum range: heap Float
          end
        end
        p = P.new
        GC.start
        GC.start
        i = 0
        while i < 200
          p.put(i.to_f)
          i += 1
        end
        GC.start
        [p.f, p.f.class.to_s]
        "##,
    );
}
