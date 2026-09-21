//! A polymorphic-inline-cache arm that serves several receiver classes at
//! once must not resolve anything class-specific.
//!
//! `pic_groups` folds every observed class that resolves to the same
//! `func_id` into one dispatch arm, and that arm's guard proves only
//! *membership in the set*. `compile_method_call` was nevertheless handed
//! the arm's first class as `recv_class` and resolved instance-variable
//! slots against it — but two classes sharing one method need not agree on
//! where its ivars live, since a class that assigns one of its own first
//! shifts every slot after it. An `attr_reader` in a module included by
//! two such classes therefore read the *first* class's slot for both, and
//! the second receiver silently answered the wrong ivar (or `nil`).
//!
//! `expand_leaf_body` already declined on exactly this ground; the three
//! lowerings below did not.

extern crate monoruby;
use monoruby::tests::*;

/// `attr_reader` reached through a module both classes include, from one
/// hot polymorphic site. `A` pads two ivars ahead of `@v`, so `@v` sits at
/// a different slot in `A` than in `B`.
#[test]
fn a_shared_attr_reader_reads_each_class_own_slot() {
    run_test(
        r##"
        module M; attr_reader :v; end
        class A; include M; def initialize; @p1 = 1; @p2 = 2; @v = :a; end; end
        class B; include M; def initialize; @v = :b; end; end
        class C; def v = :c; end
        objs = [A.new, B.new, C.new]
        res = []
        i = 0
        while i < 90
          res << objs[i % 3].v
          i += 1
        end
        res.uniq
        "##,
    );
}

/// The same for `attr_writer`: the store must land in the receiver's own
/// slot, not in whatever the arm's first class keeps there.
#[test]
fn a_shared_attr_writer_writes_each_class_own_slot() {
    run_test(
        r##"
        module M; attr_writer :v; end
        class A
          include M
          def initialize; @p1 = 1; @p2 = 2; @v = nil; end
          def peek = [@p1, @p2, @v]
        end
        class B
          include M
          def initialize; @v = nil; end
          def peek = @v
        end
        class C; def v=(x); @z = x; end; def peek = @z; end
        objs = [A.new, B.new, C.new]
        res = []
        i = 0
        while i < 90
          o = objs[i % 3]
          o.v = i
          res << o.peek
          i += 1
        end
        res
        "##,
    );
}

/// A plain Ruby method whose whole body is an ivar store — the shape
/// `expand_ivar_stores` expands frame-free — shared by two classes with
/// different layouts.
#[test]
fn a_shared_ivar_store_body_writes_each_class_own_slot() {
    run_test(
        r##"
        module M; def setv(x); @v = x; end; end
        class A
          include M
          def initialize; @p1 = 1; @p2 = 2; @v = nil; end
          def peek = [@p1, @p2, @v]
        end
        class B
          include M
          def initialize; @v = nil; end
          def peek = @v
        end
        class C; def setv(x); @z = x; end; def peek = @z; end
        objs = [A.new, B.new, C.new]
        res = []
        i = 0
        while i < 90
          o = objs[i % 3]
          o.setv(i)
          res << o.peek
          i += 1
        end
        res
        "##,
    );
}

/// The reader shape again with a plain Ruby method body rather than an
/// `attr_reader` — the `expand_leaf_body` path, which already declined on
/// an unproven class. Kept so a future widening of the fold cannot
/// regress it unnoticed.
#[test]
fn a_shared_ivar_read_body_reads_each_class_own_slot() {
    run_test(
        r##"
        module M; def v; @v; end; end
        class A; include M; def initialize; @p1 = 1; @p2 = 2; @v = :a; end; end
        class B; include M; def initialize; @v = :b; end; end
        class C; def v = :c; end
        objs = [A.new, B.new, C.new]
        res = []
        i = 0
        while i < 90
          res << objs[i % 3].v
          i += 1
        end
        res.uniq
        "##,
    );
}
