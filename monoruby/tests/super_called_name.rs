extern crate monoruby;
use monoruby::tests::*;

// `super` inside a `define_method` body searches for the super method
// under the name the body was *called* as, not the name it was first
// installed under (CRuby resolves ZSUPER with the entry's `called_id`).
// One proc installed under several names therefore has a frame-dependent
// super target — which the JIT already declines to resolve at compile
// time, and which the VM must not stamp into its inline cache either
// (issue #1346).
//
// The names are resolved per call, so every case below calls the same
// body under both names, and through the invoker boundaries (`send`,
// `Method#call`, `bind_call`) where the callsite PC is unavailable and
// the name comes from the dispatch itself.

const LOOP: usize = if cfg!(feature = "gc-stress") { 20 } else { 300 };

#[test]
fn one_proc_under_two_names_supers_under_the_called_name() {
    run_test(
        r#"
        class SupBase
          def a = "Base#a"
          def b = "Base#b"
        end
        class SupSub < SupBase
          body = -> () { "Sub(" + super() + ")" }
          define_method(:a, body)
          define_method(:b, body)
        end
        o = SupSub.new
        [o.a, o.b, o.b, o.a]
        "#,
    );
}

/// The registration order must not decide the answer.
#[test]
fn registration_order_does_not_pick_the_super_name() {
    run_test(
        r#"
        class SupBase2
          def a = "Base#a"
          def b = "Base#b"
        end
        class SupSub2 < SupBase2
          body = -> () { "Sub(" + super() + ")" }
          define_method(:b, body)
          define_method(:a, body)
        end
        o = SupSub2.new
        [o.a, o.b]
        "#,
    );
}

/// Through the invoker boundaries, where the callee frame carries no
/// callsite PC and the name comes from the dispatch.
#[test]
fn invoker_entry_supers_under_the_called_name() {
    run_test(
        r#"
        class SupBase3
          def a = "Base#a"
          def b = "Base#b"
        end
        class SupSub3 < SupBase3
          body = -> () { "Sub(" + super() + ")" }
          define_method(:a, body)
          define_method(:b, body)
        end
        o = SupSub3.new
        [
          o.send(:b), o.send(:a),
          o.public_send(:b),
          o.method(:b).call, o.method(:a).call,
          SupSub3.instance_method(:b).bind_call(o),
        ]
        "#,
    );
}

/// The same site called under both names in a hot loop: the VM must not
/// cache one answer, and the JIT must not compile one in.
#[test]
fn a_hot_site_does_not_cache_one_super_target() {
    run_test(&format!(
        r#"
        class SupBase4
          def a = "Base#a"
          def b = "Base#b"
        end
        class SupSub4 < SupBase4
          body = -> () {{ "Sub(" + super() + ")" }}
          define_method(:a, body)
          define_method(:b, body)
        end
        o = SupSub4.new
        r = []
        {LOOP}.times {{ r << o.a; r << o.b }}
        r.uniq.sort
        "#
    ));
}

/// The shapes that must keep their existing answers: a separate block
/// per name, an `alias_method` (super searches under the *original*
/// name), and one body occupying several ancestor-chain positions.
///
/// `run_test_once`: `Twice` includes a fresh anonymous module on every
/// execution of its body, so a repeated run in the same process would
/// keep growing the chain. The inner loop warms the JIT within the one
/// run instead.
#[test]
fn neighbouring_shapes_are_unchanged() {
    run_test_once(&format!(
        r#"
        class SupBase5
          def a = "Base#a"
          def b = "Base#b"
        end
        class SupSub5 < SupBase5
          [:a, :b].each {{ |n| define_method(n) {{ "Sub(" + super() + ")" }} }}
        end

        class Al1; def name = "Al1#name"; end
        class Al2 < Al1; def name = "Al2(" + super + ")"; end
        class Al3 < Al2; alias_method :name3, :name; end

        class TwiceBase
          def self.whatever
            mod = Module.new do
              def a(ary); ary << "anon"; super; end
            end
            include mod
          end
          def a(ary); ary << "non-anon"; end
        end
        class Twice < TwiceBase
          whatever
          whatever
        end

        o = SupSub5.new
        r = []
        {LOOP}.times {{ r << [o.a, o.b, Al3.new.name3, Twice.new.a([])] }}
        r.uniq
        "#
    ));
}

/// `super` with implicit arguments from a `define_method` body still
/// raises, and the message is CRuby's.
#[test]
fn zsuper_from_a_define_method_body_still_raises() {
    run_test(
        r#"
        class SupBase6
          def a = "Base#a"
        end
        class SupSub6 < SupBase6
          define_method(:a) { super }
        end
        begin
          SupSub6.new.a
        rescue => e
          [e.class, e.message]
        end
        "#,
    );
}
