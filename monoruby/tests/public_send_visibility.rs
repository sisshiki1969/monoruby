extern crate monoruby;
use monoruby::tests::*;

// `public_send` rejects a private or protected method, but the rejection
// is a `method_missing` dispatch, not a raise: CRuby's
// `rb_method_call_status` hands a visibility violation to
// `method_missing` exactly as it hands over a missing method, and
// `NoMethodError` is what the default implementation raises (issue
// #1343). An ordinary explicit-receiver call already worked this way;
// only `public_send` raised on the spot.

/// A receiver that defines `method_missing` sees the rejected name
/// arrive there, with the arguments, keywords and block intact.
#[test]
fn a_visibility_rejection_reaches_method_missing() {
    run_test(
        r#"
        class PsC
          private def priv(a, b = 2, *rest, k: 9, &blk) = [a, b, rest, k, blk&.call]
          protected def prot(x) = "prot#{x}"
          def method_missing(name, *args, **kw, &blk) = ["mm", name, args, kw, blk&.call]
        end
        c = PsC.new
        [
          c.public_send(:priv, 1, 2, 3, k: 4) { :blk },
          c.public_send(:prot, 7),
          c.public_send(:nope, 5),
        ]
        "#,
    );
}

/// Without a handler, the default `method_missing` raises — wording the
/// message by the visibility it finds, so this case is unchanged.
#[test]
fn without_a_handler_the_default_still_raises_by_visibility() {
    run_test(
        r#"
        class PsB
          private def priv = "priv"
          protected def prot = "prot"
          def pub = "pub"
        end
        b = PsB.new
        [
          (begin; b.public_send(:priv); rescue => e; [e.class, e.message]; end),
          (begin; b.public_send(:prot); rescue => e; [e.class, e.message]; end),
          (begin; b.public_send(:nope); rescue => e; [e.class, e.message]; end),
          b.public_send(:pub),
        ]
        "#,
    );
}

/// The neighbours that must not move: `send` still reaches a private
/// method, an explicit-receiver call still routes to `method_missing`,
/// and `public_send` from inside the class still refuses a protected
/// method that a plain call accepts.
#[test]
fn the_neighbouring_shapes_are_unchanged() {
    run_test(
        r#"
        class PsE
          private def priv = "priv"
          protected def prot = "prot"
          def method_missing(name, *a) = "mm:#{name}"
          def via_public_send = public_send(:prot)
          def via_plain_call = prot
          def via_send = send(:priv)
        end
        e = PsE.new
        [
          e.send(:priv),
          e.priv,
          e.prot,
          e.via_public_send,
          e.via_plain_call,
          e.via_send,
          e.respond_to?(:priv),
          e.respond_to?(:priv, true),
        ]
        "#,
    );
}

/// A private `method_missing` is still dispatched, and so is one on a
/// singleton class.
#[test]
fn a_private_or_singleton_handler_is_dispatched() {
    run_test(
        r#"
        class PsD
          private def priv = 1
          private def method_missing(n, *a) = "pmm:#{n}"
        end
        g = Object.new
        class << g
          private def sing = "sing"
          def method_missing(n, *) = "gmm:#{n}"
        end
        [PsD.new.public_send(:priv), g.public_send(:sing)]
        "#,
    );
}
