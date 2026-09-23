extern crate monoruby;
use monoruby::tests::*;

// `String#scrub` / `#scrub!` given both a replacement and a block is
// an `ArgumentError` in CRuby, before the receiver or the argument is
// looked at; `nil` counts as no replacement (#1604).

#[test]
fn a_block_and_a_replacement_together_are_refused() {
    run_test_once(
        r##"
        t = ->(&b) { begin; v = b.call; v.is_a?(String) ? [v.bytes, v.encoding.to_s] : v; rescue => e; [e.class, e.message]; end }
        broken = -> { "a\x80b".dup.force_encoding("UTF-8") }
        res = []
        [["abc"], [broken]].each do |(mk)|
          s = -> { mk.is_a?(String) ? mk.dup : mk.call }
          [123, "R", nil, "\xff".b].each do |arg|
            res << t.() { s.().scrub(arg) { "x" } }
            res << t.() { s.().scrub!(arg) { "x" } }
            res << t.() { s.().scrub(arg) }
          end
          res << t.() { s.().scrub { "x" } }
          res << t.() { s.().scrub! { "x" } }
          res << t.() { s.().freeze.scrub!("R") { "x" } }
          res << t.() { s.().freeze.scrub!(nil) { "x" } }
          res << t.() { s.().freeze.scrub! { "x" } }
          res << t.() { s.().scrub("R", "S") }
        end
        res
        "##,
    );
}
