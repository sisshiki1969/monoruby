extern crate monoruby;
use monoruby::tests::*;

// Ractor is emulated on green threads (builtins/ractor.rb): the API
// surface a program uses to get work done behaves as in CRuby, without
// parallelism or object isolation.

#[test]
fn ractor_new_value_and_make_shareable() {
    run_test_once(
        r#"
        seq = Ractor.make_shareable("GGTATT" * 100)
        rs = [1, 2, 3].map { |n| Ractor.new(seq, n) { |s, len| s.scan(/G{#{len}}/).size } }
        vals = rs.map(&:value)
        h = Ractor.make_shareable({ a: [1, "x"], b: { c: +"y" } })
        deep = h.frozen? && h[:a].frozen? && h[:a][1].frozen? && h[:b][:c].frozen?
        orig = { k: +"v" }
        copy = Ractor.make_shareable(orig, copy: true)
        [vals, seq.frozen?, deep, copy.frozen?, copy[:k].frozen?, orig.frozen?,
         Ractor.shareable?(h), Ractor.shareable?(+"m"), Ractor.shareable?(:s),
         Ractor.main?, Ractor.current.equal?(Ractor.main)]
        "#,
    );
}

#[test]
fn ractor_send_receive_and_remote_error() {
    run_test_once(
        r#"
        r = Ractor.new { Ractor.receive * 2 }
        r.send(21)
        r2 = Ractor.new { raise "boom" }
        err = begin
          r2.value
        rescue Ractor::RemoteError => e
          [e.class, e.cause.class, e.cause.message, e.ractor.equal?(r2)]
        end
        r3 = Ractor.new(name: "worker") { :done }
        [r.value, err, r3.name, r3.join.equal?(r3), r3.value]
        "#,
    );
}
