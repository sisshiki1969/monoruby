extern crate monoruby;
use monoruby::tests::*;

// `String#tr` with a negated set and a multibyte replacement writes
// the whole character. CRuby writes its low byte alone when the
// receiver's coderange happens to be cached as 7-bit — an answer that
// depends on whether `#length` was called first — and that is
// deliberately not copied (#1492, `doc/runtime_optimization/string.md`
// §7). Not oracle-checked, since CRuby is the one that differs.

#[test]
fn tr_writes_the_whole_replacement_character() {
    let v = run_test_no_result_check(
        r##"
        r = []
        r << "abc".tr("^x", "う")
        r << "abc".tr_s("^x", "う")
        r << "abc".tr("a", "う")
        r << "abc".tr("^a-b", "う")
        r << "abc".dup.tr!("^x", "う")
        mk = -> { [65, 66].pack("C*").force_encoding("EUC-JP") }
        t = [0xA1, 0xA8].pack("C*").force_encoding("EUC-JP")
        r << mk.().tr("^H", t).bytes
        s = mk.()
        s.length
        r << s.tr("^H", t).bytes
        expected = ["ううう", "う", "うbc", "abう", "ううう", [161, 168, 161, 168], [161, 168, 161, 168]]
        raise "got #{r.inspect}" unless r == expected
        r.size
        "##,
    );
    assert_eq!(v.try_fixnum(), Some(7));
}
