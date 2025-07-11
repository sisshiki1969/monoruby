use monoruby::tests::*;

#[test]
fn inject() {
    run_test(r##"[2, 3, 4, 5].inject {|result, item| result + item }"##);
    run_test(r##"(1..5).inject {|result, item| result + item }"##);
    run_test(r##"[2, 3, 4, 5].inject(0) {|result, item| result + item ** 2 }"##);
    run_test(r##"(1..5).inject(0) {|result, item| result + item ** 2 }"##);
}

#[test]
fn filter() {
    run_test(r##"[1, 2, 3, 4, 5, 6, 7].filter { |x| x.odd? }"##);
    run_test(r##"(1..7).filter { |x| x.odd? }"##);
    run_test(r##"(1..7).filter(&:odd?)"##);
}
