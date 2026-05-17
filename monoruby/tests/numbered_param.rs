extern crate monoruby;
use monoruby::tests::*;

#[test]
fn numbered_param_basic() {
    run_test("[1,2,3].map { _1 * 2 }");
    run_test("[1,2,3].select { _1 > 1 }");
    run_test("[1,2,3].each_with_object([]) { _2 << _1 }");
}

#[test]
fn numbered_param_multiple() {
    run_test("[[1,2],[3,4],[5,6]].map { _1 + _2 }");
    run_test("[[1,2,3]].map { _1 + _2 + _3 }");
}

#[test]
fn numbered_param_do_end() {
    run_test(
        r#"
        res = []
        [10, 20, 30].each do
            res << _1
        end
        res
        "#,
    );
}

#[test]
fn numbered_param_nested_block() {
    run_test(
        r#"
        [1,2,3].map { _1 * 10 }.map { _1 + 1 }
        "#,
    );
}

#[test]
fn numbered_param_with_method_chain() {
    run_test("[1,2,3,4,5].select { _1.odd? }.map { _1 ** 2 }");
}

#[test]
fn it_param_basic() {
    if parser_is_ruruby() {
        return;
    }
    run_test("[10, 20].map { it + 1 }");
    run_test("[1, 2, 3].select { it.odd? }");
    run_test("[[1, 2], [3, 4]].map { it }");
}

#[test]
fn it_param_do_end() {
    if parser_is_ruruby() {
        return;
    }
    run_test(
        r#"
        res = []
        [10, 20, 30].each do
            res << it
        end
        res
        "#,
    );
}

#[test]
fn it_param_nested_block() {
    if parser_is_ruruby() {
        return;
    }
    run_test("[1, 2].map { [4, 5].map { it * 10 } }");
}

#[test]
fn it_param_lambda() {
    if parser_is_ruruby() {
        return;
    }
    run_test("f = ->{ it * 2 }; f.call(7)");
}

#[test]
fn it_param_shadowed_by_local() {
    run_test("it = 5; it + 1");
}
