extern crate monoruby;
use monoruby::tests::*;

#[test]
fn numbered_param() {
    run_tests(&[
        "[1,2,3].map { _1 * 2 }",
        "[1,2,3].select { _1 > 1 }",
        "[1,2,3].each_with_object([]) { _2 << _1 }",
        "[[1,2],[3,4],[5,6]].map { _1 + _2 }",
        "[[1,2,3]].map { _1 + _2 + _3 }",
        r#"
        res = []
        [10, 20, 30].each do
            res << _1
        end
        res
        "#,
        r#"
        [1,2,3].map { _1 * 10 }.map { _1 + 1 }
        "#,
        "[1,2,3,4,5].select { _1.odd? }.map { _1 ** 2 }",
    ]);
}

#[test]
fn it_param_basic() {
    run_tests(&[
        "[10, 20].map { it + 1 }",
        "[1, 2, 3].select { it.odd? }",
        "[[1, 2], [3, 4]].map { it }",
    ]);
}

#[test]
fn it_param_do_end() {
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
    run_test("[1, 2].map { [4, 5].map { it * 10 } }");
}

#[test]
fn it_param_lambda() {
    run_test("f = ->{ it * 2 }; f.call(7)");
}

#[test]
fn it_param_shadowed_by_local() {
    run_test("it = 5; it + 1");
}

#[test]
fn numbered_param_lambda() {
    // Numbered parameters in a `->`/`lambda` body, like in a block.
    run_test("f = ->{ _1 }; f.call(5)");
    run_test("f = ->{ _1 + _2 }; f.call(3, 4)");
    run_test("f = lambda { _1 * 10 }; f.call(2)");
    run_test("[->{ _1 }.arity, ->{ _1 + _2 }.arity]");
    run_test("f = ->{ [_1, _2, _3] }; f.call(1, 2, 3)");
    // JIT warm-up loop.
    run_test("f = ->{ _1 + _2 }; s = 0; i = 0; while i < 200; s += f.call(i, 1); i += 1; end; s");
}
