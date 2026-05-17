extern crate monoruby;
use monoruby::tests::*;

#[test]
fn cbi_does_not_copy_string_keys() {
    run_test(
        r#"
        idh = {}.compare_by_identity
        foo = 'foo'
        idh[foo] = true
        idh[foo] = true
        [idh.size, idh.keys.first.equal?(foo), idh.keys.first.frozen?]
        "#,
    );
}

#[test]
fn cbi_distinct_objects_stay_distinct() {
    run_test(
        r#"
        idh = {}.compare_by_identity
        idh['x'.dup] = 1
        idh['x'.dup] = 2
        idh['x'.dup] = 3
        idh.size
        "#,
    );
}

#[test]
fn cbi_normal_hash_still_dups_and_freezes_string_keys() {
    run_test(
        r#"
        h = {}
        s = 'bar'
        h[s] = 1
        [h.keys.first.equal?(s), h.keys.first.frozen?, h.size]
        "#,
    );
}

#[test]
fn cbi_predicate_and_self_return() {
    run_test(
        r#"
        h = {}
        before = h.compare_by_identity?
        ret = h.compare_by_identity
        [before, ret.equal?(h), h.compare_by_identity?]
        "#,
    );
}
