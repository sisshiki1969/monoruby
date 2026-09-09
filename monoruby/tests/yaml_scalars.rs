extern crate monoruby;
use monoruby::tests::*;

// stdlib/psych.rb: multi-line flow scalars fold their line breaks into
// single spaces (double-quoted, single-quoted and plain, whether the value
// starts on the key's line or the next one), and `''` is the escaped quote
// inside a single-quoted scalar.

#[test]
fn yaml_multi_line_flow_scalars_fold() {
    run_test_once(
        r##"
        require "yaml"
        docs = [
          "a:\n  - k:\n      \"line one\n      line two\"\n    n: 1\n  - k: 2\n",
          "a:\n  - k: \"line one\n      line two\"\n    n: 1\n  - k: 2\n",
          "a:\n  - k:\n      plain one\n      plain two\n    n: 1\n  - k: 2\n",
          "a:\n  - k: 'it''s\n      two'\n    n: 1\n",
          "a: \"x\\ny\"\nb: \"tab\\there\"\nc: 'don''t'\n",
          "k:\n  \"<p>You can (555) 567-2222.</p>\n  <p>Our store is at <em>Rue d'Avignon 32</em>.</p>\"\ncreated_at: 2005-04-04 12:00\n",
          "pages:\n  - &p1\n    id: 1\n    content:\n      \"first\n      second\"\n    at: x\n\n  - &p2\n    id: 2\n    content: plain\n",
        ]
        docs.map { |d| YAML.unsafe_load(d) }
        "##,
    );
}

// The YAML merge key: `<<: *base` folds the aliased mapping in
// (`Hash#merge!`: it overrides keys already present, later keys
// override it), `<<: [*a, *b]` folds a sequence with the earlier
// mappings winning, and a `<<` with a non-mapping value is an ordinary
// key. Rails' `database.yml` is the `<<: *default` case.
#[test]
fn yaml_merge_key() {
    run_test_once(
        r##"
        require "yaml"
        docs = [
          "default: &d\n  a: 1\n  b: 2\nx:\n  <<: *d\n  b: 3\n  c: 4\ny:\n  b: 9\n  <<: *d\n",
          "a: &a\n  k: 1\nb: &b\n  k: 2\n  j: 2\nz:\n  <<: [*a, *b]\n  m: 0\n",
          "p: &p {k: 1, l: 2}\nq: {<<: *p, l: 3}\n",
          "s:\n  <<: plain\n  t: 1\n",
          "default: &default\n  adapter: sqlite3\n  pool: 5\n  timeout: 5000\n\nproduction:\n  <<: *default\n  database: db/production.sqlite3\n",
        ]
        docs.map { |d| YAML.unsafe_load(d) }
        "##,
    );
}
