extern crate monoruby;
use monoruby::tests::*;

#[test]
fn require() {
    run_test(
        r#"
        require File.expand_path("a")
        C::D
        "#,
    );
}

#[test]
fn require_relative() {
    run_test(
        r#"
        require File.expand_path("./b")
        C::D
        "#,
    );
}

#[test]
fn load() {
    run_test(
        r#"
        load "b.rb"
        load "b.rb"
        load "b.rb"
        [$count, C::D]
        "#,
    );
}

#[test]
fn load_priv() {
    run_test_once(
        r#"
        class C
          D = nil
        end
        load "a.rb", true
        load "a.rb", true
        load "a.rb", true
        [$count, C::D]
        "#,
    );
}

#[test]
fn module_autoload() {
    run_test(
        r#"
        class C
          autoload :D, File.expand_path("j")
          autoload :D, File.expand_path("a")
        end
        C::D
        "#,
    );
}
