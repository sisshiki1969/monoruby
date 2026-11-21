extern crate monoruby;
use monoruby::tests::*;

// #2103: `Class.new(S)` inherits `S`'s instance layout, as `class X < S`
// does, so a Marshal round trip of its instance keeps the class.
#[test]
fn unnamed_builtin_subclass_layout() {
    run_test_once(
        r#"
        K = Class.new(String) { attr_accessor :a }
        s = K.new("abc")
        s.a = 1
        t = Marshal.load(Marshal.dump(s))
        A = Class.new(Array)
        u = Marshal.load(Marshal.dump(A[1, 2]))
        [t, t.class == K, t.a, u, u.class == A]
        "#,
    );
}

// #2102: `instance_exec` with a `Method#to_proc` whose method takes a
// block: the given self is not handed over as the block.
#[test]
fn instance_exec_method_proc_with_block_param() {
    run_test(
        r#"
        class IeC
          def m(*a, &b) = [a, b, self.class]
        end
        pr = IeC.new.method(:m).to_proc
        ["str".instance_exec(1, &pr), 5.instance_exec(&pr), [].instance_exec { |*x| x }]
        "#,
    );
}
