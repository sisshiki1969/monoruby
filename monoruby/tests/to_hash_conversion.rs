//! Arguments converted with `to_hash` (#2139): what CRuby passes
//! through `rb_check_hash_type` takes any object that answers
//! `to_hash`, not only a Hash — the environment and the options of
//! `IO.popen`, and the argument the named references of a format
//! (`%{name}`, `%<name>d`) read.

use monoruby::tests::*;

#[test]
fn format_named_references_convert_their_argument() {
    run_test(
        r##"
        def t; yield; rescue Exception => e; [e.class, e.message]; end
        o = Object.new; def o.to_hash = {a: 1, b: "x"}
        n = Object.new; def n.to_hash = nil
        bad = Object.new; def bad.to_hash = 1
        c = 0
        q = Object.new
        q.define_singleton_method(:to_hash) { c += 1; {a: 1} }
        [
          t { "%{a}" % o }, t { "%<a>d-%{b}" % o }, t { format("%{a}", o) },
          t { format("%<a>05d", o) }, t { sprintf("%{b}%{a}", o) }, t { "%{a}" % [o] },
          t { "%{a}" % n }, t { "%{a}" % bad }, t { "%{a}" % 1 }, t { "%{a}" % "s" },
          t { "%{a}" % nil }, t { "%{a}" % [] }, t { format("%{a}") },
          t { format("%{a}", 1, o) }, t { format("%{a}", o, 1) },
          t { format("%<a>d", 1, {a: 1}) }, t { format("%{a}", {a: 1}, 1) },
          t { "%{a}%{c}" % o }, t { "%s" % o == o.to_s },
          # converted once, however many references there are
          t { ["%{a}%{a}%<a>d" % q, c] },
          t { $stdout.printf("%{a}", o) }, t { "%-5{b}|%.1{b}|" % o },
        ]
        "##,
    );
}

#[test]
fn io_popen_converts_environment_and_options() {
    run_test_once(
        r##"
        def t; yield; rescue Exception => e; [e.class, e.message]; end
        o = Object.new; def o.to_hash = {"MONORUBY_POPEN_A" => "1"}
        op = Object.new; def op.to_hash = {err: File::NULL}
        bad = Object.new; def bad.to_hash = 1
        n = Object.new; def n.to_hash = nil
        [
          t { IO.popen(o, "echo $MONORUBY_POPEN_A", &:read) },
          t { IO.popen(o, ["sh", "-c", "echo $MONORUBY_POPEN_A"], &:read) },
          t { IO.popen([o, "sh", "-c", "echo $MONORUBY_POPEN_A"], &:read) },
          t { IO.popen(o, "echo $MONORUBY_POPEN_A", "r", &:read) },
          t { IO.popen(o, "echo $MONORUBY_POPEN_A; echo e >&2", op, &:read) },
          t { IO.popen("echo x; echo e >&2", op, &:read) },
          t { IO.popen("echo x", "r", op, &:read) },
          t { IO.popen(ENV, "echo hi", &:read) },
          t { IO.popen(ENV, "echo hi", "r", {}, &:read) },
          t { IO.popen(bad, "echo hi", &:read) }, t { IO.popen(n, "echo hi", &:read) },
          t { IO.popen("echo hi", bad, &:read) }, t { IO.popen("echo hi", n, &:read) },
          t { IO.popen(1, "echo hi", &:read) }, t { IO.popen(o, &:read) },
          t { IO.popen(o, o, &:read) }, t { system(o, "exit $MONORUBY_POPEN_A") },
          t { pid = spawn(ENV, "true"); Process.wait(pid); $?.success? },
        ]
        "##,
    );
}
