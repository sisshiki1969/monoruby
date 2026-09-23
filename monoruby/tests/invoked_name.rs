extern crate monoruby;
use monoruby::tests::*;

// The name a method was *invoked by* — CRuby's `rb_frame_this_func()`,
// behind `__callee__` and the method name an Enumerator records — under
// indirect dispatch (#1505). monoruby recovered it from the caller's call
// site only, so `a.send(:select)` made `…:filter`, and `C.new.send(:m2)`
// on an alias reported the definition name. It now follows `send` /
// `__send__` / `public_send`, `Method#call` / `UnboundMethod#bind_call`
// and `super`, in the interpreter and in JIT-compiled callers alike (the
// loops below run long enough to compile).

const PRELUDE: &str = r#"
  class C1505
    def m1 = __callee__
    alias m2 m1
    def blk = [1].map { __callee__ }.first
    alias blk2 blk
  end
  class D1505 < C1505
    def m2 = super
  end
"#;

fn run(body: &str) {
    run_test_once(&format!("{PRELUDE}\n{body}"));
}

/// An Enumerator made by `send` / `Method#call` names the method it was
/// asked for, not the definition it shares with an alias.
#[test]
fn an_enumerator_records_the_name_it_was_sent() {
    run(r#"
        a = [1, 2]
        res = []
        res << a.select.inspect
        res << a.send(:select).inspect
        res << a.__send__(:select).inspect
        res << a.public_send(:select).inspect
        res << a.method(:select).call.inspect
        res << a.method(:select).to_proc.call.inspect
        res << Array.instance_method(:select).bind_call(a).inspect
        res << Array.instance_method(:select).bind(a).call.inspect
        res << a.send(:filter).inspect
        res << a.send(:collect).inspect
        res << a.send("collect").inspect
        res << a.send(:each_slice, 1).inspect
        res << {a: 1}.send(:select).inspect
        o = Object.new
        def o.method_missing(n, *args) = [1, 2].send(n)
        res << o.select.inspect
        res << a.tap { |v| break v.select.inspect }
        n = :select
        r = nil; 300.times { r = [a.send(n).inspect, a.send(:collect).inspect, a.method(:select).call.inspect] }
        res << r
        res
    "#);
}

/// `__callee__` through the same paths, `super` included, from compiled
/// and interpreted callers.
#[test]
fn callee_under_indirect_dispatch() {
    run(r#"
        c = C1505.new
        res = []
        res << c.m2
        res << c.send(:m2)
        res << c.__send__(:m2)
        res << c.public_send(:m2)
        res << c.send("m2")
        res << c.method(:m2).call
        res << c.method(:m1).call
        res << c.method(:m2).to_proc.call
        res << C1505.instance_method(:m2).bind_call(c)
        res << c.send(:m1)
        res << c.send(:blk2)
        res << c.send(:send, :m2)
        res << D1505.new.m2
        res << D1505.new.send(:m2)
        res << [:m1, :m2].map { |n| c.send(n) }
        o = Object.new
        def o.method_missing(n, *a) = C1505.new.send(n)
        res << o.m2
        r = nil; 300.times { r = [:m1, :m2].map { |n| c.send(n) } }
        res << r
        m = c.method(:m2)
        r = nil; 300.times { r = [m.call, m[], c.public_send(:m2), c.send(:m2)] }
        res << r
        def call_it(o, n) = o.send(n)
        r = nil; 300.times { r = call_it(c, :m2) }
        res << r
        res
    "#);
}
