//! `send` / `__send__` from JIT-compiled code must answer what the
//! interpreter answers.
//!
//! The JIT inlines `send` (`Codegen::object_send_inline`) and resolves the
//! name itself, through a per-call-site symbol cache. Two things went
//! wrong there: a name it could not resolve raised instead of reaching
//! `method_missing`, and the cache — keyed by name alone — handed one
//! class's method to another class's receiver.

extern crate monoruby;
use monoruby::tests::*;

/// Every argument shape the inlined `send` accepts, from a loop hot enough
/// to be compiled: no arguments, several, a block, and the `send(*ary)`
/// splat form that takes its own branch through the resolution.
#[test]
fn send_to_a_missing_method_reaches_method_missing() {
    run_test(
        r##"
        class MM
          def method_missing(name, *args, &blk) = [:mm, name, args, blk ? blk.call : nil]
          def respond_to_missing?(*) = true
        end
        m = MM.new
        res = []
        i = 0
        while i < 60
          res << m.send(:whatever)
          res << m.__send__(:whatever, 1, 2)
          res << m.send(:whatever) { :from_block }
          res << m.send(*[:whatever, 3])
          res << m.send("whatever_str")
          i += 1
        end
        res.uniq
        "##,
    );
}

/// One `send` site, several receiver classes: each call must reach the
/// method of *its* receiver's class. The per-call-site symbol cache holds
/// `name -> FuncId`, which is one class's answer, so it is retired when
/// the receiver class changes; without that, `b.send(:m)` ran `A#m` with
/// `self` a `B`.
#[test]
fn a_polymorphic_send_site_keeps_receivers_apart() {
    run_test(
        r##"
        class A
          def one(x) = [:a_one, x]
          def two(x) = [:a_two, x]
        end
        class B
          def one(x) = [:b_one, x]
          def two(x) = [:b_two, x]
        end
        objs = [A.new, B.new]
        names = [:one, :two]
        res = []
        i = 0
        while i < 60
          objs.each { |o| names.each { |n| res << o.send(n, 0) } }
          i += 1
        end
        res.uniq
        "##,
    );
}

/// The same, where one class answers through `method_missing` and the
/// other has the method — the shape that first exposed both bugs.
#[test]
fn missing_sends_keep_their_receiver_and_name() {
    run_test(
        r##"
        class A
          def method_missing(name, *args) = [:a, name, args]
        end
        class B
          def method_missing(name, *args) = [:b, name, args]
          def real(x) = [:b_real, x]
        end
        objs = [A.new, B.new]
        names = [:one, :two, :real]
        res = []
        i = 0
        while i < 60
          objs.each do |o|
            names.each do |n|
              res << o.send(n, i % 2)
            end
          end
          i += 1
        end
        res.uniq
        "##,
    );
}

/// With no `method_missing` anywhere, the JIT-compiled `send` still raises
/// NoMethodError, and says what the interpreter says.
#[test]
fn send_without_method_missing_still_raises() {
    run_test(
        r##"
        o = Object.new
        raised = nil
        i = 0
        while i < 60
          begin
            o.send(:no_such_method_here)
          rescue NoMethodError => e
            raised = e.message
          end
          i += 1
        end
        raised
        "##,
    );
    // `send` reaches a private method, which is the whole point of it —
    // that must not be diverted into `method_missing`.
    run_test(
        r##"
        class P
          def method_missing(name, *args) = :missed
          private def hidden = :private_answer
        end
        p1 = P.new
        res = []
        i = 0
        while i < 60
          res << p1.send(:hidden)
          i += 1
        end
        res.uniq
        "##,
    );
}
