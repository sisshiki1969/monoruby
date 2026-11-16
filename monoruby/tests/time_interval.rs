extern crate monoruby;
use monoruby::tests::*;

// The number of seconds a blocking call is given (#1935). CRuby reads it
// with `rb_time_interval` in `Kernel#sleep`, `Mutex#sleep`,
// `ConditionVariable#wait`, `IO.select` and `IO#wait_readable`; monoruby
// had a coercion of its own in each, with its own errors (and a Rust
// panic for `sleep(Float::INFINITY)`).

const HELPER: &str = r#"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message]; end }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

#[test]
fn sleep_refuses_what_is_not_an_interval() {
    run(r#"
        [
          t.() { sleep("x") },
          t.() { sleep(:a) },
          t.() { sleep(Object.new) },
          t.() { sleep(true) },
          t.() { sleep([1]) },
          t.() { sleep(-1) },
          t.() { sleep(-0.5) },
          t.() { sleep(-1r) },
          t.() { sleep(-Float::INFINITY) },
          t.() { sleep(Float::NAN) },
          t.() { sleep(Float::INFINITY) },
          t.() { sleep(1e30) },
          t.() { sleep(2**70) },
          t.() { sleep(-(2**70)) },
          t.() { sleep(-(2**62)) },
          t.() { sleep(0.001, 2) },
        ]
    "#);
}

#[test]
fn sleep_takes_what_divmod_makes_an_interval() {
    run(r#"
        frac = Object.new
        def frac.divmod(x) = [0, 0.001]
        not_a_pair = Object.new
        def not_a_pair.divmod(x) = 5
        negative = Object.new
        def negative.divmod(x) = [-1, 0]
        bad_whole = Object.new
        def bad_whole.divmod(x) = ["a", 0]
        bad_frac = Object.new
        def bad_frac.divmod(x) = [0, "a"]
        short = Object.new
        def short.divmod(x) = [0]
        [
          sleep(0),
          sleep(0.001),
          sleep(1/1000r),
          sleep(frac),
          t.() { sleep(not_a_pair) },
          t.() { sleep(negative) },
          t.() { sleep(bad_whole) },
          t.() { sleep(bad_frac) },
          t.() { sleep(short) },
        ]
    "#);
}

/// `Mutex#sleep` converts its timeout before it looks at the lock, so a
/// bad one is reported on an unlocked mutex too; `ConditionVariable#wait`
/// passes its timeout on.
#[test]
fn mutex_sleep_and_condvar_wait_validate_the_interval() {
    run(r#"
        m = Thread::Mutex.new
        cv = Thread::ConditionVariable.new
        frac = Object.new
        def frac.divmod(x) = [0, 0.001]
        [
          t.() { m.sleep("x") },
          t.() { m.sleep(-1) },
          t.() { m.sleep(0.001) },
          t.() { m.synchronize { m.sleep(Object.new) } },
          t.() { m.synchronize { m.sleep(:a) } },
          t.() { m.synchronize { m.sleep(Float::NAN) } },
          t.() { m.synchronize { m.sleep(-1) } },
          t.() { m.synchronize { m.sleep(0.001) } },
          t.() { m.synchronize { m.sleep(1/1000r) } },
          t.() { m.synchronize { m.sleep(frac) } },
          t.() { m.synchronize { cv.wait(m, "x") } },
          t.() { m.synchronize { cv.wait(m, -1) } },
          t.() { m.synchronize { cv.wait(m, 0.001) } },
          t.() { cv.wait(m, 0.001) },
          m.locked?,
        ]
    "#);
}

/// `ConditionVariable#wait` sends `sleep` to whatever it was given
/// without looking at visibility (`rb_funcallv`), so an object whose only
/// `sleep` is the private `Kernel#sleep` will do for a mutex.
#[test]
fn condvar_wait_calls_sleep_whatever_its_visibility() {
    run(r#"
        cv = Thread::ConditionVariable.new
        log = []
        own = Object.new
        own.define_singleton_method(:sleep) { |timeout| log << timeout; :slept }
        own.singleton_class.send(:private, :sleep)
        [cv.wait(Object.new, 0.001), cv.wait(own, 3), cv.wait(own), log]
    "#);
}

#[test]
fn io_waits_validate_the_interval() {
    run(r#"
        r, w = IO.pipe
        res = [
          t.() { IO.select(nil, nil, nil, "x") },
          t.() { IO.select(nil, nil, nil, Object.new) },
          t.() { IO.select(nil, nil, nil, Float::NAN) },
          t.() { IO.select(nil, nil, nil, -1) },
          t.() { IO.select([r], nil, nil, 0.001) },
          t.() { IO.select([r], nil, nil, 1/1000r) },
          t.() { r.wait_readable("x") },
          t.() { r.wait_readable(-1) },
          t.() { r.wait_readable(0.001) },
        ]
        r.close; w.close
        res
    "#);
}

/// `Thread#join` is not an interval (`thread_join_m`): a negative limit
/// has simply run out, an infinite one is no limit, and what is not a
/// number is `rb_num2dbl`'s error.
#[test]
fn thread_join_limit() {
    run(r#"
        q = Thread::Queue.new
        th = Thread.new { q.pop }
        res = [
          th.join(-1),
          th.join(-0.5),
          th.join(0),
          th.join(0.001),
          th.join(1/1000r),
          t.() { th.join("x") },
          t.() { th.join(Object.new) },
          t.() { th.join(:a) },
          t.() { th.join(true) },
        ]
        q << 1
        res << (th.join(Float::INFINITY) == th) << (th.join(2**70) == th) << (th.join(1e30) == th)
        res << (th.join(-1) == th) << (th.join(nil) == th)
        res
    "#);
}
