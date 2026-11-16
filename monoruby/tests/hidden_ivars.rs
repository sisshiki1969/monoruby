extern crate monoruby;
use monoruby::tests::*;

// The thread-synchronization classes are written in Ruby
// (`builtins/thread.rb`, `builtins/monitor.rb`) and used to keep their
// state in ordinary instance variables, where CRuby keeps it in a C
// struct (#1937): `inspect` and `instance_variables` listed it, a
// subclass with an `@owner` of its own broke the lock, and a frozen
// mutex could not be locked. It is now in *hidden* instance variables —
// `@__name` in a file with the `# monoruby-hidden-ivars: true` magic
// comment (doc/hidden_ivars.md).

const HELPER: &str = r#"
  require "monitor"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message.gsub(/0x\h+/, "0x")]; end }
  f = ->(x) { x.inspect.gsub(/0x\h+/, "0x") }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// The issue's program.
#[test]
fn sync_classes_show_no_state() {
    run(r#"
        m = Thread::Mutex.new; m.lock
        q = Thread::Queue.new; q << 1
        sq = Thread::SizedQueue.new(2)
        cv = Thread::ConditionVariable.new
        mon = Monitor.new; mon.enter
        tg = ThreadGroup.new
        [
          f.(m), f.(q), f.(sq), f.(cv), f.(mon), f.(tg),
          m.instance_variables, q.instance_variables, sq.instance_variables,
          cv.instance_variables, mon.instance_variables, tg.instance_variables,
          m.instance_variable_get(:@owner), m.instance_variable_get(:@__owner),
          m.instance_variable_defined?(:@__owner),
          t.() { m.remove_instance_variable(:@__owner) },
          caller_locations(0)[0].instance_variables,
        ]
    "#);
}

#[test]
fn a_thread_shows_no_state() {
    run(r#"
        th = Thread.new { sleep }
        th.name = "nm"
        th[:a] = 1
        th.thread_variable_set(:b, 2)
        th.priority = 1
        th.report_on_exception = false
        th.abort_on_exception = false
        ThreadGroup.new.add(th)
        Thread.pass until th.status == "sleep"
        res = [
          th.instance_variables, th.name, th[:a], th.thread_variable_get(:b), th.keys,
          th.thread_variables, th.report_on_exception, th.abort_on_exception,
          th.group == ThreadGroup::Default, th.group.list == [th],
          f.(th).sub(/@nm .*:\d+ /, "@nm "), Thread.instance_variables,
        ]
        th.instance_variable_set(:@x, 1)
        res << th.instance_variables << th.instance_variable_get(:@x)
        th.kill; th.join
        res
    "#);
}

/// What is not instance-variable state is not what `freeze` protects.
#[test]
fn frozen_sync_objects_still_work() {
    run(r#"
        [
          t.() { Thread::Mutex.new.freeze.lock.locked? },
          t.() { Thread::Mutex.new.freeze.synchronize { 1 } },
          # Enough rounds for the JIT to compile the lock and unlock paths.
          t.() { m = Thread::Mutex.new.freeze; 100.times { m.synchronize { 1 } }; m.locked? },
          t.() { m = Thread::Mutex.new.freeze; 100.times { m.lock; m.unlock }; [m.try_lock, m.owned?, m.unlock.locked?] },
          t.() { m = Thread::Mutex.new.freeze; m.synchronize { m.sleep(0.001) } },
          t.() { Thread::ConditionVariable.new.freeze.signal.frozen? },
          t.() { cv = Thread::ConditionVariable.new.freeze; m = Thread::Mutex.new.freeze; m.synchronize { cv.wait(m, 0.001) }; cv.broadcast.frozen? },
          t.() { Monitor.new.freeze.synchronize { 1 } },
          t.() { m = Monitor.new.freeze; 100.times { m.synchronize { m.synchronize { 1 } } }; [m.mon_locked?, m.mon_owned?] },
          t.() { m = Monitor.new.freeze; c = m.new_cond; m.synchronize { c.wait(0.001) } },
          t.() { ThreadGroup.new.enclose.freeze.enclosed? },
          # Ordinary state is still protected.
          t.() { Thread::Mutex.new.freeze.instance_variable_set(:@a, 1) },
          t.() { Thread::Queue.new.freeze },
          t.() { m = Thread::Mutex.new.freeze; [m.frozen?, m.dup.frozen?, m.clone.frozen?, m.clone.lock.locked?] },
        ]
    "#);
}

/// A subclass's own instance variables do not collide with the state of
/// the class it inherits, whatever it calls them.
#[test]
fn subclass_instance_variables_do_not_collide() {
    run(r#"
        class MyM < Thread::Mutex
          def initialize; super; @owner = "me"; @waiters = 3; @__owner = :x; end
          attr_reader :owner, :waiters
        end
        class MyQ < Thread::Queue
          def initialize; super; @items = :mine; @mutex = nil; @closed = 7; end
          attr_reader :items
        end
        class MySQ < Thread::SizedQueue
          def initialize; super(1); @max = :m; @cv_push = 1; end
        end
        class MyCV < Thread::ConditionVariable
          def initialize; super; @waiters = :w; end
        end
        class MyMon < Monitor
          def initialize; super; @owner = 1; @count = :c; @mutex = :m; end
        end
        m = MyM.new
        q = MyQ.new; q << 1; q << 2
        sq = MySQ.new; sq << 1
        c = MyCV.new; c.signal; c.broadcast
        mon = MyMon.new
        [
          m.synchronize { [m.owner, m.waiters, m.locked?, m.owned?, m.instance_variables, f.(m)] },
          [q.pop, q.size, q.items, q.closed?, q.instance_variables, f.(q)],
          [sq.max, sq.pop, sq.instance_variables],
          [c.instance_variables, f.(c)],
          mon.synchronize { mon.synchronize { [mon.mon_locked?, mon.instance_variables, f.(mon)] } },
        ]
    "#);
}

#[test]
fn copies_and_dumps() {
    run(r#"
        m = Thread::Mutex.new; m.lock
        d = m.dup; c = m.clone
        o = Object.new; o.instance_variable_set(:@m, m)
        [
          d.locked?, c.locked?, d.instance_variables, m.locked?,
          t.() { Marshal.dump(Thread::Mutex.new) },
          t.() { Marshal.dump(Thread::Queue.new) },
          t.() { Marshal.dump(Thread::ConditionVariable.new) },
          t.() { Marshal.dump(Monitor.new) },
          f.(o), f.([m]), f.({m => m}),
        ]
    "#);
}

/// Unlocking takes the mutex off its thread's list of held mutexes in
/// any order, and a thread that ends releases the ones it still holds.
#[test]
fn held_list() {
    run(r#"
        m = Thread::Mutex.new; m2 = Thread::Mutex.new
        m.lock; m2.lock; m2.unlock; m.unlock
        m2.lock; m.lock; m2.unlock; m.unlock
        res = [m.locked?, m2.locked?]
        th = Thread.new { m.lock; m2.lock; m.unlock; sleep }
        Thread.pass until th.status == "sleep"
        res << m.locked? << m2.locked?
        th.kill; th.join
        res << m.locked? << m2.locked? << m2.try_lock
        a = []
        4.times.map { |i| Thread.new { 200.times { m.synchronize { a << i } } } }.each(&:join)
        res << a.size
    "#);
}

/// The mechanism itself, which CRuby does not have: in a file with the
/// magic comment `@__name` is a slot no reflection sees, a store into it
/// is allowed on a frozen object, and `attr_*` of a `__name` is over it;
/// every other instance variable of the file is an ordinary one.
#[test]
fn hidden_ivars_magic_comment() {
    let v = run_test_no_result_check(
        r##"# monoruby-hidden-ivars: true
        class HiddenBox
          attr_accessor :__slot, :plain
          def initialize; @__state = 1; @seen = 2; @__slot = 3; @plain = 4; end
          def state = @__state
          def bump; @__state += 1; self; end
          def touch; @seen += 1; self; end
          def state? = [defined?(@__state), defined?(@__nothing), "#{@__state}/#@__slot"]
          def swap; @__state, @__slot = @__slot, @__state; @__state ||= 0; self; end
        end
        b = HiddenBox.new
        r = []
        r << b.instance_variables << b.inspect.sub(/0x\h+/, "0x") << b.state << b.__slot
        r << b.instance_variable_get(:@__state) << b.instance_variable_defined?(:@__slot)
        r << b.state? << b.swap.state << b.__slot
        # A name that is none: only the file's own spelling reaches it.
        r << (begin; b.instance_variable_get(:"/__state"); rescue NameError => e; e.class; end)
        b.instance_variable_set(:@__state, :mine)
        r << b.instance_variables << b.state
        # Marshal and dup see the same split.
        c = Marshal.load(Marshal.dump(b))
        r << c.instance_variables << c.state << b.dup.state
        # Frozen: the hidden slots stay writable, in the interpreter and
        # once the JIT has compiled the methods.
        b.freeze
        60.times { b.bump; b.__slot = b.__slot + 1 }
        r << b.state << b.__slot << b.frozen?
        r << (begin; b.touch; rescue FrozenError => e; e.class; end)
        r << (begin; b.plain = 1; rescue FrozenError => e; e.class; end)
        expected = [
          [:@seen, :@plain], "#<HiddenBox:0x @seen=2, @plain=4>", 1, 3,
          nil, false,
          ["instance-variable", nil, "1/3"], 3, 1,
          NameError,
          [:@seen, :@plain, :@__state], 3,
          [:@seen, :@plain, :@__state], nil, 3,
          63, 61, true,
          FrozenError, FrozenError,
        ]
        raise "got #{r.inspect}" unless r == expected
        r.size
        "##,
    );
    assert_eq!(v.try_fixnum(), Some(20));
}

/// Without the magic comment `@__name` is an instance variable like any
/// other — as it is in a file that merely happens to use the prefix.
#[test]
fn double_underscore_ivars_are_ordinary_elsewhere() {
    run(r#"
        class PlainBox
          attr_accessor :__x
          def initialize; @__x = 1; end
        end
        a = PlainBox.new
        [a.instance_variables, a.__x, f.(a), t.() { a.freeze.__x = 2 }]
    "#);
}

/// `Process.detach` answers a `Process::Waiter` — a Thread subclass
/// with no `new` and no allocator, and no state to show.
#[test]
fn process_detach_waiter() {
    run(r#"
        pid = fork { exit! 3 }
        w = Process.detach(pid)
        [
          w.class.to_s, w.class.superclass.to_s, w.pid == pid,
          w.value.exitstatus, w.join.equal?(w), w.instance_variables,
          w[:pid] == pid,
          Process::Waiter.instance_methods(false).include?(:pid),
          Process::Waiter.respond_to?(:new),
          t.() { Process::Waiter.new },
          t.() { Process::Waiter.allocate },
          t.() { Thread.allocate },
          defined?(Thread::Waiter),
        ]
    "#);
}

/// The scheduler reads a thread's name and its two exception flags by
/// slot name (`scheduler.rs`), so they moved together with the Ruby side.
#[test]
fn the_scheduler_reads_a_threads_hidden_state() {
    run(r#"
        require "stringio"
        err = $stderr = StringIO.new
        begin
          Thread.new { Thread.current.name = "worker"; raise "boom" }.join rescue nil
          Thread.new { Thread.current.report_on_exception = false; raise "quiet" }.join rescue nil
          Thread.report_on_exception = false
          Thread.new { raise "quiet2" }.join rescue nil
          Thread.new { Thread.current.report_on_exception = true; raise "loud" }.join rescue nil
        ensure
          Thread.report_on_exception = true
          $stderr = STDERR
        end
        reports = err.string.lines.grep(/^#<Thread/).map { |l| l.gsub(/0x\h+/, "0x").gsub(/ \S+:\d+ /, " LOC ") }
        forwarded = begin
          Thread.new { Thread.current.report_on_exception = false; Thread.current.abort_on_exception = true; raise "abort" }
          sleep
        rescue => e
          e.message
        end
        [reports, forwarded, Thread.current.instance_variables, Thread.instance_variables]
    "#);
}

/// A store into a hidden slot checks nothing, so it must not leave the
/// JIT believing the receiver is unfrozen: the ordinary store after it
/// in the same compiled method still raises on a frozen object.
#[test]
fn a_hidden_store_proves_nothing_about_frozen() {
    let v = run_test_no_result_check(
        r##"# monoruby-hidden-ivars: true
        class Mixed
          attr_accessor :__w, :w
          def initialize; @__h = 0; @o = 0; @__w = 0; @w = 0; end
          def hidden_then_plain; @__h += 1; @o += 1; end
          def plain_then_hidden; @o += 1; @__h += 1; end
          def writers(x); x.__w = x.__w + 1; x.w = x.w + 1; end
          def state = [@__h, @o, @__w, @w]
        end
        m = Mixed.new
        # Compile all three on an unfrozen receiver first.
        60.times { m.hidden_then_plain; m.plain_then_hidden; m.writers(m) }
        r = [m.state]
        m.freeze
        3.times do
          r << (begin; m.hidden_then_plain; rescue FrozenError => e; e.class; end)
          r << (begin; m.plain_then_hidden; rescue FrozenError => e; e.class; end)
          r << (begin; m.writers(m); rescue FrozenError => e; e.class; end)
        end
        r << m.state
        expected = [[120, 120, 60, 60]] + [FrozenError] * 9 + [[123, 120, 63, 60]]
        raise "got #{r.inspect}" unless r == expected
        r.size
        "##,
    );
    assert_eq!(v.try_fixnum(), Some(11));
}
