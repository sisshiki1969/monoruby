extern crate monoruby;
use monoruby::tests::*;

// `Monitor`, `MonitorMixin` and `MonitorMixin::ConditionVariable`
// (#1927). `builtins/monitor.rb` used to be a stand-in from before
// monoruby had threads: `enter` recorded the caller as owner without
// waiting, so a second thread walked into a held monitor, and the
// condition variable's `wait` / `signal` / `broadcast` did nothing.

const HELPER: &str = r#"
  require "monitor"
  t = ->(&b) { begin; b.call; rescue Exception => e; [e.class.to_s, e.message.sub(/ for (an instance of|nil).*/, "")]; end }
"#;

fn run(body: &str) {
    run_test_once(&format!("{HELPER}\n{body}"));
}

/// The issue's program: two threads in and out of one monitor, each
/// yielding the processor while inside. Nobody is ever inside with
/// another, and nobody's `exit` raises.
#[test]
fn synchronize_excludes_other_threads() {
    run(r#"
        m = Monitor.new
        inside = 0
        overlap = 0
        total = 0
        work = -> {
          200.times do
            m.synchronize do
              inside += 1
              overlap += 1 if inside > 1
              Thread.pass
              m.synchronize { total += 1 }
              inside -= 1
            end
          end
        }
        ts = 3.times.map { Thread.new(&work) }
        work.call
        ts.each(&:join)
        [overlap, total, inside, m.mon_locked?]
    "#);
}

/// `MonitorMixin` sits on the same class, and so does everything that
/// includes it.
#[test]
fn monitor_mixin_excludes_other_threads() {
    run(r#"
        class MonCounter
          include MonitorMixin
          attr_reader :n, :overlap
          def initialize
            super
            @n = @inside = @overlap = 0
          end
          def bump
            synchronize do
              @inside += 1
              @overlap += 1 if @inside > 1
              Thread.pass
              @n += 1
              @inside -= 1
            end
          end
        end
        c = MonCounter.new
        4.times.map { Thread.new { 100.times { c.bump } } }.each(&:join)
        [c.n, c.overlap, c.mon_locked?]
    "#);
}

/// Entering, leaving and asking who holds it: re-entrant for the owner,
/// closed to every other thread, and `exit` by anyone else raises.
#[test]
fn enter_exit_and_ownership() {
    run(r#"
        res = []
        m = Monitor.new
        res << t.() { [m.mon_locked?, m.mon_owned?, m.try_enter, m.enter, m.mon_locked?, m.mon_owned?] }
        res << t.() { Thread.new { [m.mon_locked?, m.mon_owned?, m.try_enter, (m.exit rescue [$!.class.to_s, $!.message]), (m.mon_check_owner rescue $!.message)] }.value }
        res << t.() { [m.exit, m.mon_locked?, m.mon_owned?, m.exit, m.mon_locked?, m.mon_owned?] }
        res << t.() { m.exit }
        res << t.() { m.mon_exit }
        res << t.() { m.mon_check_owner }
        res << t.() { m.synchronize { m.synchronize { m.synchronize { [m.mon_locked?, m.mon_owned?] } } } }
        res << t.() { [m.mon_locked?, m.synchronize { 42 }, m.mon_synchronize { 43 }] }
        res << t.() { (m.synchronize { raise "boom" } rescue $!.message) + m.mon_locked?.to_s }
        res << t.() { [catch(:x) { m.synchronize { throw :x, 1 } }, m.mon_locked?] }
        res << t.() { [[1, 2].each { |x| m.synchronize { break x * 10 } }, m.mon_locked?] }
        # The block left the monitor itself: the `exit` on the way out raises.
        res << t.() { m.synchronize { m.exit; 1 } }
        res << t.() { [m.mon_locked?, m.mon_owned?] }
        res << t.() { m.synchronize }
        res << t.() { m.mon_locked? }
        res << t.() {
          m.try_enter; m.try_mon_enter; m.mon_try_enter; m.mon_enter
          r = Thread.new { m.try_enter }.value
          4.times { m.mon_exit }
          [r, m.mon_locked?, Thread.new { r = m.try_enter; m.exit; r }.value]
        }
        res
    "#);
}

/// The owner is a Fiber, not a Thread (CRuby 3.0 on): another fiber of
/// the owning thread does not own the monitor.
#[test]
fn the_owner_is_a_fiber() {
    run(r#"
        res = []
        m = Monitor.new
        res << t.() { f = Fiber.new { [m.mon_owned?, m.mon_locked?, (m.exit rescue $!.message), (m.mon_check_owner rescue $!.message)] }; m.synchronize { f.resume } }
        res << t.() {
          f = Fiber.new { m.enter; Fiber.yield m.mon_owned?; m.exit; m.mon_locked? }
          [f.resume, m.mon_owned?, m.mon_locked?, (m.exit rescue $!.message), f.resume, m.mon_locked?]
        }
        res
    "#);
}

/// What the class answers to, and the corners of allocation: CRuby makes
/// the mutex in the allocator, so a subclass that never calls `super`
/// and a bare `allocate` both work; a copy is a monitor of its own.
#[test]
fn monitor_class_shape() {
    run(r#"
        res = []
        m = Monitor.new
        names = %i[try_enter enter exit synchronize mon_locked? mon_check_owner mon_owned? wait_for_cond new_cond
                   try_mon_enter mon_try_enter mon_enter mon_exit mon_synchronize owned? locked? count owner]
        res << t.() { names.map { Monitor.method_defined?(_1) } }
        res << t.() { [Monitor.const_defined?(:ConditionVariable), m.new_cond.class.to_s, Monitor.instance_method(:enter).arity, Monitor.instance_method(:wait_for_cond).arity, Monitor.ancestors.take(2).map(&:to_s)] }
        res << t.() { m.enter; d = m.dup; r = [d.mon_locked?, d.mon_owned?, d.try_enter, d.mon_locked?, m.mon_locked?]; d.exit; m.exit; r + [d.mon_locked?, m.mon_locked?, d.class.to_s] }
        res << t.() { m.enter; d = m.clone; r = [d.mon_locked?, Thread.new { d.try_enter }.value]; m.exit; r }
        res << t.() { Marshal.dump(Monitor.new) }
        res << t.() { m.enter; r = [m.inspect.sub(/0x\h+/, "0x"), m.frozen?]; m.exit; r }
        res << t.() { c = Class.new(Monitor) { def initialize(x) = @x = x }; o = c.new(1); [o.synchronize { o.mon_owned? }, o.mon_locked?, o.try_enter, o.exit] }
        res << t.() { o = Monitor.allocate; [o.mon_locked?, o.mon_owned?, (o.exit rescue $!.message), o.enter, o.mon_owned?, o.exit] }
        res << t.() { [MonitorMixin::ConditionVariable.instance_methods(false).sort, MonitorMixin::ConditionVariable.private_instance_methods(false).sort] }
        res
    "#);
}

/// `MonitorMixin::ConditionVariable` on one thread: `wait` with a
/// timeout answers false, keeps the entry count across the wait, and
/// every operation needs the monitor.
#[test]
fn condition_variable_needs_the_monitor() {
    run(r#"
        res = []
        m = Monitor.new
        res << t.() { Monitor.new.wait_for_cond(Thread::ConditionVariable.new, 0.01) }
        res << t.() { m.synchronize { [m.wait_for_cond(Thread::ConditionVariable.new, 0.01), m.mon_owned?] } }
        res << t.() { m.synchronize { m.synchronize { [m.new_cond.wait(0.01), m.mon_owned?, m.exit, m.mon_owned?, m.enter] } } }
        res << t.() { m.mon_locked? }
        res << t.() { m.new_cond.wait(0.01) }
        res << t.() { m.new_cond.signal }
        res << t.() { m.new_cond.broadcast }
        res << t.() { m.synchronize { c = m.new_cond; [c.signal.class.to_s, c.broadcast.class.to_s] } }
        res << t.() { m.synchronize { m.new_cond.wait(-1) } }
        res << t.() { [m.mon_locked?, m.mon_owned?] }
        res << t.() {
          n = 0
          c = m.new_cond
          m.synchronize do
            [c.wait_while { (n += 1) < 3 && Thread.new { m.synchronize { c.signal } } },
             c.wait_until { (n += 1) > 5 || !Thread.new { m.synchronize { c.broadcast } } },
             n]
          end
        }
        res << t.() { MonitorMixin::ConditionVariable.new(m).wait(0.01) }
        res
    "#);
}

/// Producers and consumers over a bounded buffer: `wait_while` /
/// `wait_until` give the monitor up while they wait, `signal` and
/// `broadcast` wake the waiters.
#[test]
fn condition_variable_producer_consumer() {
    run(r#"
        m = Monitor.new
        not_empty = m.new_cond
        not_full = m.new_cond
        buf = []
        out = []
        consumers = 3.times.map do
          Thread.new do
            loop do
              v = m.synchronize do
                not_empty.wait_while { buf.empty? }
                x = buf.shift
                not_full.signal
                x
              end
              break if v == :done
              m.synchronize { out << v }
            end
          end
        end
        producers = 2.times.map do |k|
          Thread.new do
            100.times do |i|
              m.synchronize do
                not_full.wait_until { buf.size < 4 }
                buf << k * 1000 + i
                not_empty.signal
              end
            end
          end
        end
        producers.each(&:join)
        m.synchronize { 3.times { buf << :done }; not_empty.broadcast }
        consumers.each(&:join)
        [out.size, out.sort == (0...100).to_a + (1000...1100).to_a, buf, m.mon_locked?]
    "#);
}

/// A waiter gives the monitor up with its whole entry count and has it
/// back, with the same count, when `wait` returns — true when signaled.
#[test]
fn wait_releases_and_retakes_the_whole_count() {
    run(r#"
        m = Monitor.new
        c = m.new_cond
        log = []
        th = Thread.new do
          m.synchronize { m.synchronize { m.enter; log << :waiting; r = c.wait; log << [:woken, r, m.mon_owned?]; m.exit; log << m.mon_owned? } }
          log << m.mon_owned?
        end
        Thread.pass until log.first == :waiting && th.status == "sleep"
        seen = m.synchronize { log << :signal; c.signal; log.dup }
        th.join
        timed = m.synchronize { t0 = Process.clock_gettime(Process::CLOCK_MONOTONIC); r = c.wait(0.05); [r, Process.clock_gettime(Process::CLOCK_MONOTONIC) - t0 >= 0.04, m.mon_owned?] }
        [seen, log, m.mon_locked?, timed]
    "#);
}

/// A thread waiting to enter can be interrupted and does not end up
/// owning the monitor; one that ends, or is killed, while holding it
/// gives it up to the next in line.
#[test]
fn blocked_enter_and_abandoned_monitor() {
    run(r#"
        res = []
        m = Monitor.new
        m.enter
        th = Thread.new { begin; m.enter; :entered; rescue => e; [e.class.to_s, e.message, m.mon_owned?, m.mon_locked?]; end }
        Thread.pass until th.status == "sleep"
        th.raise(RuntimeError, "stop")
        res << th.value
        th = Thread.new { m.synchronize { :got } }
        Thread.pass until th.status == "sleep"
        m.exit
        res << th.value << m.mon_locked?

        th = Thread.new { m.enter; m.enter; :held }
        res << th.value << m.mon_locked? << m.mon_owned? << m.try_enter << m.mon_owned?
        m.exit
        th = Thread.new { m.enter; sleep }
        Thread.pass until th.status == "sleep"
        w = Thread.new { m.synchronize { :after_kill } }
        Thread.pass until w.status == "sleep"
        th.kill
        res << w.value << th.join.status << m.mon_locked?
        res
    "#);
}

/// A waiter that is raised into, or killed, holds the monitor again by
/// the time its `rescue` / `ensure` runs.
#[test]
fn interrupted_waiter_retakes_the_monitor() {
    run(r#"
        res = []
        m = Monitor.new
        c = m.new_cond
        log = []
        th = Thread.new do
          m.synchronize do
            begin
              log << :w
              c.wait
            rescue => e
              log << [e.message, m.mon_owned?]
            ensure
              log << [:ensure, m.mon_owned?, m.mon_locked?]
            end
          end
          log << [:out, m.mon_owned?]
        end
        Thread.pass until log.first == :w && th.status == "sleep"
        m.synchronize { th.raise("r") }
        th.join
        res << log << m.mon_locked?
        log = []
        th = Thread.new do
          m.synchronize do
            begin
              log << :w
              c.wait
            ensure
              log << [:ensure, m.mon_owned?, m.mon_locked?]
            end
          end
        end
        Thread.pass until log.first == :w && th.status == "sleep"
        m.synchronize { th.kill }
        th.join
        res << log << m.mon_locked? << m.try_enter
        m.exit
        res
    "#);
}

/// `Thread#raise` in a stream against a thread going in and out: the
/// release on the way out of `synchronize` cannot be cut in half, so the
/// monitor is never left held and the thread never finds its own mutex
/// locked under a monitor it does not own ("deadlock; recursive
/// locking").
#[test]
fn synchronize_release_is_uninterruptible() {
    run(r#"
        m = Monitor.new
        n = 0
        hits = 0
        other = []
        ready = false
        stop = false
        th = Thread.new do
          Thread.handle_interrupt(RuntimeError => :never) do
            until stop
              begin
                Thread.handle_interrupt(RuntimeError => :immediate) do
                  ready = true
                  m.synchronize { m.synchronize { n += 1 } }
                end
              rescue RuntimeError => e
                e.message == "x" ? hits += 1 : other << e.message
              rescue ThreadError => e
                other << e.message
              end
            end
          end
          m.mon_owned?
        end
        Thread.pass until ready
        300.times { th.raise(RuntimeError, "x"); Thread.pass }
        Thread.pass while th.pending_interrupt?
        stop = true
        [th.value, other, hits > 0, n > 0, m.mon_locked?, m.try_enter, m.mon_owned?]
    "#);
}

/// `MonitorMixin`: included, extended, initialized late or twice.
#[test]
fn monitor_mixin_initialization() {
    run(r#"
        res = []
        class MmA; include MonitorMixin; def initialize(x, k: 1); super(); @x = [x, k]; end; attr_reader :x; end
        class MmB; include MonitorMixin; def initialize; end; end
        class MmC; include MonitorMixin; end
        class MmD < Array; include MonitorMixin; end
        class MmE; include MonitorMixin; def initialize; mon_initialize; end; end
        class MmF; include MonitorMixin; def initialize; super; mon_initialize; end; end
        MmS = Struct.new(:a) { include MonitorMixin }
        res << t.() { a = MmA.new(1, k: 2); [a.x, a.mon_locked?, a.mon_owned?, a.synchronize { [a.mon_locked?, a.mon_owned?, a.mon_synchronize { 1 }] }, a.mon_try_enter, a.try_mon_enter, a.mon_enter, a.mon_exit, a.mon_exit, a.mon_exit, a.mon_locked?] }
        res << t.() { MmA.new(1).mon_exit }
        # No `super` in `initialize`: there is no monitor.
        res << t.() { MmB.new.synchronize { 1 } }
        res << t.() { MmB.new.mon_locked? }
        # ... until `new_cond` makes one.
        res << t.() { b = MmB.new; c = b.new_cond; [c.class.to_s, b.synchronize { c.wait(0.01) }, b.mon_locked?, b.instance_variables.sort] }
        res << t.() { b = MmB.new; b.new_cond; b.__send__(:mon_initialize); b.synchronize { 2 } }
        res << t.() { MmC.new.synchronize { 3 } }
        res << t.() { d = MmD.new(3, 0); [d, d.synchronize { d.size }] }
        res << t.() { MmE.new.synchronize { 4 } }
        res << t.() { MmF.new }
        res << t.() { s = MmS.new(5); [s.a, s.synchronize { s.mon_owned? }] }
        res << t.() { o = Object.new; o.extend(MonitorMixin); [o.synchronize { o.mon_owned? }, o.mon_locked?] }
        res << t.() {
          o = +"str"
          o.extend(MonitorMixin)
          c = o.new_cond
          th = Thread.new { o.synchronize { c.wait_until { o.size > 3 }; o.dup } }
          Thread.pass
          o.synchronize { o << "!"; c.signal }
          th.value
        }
        res << t.() { o = Object.new; o.extend(MonitorMixin); o.extend(MonitorMixin) }
        # A copy shares the monitor until it is initialized again.
        res << t.() { a = MmA.new(1); b = a.dup; [b.mon_locked?, a.synchronize { b.mon_locked? }, (b.__send__(:mon_initialize); a.synchronize { b.mon_locked? })] }
        res << t.() { a = MmA.new(1); a.__send__(:mon_initialize) }
        res << t.() { a = MmA.new(1); a.mon_enter; Thread.new { [a.mon_try_enter, (a.mon_exit rescue [$!.class.to_s, $!.message]), a.mon_locked?, a.mon_owned?] }.value }
        res << t.() { [MonitorMixin.instance_methods.sort, MonitorMixin.private_instance_methods(false).sort, MonitorMixin.singleton_methods.sort, MonitorMixin.constants.sort] }
        res << t.() { [MmA.instance_method(:synchronize).owner.to_s, MonitorMixin.instance_method(:mon_synchronize) == MonitorMixin.instance_method(:synchronize), MonitorMixin.instance_method(:initialize).arity, MonitorMixin.instance_method(:initialize).parameters] }
        res
    "#);
}
