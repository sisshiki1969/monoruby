extern crate monoruby;
use monoruby::tests::*;

// The cool.io gem over monoruby's stand-in for cool.io_ext.so
// (gem/cool.io_ext.rb), a select-driven event loop in place of libev.
// Every case is compared against the host CRuby, which runs the gem's real
// C extension — so these pin the stand-in to the extension's bookkeeping
// and error texts, and to what its watchers deliver.
//
// `require "rubygems"` first: the harness spawns the reference CRuby with
// `--disable=gems`, and cool.io is an ordinary gem. Cases with timers are
// written so that the observable outcome does not depend on the loop's
// scheduling granularity.

/// The watcher / loop bookkeeping: `attach` / `detach` / `enable` /
/// `disable` and the loop's `@watchers` / `@active_watchers` they keep,
/// `attached?` answering the extension's `0` when attached, the error
/// texts, the argument checks, and the quirk that attaching an attached
/// watcher again is an ArgumentError.
#[test]
fn coolio_watcher_bookkeeping() {
    run_test_once(
        r##"
        require "rubygems"
        require "cool.io"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        res << t.() { Coolio::Watcher.new }
        res << t.() { Coolio::Loop.new.run }
        res << t.() { Coolio::Loop.new.stop }
        l = Coolio::Loop.new
        res << [l.watchers, l.has_active_watchers?, l.run_once, l.run_once(0.01), l.run_nonblock]
        res << t.() { Coolio::Loop.new.run_once(-1) }
        l = Coolio::Loop.new
        w = Coolio::TimerWatcher.new(10)
        w.attach(l)
        res << [w.attached?, w.enabled?, l.has_active_watchers?, l.watchers.size, w.evloop.equal?(l), l.instance_variable_get(:@active_watchers)]
        w.detach
        res << [w.attached?, w.enabled?, l.watchers.size, l.has_active_watchers?, t.() { w.detach }]
        w.attach(l)
        w.disable
        res << [w.enabled?, l.has_active_watchers?, t.() { w.disable }]
        w.enable
        res << [w.enabled?, t.() { w.enable }, l.instance_variable_get(:@active_watchers)]
        w = Coolio::TimerWatcher.new(1)
        res << [t.() { w.enable }, t.() { w.disable }, t.() { w.reset }, w.attached?, w.enabled?, w.evloop]
        w = Coolio::TimerWatcher.new(2, true)
        res << [w.instance_variable_get(:@interval), w.instance_variable_get(:@repeating)]
        res << Coolio::TimerWatcher.new(nil).instance_variable_get(:@interval)
        res << t.() { Coolio::TimerWatcher.new(1).attach(1) }
        res << t.() { Coolio::TimerWatcher.new(Object.new) }
        l1 = Coolio::Loop.new
        w = Coolio::TimerWatcher.new(1)
        w.attach(l1)
        res << t.() { w.attach(Coolio::Loop.new) }
        res << [t.() { Coolio::IOWatcher.new($stdin, "x") }, t.() { Coolio::IOWatcher.new(1) }, t.() { Coolio::IOWatcher.new(nil) }]
        res << [t.() { Coolio::IOWatcher.new($stdin).detach }, t.() { Coolio::IOWatcher.new($stdin).enable }, t.() { Coolio::IOWatcher.new($stdin).disable }]
        r, w2 = IO.pipe
        iw = Coolio::IOWatcher.new(r, :rw)
        l = Coolio::Loop.new
        iw.attach(l)
        res << [iw.attached?, iw.enabled?, l.watchers.size, t.() { iw.attach(l) }]
        res << t.() { Coolio::StatWatcher.new("/tmp").attach(1) }
        res << [Coolio::StatWatcher.new("/tmp").path, Coolio::StatWatcher.new(:sym).path]
        res << [Coolio::Utils.ncpus.class.name, Coolio::Utils.maxfds.class.name, Coolio.inspect, Cool.io.name, Coolio::Loop.default.class.name]
        c = Class.new(Coolio::TimerWatcher) { def on_timer; :sub; end }
        res << [c.new(1).on_timer, Coolio::TimerWatcher.new(1).on_timer, Coolio::IOWatcher.new($stdin).on_readable, Coolio::IOWatcher.new($stdin).on_writable]
        res << [Coolio::TimerWatcher.superclass.name, Coolio::IOWatcher.superclass.name, Coolio::StatWatcher.superclass.name, Coolio::AsyncWatcher.superclass.name]
        res
        "##,
    );
}

/// Events: a repeating timer counted through `Loop#run` and `stop`,
/// `reset` re-arming a repeating timer and disarming a one-shot one, a
/// `run_once` timeout returning before a long timer, readable / writable
/// / both-ready IO watchers, a watcher detached while another event is
/// pending, `AsyncWatcher#signal`, and `Coolio::IO` reading, seeing the
/// close, and writing through its buffer.
#[test]
fn coolio_timers_and_io() {
    run_test_once(
        r##"
        require "rubygems"
        require "cool.io"
        res = []
        l = Coolio::Loop.new
        r = 0
        w = Coolio::TimerWatcher.new(0.005, true)
        w.on_timer { r += 1; l.stop if r == 3 }
        w.attach(l)
        l.run
        res << [r, w.attached?, w.enabled?]
        l = Coolio::Loop.new
        r = 0
        w = Coolio::TimerWatcher.new(0.005, true)
        w.on_timer { r += 1; w.reset; l.stop if r == 2 }
        w.attach(l)
        l.run
        res << r
        l = Coolio::Loop.new
        fired = false
        w = Coolio::TimerWatcher.new(0.01)
        w.on_timer { fired = true }
        w.attach(l)
        w.reset
        l.run_once(0.03)
        res << [fired, w.enabled?]
        l = Coolio::Loop.new
        w = Coolio::TimerWatcher.new(10)
        w.attach(l)
        res << l.run_once(0.01)
        l = Coolio::Loop.new
        fired = 0
        w = Coolio::TimerWatcher.new(0.005)
        w.on_timer { fired += 1 }
        w.attach(l)
        3.times { l.run_once(0.02) }
        res << [fired, w.enabled?, l.has_active_watchers?]
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        got = []
        iw = Coolio::IOWatcher.new(rd)
        iw.on_readable { got << rd.read_nonblock(10) }
        iw.attach(l)
        wr.write("hi")
        n = l.run_once
        res << [n, got, iw.enabled?]
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        got = []
        iw = Coolio::IOWatcher.new(wr, "w")
        iw.on_writable { got << :w; iw.detach }
        iw.attach(l)
        l.run
        res << [got, l.has_active_watchers?]
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        got = []
        iw = Coolio::IOWatcher.new(wr, :rw)
        iw.on_writable { got << :w; iw.disable }
        iw.on_readable { got << :r }
        iw.attach(l)
        l.run_once
        res << got
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        iw = Coolio::IOWatcher.new(rd)
        iw.attach(l)
        res << [l.run_nonblock, l.run_once(0.01)]
        l = Coolio::Loop.new
        r1, w1 = IO.pipe
        r2, w2 = IO.pipe
        got = []
        a = Coolio::IOWatcher.new(r1)
        a.on_readable { got << 1; r1.read(1) }
        b = Coolio::IOWatcher.new(r2)
        b.on_readable { got << 2; r2.read(1) }
        a.attach(l)
        b.attach(l)
        w1.write("x")
        w2.write("y")
        res << [l.run_once, got.sort]
        l = Coolio::Loop.new
        r1, w1 = IO.pipe
        r2, w2 = IO.pipe
        got = []
        a = Coolio::IOWatcher.new(r1)
        b = Coolio::IOWatcher.new(r2)
        a.on_readable { got << 1; r1.read(1); b.detach }
        b.on_readable { got << 2; r2.read(1); a.detach }
        a.attach(l)
        b.attach(l)
        w1.write("x")
        w2.write("y")
        n = l.run_once
        res << [n, got.size, l.has_active_watchers?]
        l = Coolio::Loop.new
        got = []
        aw = Coolio::AsyncWatcher.new
        aw.on_signal { got << :sig; l.stop }
        aw.attach(l)
        aw.signal
        l.run
        res << got
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        got = []
        c = Class.new(Coolio::IO) { define_method(:on_read) { |d| got << d }; define_method(:on_close) { got << :closed } }
        io = c.new(rd)
        io.attach(l)
        wr.write("data")
        l.run_once
        wr.close
        l.run_once
        res << [got, io.attached?, l.has_active_watchers?]
        l = Coolio::Loop.new
        rd, wr = IO.pipe
        io = Coolio::IO.new(wr)
        io.attach(l)
        io.write("abc")
        l.run_once(0.01)
        res << rd.read_nonblock(10)
        res
        "##,
    );
}

/// `Coolio::Buffer`: append / prepend / read / `read_frame` over a byte
/// queue, `read_from` (reads until the IO would block, nil at end of
/// file) and `write_to`, the constants, and the argument checks.
#[test]
fn coolio_buffer() {
    run_test_once(
        r##"
        require "rubygems"
        require "cool.io"
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        res = []
        b = Coolio::Buffer.new
        b << "abc"
        b.append("d")
        res << [b.size, b.empty?, b.to_str, b.read(2), b.read, b.read, b.size, t.() { b.read(0) }, b.clear, b.empty?]
        b = Coolio::Buffer.new
        b << "é"
        res << [b.read.encoding.name, b.write("x").class, b.read(10)]
        b = Coolio::Buffer.new
        b << "bc"
        b.prepend("a")
        res << b.to_str
        b = Coolio::Buffer.new
        b << "ab\ncd"
        s = +""
        r1 = b.read_frame(s, 10)
        s2 = +""
        r2 = b.read_frame(s2, 10)
        res << [r1, s, r2, s2, b.size]
        b = Coolio::Buffer.new
        rd, wr = IO.pipe
        wr.write("hello")
        res << [b.read_from(rd), b.to_str, b.read_from(rd), (wr.close; b.read_from(rd))]
        b = Coolio::Buffer.new
        rd, wr = IO.pipe
        b << "hello"
        res << [b.write_to(wr), b.size, rd.read_nonblock(10)]
        res << [Coolio::Buffer::MAX_SIZE, Coolio::Buffer.default_node_size.class, Coolio::Buffer.new(4096).size]
        res << [t.() { Coolio::Buffer.new << 1 }, t.() { Coolio::Buffer.new.read_from(1) }, t.() { Coolio::Buffer.new.prepend(nil) }]
        res
        "##,
    );
}

/// `Coolio::StatWatcher`: `on_change` with the previous and current
/// `Struct::StatInfo` once the file changes, and a missing path.
#[test]
fn coolio_stat_watcher() {
    run_test_once(
        r##"
        require "rubygems"
        require "cool.io"
        require "tempfile"
        res = []
        f = Tempfile.new("cw")
        f.write("a")
        f.flush
        l = Coolio::Loop.new
        got = []
        # unlike the other watchers, on_change is not an event_callback in
        # the gem's Ruby half: it has to be overridden
        klass = Class.new(Coolio::StatWatcher) do
          define_method(:on_change) { |prev, cur| got << [prev.size, cur.size, prev.class.name, cur.mtime.class.name]; l.stop }
        end
        sw = klass.new(f.path, 0.02)
        sw.attach(l)
        sleep 0.03
        f.write("bb")
        f.flush
        l.run
        res << [got, sw.path == f.path, sw.attached?]
        res << (begin; Coolio::StatWatcher.new(f.path).on_change { }; rescue ArgumentError => e; e.message; end)
        l = Coolio::Loop.new
        got = nil
        klass = Class.new(Coolio::StatWatcher) { define_method(:on_change) { |p, c| got = [p, c] } }
        sw = klass.new("/nonexistent/x", 0.01)
        sw.attach(l)
        l.run_once(0.03)
        res << [got, Struct::StatInfo.members]
        res
        "##,
    );
}
