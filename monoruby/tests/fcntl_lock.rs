extern crate monoruby;
use monoruby::tests::*;

// The native offload path (`src/native_pool.rs`) and its first new
// caller: `IO#fcntl` with a String argument — the packed `struct flock`
// the record-locking commands take. `F_SETLKW` waits in the kernel with
// no fd to poll, so it runs on a native worker while the calling green
// thread parks; every other command is immediate and runs inline, with
// the kernel writing back into the String for `F_GETLK` (issue #1345).

// Every lock file below is named after the running process. The lock
// commands wait in the kernel for whoever holds the file, so a fixed
// name couples runs that never meant to meet: a test process that dies
// holding its lock (one did here, after a disk-full crash, stuck in a
// futex wait with `F_WRLCK` on `/tmp/monoruby_fcntl_lock_live`) blocks
// every later `cargo test` on the machine at `F_SETLKW`, indefinitely --
// and each blocked run then holds the next one. The CRuby oracle process
// runs the same code, so it too gets a path of its own instead of
// contending with the interpreter under test. `Process.pid` keeps the
// code text constant, so the snapshot oracle's key does not move.

/// `struct flock` is laid out differently per platform, and the buffer
/// has to be the size and shape the kernel expects — prepended to each
/// test's code so both the interpreter under test and the CRuby oracle
/// build the same bytes.
///
/// * Linux: `short l_type; short l_whence; off_t l_start; off_t l_len;
///   pid_t l_pid;` — 32 bytes with the alignment padding.
/// * macOS: `off_t l_start; off_t l_len; pid_t l_pid; short l_type;
///   short l_whence;` — 24 bytes.
const FLOCK_PRELUDE: &str = r#"
        require 'fcntl'
        DARWIN = RUBY_PLATFORM.include?("darwin")
        FMT = DARWIN ? "q!q!i!s!s!" : "s!s!x4q!q!i!x4"
        def flock_struct(type)
          DARWIN ? [0, 0, 0, type, IO::SEEK_SET].pack(FMT)
                 : [type, IO::SEEK_SET, 0, 0, 0].pack(FMT)
        end
        def flock_type(buf) = DARWIN ? buf.unpack(FMT)[3] : buf.unpack(FMT)[0]
"#;

/// The lock commands round-trip through a packed `struct flock`, and
/// `F_GETLK` writes its answer back into the String.
#[test]
fn record_locks_take_a_packed_struct_flock() {
    run_test_once(&format!(
        r#"{FLOCK_PRELUDE}
        path = "/tmp/monoruby_fcntl_lock_basic_#{{Process.pid}}"
        File.write(path, "x")
        f = File.open(path, "r+")
        r = []
        r << f.fcntl(Fcntl::F_SETLKW, flock_struct(Fcntl::F_WRLCK))
        r << f.fcntl(Fcntl::F_SETLK, flock_struct(Fcntl::F_UNLCK))
        # F_GETLK reports the lock that would block us, into the buffer.
        buf = flock_struct(Fcntl::F_WRLCK)
        r << f.fcntl(Fcntl::F_GETLK, buf)
        r << flock_type(buf)        # F_UNLCK: nothing in the way
        r << (begin
          f.fcntl(Fcntl::F_GETLK, flock_struct(Fcntl::F_WRLCK).freeze)
        rescue => e
          e.class
        end)
        f.close
        File.unlink(path)
        r
        "#
    ));
}

/// The error paths: a command the kernel rejects, and a lock the fd
/// cannot take — the latter through the offloaded `F_SETLKW`, so its
/// errno travels back from the worker. Plus Linux's open-file-description
/// locks, which wait the same way and are reachable only by their raw
/// number (neither CRuby's `Fcntl` nor ours defines a constant).
#[test]
fn lock_errors_and_ofd_locks() {
    run_test_once(&format!(
        r#"{FLOCK_PRELUDE}
        path = "/tmp/monoruby_fcntl_lock_errors_#{{Process.pid}}"
        File.write(path, "x")
        r = []
        File.open(path, "r+") do |f|
          r << (begin
            f.fcntl(-1, flock_struct(Fcntl::F_WRLCK))
          rescue => e
            e.class
          end)
        end
        # A write lock wants a writable fd.
        File.open(path, "r") do |f|
          r << (begin
            f.fcntl(Fcntl::F_SETLKW, flock_struct(Fcntl::F_WRLCK))
          rescue => e
            e.class
          end)
        end
        unless DARWIN
          File.open(path, "r+") do |f|
            r << f.fcntl(38, flock_struct(Fcntl::F_WRLCK))   # F_OFD_SETLKW
            r << f.fcntl(37, flock_struct(Fcntl::F_UNLCK))   # F_OFD_SETLK
          end
        end
        File.unlink(path)
        r
        "#
    ));
}

/// A green thread waiting for a record lock must not stop the others:
/// the wait happens on a native worker (`native_pool`), not on the
/// interpreter thread.
#[test]
fn a_blocked_lock_wait_lets_other_green_threads_run() {
    run_test_once(&format!(
        r#"{FLOCK_PRELUDE}
        path = "/tmp/monoruby_fcntl_lock_live_#{{Process.pid}}"
        File.write(path, "x")
        # A child process holds the lock for a while.
        child = fork do
          f = File.open(path, "r+")
          f.fcntl(Fcntl::F_SETLKW, flock_struct(Fcntl::F_WRLCK))
          sleep 0.4
          exit!(0)
        end
        sleep 0.15

        waiter = Thread.new do
          File.open(path, "r+") {{ |f| f.fcntl(Fcntl::F_SETLKW, flock_struct(Fcntl::F_WRLCK)) }}
          :locked
        end
        # ... while this thread keeps being scheduled.
        ticks = 0
        deadline = Time.now + 5
        while waiter.alive? && Time.now < deadline
          ticks += 1
          sleep 0.005
        end
        Process.wait(child)
        result = waiter.value
        File.unlink(path)
        [result, ticks > 5]
        "#
    ));
}

/// A forked child keeps offloading. Workers do not survive `fork(2)`,
/// so the child must not believe the inherited "an idle worker will
/// take it" bookkeeping — before `native_pool::ForkLocks::reset_child`
/// this deadlocked whenever the parent had offloaded anything first.
#[test]
fn offload_still_works_in_a_forked_child() {
    run_test_once(
        r#"
        a = "/tmp/monoruby_offload_fork_parent_#{Process.pid}"
        b = "/tmp/monoruby_offload_fork_child_#{Process.pid}"
        File.write(a, "x")
        File.write(b, "x")
        f = File.open(a, "r+")
        # Offload once in the parent, leaving a worker parked.
        f.flock(File::LOCK_EX)
        f.flock(File::LOCK_UN)
        child = fork do
          g = File.open(b, "r+")
          g.flock(File::LOCK_EX)
          g.flock(File::LOCK_UN)
          exit!(0)
        end
        _, status = Process.wait2(child)
        f.close
        File.unlink(a)
        File.unlink(b)
        status.success?
        "#,
    );
}

/// Forking *while the pool is in use*, repeatedly — and then carrying on
/// in the parent, which is what the existing fork test never does.
///
/// Two ways `fork(2)` used to wedge this (issue #1358), both because the
/// child inherits pool state that belongs to the parent:
///
/// 1. **The completion pipe is shared, not copied.** The child's own
///    `drain` can swallow a byte the *parent's* waiter was about to
///    consume, parking that waiter forever with its result already in the
///    parent's table. That is what this test catches: on master it hangs
///    every run (15/15), and with `replace_pipe` it passes every run
///    (65/65, 25 of them under load).
/// 2. **A worker can hold a pool mutex at the instant of the fork**, and
///    the child does not get that worker — so it inherits a locked mutex
///    nobody will ever release. `native_pool::prepare_fork` takes the
///    locks on the forking thread first, so the surviving thread owns
///    them in both processes. That window is a few instructions wide, so
///    this test does not reliably reach it; the deterministic proof needs
///    a `sleep` inserted into `worker()` while it holds the mutex, as the
///    issue describes.
#[test]
fn forking_while_a_thread_keeps_offloading() {
    run_test_once(
        r#"
        a = "/tmp/monoruby_forkrace_parent_#{Process.pid}"
        b = "/tmp/monoruby_forkrace_child_#{Process.pid}"
        File.write(a, "x")
        File.write(b, "x")
        # A green thread offloading continuously, so a worker is live
        # across the forks below.
        churn = File.open(a, "r+")
        stop = false
        t = Thread.new do
          until stop
            churn.flock(File::LOCK_EX)
            churn.flock(File::LOCK_UN)
          end
          :done
        end
        ok = 0
        20.times do
          child = fork do
            g = File.open(b, "r+")
            g.flock(File::LOCK_EX)
            g.flock(File::LOCK_UN)
            exit!(0)
          end
          _, status = Process.wait2(child)
          ok += 1 if status.success?
        end
        stop = true
        finished = t.value
        churn.close
        File.unlink(a)
        File.unlink(b)
        [ok, finished]
        "#,
    );
}
