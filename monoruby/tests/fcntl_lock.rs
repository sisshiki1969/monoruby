extern crate monoruby;
use monoruby::tests::*;

// The native offload path (`src/native_pool.rs`) and its first new
// caller: `IO#fcntl` with a String argument — the packed `struct flock` the
// record-locking commands take. `F_SETLKW` waits in the kernel with no
// fd to poll, so it runs on a native worker while the calling green
// thread parks; every other command is immediate and runs inline, with
// the kernel writing back into the String for `F_GETLK` (issue #1345).

/// The lock commands round-trip through a packed `struct flock`, and
/// `F_GETLK` writes its answer back into the String.
#[test]
fn record_locks_take_a_packed_struct_flock() {
    run_test_once(
        r#"
        require 'fcntl'
        fmt = "s!s!l!l!i!"
        path = "/tmp/monoruby_fcntl_lock_basic"
        File.write(path, "x")
        f = File.open(path, "r+")
        r = []
        r << f.fcntl(Fcntl::F_SETLKW, [Fcntl::F_WRLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt))
        r << f.fcntl(Fcntl::F_SETLK, [Fcntl::F_UNLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt))
        # F_GETLK reports the lock that would block us, into the buffer.
        buf = [Fcntl::F_WRLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt)
        r << f.fcntl(Fcntl::F_GETLK, buf)
        r << buf.unpack(fmt)[0]     # F_UNLCK: nothing in the way
        r << (begin
          f.fcntl(Fcntl::F_GETLK, [Fcntl::F_WRLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt).freeze)
        rescue => e
          e.class
        end)
        f.close
        File.unlink(path)
        r
        "#,
    );
}

/// A green thread waiting for a record lock must not stop the others:
/// the wait happens on a native worker (`native_pool`), not on the
/// interpreter thread.
#[test]
fn a_blocked_lock_wait_lets_other_green_threads_run() {
    run_test_once(
        r#"
        require 'fcntl'
        fmt = "s!s!l!l!i!"
        path = "/tmp/monoruby_fcntl_lock_live"
        File.write(path, "x")
        # A child process holds the lock for a while.
        child = fork do
          f = File.open(path, "r+")
          f.fcntl(Fcntl::F_SETLKW, [Fcntl::F_WRLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt))
          sleep 0.4
          exit!(0)
        end
        sleep 0.15

        waiter = Thread.new do
          File.open(path, "r+") do |f|
            f.fcntl(Fcntl::F_SETLKW, [Fcntl::F_WRLCK, IO::SEEK_SET, 0, 0, 0].pack(fmt))
          end
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
        "#,
    );
}

/// A forked child keeps offloading. Workers do not survive `fork(2)`,
/// so the child must not believe the inherited "an idle worker will
/// take it" bookkeeping — before `native_pool::reset_after_fork` this
/// deadlocked whenever the parent had offloaded anything first.
#[test]
fn offload_still_works_in_a_forked_child() {
    run_test_once(
        r#"
        a = "/tmp/monoruby_offload_fork_parent"
        b = "/tmp/monoruby_offload_fork_child"
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
