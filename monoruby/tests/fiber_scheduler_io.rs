//! Socket waits through a fiber scheduler, checked against a fixed
//! expectation: the host CRuby may predate the scheduler's `io_wait` on
//! `accept` (3.4), so the usual comparison with it does not apply.

use std::process::Command;

fn monoruby(code: &str) -> String {
    let out = Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .arg("-e")
        .arg(code)
        .output()
        .unwrap();
    assert!(
        out.status.success(),
        "{}",
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).into_owned()
}

/// `TCPServer#accept` with no client, in a non-blocking fiber with a
/// scheduler: the wait goes through `io_wait(server, IO::READABLE, nil)`;
/// a false answer is IO::TimeoutError, a ready answer retries the accept.
#[test]
fn tcpserver_accept_waits_through_the_fiber_scheduler() {
    let out = monoruby(
        r#"
        require "socket"
        class Sched
          attr_reader :events
          attr_accessor :answer
          def initialize = (@events = []; @answer = true)
          def block(*) = Fiber.yield
          def unblock(*) = nil
          def kernel_sleep(*) = Fiber.yield
          def close = nil
          def io_wait(io, events, timeout)
            @events << [io.class, events, timeout]
            Fiber.yield
            @answer
          end
        end
        server = TCPServer.new("127.0.0.1", 0)
        sched = Sched.new
        Fiber.set_scheduler(sched)
        f = Fiber.new(blocking: false) { server.accept }
        f.resume
        p sched.events, f.alive?
        sched.answer = false
        p((begin; f.resume; rescue IO::TimeoutError => e; e.class; end))
        sched.answer = IO::READABLE
        g = Fiber.new(blocking: false) { server.accept }
        g.resume
        client = TCPSocket.new("127.0.0.1", server.addr[1])
        p g.resume.class
        client.close
        Fiber.set_scheduler(nil)
        server.close
        "#,
    );
    assert_eq!(
        out,
        "[[TCPServer, 1, nil]]\ntrue\nIO::TimeoutError\nTCPSocket\n"
    );
}
