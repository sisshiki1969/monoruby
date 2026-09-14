//! `io/console` against CRuby.
//!
//! The terminal-mode methods need a real tty, so each test opens a
//! pseudo terminal with `openpty(3)` and hands the slave's path to the
//! Ruby code; both the in-process monoruby run and the CRuby process
//! that verifies it open that same slave. A background thread plays the
//! terminal on the master side: it swallows whatever the slave writes
//! and answers a few in-band requests, so the blocking reads (`getch`,
//! `getpass`, `cursor`) are fed the same bytes in both runs:
//!
//! | slave writes | terminal feeds back |
//! |--------------|---------------------|
//! | `0x01`       | `xy`                |
//! | `0x02`       | `secret\n`          |
//! | `ESC [ 6 n`  | `ESC [ 5 ; 7 R`     |
//!
//! Everything is `_live`: the slave path is per-run, so no snapshot
//! could be replayed. Tests set the mode they rely on before reading it,
//! since the two runs share one terminal and the second sees whatever
//! the first left behind.

use monoruby::tests::*;
use std::ffi::CStr;
use std::io::{Read, Write};
use std::os::fd::FromRawFd;

struct Pty {
    path: String,
    _master: std::fs::File,
    _slave: std::fs::File,
}

fn open_pty() -> Pty {
    let mut master = -1;
    let mut slave = -1;
    let mut name = [0u8; 256];
    // SAFETY: openpty writes two descriptors and a NUL-terminated name
    // into the buffers we pass; the termios / winsize pointers may be null.
    let rc = unsafe {
        libc::openpty(
            &mut master,
            &mut slave,
            name.as_mut_ptr() as *mut libc::c_char,
            std::ptr::null_mut(),
            std::ptr::null_mut(),
        )
    };
    assert_eq!(rc, 0, "openpty failed: {}", std::io::Error::last_os_error());
    let path = CStr::from_bytes_until_nul(&name).unwrap().to_str().unwrap().to_string();
    // SAFETY: both descriptors were just returned by openpty and are owned here.
    let master = unsafe { std::fs::File::from_raw_fd(master) };
    let slave = unsafe { std::fs::File::from_raw_fd(slave) };

    let mut reader = master.try_clone().unwrap();
    let mut writer = master.try_clone().unwrap();
    std::thread::spawn(move || {
        let mut buf = [0u8; 256];
        let mut tail: Vec<u8> = Vec::new();
        loop {
            let n = match reader.read(&mut buf) {
                Ok(0) | Err(_) => break,
                Ok(n) => n,
            };
            for &b in &buf[..n] {
                let feed: &[u8] = match b {
                    0x01 => b"xy",
                    0x02 => b"secret\n",
                    _ => b"",
                };
                if !feed.is_empty() && writer.write_all(feed).is_err() {
                    return;
                }
                tail.push(b);
                if tail.len() > 4 {
                    tail.remove(0);
                }
                if tail.ends_with(b"\x1b[6n") {
                    if writer.write_all(b"\x1b[5;7R").is_err() {
                        return;
                    }
                    tail.clear();
                }
            }
        }
    });

    Pty {
        path,
        _master: master,
        _slave: slave,
    }
}

/// Everything that works without a terminal: the method table, the
/// Errno / ArgumentError / TypeError forms on a pipe, and the constants.
#[test]
fn io_console_without_a_tty() {
    run_test_once_live(
        r##"
        require "io/console"
        r, w = IO.pipe
        res = []
        res << (IO.instance_methods & [:raw, :raw!, :cooked, :cooked!, :getch, :echo=, :echo?, :noecho,
          :winsize, :winsize=, :iflush, :oflush, :ioflush, :beep, :goto, :cursor, :cursor=, :cursor_up,
          :cursor_down, :cursor_left, :cursor_right, :goto_column, :erase_line, :erase_screen,
          :scroll_forward, :scroll_backward, :clear_screen, :pressed?, :check_winsize_changed,
          :getpass, :ttyname, :console_mode, :console_mode=]).size
        res << IO.respond_to?(:console) << IO::ConsoleMode.instance_methods(false).sort
        res << (begin; w.winsize; rescue SystemCallError => e; e.class; end)
        res << (begin; r.raw {}; rescue SystemCallError => e; [e.class, e.message]; end)
        res << (begin; r.noecho {}; rescue SystemCallError => e; e.class; end)
        res << (begin; r.cooked {}; rescue SystemCallError => e; e.class; end)
        res << (begin; r.getch; rescue SystemCallError => e; e.class; end)
        res << (begin; r.raw!; rescue SystemCallError => e; e.class; end)
        res << (begin; r.cooked!; rescue SystemCallError => e; e.class; end)
        res << (begin; r.echo?; rescue SystemCallError => e; e.class; end)
        res << (begin; r.echo = true; rescue SystemCallError => e; e.class; end)
        res << (begin; r.console_mode; rescue SystemCallError => e; e.class; end)
        res << (begin; w.winsize = [1, 2]; rescue SystemCallError => e; e.class; end)
        res << (begin; w.winsize = [1, 2, 3]; rescue ArgumentError => e; e.message; end)
        res << (begin; r.iflush; rescue SystemCallError => e; e.class; end)
        res << (begin; r.getpass("x"); rescue IOError, SystemCallError => e; [e.class, e.message]; end)
        res << (begin; w.getpass("x"); rescue IOError, SystemCallError => e; e.class; end)
        res << (begin; w.cursor; rescue SystemCallError => e; e.class; end)
        res << r.ttyname
        res << (begin; r.pressed?(1); rescue NotImplementedError => e; e.message; end)
        res << (begin; r.check_winsize_changed; rescue NotImplementedError => e; e.message; end)
        res << (begin; IO::ConsoleMode.new; rescue NoMethodError; :no_new; end)
        res << (begin; IO.console(3); rescue TypeError => e; e.message; end)
        res << (begin; w.erase_line(3); rescue ArgumentError => e; e.message; end)
        res << (begin; w.erase_screen(4); rescue ArgumentError => e; e.message; end)
        res << (begin; w.erase_line(:a); rescue ArgumentError => e; e.message; end)
        res << (begin; w.cursor = [1]; rescue ArgumentError => e; e.message; end)
        res << (begin; w.cursor = 1; rescue TypeError => e; e.message; end)
        res << (begin; r.raw(intr: 1) {}; rescue ArgumentError => e; e.message; end)
        res << (begin; r.raw(foo: 1) {}; rescue ArgumentError => e; e.message; end)
        null = File.open("/dev/null", "r+")
        res << (begin; null.winsize; rescue SystemCallError => e; e.message; end)
        res << (begin; null.raw!; rescue SystemCallError => e; e.message; end)
        res << (begin; null.raw {}; rescue SystemCallError => e; e.message; end)
        null.close
        c = IO.console
        res << (c.nil? || (c.is_a?(File) && c.sync && c.path == "/dev/tty" && c.equal?(IO.console)))
        res << IO.console(:close) << IO.console(:nil?).inspect.size
        require "io/console/size"
        res << IO.default_console_size.size
        ENV["LINES"] = "7"; ENV["COLUMNS"] = "9"
        res << IO.default_console_size
        ENV["LINES"] = "-1"; ENV["COLUMNS"] = "x"
        res << IO.default_console_size
        res << IO.console_size.size
        res
        "##,
    );
}

/// The escape sequences are plain writes, so a pipe captures them.
#[test]
fn io_console_escape_sequences() {
    run_test_once_live(
        r##"
        require "io/console"
        r, w = IO.pipe
        res = []
        res << w.beep.equal?(w) << w.goto(1, 2).equal?(w) << w.goto_column(4).equal?(w)
        res << w.cursor_up(2).equal?(w) << w.cursor_down(3).equal?(w)
        res << w.cursor_left(1).equal?(w) << w.cursor_right(5).equal?(w) << w.cursor_up(0).equal?(w)
        res << w.erase_line(0).equal?(w) << w.erase_line(nil).equal?(w) << w.erase_line(2).equal?(w)
        res << w.erase_screen(3).equal?(w) << w.erase_screen(nil).equal?(w)
        res << w.scroll_forward(2).equal?(w) << w.scroll_backward(1).equal?(w)
        res << w.scroll_forward(0).equal?(w) << w.clear_screen.equal?(w)
        res << (w.cursor = [4, 5])
        w.goto(1.9, 2.1)
        w.close
        res << r.read
        res
        "##,
    );
}

/// Terminal modes on a pty: raw / cooked / echo, the block forms restore,
/// the bang forms stick, and ConsoleMode round-trips.
#[test]
fn io_console_modes_on_a_pty() {
    let pty = open_pty();
    let stty = if cfg!(target_os = "linux") {
        // stty(1) reports every termios flag, so the exact cfmakeraw /
        // cooked flag sets are compared, not just echo.
        format!(r#"`stty -a -F {}`.sub(/speed.*?;/, "")"#, pty.path)
    } else {
        "nil".to_string()
    };
    run_test_once_live(&format!(
        r##"
        require "io/console"
        f = File.open("{path}", "r+")
        f.sync = true
        f.winsize = [24, 80]
        res = []
        f.cooked!
        res << f.echo? << f.raw {{ f.echo? }} << f.echo? << f.noecho {{ f.echo? }} << f.cooked {{ f.echo? }}
        res << f.raw {{ |io| io.equal?(f) }} << f.raw(min: 2, time: 0.5, intr: true) {{ f.echo? }}
        res << f.raw!.equal?(f) << f.echo?
        res << {stty}
        res << f.cooked!.equal?(f) << f.echo?
        res << {stty}
        res << (f.echo = false) << f.echo? << (f.echo = true) << f.echo?
        res << f.raw!(min: 3, time: 1).equal?(f)
        res << {stty}
        res << f.raw!(intr: true).equal?(f)
        res << {stty}
        f.cooked!
        res << f.noecho {{ {stty} }}
        res << f.iflush.equal?(f) << f.oflush.equal?(f) << f.ioflush.equal?(f)
        res << f.ttyname
        cm = f.console_mode
        res << cm.class << cm.raw.class << cm.raw.equal?(cm) << cm.raw!.equal?(cm) << (cm.echo = false)
        f.console_mode = cm
        res << f.echo? << {stty}
        cm2 = cm.dup
        cm2.echo = true
        res << (f.console_mode = cm2).equal?(cm2) << f.echo?
        res << (begin; f.console_mode = 1; rescue TypeError => e; e.message; end)
        r, w = IO.pipe
        res << (begin; r.console_mode = cm; rescue SystemCallError => e; e.class; end)
        r.close; w.close
        res << (begin; f.raw {{ raise "boom" }}; rescue RuntimeError => e; e.message; end) << f.echo?
        f.cooked!
        f.close
        res
        "##,
        path = pty.path,
        stty = stty,
    ));
}

/// Window size on a pty, including the four-element and nil forms.
#[test]
fn io_console_winsize_on_a_pty() {
    let pty = open_pty();
    run_test_once_live(&format!(
        r##"
        require "io/console"
        f = File.open("{path}", "r+")
        res = []
        f.winsize = [10, 20]
        res << f.winsize
        f.winsize = [5, 6, 7, 8]
        res << f.winsize
        res << (f.winsize = [3, 4])
        res << (begin; f.winsize = [1]; rescue ArgumentError => e; e.message; end)
        res << (begin; f.winsize = [1, 2, 3, 4, 5]; rescue ArgumentError => e; e.message; end)
        f.winsize = [nil, 9]
        res << f.winsize
        f.winsize = [11.7, 12]
        res << f.winsize
        require "io/console/size"
        res << IO.console_size.size
        f.close
        res
        "##,
        path = pty.path,
    ));
}

/// The termios blob is opaque to Ruby, but the primitives still refuse a
/// String of the wrong length rather than reading past it.
#[test]
fn io_console_rejects_a_malformed_termios_blob() {
    run_test_error(r#"require "io/console"; IO.__termios_echo?("x")"#);
    run_test_error(r#"require "io/console"; IO.__termios_raw("", nil, nil, false)"#);
}

/// Reads fed by the fake terminal: getch, raw getc, getpass and the
/// cursor-position query.
#[test]
fn io_console_reads_on_a_pty() {
    let pty = open_pty();
    run_test_once_live(&format!(
        r##"
        require "io/console"
        f = File.open("{path}", "r+")
        f.sync = true
        res = []
        f.cooked!
        f.write("\x01")
        res << f.getch << f.getch
        f.write("\x01")
        res << f.raw {{ f.getc }} << f.getch(min: 1)
        f.write("\x01")
        res << f.getch(min: 1, time: 1) << f.getch(intr: true)
        res << f.echo?
        f.write("\x02")
        res << f.getpass("pw:")
        f.write("\x02")
        res << f.getpass
        res << (begin; f.getpass(1); rescue TypeError => e; e.message; end)
        res << f.cursor
        res << f.echo?
        f.close
        res
        "##,
        path = pty.path,
    ));
}
