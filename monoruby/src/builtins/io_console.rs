//! Hidden termios / ioctl primitives behind `io/console`.
//!
//! The public surface (`IO#raw`, `IO#getch`, `IO#winsize`, `IO.console`,
//! `IO::ConsoleMode`, …) lives in Ruby, in `stdlib/io/console.rb`; this
//! module only exposes what needs libc: reading and writing a terminal's
//! attributes, transforming an attribute set the way CRuby's
//! `set_rawmode` / `set_cookedmode` / `set_echo` do, the window size
//! ioctls, `tcflush(3)` and `ttyname(3)`.
//!
//! A `struct termios` travels between Ruby and Rust as an opaque
//! binary String of exactly `size_of::<libc::termios>()` bytes — that is
//! what an `IO::ConsoleMode` wraps — so the flag layout, which differs
//! between Linux and Darwin, never leaks into Ruby.

use super::*;

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_func(IO_CLASS, "__tcgetattr", io_tcgetattr, 1);
    globals.define_builtin_func(IO_CLASS, "__tcsetattr", io_tcsetattr, 2);
    globals.define_builtin_func(IO_CLASS, "__tcflush", io_tcflush, 1);
    globals.define_builtin_func(IO_CLASS, "__winsize", io_winsize, 0);
    globals.define_builtin_func(IO_CLASS, "__set_winsize", io_set_winsize, 4);
    globals.define_builtin_func(IO_CLASS, "__ttyname", io_ttyname, 0);
    globals.define_builtin_class_func(IO_CLASS, "__termios_raw", termios_raw, 4);
    globals.define_builtin_class_func(IO_CLASS, "__termios_cooked", termios_cooked, 1);
    globals.define_builtin_class_func(IO_CLASS, "__termios_echo", termios_echo, 2);
    globals.define_builtin_class_func(IO_CLASS, "__termios_echo?", termios_echo_p, 1);
}

/// The `Errno::*` CRuby's console.c raises: `sys_fail(io)` appends the
/// stream's path (`Inappropriate ioctl for device - <STDOUT>`), while the
/// `ttymode` wrapper raises the bare description. Must be called right
/// after the failing libc call, before anything else can touch `errno`.
fn sys_fail(store: &Store, io: Value, with_path: bool) -> MonorubyErr {
    let err = std::io::Error::last_os_error();
    match (with_path, io.as_io_inner().path()) {
        (true, Some(path)) => MonorubyErr::errno_with_msg(store, &err, path),
        _ => MonorubyErr::errno_plain(store, &err),
    }
}

fn termios_to_value(t: &libc::termios) -> Value {
    // SAFETY: `t` was zero-initialised before the kernel filled it, so
    // every byte of the struct (padding included) is initialised, and
    // `termios` is plain old data.
    let bytes = unsafe {
        std::slice::from_raw_parts(
            t as *const libc::termios as *const u8,
            std::mem::size_of::<libc::termios>(),
        )
    };
    Value::bytes(bytes.to_vec())
}

fn termios_from_value(store: &Store, v: Value) -> Result<libc::termios> {
    let bytes = v.expect_bytes(store)?;
    if bytes.len() != std::mem::size_of::<libc::termios>() {
        return Err(MonorubyErr::typeerr(
            "wrong argument type (expected IO::ConsoleMode)",
        ));
    }
    // SAFETY: the length was checked against the struct size, and
    // `read_unaligned` copies the plain-old-data struct out of the byte
    // buffer without an alignment requirement.
    Ok(unsafe { std::ptr::read_unaligned(bytes.as_ptr() as *const libc::termios) })
}

fn fetch_termios(store: &Store, io: Value, with_path: bool) -> Result<libc::termios> {
    let fd = io.as_io_inner().fileno()?;
    // SAFETY: an all-zero `termios` is a valid (if meaningless) value;
    // tcgetattr(3) overwrites it on success.
    let mut t: libc::termios = unsafe { std::mem::zeroed() };
    // SAFETY: `t` is a live, writable `termios`.
    if unsafe { libc::tcgetattr(fd, &mut t) } != 0 {
        return Err(sys_fail(store, io, with_path));
    }
    Ok(t)
}

fn store_termios(store: &Store, io: Value, t: &libc::termios, with_path: bool) -> Result<()> {
    let fd = io.as_io_inner().fileno()?;
    loop {
        // SAFETY: `t` is a live `termios`; TCSANOW applies it immediately,
        // like console.c's `setattr`, retrying on EINTR.
        if unsafe { libc::tcsetattr(fd, libc::TCSANOW, t) } == 0 {
            return Ok(());
        }
        let err = std::io::Error::last_os_error();
        if err.raw_os_error() != Some(libc::EINTR) {
            return Err(sys_fail(store, io, with_path));
        }
    }
}

/// `clamp_uchar` in console.c: VMIN / VTIME are `cc_t` (a byte).
fn clamp_uchar(n: i64) -> libc::cc_t {
    n.clamp(0, u8::MAX as i64) as libc::cc_t
}

fn opt_cc(store: &Store, v: Value) -> Result<Option<libc::cc_t>> {
    if v.is_nil() {
        Ok(None)
    } else {
        Ok(Some(clamp_uchar(v.expect_integer(store)?)))
    }
}

///
/// ### IO#__tcgetattr(with_path) -> String
///
/// The stream's current terminal attributes as an opaque `termios` blob.
#[monoruby_builtin]
fn io_tcgetattr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let t = fetch_termios(&globals.store, lfp.self_val(), lfp.arg(0).as_bool())?;
    Ok(termios_to_value(&t))
}

///
/// ### IO#__tcsetattr(blob, with_path) -> nil
#[monoruby_builtin]
fn io_tcsetattr(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let t = termios_from_value(&globals.store, lfp.arg(0))?;
    store_termios(&globals.store, lfp.self_val(), &t, lfp.arg(1).as_bool())?;
    Ok(Value::nil())
}

///
/// ### IO#__tcflush(queue) -> nil
///
/// `queue`: 0 = input (`iflush`), 1 = output (`oflush`), 2 = both.
#[monoruby_builtin]
fn io_tcflush(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let io = lfp.self_val();
    let fd = io.as_io_inner().fileno()?;
    let queue = match lfp.arg(0).expect_integer(&globals.store)? {
        0 => libc::TCIFLUSH,
        1 => libc::TCOFLUSH,
        _ => libc::TCIOFLUSH,
    };
    // SAFETY: tcflush(3) takes only an fd and a queue selector.
    if unsafe { libc::tcflush(fd, queue) } != 0 {
        return Err(sys_fail(&globals.store, io, true));
    }
    Ok(Value::nil())
}

///
/// ### IO#__winsize -> [rows, columns]
#[monoruby_builtin]
fn io_winsize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let io = lfp.self_val();
    let fd = io.as_io_inner().fileno()?;
    // SAFETY: an all-zero `winsize` is a valid value; TIOCGWINSZ fills it.
    let mut ws: libc::winsize = unsafe { std::mem::zeroed() };
    // SAFETY: TIOCGWINSZ writes a `struct winsize` through its pointer
    // argument, which `ws` is.
    if unsafe { libc::ioctl(fd, libc::TIOCGWINSZ as _, &mut ws as *mut libc::winsize) } != 0 {
        return Err(sys_fail(&globals.store, io, true));
    }
    Ok(Value::array_from_vec(vec![
        Value::integer(ws.ws_row as i64),
        Value::integer(ws.ws_col as i64),
    ]))
}

///
/// ### IO#__set_winsize(rows, columns, xpixel, ypixel) -> nil
///
/// Each argument is an Integer already validated by the Ruby side (nil
/// was mapped to 0 there); they are truncated to `unsigned short` like
/// console.c's `SET` macro.
#[monoruby_builtin]
fn io_set_winsize(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let io = lfp.self_val();
    let fd = io.as_io_inner().fileno()?;
    let field = |i: usize| -> Result<u16> {
        Ok(lfp.arg(i).expect_integer(&globals.store)? as u16)
    };
    let ws = libc::winsize {
        ws_row: field(0)?,
        ws_col: field(1)?,
        ws_xpixel: field(2)?,
        ws_ypixel: field(3)?,
    };
    // SAFETY: TIOCSWINSZ reads a `struct winsize` through its pointer
    // argument, which `ws` is.
    if unsafe { libc::ioctl(fd, libc::TIOCSWINSZ as _, &ws as *const libc::winsize) } != 0 {
        return Err(sys_fail(&globals.store, io, true));
    }
    Ok(Value::nil())
}

///
/// ### IO#__ttyname -> String | nil
///
/// `nil` when the descriptor is not a terminal (console.c checks
/// `isatty` first), otherwise the device path from `ttyname_r(3)`.
#[monoruby_builtin]
fn io_ttyname(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let fd = lfp.self_val().as_io_inner().fileno()?;
    // SAFETY: isatty(3) only inspects the descriptor.
    if unsafe { libc::isatty(fd) } == 0 {
        return Ok(Value::nil());
    }
    let mut buf = vec![0u8; 1024];
    loop {
        // SAFETY: `buf` is a writable buffer of the length passed.
        let e = unsafe { libc::ttyname_r(fd, buf.as_mut_ptr() as *mut libc::c_char, buf.len()) };
        if e == 0 {
            let len = buf.iter().position(|&b| b == 0).unwrap_or(buf.len());
            buf.truncate(len);
            return Ok(Value::string(String::from_utf8_lossy(&buf).into_owned()));
        }
        if e == libc::ERANGE {
            buf.resize(buf.len() * 2, 0);
            continue;
        }
        let err = std::io::Error::from_raw_os_error(e);
        return Err(MonorubyErr::errno_with_msg(&globals.store, &err, "ttyname_r"));
    }
}

///
/// ### IO.__termios_raw(blob, min, time, intr) -> String
///
/// console.c's `set_rawmode` on a copy of `blob`: `cfmakeraw(3)`, then
/// ECHOE / ECHOK off; `min` / `time` (nil = leave cfmakeraw's 1 / 0)
/// set VMIN / VTIME; a true `intr` re-enables BRKINT, ISIG and OPOST.
#[monoruby_builtin]
fn termios_raw(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut t = termios_from_value(&globals.store, lfp.arg(0))?;
    let min = opt_cc(&globals.store, lfp.arg(1))?;
    let time = opt_cc(&globals.store, lfp.arg(2))?;
    let intr = lfp.arg(3).as_bool();
    // SAFETY: `t` is a live `termios`.
    unsafe { libc::cfmakeraw(&mut t) };
    t.c_lflag &= !(libc::ECHOE | libc::ECHOK);
    if let Some(min) = min {
        t.c_cc[libc::VMIN] = min;
    }
    if let Some(time) = time {
        t.c_cc[libc::VTIME] = time;
    }
    if intr {
        t.c_iflag |= libc::BRKINT;
        t.c_lflag |= libc::ISIG;
        t.c_oflag |= libc::OPOST;
    }
    Ok(termios_to_value(&t))
}

///
/// ### IO.__termios_cooked(blob) -> String
///
/// console.c's `set_cookedmode`: BRKINT | ISTRIP | ICRNL | IXON on
/// input, OPOST on output, echo + canonical + signals + extensions on.
#[monoruby_builtin]
fn termios_cooked(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut t = termios_from_value(&globals.store, lfp.arg(0))?;
    t.c_iflag |= libc::BRKINT | libc::ISTRIP | libc::ICRNL | libc::IXON;
    t.c_oflag |= libc::OPOST;
    t.c_lflag |= libc::ECHO
        | libc::ECHOE
        | libc::ECHOK
        | libc::ECHONL
        | libc::ICANON
        | libc::ISIG
        | libc::IEXTEN;
    Ok(termios_to_value(&t))
}

const ECHO_FLAGS: libc::tcflag_t = libc::ECHO | libc::ECHOE | libc::ECHOK | libc::ECHONL;

///
/// ### IO.__termios_echo(blob, flag) -> String
///
/// `set_echo` / `set_noecho`: ECHO | ECHOE | ECHOK | ECHONL on or off.
#[monoruby_builtin]
fn termios_echo(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut t = termios_from_value(&globals.store, lfp.arg(0))?;
    if lfp.arg(1).as_bool() {
        t.c_lflag |= ECHO_FLAGS;
    } else {
        t.c_lflag &= !ECHO_FLAGS;
    }
    Ok(termios_to_value(&t))
}

///
/// ### IO.__termios_echo?(blob) -> bool
///
/// `echo_p`: true when ECHO or ECHONL is set.
#[monoruby_builtin]
fn termios_echo_p(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let t = termios_from_value(&globals.store, lfp.arg(0))?;
    Ok(Value::bool((t.c_lflag & (libc::ECHO | libc::ECHONL)) != 0))
}
