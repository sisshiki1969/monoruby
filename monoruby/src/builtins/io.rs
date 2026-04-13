use super::*;
use std::fs::File;
use std::os::fd::FromRawFd;
use std::os::unix::process::CommandExt;
use std::process::{Command, Stdio};
use std::rc::Rc;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS, ObjTy::IO);
    globals.define_builtin_class_func_with(IO_CLASS, "new", io_new, 1, 3, false);
    globals.store[IO_CLASS].set_alloc_func(io_alloc_func);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_func_with(IO_CLASS, "puts", puts, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "print", print, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "printf", printf, 1, 1, true);
    globals.define_builtin_func(IO_CLASS, "flush", flush, 0);
    globals.define_builtin_func_with(IO_CLASS, "gets", gets, 0, 2, false);
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
    globals.define_builtin_func(IO_CLASS, "close", close, 0);
    globals.define_builtin_func(IO_CLASS, "close_write", close_write, 0);
    globals.define_builtin_func(IO_CLASS, "close_read", close_read, 0);
    globals.define_builtin_func(IO_CLASS, "closed?", closed_, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);
    globals.define_builtin_func_with(IO_CLASS, "read", read, 0, 2, false);
    globals.define_builtin_func_with(IO_CLASS, "readline", readline, 0, 2, false);
    globals.define_builtin_funcs(IO_CLASS, "each", &["each_line"], each_line, 0);
    globals.define_builtin_class_func_with(IO_CLASS, "read", io_class_read, 1, 4, false);
    globals.define_builtin_class_func_with(IO_CLASS, "sysopen", io_sysopen, 1, 3, false);
    globals.define_builtin_class_func_with(IO_CLASS, "pipe", io_pipe, 0, 3, false);
    globals.define_builtin_class_func_rest(IO_CLASS, "popen", io_popen);
    globals.define_builtin_func(IO_CLASS, "pid", io_pid, 0);
    globals.define_builtin_func(IO_CLASS, "fileno", io_fileno, 0);
    globals.define_builtin_func_with(IO_CLASS, "write", io_write_method, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "syswrite", io_syswrite, 1, 1, false);
    globals.define_builtin_class_func_with(IO_CLASS, "select", io_select, 1, 4, false);
    globals.define_builtin_class_func_with(IO_CLASS, "foreach", io_foreach, 1, 2, false);
    globals.define_builtin_class_func_with(IO_CLASS, "copy_stream", io_copy_stream, 2, 4, false);
    globals.define_builtin_func_with(IO_CLASS, "set_encoding", set_encoding, 1, 3, false);
    globals.define_builtin_func(IO_CLASS, "external_encoding", external_encoding, 0);
    let stdin = Value::new_io_stdin();
    globals.set_constant_by_str(OBJECT_CLASS, "STDIN", stdin);
    globals.set_gvar(IdentId::get_id("$stdin"), stdin);

    let stdout = Value::new_io_stdout();
    globals.set_constant_by_str(OBJECT_CLASS, "STDOUT", stdout);
    globals.set_gvar(IdentId::get_id("$stdout"), stdout);
    globals.set_gvar(IdentId::get_id("$>"), stdout);

    let stderr = Value::new_io_stderr();
    globals.set_constant_by_str(OBJECT_CLASS, "STDERR", stderr);
    globals.set_gvar(IdentId::get_id("$stderr"), stderr);
}

#[monoruby_builtin]
fn io_new(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    // IO.new(fd [, mode [, opt]]) -> creates an IO object from an integer file descriptor.
    if let Some(fd) = lfp.arg(0).try_fixnum() {
        let fd_i32 = fd as i32;
        if fd_i32 < 0 || unsafe { libc::fcntl(fd_i32, libc::F_GETFD) } == -1 {
            let err = std::io::Error::from_raw_os_error(9); // EBADF
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &format!("fd {}", fd),
            ));
        }
        let name = format!("fd {}", fd);
        let io_inner = IoInner::from_raw_fd(fd_i32, name);
        return Ok(Value::new_io(io_inner));
    }
    Err(MonorubyErr::argumenterr("IO.new requires an integer file descriptor"))
}

/// Allocator for `IO` and its subclasses.
pub(crate) extern "C" fn io_alloc_func(class_id: ClassId, _: &mut Globals) -> Value {
    Value::new_io_with_class(IoInner::Closed, class_id)
}

///
/// ### IO#<<
///
/// - self << object -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if let Some(b) = lfp.arg(0).try_bytes() {
        io.write(b.as_bytes())?;
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        io.write(s.as_bytes())?;
    };
    io.flush()?;
    Ok(lfp.self_val())
}

///
/// ### IO#puts
///
/// - puts(*obj) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/puts.html]
#[monoruby_builtin]
fn puts(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value, seen: &mut Vec<u64>) {
        match val.try_array_ty() {
            Some(ary) => {
                let id = val.id();
                if seen.contains(&id) {
                    collector.push(Value::string("[...]".to_string()));
                } else {
                    seen.push(id);
                    ary.iter().for_each(|v| decompose(collector, *v, seen));
                    seen.pop();
                }
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    let mut seen = Vec::new();
    let args = lfp.arg(0).as_array();
    for v in args.iter().cloned() {
        decompose(&mut collector, v, &mut seen);
    }

    let self_val = lfp.self_val();
    let write_id = IdentId::get_id("write");
    if collector.is_empty() {
        // puts with no args writes a newline
        let newline = Value::string_from_str("\n");
        vm.invoke_method_inner(globals, write_id, self_val, &[newline], None, None)?;
    } else {
        for v in collector {
            let s = if v.is_nil() {
                String::new()
            } else if let Some(rs) = v.is_rstring() {
                String::from_utf8_lossy(rs.as_bytes()).into_owned()
            } else {
                v.to_s(globals)
            };
            let needs_newline = !s.ends_with('\n');
            let write_str = if needs_newline {
                Value::string(s + "\n")
            } else {
                Value::string(s)
            };
            vm.invoke_method_inner(globals, write_id, self_val, &[write_str], None, None)?;
        }
    }
    // Flush after all writes
    let mut self_ = lfp.self_val();
    self_.as_io_inner_mut().flush()?;
    Ok(Value::nil())
}

///
/// ### IO#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/print.html]
#[monoruby_builtin]
fn print(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_val = lfp.self_val();
    let write_id = IdentId::get_id("write");
    for v in lfp.arg(0).as_array().iter().cloned() {
        let str_val = if v.is_rstring().is_some() {
            v
        } else {
            let s = vm.to_s(globals, v)?;
            Value::string(s)
        };
        vm.invoke_method_inner(globals, write_id, self_val, &[str_val], None, None)?;
    }
    Ok(Value::nil())
}

///
/// ### IO#printf
///
/// - printf(format, *arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/printf.html]
#[monoruby_builtin]
fn printf(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let format_str = lfp.arg(0).coerce_to_string(vm, globals)?;
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let args = lfp.arg(1).as_array();

    let buf = vm.format_by_args(globals, &format_str, &args)?;
    io.write(buf.as_bytes())?;

    Ok(Value::nil())
}

///
/// ### IO#flush
///
/// - flush -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/flush.html]
#[monoruby_builtin]
fn flush(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    self_.as_io_inner_mut().flush()?;

    Ok(lfp.self_val())
}

///
/// ### IO#gets
///
/// - gets([NOT SUPPORTED]rs = $/) -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/gets.html]
#[monoruby_builtin]
fn gets(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    Ok(if let Some(buf) = io.read_line()? {
        Value::string(buf)
    } else {
        Value::nil()
    })
}

///
/// ### IO#isatty
///
/// - isatty -> bool
/// - tty? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/isatty.html]
#[monoruby_builtin]
fn isatty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner_mut().isatty()))
}

///
/// ### IO#close
///
/// - close -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/close.html]
#[monoruby_builtin]
fn close(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let popen_result = lfp.self_val().as_io_inner_mut().close()?;
    if let Some((exit_status, pid)) = popen_result {
        // Set $? (Process::Status) via Process::Status.new(exitstatus, pid)
        let status_class =
            vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])?;
        let status_obj = vm.invoke_method_inner(
            globals,
            IdentId::NEW,
            status_class,
            &[
                Value::integer(exit_status as i64),
                Value::integer(pid as i64),
            ],
            None,
            None,
        )?;
        globals.set_gvar(IdentId::get_id("$?"), status_obj);
    }
    Ok(Value::nil())
}

///
/// ### IO#closed?
///
/// ### IO#close_write
#[monoruby_builtin]
fn close_write(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    match io {
        IoInner::Popen(popen) => {
            let popen = Rc::get_mut(popen).unwrap();
            popen.writer = None;
            Ok(Value::nil())
        }
        IoInner::Closed => Err(MonorubyErr::ioerr("closed stream")),
        _ => Err(MonorubyErr::ioerr("closing non-duplex IO for writing")),
    }
}

/// ### IO#close_read
#[monoruby_builtin]
fn close_read(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    match io {
        IoInner::Popen(popen) => {
            let popen = Rc::get_mut(popen).unwrap();
            popen.reader = None;
            Ok(Value::nil())
        }
        IoInner::Closed => Err(MonorubyErr::ioerr("closed stream")),
        _ => Err(MonorubyErr::ioerr("closing non-duplex IO for reading")),
    }
}

/// - closed? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/closed=3f.html]
#[monoruby_builtin]
fn closed_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner().is_closed()))
}

#[monoruby_builtin]
fn assign_sync(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    Ok(lfp.arg(0))
}

///
/// ### IO#read
///
/// - read(length = nil, [NOT SUPPRTED] outbuf = "") -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/read.html
#[monoruby_builtin]
fn read(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let length = match lfp.try_arg(0) {
        Some(v) => {
            if v.is_nil() {
                None
            } else {
                let length = v.coerce_to_int_i64(vm, globals)?;
                if length < 0 {
                    return Err(MonorubyErr::argumenterr("negative length"));
                }
                Some(length as usize)
            }
        }
        None => None,
    };
    let buf = lfp.self_val().as_io_inner_mut().read(length)?;
    if buf.is_empty() && length.is_some() && length != Some(0) {
        return Ok(Value::nil());
    }
    Ok(Value::string_from_vec(buf))
}

///
/// ### IO#readline
///
/// - readline([NOT SUPPORTED] rs = $/, [NOT SUPPORTED] chomp: false) -> String
/// - readline([NOT SUPPORTED] limit, [NOT SUPPORTED] chomp: false) -> String
/// - readline([NOT SUPPORTED] rs, [NOT SUPPORTED] limit, [NOT SUPPORTED] chomp: false) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/readline.html]
#[monoruby_builtin]
fn readline(_: &mut Executor, _: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let s = match lfp.self_val().as_io_inner_mut().read_line()? {
        Some(s) => s,
        None => return Err(MonorubyErr::runtimeerr("end of file reached")),
    };
    Ok(Value::string(s))
}

///
/// ### IO#each
///
/// - each(rs = $/, chomp: false) {|line| ... } -> self
/// - each(limit, chomp: false) {|line| ... } -> self
/// - each(rs, limit, chomp: false) {|line| ... } -> self
/// - each(rs = $/, chomp: false) -> Enumerator
/// - each(limit, chomp: false) -> Enumerator
/// - each(rs, limit, chomp: false) -> Enumerator
///
/// - each_line(rs = $/, chomp: false) {|line| ... } -> self
/// - each_line(limit, chomp: false) {|line| ... } -> self
/// - each_line(rs, limit, chomp: false) {|line| ... } -> self
/// - each_line(rs = $/, chomp: false) -> Enumerator
/// - each_line(limit, chomp: false) -> Enumerator
/// - each_line(rs, limit, chomp: false) -> Enumerator
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/each.html]
#[monoruby_builtin]
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if let Some(bh) = lfp.block() {
        let p = vm.get_block_data(globals, bh)?;
        while let Some(s) = io.read_line()? {
            vm.invoke_block(globals, &p, &[Value::string(s)])?;
        }
    } else {
        return Err(MonorubyErr::runtimeerr(
            "IO#each_line without block is not yet supported",
        ));
    };
    Ok(lfp.self_val())
}

///
/// ### IO.read
///
/// - read(path) -> String
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/read.html]
#[monoruby_builtin]
fn io_class_read(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let filename = lfp.arg(0).coerce_to_string(vm, globals)?;
    let mut file = match File::open(&filename) {
        Ok(file) => file,
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_sysopen",
                &filename,
            ));
        }
    };
    let mut contents = Vec::new();
    match std::io::Read::read_to_end(&mut file, &mut contents) {
        Ok(_) => {}
        Err(err) => {
            return Err(MonorubyErr::errno_with_path(
                &globals.store,
                &err,
                "rb_io_read",
                &filename,
            ));
        }
    };
    Ok(Value::bytes(contents))
}

///
/// ### IO.foreach
///
/// - foreach(path, sep = "\n") {|line| ... } -> nil
/// - foreach(path, sep = "\n") -> Enumerator
///
/// Opens the file at `path`, reads each line separated by `sep`,
/// and yields each line to the block. Without a block, returns an Enumerator.
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/foreach.html]
#[monoruby_builtin]
fn io_foreach(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let bh = match lfp.block() {
        Some(bh) => bh,
        None => {
            return Err(MonorubyErr::runtimeerr(
                "IO.foreach without block is not yet supported",
            ));
        }
    };
    let p = vm.get_block_data(globals, bh)?;
    let content = std::fs::read_to_string(&path)
        .map_err(|e| MonorubyErr::runtimeerr(format!("{}: {}", path, e)))?;
    let sep = if let Some(sep_val) = lfp.try_arg(1) {
        if sep_val.is_nil() {
            None
        } else {
            Some(sep_val.coerce_to_str(vm, globals)?)
        }
    } else {
        Some("\n".to_string())
    };
    match sep {
        None => {
            // When sep is nil, yield the entire content as one string
            vm.invoke_block(globals, &p, &[Value::string(content)])?;
        }
        Some(sep) => {
            let mut start = 0;
            let content_bytes = content.as_bytes();
            let sep_bytes = sep.as_bytes();
            if sep_bytes.is_empty() {
                // Paragraph mode: split on double newlines
                let parts: Vec<&str> = content.split("\n\n").collect();
                for part in parts {
                    let trimmed = part.trim_start_matches('\n');
                    if !trimmed.is_empty() {
                        let mut line = trimmed.to_string();
                        line.push('\n');
                        vm.invoke_block(globals, &p, &[Value::string(line)])?;
                    }
                }
            } else {
                while start < content_bytes.len() {
                    if let Some(pos) = content[start..].find(&sep) {
                        let end = start + pos + sep.len();
                        let line = &content[start..end];
                        vm.invoke_block(globals, &p, &[Value::string(line.to_string())])?;
                        start = end;
                    } else {
                        // Last line without separator
                        let line = &content[start..];
                        vm.invoke_block(globals, &p, &[Value::string(line.to_string())])?;
                        break;
                    }
                }
            }
        }
    }
    Ok(Value::nil())
}

///
/// ### IO.pipe
///
/// ### IO.sysopen
///
/// - sysopen(path, mode_str = "r", perm = 0666) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/sysopen.html]
#[monoruby_builtin]
fn io_sysopen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    use std::os::unix::io::IntoRawFd;
    let path = lfp.arg(0).coerce_to_str(vm, globals)?;
    let mode_str = if let Some(m) = lfp.try_arg(1) {
        m.coerce_to_str(vm, globals)?
    } else {
        "r".to_string()
    };
    let mut opts = std::fs::OpenOptions::new();
    // Strip encoding suffix (e.g. ":UTF-8") and remove 'b' (binary) flag.
    let mode_base = mode_str.split(':').next().unwrap().replace('b', "");
    match mode_base.as_str() {
        "r" => {
            opts.read(true);
        }
        "w" => {
            opts.write(true).create(true).truncate(true);
        }
        "a" => {
            opts.append(true).create(true);
        }
        "r+" | "+r" => {
            opts.read(true).write(true);
        }
        "w+" | "+w" => {
            opts.read(true).write(true).create(true).truncate(true);
        }
        "a+" | "+a" => {
            opts.read(true).append(true).create(true);
        }
        _ => {
            opts.read(true);
        }
    }
    let file = opts
        .open(&path)
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path))?;
    let fd = file.into_raw_fd();
    Ok(Value::integer(fd as i64))
}

/// - pipe -> [read_io, write_io]
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/pipe.html]
#[monoruby_builtin]
fn io_pipe(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let mut fds: [libc::c_int; 2] = [0; 2];
    // SAFETY: fds is a valid pointer to a 2-element array of c_int.
    let ret = unsafe { libc::pipe(fds.as_mut_ptr()) };
    if ret == -1 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(
            &_globals.store,
            &err,
            "pipe(2)",
        ));
    }
    let read_io = Value::new_io(IoInner::from_raw_fd(fds[0], "pipe".to_string()));
    let write_io = Value::new_io(IoInner::from_raw_fd(fds[1], "pipe".to_string()));
    Ok(Value::array2(read_io, write_io))
}

/// ### IO.popen
///
/// - IO.popen(command) -> IO
/// - IO.popen(command) {|io| ... } -> object
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/popen.html]
#[monoruby_builtin]
fn io_popen(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let args = lfp.arg(0).as_array();
    if args.is_empty() {
        return Err(MonorubyErr::argumenterr(
            "wrong number of arguments (given 0, expected 1+)",
        ));
    }
    let cmd_val = args[0];

    // Build the command from either a String or an Array of strings.
    let (mut command, cmd_name) = if let Some(ary) = cmd_val.try_array_ty() {
        let parts: Vec<String> = ary.iter().map(|v| v.to_s(globals)).collect();
        if parts.is_empty() {
            return Err(MonorubyErr::argumenterr("popen: empty command array"));
        }
        let name = parts[0].clone();
        let mut cmd = Command::new(&parts[0]);
        for part in &parts[1..] {
            cmd.arg(part);
        }
        (cmd, name)
    } else {
        let cmd_str = cmd_val.coerce_to_str(vm, globals)?;
        let mut cmd = Command::new("sh");
        cmd.arg("-c").arg(cmd_str.to_string());
        (cmd, cmd_str)
    };

    // Parse mode and options.
    // IO.popen accepts:
    //   IO.popen(cmd)
    //   IO.popen(cmd, mode)
    //   IO.popen(cmd, mode, opts)
    //   IO.popen(cmd, opts)   -- Hash as second arg means spawn options
    let mut opts_hash = None;
    let mode = if args.len() > 1 {
        if args[1].try_hash_ty().is_some() {
            // Second arg is options hash, mode defaults to "r"
            opts_hash = Some(args[1]);
            "r".to_string()
        } else {
            let m = args[1].coerce_to_str(vm, globals)?;
            if args.len() > 2 && args[2].try_hash_ty().is_some() {
                opts_hash = Some(args[2]);
            }
            m
        }
    } else {
        "r".to_string()
    };
    let readable = mode.contains('r') || mode.contains('+');
    let writable = mode.contains('w') || mode.contains('+');

    if writable {
        command.stdin(Stdio::piped());
    } else {
        command.stdin(Stdio::null());
    }
    if readable {
        command.stdout(Stdio::piped());
    } else {
        command.stdout(Stdio::inherit());
    }

    // Handle err: [:child, :out] option to redirect stderr to stdout
    let mut stderr_to_stdout = false;
    if let Some(opts) = opts_hash {
        let err_key = Value::symbol_from_str("err");
        if let Ok(Some(err_val)) = opts.as_hash().get(err_key, vm, globals) {
            if let Some(ary) = err_val.try_array_ty() {
                if ary.len() == 2
                    && ary[0].try_symbol_or_string() == Some(IdentId::get_id("child"))
                    && ary[1].try_symbol_or_string() == Some(IdentId::get_id("out"))
                {
                    stderr_to_stdout = true;
                }
            }
        }
    }
    if stderr_to_stdout {
        // Redirect stderr to stdout via OS-level dup2
        // SAFETY: dup2(1, 2) duplicates stdout fd to stderr fd, which is safe
        // for child processes about to exec.
        unsafe {
            command.pre_exec(|| {
                libc::dup2(1, 2);
                Ok(())
            });
        }
        command.stderr(Stdio::inherit());
    } else {
        command.stderr(Stdio::inherit());
    }

    let child = command
        .spawn()
        .map_err(|e| MonorubyErr::errno_with_path(&globals.store, &e, "rb_f_spawn", &cmd_name))?;

    let io_val = Value::new_io(IoInner::popen(child));

    if let Some(bh) = lfp.block() {
        let data = vm.get_block_data(globals, bh)?;
        let res = vm.invoke_block(globals, &data, &[io_val]);
        let mut io_close = io_val;
        if let Ok(Some((exit_status, pid))) = io_close.as_io_inner_mut().close() {
            if let Ok(status_class) =
                vm.get_qualified_constant(globals, OBJECT_CLASS, &["Process", "Status"])
            {
                if let Ok(status_obj) = vm.invoke_method_inner(
                    globals,
                    IdentId::NEW,
                    status_class,
                    &[
                        Value::integer(exit_status as i64),
                        Value::integer(pid as i64),
                    ],
                    None,
                    None,
                ) {
                    globals.set_gvar(IdentId::get_id("$?"), status_obj);
                }
            }
        }
        res
    } else {
        Ok(io_val)
    }
}

/// ### IO#pid
///
/// - pid -> Integer | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/pid.html]
#[monoruby_builtin]
fn io_pid(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let self_ = lfp.self_val();
    match self_.as_io_inner().pid() {
        Some(pid) => Ok(Value::integer(pid as i64)),
        None => Ok(Value::nil()),
    }
}

/// ### IO#fileno
///
/// - fileno -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/fileno.html]
#[monoruby_builtin]
fn io_fileno(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let self_ = lfp.self_val();
    let fd = self_.as_io_inner().fileno()?;
    Ok(Value::integer(fd as i64))
}

/// ### IO#write
///
/// - write(*str) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/write.html]
#[monoruby_builtin]
fn io_write_method(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let args = lfp.arg(0).as_array();
    let mut total = 0i64;
    for v in args.iter().cloned() {
        let bytes = if let Some(b) = v.try_bytes() {
            b.to_vec()
        } else {
            let s = vm.to_s(globals, v)?;
            s.into_bytes()
        };
        total += bytes.len() as i64;
        io.write(&bytes)?;
    }
    Ok(Value::integer(total))
}

/// ### IO#syswrite
///
/// - syswrite(string) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/syswrite.html]
#[monoruby_builtin]
fn io_syswrite(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let bytes = if let Some(b) = lfp.arg(0).try_bytes() {
        b.to_vec()
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        s.into_bytes()
    };
    let fd = io.fileno()?;
    // SAFETY: fd is a valid file descriptor, bytes is a valid buffer.
    let written = unsafe { libc::write(fd, bytes.as_ptr() as *const libc::c_void, bytes.len()) };
    if written < 0 {
        let err = std::io::Error::last_os_error();
        return Err(MonorubyErr::errno_with_msg(
            &globals.store,
            &err,
            "syswrite",
        ));
    }
    Ok(Value::integer(written as i64))
}

/// Helper: extract raw fd from a Value that is an IO (or responds to to_io).
fn value_to_fd(globals: &Globals, v: Value) -> Result<i32> {
    if let Some(rv) = v.try_rvalue() {
        if rv.ty() == ObjTy::IO {
            return v.as_io_inner().fileno();
        }
    }
    Err(MonorubyErr::typeerr(format!(
        "no implicit conversion of {} into IO",
        globals.get_class_name(v.class())
    )))
}

/// ### IO.select
///
/// - IO.select(read_array [, write_array [, error_array [, timeout]]]) -> array or nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/select.html]
#[monoruby_builtin]
fn io_select(vm: &mut Executor, globals: &mut Globals, lfp: Lfp, _: BytecodePtr) -> Result<Value> {
    let read_arg = lfp.arg(0);
    let write_arg = lfp.try_arg(1).unwrap_or(Value::nil());
    let error_arg = lfp.try_arg(2).unwrap_or(Value::nil());
    let timeout_arg = lfp.try_arg(3).unwrap_or(Value::nil());

    // Parse timeout
    let timeout = if timeout_arg.is_nil() {
        None // block forever
    } else {
        let f = timeout_arg.coerce_to_f64(vm, globals)?;
        if f.is_nan() {
            return Err(MonorubyErr::rangeerr("NaN out of Time range"));
        }
        if f < 0.0 {
            return Err(MonorubyErr::argumenterr(
                "time interval must not be negative",
            ));
        }
        let secs = f.floor() as libc::time_t;
        let usecs = ((f - f.floor()) * 1_000_000.0) as libc::suseconds_t;
        Some(libc::timeval {
            tv_sec: secs,
            tv_usec: usecs,
        })
    };

    // Collect IO values and their fds
    let read_ios: Vec<Value> = if read_arg.is_nil() {
        vec![]
    } else {
        let ary = read_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    let write_ios: Vec<Value> = if write_arg.is_nil() {
        vec![]
    } else {
        let ary = write_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    let error_ios: Vec<Value> = if error_arg.is_nil() {
        vec![]
    } else {
        let ary = error_arg
            .try_array_ty()
            .ok_or_else(|| MonorubyErr::typeerr("no implicit conversion of Object into Array"))?;
        ary.to_vec()
    };

    // Get fds
    let read_fds: Vec<i32> = read_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;
    let write_fds: Vec<i32> = write_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;
    let error_fds: Vec<i32> = error_ios
        .iter()
        .map(|v| value_to_fd(globals, *v))
        .collect::<Result<Vec<_>>>()?;

    // If all arrays are empty with no timeout, return nil immediately.
    // In CRuby this would block forever, but monoruby is single-threaded
    // so nothing could ever wake us up.
    if read_ios.is_empty() && write_ios.is_empty() && error_ios.is_empty() && timeout.is_none() {
        return Ok(Value::nil());
    }

    // Find max fd
    let nfds = read_fds
        .iter()
        .chain(write_fds.iter())
        .chain(error_fds.iter())
        .copied()
        .max()
        .map(|m| m + 1)
        .unwrap_or(0);

    // SAFETY: libc fd_set operations are standard POSIX.
    unsafe {
        let mut readfds: libc::fd_set = std::mem::zeroed();
        let mut writefds: libc::fd_set = std::mem::zeroed();
        let mut errorfds: libc::fd_set = std::mem::zeroed();

        libc::FD_ZERO(&mut readfds);
        libc::FD_ZERO(&mut writefds);
        libc::FD_ZERO(&mut errorfds);

        for &fd in &read_fds {
            libc::FD_SET(fd, &mut readfds);
        }
        for &fd in &write_fds {
            libc::FD_SET(fd, &mut writefds);
        }
        for &fd in &error_fds {
            libc::FD_SET(fd, &mut errorfds);
        }

        let mut tv_storage = timeout.unwrap_or(libc::timeval {
            tv_sec: 0,
            tv_usec: 0,
        });
        let timeout_ptr = if timeout.is_some() {
            &mut tv_storage as *mut libc::timeval
        } else {
            std::ptr::null_mut()
        };

        let ret = libc::select(
            nfds,
            if read_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut readfds
            },
            if write_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut writefds
            },
            if error_fds.is_empty() {
                std::ptr::null_mut()
            } else {
                &mut errorfds
            },
            timeout_ptr,
        );

        if ret < 0 {
            let err = std::io::Error::last_os_error();
            return Err(MonorubyErr::errno_with_msg(
                &globals.store,
                &err,
                "select(2)",
            ));
        }

        if ret == 0 {
            return Ok(Value::nil());
        }

        // Build result arrays
        let mut ready_read = vec![];
        for (i, &fd) in read_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &readfds) {
                ready_read.push(read_ios[i]);
            }
        }
        let mut ready_write = vec![];
        for (i, &fd) in write_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &writefds) {
                ready_write.push(write_ios[i]);
            }
        }
        let mut ready_error = vec![];
        for (i, &fd) in error_fds.iter().enumerate() {
            if libc::FD_ISSET(fd, &errorfds) {
                ready_error.push(error_ios[i]);
            }
        }

        let r = Value::array_from_vec(ready_read);
        let w = Value::array_from_vec(ready_write);
        let e = Value::array_from_vec(ready_error);
        Ok(Value::array_from_vec(vec![r, w, e]))
    }
}

///
/// ### IO#set_encoding
///
/// - set_encoding(ext_enc) -> self
/// - set_encoding(ext_enc, int_enc) -> self
/// - set_encoding(ext_enc, int_enc, opt) -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/set_encoding.html]
///
/// Stub: validates encoding arguments but does not actually change encoding.
#[monoruby_builtin]
fn set_encoding(
    _vm: &mut Executor,
    _globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    use crate::Encoding;
    let arg0 = lfp.arg(0);
    // Validate the encoding name if it's a string
    if let Some(s) = arg0.is_str() {
        // Handle "enc1:enc2" format (e.g. "UTF-8:UTF-8")
        let ext = s.split(':').next().unwrap_or(s);
        Encoding::try_from_str(ext)?;
    }
    // Validate optional second argument
    if let Some(arg1) = lfp.try_arg(1) {
        if let Some(s) = arg1.is_str() {
            Encoding::try_from_str(s)?;
        }
    }
    Ok(lfp.self_val())
}

///
/// ### IO#external_encoding
///
/// - external_encoding -> Encoding
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/external_encoding.html]
///
/// Stub: always returns Encoding::UTF_8.
#[monoruby_builtin]
fn external_encoding(
    _vm: &mut Executor,
    globals: &mut Globals,
    _lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let enc_class = globals
        .get_constant_noautoload(OBJECT_CLASS, IdentId::ENCODING)
        .unwrap()
        .as_class_id();
    let utf8 = globals
        .get_constant_noautoload(enc_class, IdentId::UTF_8)
        .unwrap();
    Ok(utf8)
}

///

///
/// ### IO.copy_stream
///
/// - IO.copy_stream(src, dst) -> Integer
/// - IO.copy_stream(src, dst, copy_length) -> Integer
/// - IO.copy_stream(src, dst, copy_length, src_offset) -> Integer
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/s/copy_stream.html]
#[monoruby_builtin]
fn io_copy_stream(
    vm: &mut Executor,
    globals: &mut Globals,
    lfp: Lfp,
    _: BytecodePtr,
) -> Result<Value> {
    let copy_length: Option<i64> = lfp
        .try_arg(2)
        .and_then(|v| if v.is_nil() { None } else { Some(v) })
        .map(|v| v.coerce_to_int_i64(vm, globals))
        .transpose()?;
    let src_offset: Option<i64> = lfp
        .try_arg(3)
        .and_then(|v| if v.is_nil() { None } else { Some(v) })
        .map(|v| v.coerce_to_int_i64(vm, globals))
        .transpose()?;

    // Helper: duplicate an fd from an IO Value for use as a std::fs::File.
    fn dup_fd_from_io(io_val: Value, globals: &Globals) -> Result<File> {
        let mut v = io_val;
        if v.try_rvalue().map_or(true, |rv| rv.ty() != ObjTy::IO) {
            return Err(MonorubyErr::typeerr(format!(
                "no implicit conversion of {} into IO",
                v.get_real_class_name(&globals.store)
            )));
        }
        let fd = v.as_io_inner_mut().fileno()?;
        // SAFETY: dup the fd so that the original IO still owns its fd.
        let new_fd = unsafe { libc::dup(fd) };
        if new_fd == -1 {
            return Err(MonorubyErr::runtimeerr("dup failed"));
        }
        // SAFETY: new_fd is a valid, newly duplicated file descriptor.
        Ok(unsafe { File::from_raw_fd(new_fd) })
    }

    // Open source
    let src_val = lfp.arg(0);
    let mut src_owned: File;
    let src_is_path = src_val.try_bytes().is_some();
    if src_is_path {
        let path = src_val.coerce_to_string(vm, globals)?;
        src_owned = File::open(&path).map_err(|e| {
            MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path)
        })?;
        if let Some(offset) = src_offset {
            use std::io::Seek;
            src_owned
                .seek(std::io::SeekFrom::Start(offset as u64))
                .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
        }
    } else {
        src_owned = dup_fd_from_io(src_val, globals)?;
        if let Some(offset) = src_offset {
            use std::io::Seek;
            src_owned
                .seek(std::io::SeekFrom::Start(offset as u64))
                .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?;
        }
    }

    // Open destination
    let dst_val = lfp.arg(1);
    let dst_is_path = dst_val.try_bytes().is_some();
    let mut dst_owned: File;
    if dst_is_path {
        let path = dst_val.coerce_to_string(vm, globals)?;
        dst_owned = File::create(&path).map_err(|e| {
            MonorubyErr::errno_with_path(&globals.store, &e, "rb_sysopen", &path)
        })?;
    } else {
        dst_owned = dup_fd_from_io(dst_val, globals)?;
    }

    // Copy
    use std::io::Read;
    let copied = if let Some(length) = copy_length {
        let mut limited = (&mut src_owned).take(length as u64);
        std::io::copy(&mut limited, &mut dst_owned)
            .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?
    } else {
        std::io::copy(&mut src_owned, &mut dst_owned)
            .map_err(|e| MonorubyErr::runtimeerr(e.to_string()))?
    };

    Ok(Value::integer(copied as i64))
}

#[cfg(test)]
mod tests {
    use crate::tests::*;

    #[test]
    fn io_test() {
        run_test_no_result_check(
            r#"
            $stdout << "a"
            $stdout << 5
            $stdout.puts 100
            $stdout.print 100
            $stdout.printf("%b%10.5f", 100, 3.14)
            File.open("/dev/null", "w") << 5
            File.open("/dev/null", "w+") << 5
            $stdin.sync
            $stdin.sync = true
            [$stdin.isatty, $stdout.isatty, $stderr.isatty]
        "#,
        );
    }

    #[test]
    fn read() {
        run_test_error(r#"$stdout.read"#);
        run_test_error(r#"$stderr.read"#);
        run_test_error(r#"File.open("")"#);
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read
        "#,
        );
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read(17)
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null")
            f.read
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null", "r")
            f.read(0)
        "#,
        );
        run_test(
            r#"
            f = File.open("/dev/null", "r+")
            f.read(nil)
        "#,
        );
        run_test(
            r#"
            f = File.open("Cargo.toml", "r")
            f.readline
        "#,
        );
    }

    #[test]
    fn io_close() {
        run_test(
            r#"
            r, w = IO.pipe
            w.close
            [w.closed?, r.closed?]
        "#,
        );
        run_test(
            r#"
            r, w = IO.pipe
            r.close
            w.close
            [r.closed?, w.closed?]
        "#,
        );
        run_test(
            r#"
            IO.read("Cargo.toml").is_a?(String)
        "#,
        );
    }

    #[test]
    fn io_pipe() {
        run_test(
            r#"
            r, w = IO.pipe
            w << "hello"
            w.close
            r.read
        "#,
        );
    }

    #[test]
    fn each_line() {
        run_test(
            r##"
        f = File.open("a.rb");
        res = []
        f.each_line do |line|
            res << line
        end
        res
        "##,
        );
    }

    #[test]
    fn popen_close() {
        // Verify IO.popen + close doesn't deadlock when child writes to stdout.
        run_test_no_result_check(
            r#"
            io = IO.popen("echo hello")
            s = io.read
            io.close
            s
            "#,
        );
    }

    #[test]
    fn popen_close_without_read() {
        // Closing without reading should not hang (child gets SIGPIPE).
        run_test_no_result_check(
            r#"
            io = IO.popen("echo hello")
            io.close
            "#,
        );
    }

    #[test]
    fn popen_block() {
        // IO.popen with block should auto-close.
        run_test_no_result_check(
            r#"
            result = IO.popen("echo hello") {|io| io.read }
            result
            "#,
        );
    }

    #[test]
    fn stringio_write() {
        run_test(
            r#"
            require "stringio"
            sio = StringIO.new
            sio.write("hello", "", " world")
            sio.string
        "#,
        );
        run_test(
            r#"
            require "stringio"
            sio = StringIO.new
            n = sio.write("ab", "cd", "ef")
            [sio.string, n]
        "#,
        );
        run_test(
            r#"
            require "stringio"
            sio = StringIO.new
            n = sio.write("", "hello", "")
            [sio.string, n]
        "#,
        );
    }

    #[test]
    fn io_select() {
        // select with readable pipe
        run_test(
            r#"
            r, w = IO.pipe
            w.write("hello")
            result = IO.select([r], nil, nil, 0)
            result[0].size
            "#,
        );
        // select with timeout (no data available)
        run_test(
            r#"
            r, w = IO.pipe
            result = IO.select([r], nil, nil, 0)
            result.nil?
            "#,
        );
        // select with writable pipe
        run_test(
            r#"
            r, w = IO.pipe
            result = IO.select(nil, [w], nil, 0)
            result[1].size
            "#,
        );
    }

    #[test]
    fn io_fileno() {
        run_test("$stdin.fileno == 0");
        run_test("$stdout.fileno == 1");
        run_test("$stderr.fileno == 2");
        run_test(
            r#"
            r, w = IO.pipe
            [r.fileno.is_a?(Integer), w.fileno.is_a?(Integer)]
            "#,
        );
    }

    #[test]
    fn io_write_variadic() {
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            n = w.write("hello", " ", "world")
            w.close
            [r.read, n]
            "#,
        );
    }

    #[test]
    fn io_syswrite() {
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            n = w.syswrite("hello")
            w.close
            [r.read, n]
            "#,
        );
    }

    #[test]
    fn puts_delegates_to_write() {
        // IO#puts should call the Ruby-level write method, not bypass it.
        run_test(
            r#"
            r, w = IO.pipe
            $test_written = []
            def w.write(s)
              $test_written << s
              super(s)
            end
            w.puts("hello")
            w.close
            [$test_written.join, r.read]
            "#,
        );
    }

    #[test]
    fn print_delegates_to_write() {
        // IO#print should call the Ruby-level write method, not bypass it.
        run_test(
            r#"
            r, w = IO.pipe
            $test_written = []
            def w.write(s)
              $test_written << s
              super(s)
            end
            w.print("hello", "world")
            w.close
            [$test_written.join, r.read]
            "#,
        );
    }

    #[test]
    fn io_sysopen() {
        run_test_no_result_check(
            r#"
            fd = IO.sysopen("Cargo.toml", "r")
            raise "should be integer" unless fd.is_a?(Integer)
            raise "should be positive" unless fd > 0
            "#,
        );
    }

    #[test]
    fn popen_rw_mode() {
        run_test(
            r#"
            IO.popen("cat", "r+") do |io|
              io.write("hello")
              io.close_write
              io.read
            end
            "#,
        );
    }

    #[test]
    fn popen_write_mode() {
        run_test_no_result_check(
            r#"
            IO.popen("cat > /dev/null", "w") do |io|
              io.write("hello")
            end
            "#,
        );
    }

    #[test]
    fn close_read() {
        run_test(
            r#"
            IO.popen("echo hello", "r") do |io|
              data = io.read
              io.close_read
              data
            end
            "#,
        );
    }

    #[test]
    fn close_write() {
        run_test(
            r#"
            IO.popen("cat", "r+") do |io|
              io.write("hello")
              io.close_write
              io.read
            end
            "#,
        );
    }

    #[test]
    fn external_encoding() {
        run_test_no_result_check(
            r#"
            enc = $stdout.external_encoding
            raise "should be Encoding" unless enc.is_a?(Encoding)
            enc.name
            "#,
        );
    }

    #[test]
    fn internal_encoding() {
        run_test_no_result_check(
            r#"
            $stdout.internal_encoding
            "#,
        );
    }

    #[test]
    fn set_encoding_test() {
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            w.set_encoding("UTF-8")
            w.close
            r.close
            "#,
        );
        run_test_no_result_check(
            r#"
            r, w = IO.pipe
            w.set_encoding("UTF-8", "UTF-8")
            w.close
            r.close
            "#,
        );
    }

    #[test]
    fn io_foreach_test() {
        run_test(
            r#"
            path = "/tmp/monoruby_test_foreach_#{Process.pid}"
            File.write(path, "hello\nworld\nfoo\n")
            res = []
            IO.foreach(path) { |line| res << line }
            File.delete(path)
            res
            "#,
        );
    }

    #[test]
    fn file_foreach_test() {
        run_test(
            r#"
            path = "/tmp/monoruby_test_file_foreach_#{Process.pid}"
            File.write(path, "aaa\nbbb\n")
            res = []
            File.foreach(path) { |line| res << line }
            File.delete(path)
            res
            "#,
        );
    }

    #[test]
    fn io_new_with_fd() {
        run_test_no_result_check(
            r#"
            path = "/tmp/monoruby_test_ionew_#{Process.pid}"
            File.write(path, "io new fd")
            fd = IO.sysopen(path)
            io = IO.new(fd)
            content = io.read
            io.close
            raise "expected 'io new fd'" unless content == "io new fd"
            File.delete(path)
            "#,
        );
    }

    #[test]
    fn io_new_invalid_fd() {
        run_test_error(r#"IO.new(-1)"#);
        run_test_error(r#"IO.new(9999)"#);
    }

    #[test]
    fn io_copy_stream() {
        run_test(
            r#"
            path_src = "/tmp/monoruby_test_cs_src_#{Process.pid}"
            path_dst = "/tmp/monoruby_test_cs_dst_#{Process.pid}"
            File.write(path_src, "hello world")
            n = IO.copy_stream(path_src, path_dst)
            raise "expected 11 but got #{n}" unless n == 11
            raise unless File.read(path_dst) == "hello world"
            n = IO.copy_stream(path_src, path_dst, 5)
            raise "expected 5 but got #{n}" unless n == 5
            raise unless File.read(path_dst) == "hello"
            n = IO.copy_stream(path_src, path_dst, 5, 6)
            raise "expected 5 but got #{n}" unless n == 5
            raise unless File.read(path_dst) == "world"
            File.delete(path_src)
            File.delete(path_dst)
            "#,
        );
    }
}
