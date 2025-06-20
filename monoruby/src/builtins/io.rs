use super::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS, ObjTy::IO);
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_func_with(IO_CLASS, "puts", puts, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "print", print, 0, 0, true);
    globals.define_builtin_func_with(IO_CLASS, "printf", printf, 1, 1, true);
    globals.define_builtin_func(IO_CLASS, "flush", flush, 0);
    globals.define_builtin_func(IO_CLASS, "gets", gets, 0);
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
    globals.define_builtin_func(IO_CLASS, "closed?", closed_, 0);
    globals.define_builtin_func(IO_CLASS, "sync", sync, 0);
    globals.define_builtin_func(IO_CLASS, "sync=", assign_sync, 1);
    globals.define_builtin_func_with(IO_CLASS, "read", read, 0, 1, false);
    globals.define_builtin_func(IO_CLASS, "readline", readline, 0);
    globals.define_builtin_funcs(IO_CLASS, "each", &["each_line"], each_line, 0);

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

///
/// ### IO#<<
///
/// - self << object -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/=3c=3c.html]
#[monoruby_builtin]
fn shl(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn puts(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    fn decompose(collector: &mut Vec<Value>, val: Value) {
        match val.try_array_ty() {
            Some(ary) => {
                ary.iter().for_each(|v| decompose(collector, *v));
            }
            None => collector.push(val),
        }
    }
    let mut collector = Vec::new();
    for v in lfp.arg(0).as_array().iter().cloned() {
        decompose(&mut collector, v);
    }

    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    for v in collector {
        io_writeline(globals, io, v)?;
    }
    io.flush()?;
    Ok(Value::nil())
}

///
/// ### IO#print
///
/// - print(*arg) -> nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/print.html]
#[monoruby_builtin]
fn print(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    for v in lfp.arg(0).as_array().iter().cloned() {
        io_write(globals, io, v)?;
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
fn printf(_vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    let format_str = lfp.arg(0).expect_string(globals)?;
    let args = lfp.arg(1).as_array();

    let buf = globals.format_by_args(&format_str, &args)?;
    io.write(buf.as_bytes())?;

    Ok(Value::nil())
}

fn io_writeline(globals: &Globals, io: &mut IoInner, v: Value) -> Result<()> {
    if let Some(s) = v.is_rstring() {
        io.write(&s)?;
        if s.last() != Some(&b'\n') {
            io.write(b"\n")?;
        }
    } else {
        let v = v.to_s(globals).into_bytes();
        io.write(&v)?;
        if v.last() != Some(&b'\n') {
            io.write(b"\n")?;
        }
    }
    Ok(())
}

fn io_write(globals: &Globals, io: &mut IoInner, v: Value) -> Result<()> {
    if let Some(s) = v.is_rstring() {
        io.write(&s)
    } else {
        let v = v.to_s(globals).into_bytes();
        io.write(&v)
    }
}

///
/// ### IO#flush
///
/// - flush -> self
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/flush.html]
#[monoruby_builtin]
fn flush(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn gets(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn isatty(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner_mut().isatty()))
}

///
/// ### IO#closed?
///
/// - closed? -> bool
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/closed=3f.html]
#[monoruby_builtin]
fn closed_(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(lfp.self_val().as_io_inner().is_closed()))
}

#[monoruby_builtin]
fn sync(_vm: &mut Executor, _globals: &mut Globals, _lfp: Lfp) -> Result<Value> {
    Ok(Value::bool(false))
}

#[monoruby_builtin]
fn assign_sync(_vm: &mut Executor, _globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    Ok(lfp.arg(0))
}

///
/// ### IO#read
///
/// - read(length = nil, [NOT SUPPRTED] outbuf = "") -> String | nil
///
/// [https://docs.ruby-lang.org/ja/latest/method/IO/i/read.html
#[monoruby_builtin]
fn read(_: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let length = match lfp.try_arg(0) {
        Some(v) => {
            if v.is_nil() {
                None
            } else {
                let length = v.expect_integer(globals)?;
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
fn readline(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
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
fn each_line(vm: &mut Executor, globals: &mut Globals, lfp: Lfp) -> Result<Value> {
    let mut self_ = lfp.self_val();
    let io = self_.as_io_inner_mut();
    if let Some(bh) = lfp.block() {
        let p = vm.get_block_data(globals, bh)?;
        while let Some(s) = io.read_line()? {
            vm.invoke_block(globals, &p, &[Value::string(s)])?;
        }
    } else {
        unimplemented!()
    };
    Ok(lfp.self_val())
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
        run_test_once(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read
        "#,
        );
        run_test_once(
            r#"
            f = File.open("Cargo.toml", "r")
            f.read(17)
        "#,
        );
        run_test_once(
            r#"
            f = File.open("/dev/null")
            f.read
        "#,
        );
        run_test_once(
            r#"
            f = File.open("/dev/null", "r")
            f.read(0)
        "#,
        );
        run_test_once(
            r#"
            f = File.open("/dev/null", "r+")
            f.read(nil)
        "#,
        );
        run_test_once(
            r#"
            f = File.open("Cargo.toml", "r")
            f.readline
        "#,
        );
    }

    #[test]
    fn each_line() {
        run_test_once(
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
}
