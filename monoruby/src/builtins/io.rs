use super::*;

//
// IO class
//

pub(super) fn init(globals: &mut Globals) {
    globals.define_builtin_class_under_obj("IO", IO_CLASS);
    //globals.define_builtin_singleton_func(IO_CLASS, "new", now, 0);
    globals.define_builtin_func(IO_CLASS, "<<", shl, 1);
    globals.define_builtin_funcs(IO_CLASS, "isatty", &["tty?"], isatty, 0);
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
    if let Some(b) = lfp.arg(0).try_bytes() {
        lfp.self_val().as_io_inner_mut().write(b.as_bytes())?;
    } else {
        let s = vm.to_s(globals, lfp.arg(0))?;
        lfp.self_val().as_io_inner_mut().write(s.as_bytes())?;
    };
    globals.flush_stdout();
    Ok(lfp.self_val())
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
fn read(_: &mut Executor, _: &mut Globals, lfp: Lfp) -> Result<Value> {
    let length = match lfp.try_arg(0) {
        Some(v) => {
            if v.is_nil() {
                None
            } else {
                let length = v.expect_integer()?;
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
mod test {
    use super::tests::*;

    #[test]
    fn test() {
        run_test_no_result_check(
            r#"
            $stdout << "a"
            $stdout << 5
            File.open("/dev/null", "w") << 5
            File.open("/dev/null", "w+") << 5
            $stdin.sync
            $stdin.sync = true
        "#,
        );
        run_test(
            r#"
            [$stdin.isatty, $stdout.isatty, $stderr.isatty]
        "#,
        )
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
