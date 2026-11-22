//! Four CRuby incompatibilities.
//!
//! - **"possibly useless use of … in void context"** (#2115) under `-w`,
//!   for every kind prism reports (a variable, a literal, an operator,
//!   `self`, `nil`, …), in a script, a `-e` script and an eval — where
//!   the last statement is the eval's value and is left out.
//! - **Autoload of a file that is being loaded** (#2116). Reading a
//!   constant whose autoload names the file now running does not
//!   `require` it again: the constant is undefined for the moment and
//!   the registration stays, so the file can still define it. No
//!   "circular require" and no "Expected … to define …" warning.
//! - **An IO that was allocated and never initialized** (#2108) raises
//!   `IOError: uninitialized stream`, not "closed stream", and
//!   `closed?` raises too rather than answering `true`.
//!
//! `ENV` reading the live environment (#2109) is tested next to its
//! implementation, in `builtins/hash.rs`.

use monoruby::tests::*;

fn monoruby_stderr(args: &[&str]) -> String {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .arg("--disable=gems")
        .args(args)
        .output()
        .expect("failed to spawn monoruby");
    assert!(out.status.success(), "{out:?}");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

const VOID: &str = "possibly useless use of";

#[test]
fn void_context_warnings_in_a_script() {
    let path = std::env::temp_dir().join(format!("i2115_void_{}.rb", std::process::id()));
    std::fs::write(
        &path,
        "def m(a)\n  a\n  @iv\n  $gv\n  Foo\n  1 + 2\n  :sym\n  \"str\"\n  self\n  nil\n  \
         a.b\n  a == 1\n  __FILE__\n  1..2\n  1\nend\n",
    )
    .unwrap();
    let file = path.to_str().unwrap();
    // As CRuby 4.0 prints them. A method call (`a.b`) may have an
    // effect, and the last statement is the method's value.
    let expected: Vec<String> = [
        (2, "a variable"),
        (3, "a variable"),
        (4, "a variable"),
        (5, "a constant"),
        (6, "+"),
        (7, "a literal"),
        (8, "a literal"),
        (9, "self"),
        (10, "nil"),
        (12, "=="),
        (13, "a literal"),
        (14, ".."),
    ]
    .iter()
    .map(|(line, what)| format!("{file}:{line}: warning: {VOID} {what} in void context"))
    .collect();
    let err = monoruby_stderr(&["-w", file]);
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
    // `-c` reports them too, without running anything.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .args(["--disable=gems", "-wc", file])
        .output()
        .unwrap();
    assert_eq!(String::from_utf8_lossy(&out.stdout), "Syntax OK\n");
    let err = String::from_utf8_lossy(&out.stderr).into_owned();
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
    // Verbose only: nothing at the default level or under `-W1`.
    assert_eq!(monoruby_stderr(&[file]), "");
    assert_eq!(monoruby_stderr(&["-W1", file]), "");
    let _ = std::fs::remove_file(&path);
}

#[test]
fn void_context_warnings_in_a_dash_e_script() {
    // A `-e` script is a script, not an eval: its last statement is
    // void as well.
    let err = monoruby_stderr(&["-w", "-e", "x = 1; x; 1; x == 2; nil"]);
    let expected: Vec<String> = ["a variable", "a literal", "==", "nil"]
        .iter()
        .map(|what| format!("-e:1: warning: {VOID} {what} in void context"))
        .collect();
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
    assert_eq!(monoruby_stderr(&["-e", "x = 1; x; 1"]), "");
}

#[test]
fn void_context_warnings_in_an_eval() {
    // The last statement of an eval is its value.
    let err = monoruby_stderr(&[
        "-w",
        "-e",
        r#"eval("y = 1; y; 4; 5"); instance_eval("self; 8"); Object.class_eval("10; 11")
           binding.eval("5; 6"); eval("7; 8", nil, "ev.rb", 30); eval("9")"#,
    ]);
    let expected = [
        format!("(eval at -e:1):1: warning: {VOID} a variable in void context"),
        format!("(eval at -e:1):1: warning: {VOID} a literal in void context"),
        format!("(eval at -e:1):1: warning: {VOID} self in void context"),
        format!("(eval at -e:1):1: warning: {VOID} a literal in void context"),
        format!("(eval at -e:2):1: warning: {VOID} a literal in void context"),
        format!("ev.rb:30: warning: {VOID} a literal in void context"),
    ];
    assert_eq!(err.lines().collect::<Vec<_>>(), expected, "stderr: {err}");
}

const CAP: &str = r##"
  def cap
    saved = $stderr
    verbose = $VERBOSE
    buf = +""
    io = Object.new
    io.define_singleton_method(:write) { |*a| a.each { |x| buf << x.to_s } }
    $stderr = io
    $VERBOSE = true
    yield
    buf.lines.map { |l| l.sub(/\A.*?warning: /, "") }
  ensure
    $stderr = saved
    $VERBOSE = verbose
  end
"##;

#[test]
fn autoload_of_the_file_being_loaded_is_left_alone() {
    run_test_once(&format!(
        "{CAP}{}",
        r##"
        require "tmpdir"
        res = []
        $res = res
        Dir.mktmpdir do |dir|
          # One file is the autoload target of three constants; it reads
          # the second and the third while it runs for the first.
          File.write("#{dir}/i2116_v.rb", <<~RUBY)
            module I2116
              class BV; end
              $res << [:in_file, constants.sort]
              begin; W; rescue NameError => e; $res << e.message; end
              class V < BV; end
              $res << V.superclass.name
            end
          RUBY
          $LOAD_PATH.unshift dir
          module I2116
            autoload :BV, "i2116_v"
            autoload :V, "i2116_v"
            autoload :W, "i2116_v"
          end
          res << cap { res << I2116::BV.name << I2116::V.name }
          res << $LOADED_FEATURES.count { |f| f.end_with?("/i2116_v.rb") }
          # The third constant is still registered: reading it now is
          # what finds the file did not define it.
          res << I2116.autoload?(:W)
          res << cap { res << (begin; I2116::W; rescue NameError => e; e.message; end) }
          res << I2116.autoload?(:W) << I2116.constants.sort

          # The same through an absolute path, required directly.
          File.write("#{dir}/i2116_abs.rb", <<~RUBY)
            module I2116Abs
              class A; end
              class B < A; end
            end
          RUBY
          module I2116Abs; end
          I2116Abs.autoload :A, "#{dir}/i2116_abs.rb"
          I2116Abs.autoload :B, "#{dir}/i2116_abs.rb"
          res << cap { res << I2116Abs::B.superclass.name }
          $LOAD_PATH.delete(dir)
        end
        res
        "##
    ));
}

/// The gem that showed it: `prism/visitor` is the autoload target of
/// both `Prism::BasicVisitor` and `Prism::Visitor`.
#[test]
fn requiring_ripper_under_w_is_silent() {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .args([
            "-w",
            "-e",
            r#"require "ripper"; p [Prism::Visitor.superclass, Ripper.sexp("1").class]"#,
        ])
        .output()
        .unwrap();
    assert!(out.status.success(), "{out:?}");
    assert_eq!(String::from_utf8_lossy(&out.stderr), "");
    assert_eq!(
        String::from_utf8_lossy(&out.stdout),
        "[Prism::BasicVisitor, Array]\n"
    );
}

const PROBE: &str = r##"
  def t
    r = yield
    r.is_a?(String) ? r.gsub(/0x\h+/, "0xX") : r
  rescue Exception => e
    "#{e.class}: #{e.message}"
  end
"##;

#[test]
fn uninitialized_io_raises_uninitialized_stream() {
    run_test_with_prelude(
        r##"
        res = []
        res << t { IO.allocate.fileno } << t { IO.allocate.closed? }
        res << t { IO.allocate.close } << t { IO.allocate.to_i }
        res << t { IO.allocate.inspect } << t { File.allocate.inspect }
        res << t { IO.allocate.to_io.class } << t { IO.allocate.frozen? }
        # A subclass whose `initialize` never reaches `IO#initialize`.
        sub = Class.new(IO) { def initialize; end }
        res << t { sub.new.closed? } << t { sub.new.fileno }
        # Every way in: reading, writing, positioning, modes, encodings.
        calls = {
          read: [], readline: [], readlines: [], readpartial: [1], gets: [],
          getc: [], getbyte: [], readchar: [], readbyte: [], ungetc: ["a"],
          ungetbyte: [1], each_line: [], each_byte: [], each_char: [],
          sysread: [1], read_nonblock: [1], pread: [1, 0],
          write: ["a"], print: ["a"], puts: ["a"], putc: ["a"], "<<": ["a"],
          syswrite: ["a"], write_nonblock: ["a"], pwrite: ["a", 0],
          flush: [], fsync: [], fdatasync: [], sync: [], "sync=": [true],
          eof?: [], eof: [], pos: [], tell: [], "pos=": [0], seek: [0],
          rewind: [], sysseek: [0], lineno: [], "lineno=": [1],
          binmode: [], binmode?: [], tty?: [], isatty: [], stat: [],
          close_read: [], close_write: [], close_on_exec?: [],
          "close_on_exec=": [true], autoclose?: [], "autoclose=": [true],
          fcntl: [1], ioctl: [1], pid: [], set_encoding: ["utf-8"],
          set_encoding_by_bom: [], timeout: [], "timeout=": [1],
        }
        res << calls.map { |m, args|
          r = t { o = IO.allocate; o.send(m, *args) { }; :returned }
          r == "IOError: uninitialized stream" ? nil : [m, r]
        }.compact
        fcalls = {
          atime: [], mtime: [], ctime: [], size: [], lstat: [], chmod: [0644],
          chown: [nil, nil], flock: [File::LOCK_UN], truncate: [0],
        }
        res << fcalls.map { |m, args|
          r = t { File.allocate.send(m, *args); :returned }
          r == "IOError: uninitialized stream" ? nil : [m, r]
        }.compact
        res << t { File.allocate.path } << t { File.allocate.to_path }
        # Taken as an argument.
        res << t { IO.select([IO.allocate], nil, nil, 0) }
        res << t { IO.copy_stream(IO.allocate, IO.allocate) }
        res << t { STDOUT.dup.reopen(IO.allocate) }
        res << t { IO.allocate.reopen(STDIN).class }
        res << t { IO.allocate.dup } << t { IO.allocate.clone }
        # Initializing afterwards makes it an ordinary stream; a stream
        # that was open and is closed says "closed stream".
        res << t {
          o = IO.allocate
          o.send(:initialize, 1, "w")
          o.autoclose = false
          [o.fileno, o.closed?]
        }
        res << t { r, w = IO.pipe; w.close; r.close; [t { r.fileno }, t { r.read }, r.closed?] }
        res << t { f = File.open(File::NULL); f.close; [t { f.lstat }, t { f.mtime }, t { f.chmod(0644) }] }
        res
        "##,
        PROBE,
    );
}
