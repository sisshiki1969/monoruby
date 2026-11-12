//! The standard streams seek like any other descriptor (#1867), STDIN
//! takes `ungetc` / `ungetbyte` pushback (#1877), and `IO.copy_stream`
//! between two descriptors copies on the fds with CRuby's syscalls and
//! error names (#1868, #1878).
//!
//! Each case runs in a process of its own, on monoruby and on the host
//! CRuby, with stdin / stdout on a regular file or on a pipe, and
//! compares what the two printed (on stderr, so that stdout stays free to
//! be the stream under test) and what they left in the stdout file.

use std::io::Write;
use std::process::{Command, Stdio};

enum Stdin {
    File(&'static str),
    Pipe(&'static str),
}

fn temp(name: &str, ruby: bool) -> std::path::PathBuf {
    let tag = if ruby { "ruby" } else { "monoruby" };
    let path = std::env::temp_dir().join(format!(
        "monoruby_std_stream_seek_{name}_{tag}_{}.txt",
        std::process::id()
    ));
    let _ = std::fs::remove_file(&path);
    path
}

/// Run `script` with the given stdin and stdout on a fresh file; return
/// (stderr, the stdout file's contents).
fn run(exe: &std::path::Path, ruby: bool, name: &str, script: &str, stdin: &Stdin) -> (String, Vec<u8>) {
    let out_path = temp(&format!("{name}_out"), ruby);
    let out = std::fs::File::create(&out_path).unwrap();
    let mut cmd = Command::new(exe);
    cmd.env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .arg("--disable=gems")
        .arg("-e")
        .arg(script)
        .stdout(Stdio::from(out))
        .stderr(Stdio::piped());
    let in_path = temp(&format!("{name}_in"), ruby);
    let child = match stdin {
        Stdin::File(data) => {
            std::fs::write(&in_path, data).unwrap();
            cmd.stdin(Stdio::from(std::fs::File::open(&in_path).unwrap()));
            cmd.spawn().unwrap()
        }
        Stdin::Pipe(data) => {
            cmd.stdin(Stdio::piped());
            let mut child = cmd.spawn().unwrap();
            let mut w = child.stdin.take().unwrap();
            w.write_all(data.as_bytes()).unwrap();
            drop(w);
            child
        }
    };
    let res = child.wait_with_output().unwrap();
    assert!(
        res.status.success(),
        "{} exited with {:?}\nstderr: {}",
        exe.display(),
        res.status,
        String::from_utf8_lossy(&res.stderr)
    );
    let written = std::fs::read(&out_path).unwrap();
    let _ = std::fs::remove_file(&out_path);
    let _ = std::fs::remove_file(&in_path);
    (String::from_utf8_lossy(&res.stderr).into_owned(), written)
}

#[track_caller]
fn assert_same_as_cruby(name: &str, script: &str, stdin: Stdin) {
    let ruby = monoruby::tests::ruby_path();
    let expected = run(std::path::Path::new(&ruby), true, name, script, &stdin);
    let actual = run(
        std::path::Path::new(env!("CARGO_BIN_EXE_monoruby")),
        false,
        name,
        script,
        &stdin,
    );
    assert_eq!(actual, expected);
}

const STD_SEEK: &str = r##"
def t; yield; rescue => e; "#{e.class}: #{e.message}"; end
r = []
r << t { STDIN.tell }
r << t { STDIN.read(2) }
r << t { STDIN.pos }
r << t { STDIN.ungetc("Z"); STDIN.read(3) }
r << t { STDIN.eof? }
r << t { STDIN.gets }
r << t { STDIN.ungetbyte(65); STDIN.getbyte }
r << t { STDIN.rewind; STDIN.read }
r << t { STDIN.seek(1); STDIN.read(3) }
r << t { STDIN.ungetc("xy"); STDIN.pos }
r << t { $stdin.sysseek(0, :CUR) }
r << t { STDOUT.tell }
STDOUT.write "abcdef"
r << t { STDOUT.pos }
r << t { STDOUT.rewind; STDOUT.write "X"; STDOUT.seek(0, :END) }
r << t { STDOUT.seek(-2, :CUR); STDOUT.write "YZ"; STDOUT.pos }
STDOUT.flush
r << t { STDOUT.sysseek(1) }
STDERR.puts r.inspect
"##;

#[test]
fn std_streams_seek_on_files() {
    assert_same_as_cruby("files", STD_SEEK, Stdin::File("line1\nline2\n"));
}

#[test]
fn std_streams_on_a_pipe() {
    assert_same_as_cruby("pipe", STD_SEEK, Stdin::Pipe("line1\nline2\n"));
}

/// copy_stream from the standard input (pushback first) to the standard
/// output, which is then rewound.
#[test]
fn copy_stream_between_std_streams() {
    let script = r##"
    STDOUT.write "head:"
    STDIN.ungetc("U")
    n = IO.copy_stream(STDIN, STDOUT, 8)
    STDOUT.write ":#{n}:"
    m = IO.copy_stream(STDIN, STDOUT)
    STDOUT.write ":#{m}"
    STDERR.puts [n, m, STDOUT.pos, STDOUT.binmode?, STDOUT.external_encoding].inspect
    "##;
    assert_same_as_cruby("cs_file", script, Stdin::File("line1\nline2\n"));
    assert_same_as_cruby("cs_pipe", script, Stdin::Pipe("line1\nline2\n"));
}

/// The fd copy itself: buffered data first, offsets, lengths, an
/// `O_APPEND` destination, pipes on both sides with threads, a trap
/// during a blocking copy, and the syscall each failure is named after.
#[test]
fn copy_stream_on_descriptors() {
    let script = r##"
    def t; yield; rescue => e; "#{e.class}: #{e.message}"; end
    require "tmpdir"
    r = []
    Dir.mktmpdir do |d|
      src = File.join(d, "src"); dst = File.join(d, "dst")
      File.write(src, (0..9999).map { |i| "line #{i}\n" }.join)
      w = File.open(dst, "w"); r << IO.copy_stream(src, w)
      r << [w.binmode?, w.external_encoding, w.pos]; w.close; r << File.size(dst)
      f = File.open(src); f.gets; f.ungetc("ZZ"); w = File.open(dst, "w")
      r << IO.copy_stream(f, w, 20) << f.pos; w.close; r << File.read(dst)
      f = File.open(src); f.read(3); w = File.open(dst, "w")
      r << IO.copy_stream(f, w, 10, 100) << f.pos; w.close; r << File.read(dst)
      r << IO.copy_stream(src, dst, 5, 7) << File.read(dst)
      w = File.open(dst, "a"); r << IO.copy_stream(src, w, 4); w.close; r << File.read(dst)
      w = File.open(dst, "w"); w.write "pre-"; r << IO.copy_stream(src, w, 3); w.close; r << File.read(dst)
      # pipe -> pipe with a concurrent writer and reader
      r1, w1 = IO.pipe; r2, w2 = IO.pipe
      wt = Thread.new { w1.write "x" * 300_000; w1.close }
      rt = Thread.new { r2.read }
      r << IO.copy_stream(r1, w2); w2.close; r << rt.value.size; wt.join
      # file -> pipe with a slow reader
      rp, wp = IO.pipe
      rt = Thread.new { s = +""; while (c = rp.read(1000)); s << c; end; s.size }
      r << IO.copy_stream(src, wp); wp.close; r << rt.value
      # a pipe source with an offset
      rp, wp = IO.pipe; wp.write "piped"; wp.close
      r << t { IO.copy_stream(rp, dst, nil, 1) }
      rp, wp = IO.pipe; wp.write "piped"; wp.close
      out = File.open(dst, "w"); r << IO.copy_stream(rp, out); out.close; r << File.read(dst)
      # a trap while the copy waits on an empty pipe
      rp, wp = IO.pipe
      got = []
      trap(:USR1) { got << :usr1 }
      pid = Process.pid
      th = Thread.new { sleep 0.2; Process.kill(:USR1, pid); sleep 0.1; wp.write "late"; wp.close }
      out = File.open(dst, "w"); r << IO.copy_stream(rp, out); out.close; th.join
      r << got << File.read(dst)
      # failures
      rp, wp = IO.pipe; rp.close
      r << t { IO.copy_stream(src, wp) }
      rp, wp = IO.pipe; rp.close
      r << t { IO.copy_stream(File.open(src), wp, 3) }
      r << t { IO.copy_stream(src, File.open(src)) }
      if File.exist?("/dev/full")
        r << t { IO.copy_stream(src, "/dev/full") }
        r << t { IO.copy_stream(File.open(src), File.open("/dev/full", "w")) }
        w = File.open("/dev/full", "w"); w.write "x"; r << t { IO.copy_stream(src, w) }
      end
    end
    STDERR.puts r.inspect
    "##;
    assert_same_as_cruby("cs_fds", script, Stdin::File(""));
}
