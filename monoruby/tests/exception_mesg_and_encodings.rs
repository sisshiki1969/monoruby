//! An exception's message object, the encodings of what the exception
//! machinery builds, and the messages of a failed read.
//!
//! - **The message object** (#2031). `Exception#initialize(message = nil)`
//!   keeps the object it is given as it is (CRuby's `mesg`): `#to_s`
//!   answers a String as that very object and converts anything else
//!   (`rb_String`) each time it is read, not at `initialize`. `raise "x"`,
//!   `#exception(msg)` and `Marshal.load` keep it the same way, and `==`
//!   compares the two objects. `initialize` takes at most one argument
//!   (a keyword Hash counts as one); `receiver:` / `key:` belong to
//!   `FrozenError` / `KeyError`.
//! - **Encodings** (#2090). A backtrace line is in the encoding of its
//!   path String: the locale's for the main script, the feature String's
//!   for a file `require` / `load` ran (the filesystem's when that is
//!   US-ASCII or BINARY), BINARY for an `(eval at …)`
//!   frame. `#detailed_message` is US-ASCII and `#full_message` BINARY
//!   while they are all ASCII, and a message the core's own Ruby code
//!   raises is BINARY, as `rb_sprintf` makes CRuby's.
//! - **A failed read** (#2091) names the C function CRuby's message
//!   names (`io_fread`, `io_fillbuf` with the fd, `io_getpartial`, …), and
//!   `load` of a file it cannot read says only `cannot load such file`.
//! - **`Integer.new`** (#2092) is undefined, as `Float.new` is.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

#[test]
fn exception_keeps_its_message_object() {
    run_test_once(&format!(
        r#"
        {HELPER}
        $c = 0
        o = Object.new; def o.to_s = ($c += 1; "x")
        [
          t.() {{ e = RuntimeError.new(o); a = $c; e.message; e.message; [a, $c] }},
          t.() {{ s = +"msg"; e = RuntimeError.new(s); [e.message.equal?(s), e.to_s.equal?(s), e.message.equal?(e.message)] }},
          t.() {{ s = +"msg"; e = RuntimeError.new(s); s << "!"; e.message }},
          t.() {{ s = +"m"; begin; raise s; rescue => e; e.message.equal?(s); end }},
          t.() {{ s = +"m"; begin; raise RuntimeError, s; rescue => e; e.message.equal?(s); end }},
          t.() {{ s = "fz".freeze; RuntimeError.new(s).message.equal?(s) }},
          t.() {{ RuntimeError.new(3).message }},
          t.() {{ begin; raise RuntimeError, 3; rescue => e; e.message; end }},
          t.() {{ x = Object.new; def x.to_str = "S"; def x.to_s = "T"; RuntimeError.new(x).message }},
          t.() {{ x = Object.new; def x.to_s = 3; RuntimeError.new(x).message }},
          t.() {{ RuntimeError.new(BasicObject.new).message }},
          t.() {{ x = Object.new; def x.to_s = raise("boom"); RuntimeError.new(x).inspect }},
          t.() {{ RuntimeError.new(nil).message }},
          t.() {{ StandardError.new(:sym).message }},
          t.() {{ NameError.new(3, :n).message }},
          t.() {{ NoMethodError.new(3, :n).message }},
          t.() {{ e = RuntimeError.new("a"); e2 = e.exception(Object.new); [e2.message.class, e2.message] }},
          t.() {{ RuntimeError.new("a").exception(3).message }},
          t.() {{ RuntimeError.new("a").exception(nil).message }},
          t.() {{ e = RuntimeError.new("a"); e.exception("a").equal?(e) }},
          t.() {{ [RuntimeError.new(3).inspect, RuntimeError.new(3).detailed_message] }},
          t.() {{ Marshal.load(Marshal.dump(RuntimeError.new(3))).message }},
          t.() {{ Marshal.load(Marshal.dump(RuntimeError.new)).message }},
          t.() {{ Marshal.dump(RuntimeError.new(3)) == Marshal.dump(Marshal.load(Marshal.dump(RuntimeError.new(3)))) }},
          t.() {{ [RuntimeError.new(3) == RuntimeError.new(3), RuntimeError.new(3) == RuntimeError.new(3.0)] }},
          t.() {{ [RuntimeError.new("3") == RuntimeError.new(3), RuntimeError.new == RuntimeError.new("RuntimeError")] }},
          t.() {{ [RuntimeError.new == RuntimeError.new(nil), RuntimeError.new("a") == StandardError.new("a")] }},
        ]
        "#
    ));
}

#[test]
fn exception_initialize_arity() {
    run_test_once(&format!(
        r#"
        {HELPER}
        [
          t.() {{ RuntimeError.new(3, 4) }},
          t.() {{ RuntimeError.new("x", cause: nil) }},
          t.() {{ RuntimeError.new("a", receiver: 1) }},
          t.() {{ RuntimeError.new(**{{}}).message }},
          t.() {{ UncaughtThrowError.new(1, 2, 3).to_s }},
          t.() {{ x = Object.new; def x.to_str = "fmt %p"; UncaughtThrowError.new(:tg, 2, x).to_s }},
          t.() {{ UncaughtThrowError.new(1, 2, 3, 4).to_s }},
          t.() {{ KeyError.new("x", receiver: 1, key: 2).then {{ [_1.receiver, _1.key, _1.message] }} }},
          t.() {{ FrozenError.new("x", receiver: 1).then {{ [_1.receiver, _1.message] }} }},
          t.() {{ KeyError.new("a", 3) }},
          t.() {{ FrozenError.new("a", 3) }},
          t.() {{ [KeyError.instance_method(:initialize).owner, FrozenError.instance_method(:initialize).owner] }},
          t.() {{ SystemExit.new(3, 4).message }},
        ]
        "#
    ));
}

#[test]
fn backtrace_and_report_encodings() {
    // Locale-dependent (the main script's path is in the locale's
    // encoding), so against a live CRuby run in the same environment.
    run_test_once_live(
        r##"
        def __bt_m = raise("x")
        e = (__bt_m rescue $!)
        path = "/tmp/monoruby_bt_enc_#{$$}.rb"
        File.write(path, "def __bt_loaded = caller(0)\n")
        load path.b
        bin = __bt_loaded.map(&:encoding).uniq
        load path.encode("US-ASCII")
        ascii = __bt_loaded.map(&:encoding).uniq
        load path
        utf8 = __bt_loaded.map(&:encoding).uniq
        File.delete(path)
        m = Thread::Mutex.new
        d = Data.define(:a)
        # An Encoding's inspect is no literal, so the comparison is on the
        # rendering of the whole result.
        [
          e.backtrace.map(&:encoding).uniq,
          caller(0).map(&:encoding).uniq,
          [1].map { caller(0) }[0].map(&:encoding).uniq,
          eval("caller(0)").map(&:encoding),
          eval("caller(0)", nil, "foo.rb").map(&:encoding),
          binding.eval("caller(0)").map(&:encoding),
          instance_eval("caller(0)").map(&:encoding),
          Thread.current.backtrace.map(&:encoding).uniq,
          e.backtrace_locations.map { _1.path.encoding }.uniq,
          bin, ascii, utf8,
          [e.detailed_message, e.full_message, e.inspect].map(&:encoding),
          (raise "é" rescue [$!.detailed_message, $!.full_message, $!.inspect].map(&:encoding)),
          (raise "x".b rescue [$!.message, $!.detailed_message, $!.full_message, $!.inspect].map(&:encoding)),
          (nil.foo rescue [$!.message, $!.detailed_message, $!.full_message, $!.inspect].map(&:encoding)),
          [RuntimeError.new("").detailed_message, RuntimeError.new("a").detailed_message(highlight: true)].map(&:encoding),
          RuntimeError.new("a\nb").detailed_message.encoding,
          (m.unlock rescue [$!.message, $!.message.encoding]),
          (d.new rescue [$!.message, $!.message.encoding]),
          (raise ArgumentError, "x" rescue $!.message.encoding),
        ].inspect
        "##,
    );
}

#[test]
fn errno_of_a_failed_read() {
    run_test_once(&format!(
        r#"
        {HELPER}
        f = ->(&b) {{ t.(&b).gsub(/fd:\d+/, "fd:N") }}
        [
          f.() {{ File.read("/") }},
          f.() {{ IO.read("/") }},
          f.() {{ File.binread("/") }},
          f.() {{ File.read("/", 3) }},
          f.() {{ File.open("/") {{ _1.read }} }},
          f.() {{ File.open("/") {{ _1.read(3) }} }},
          f.() {{ File.open("/") {{ _1.gets }} }},
          f.() {{ File.readlines("/") }},
          f.() {{ File.foreach("/") {{}} }},
          f.() {{ File.open("/") {{ _1.each_line {{}} }} }},
          f.() {{ File.open("/") {{ _1.getc }} }},
          f.() {{ File.open("/") {{ _1.getbyte }} }},
          f.() {{ File.open("/") {{ _1.each_char {{}} }} }},
          f.() {{ File.open("/") {{ _1.readline }} }},
          f.() {{ File.open("/") {{ _1.eof? }} }},
          f.() {{ File.open("/") {{ _1.sysread(3) }} }},
          f.() {{ File.open("/") {{ _1.readpartial(3) }} }},
          f.() {{ File.open("/") {{ _1.read_nonblock(3) }} }},
          f.() {{ File.open("/") {{ _1.pread(3, 0) }} }},
          f.() {{ File.open("/") {{ |d| File.new(d.fileno, autoclose: false).read }} }},
          f.() {{ File.open("/") {{ |d| IO.new(d.fileno, autoclose: false).gets }} }},
          f.() {{ load "no_such_file_for_2091" }},
          f.() {{ load "/" }},
        ]
        "#
    ));
}

#[test]
fn integer_new_is_undefined() {
    run_test_once(
        r##"
        [
          (Integer.new rescue [$!.class, $!.message]),
          (Float.new rescue [$!.class, $!.message]),
          Integer.respond_to?(:new),
          (Integer.allocate rescue [$!.class, $!.message]),
        ]
        "##,
    );
}
