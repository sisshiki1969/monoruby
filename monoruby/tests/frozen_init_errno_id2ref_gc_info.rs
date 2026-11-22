//! A batch of small CRuby incompatibilities.
//!
//! - **Frozen `initialize`** (#2105). `Struct#initialize` and
//!   `Data#initialize` raise `FrozenError` on a frozen receiver before
//!   they look at the arguments (`rb_struct_modify`).
//! - **`SystemCallError#errno`** (#2110) of an `Errno::*` the core raises
//!   itself is the class's errno, not nil.
//! - **`ObjectSpace._id2ref`** (#2093) warns in the `deprecated`
//!   category only, and resolves the id of an immediate back to it.
//! - **`Ractor.new`** (#2094) warns once per process that the API is
//!   experimental.
//! - **`GC.latest_gc_info`** (#2107) exists, with CRuby's keys and
//!   argument checks.
//! - **`IO#readpartial`** (#2111) with an empty buffer reads at most the
//!   requested length from the descriptor.
//! - **Autoload of a bare feature** (#2113): while `require` of the
//!   autoload's own file runs, the autoload is no longer pending, also
//!   for a file found on `$LOAD_PATH` or in the stub tree.
//! - **`assigned but unused variable`** (#2078) under `-w`.

use monoruby::tests::*;

const CAP: &str = r##"
  def cap
    saved = $stderr
    buf = +""
    io = Object.new
    io.define_singleton_method(:write) { |*a| a.each { |x| buf << x.to_s } }
    $stderr = io
    yield
    buf.lines.map { |l| l.sub(/\A.*?warning: /, "") }
  ensure
    $stderr = saved
  end
  def t
    yield.inspect
  rescue Exception => e
    "#{e.class}: #{e.message}"
  end
"##;

#[test]
fn struct_and_data_initialize_check_frozen() {
    run_test_once(&format!(
        r##"
        {CAP}
        D = Data.define(:a)
        d = D.new(a: 1)
        S = Struct.new(:a)
        s = S.new(1).freeze
        [
          t {{ d.send(:initialize, a: 2) }},
          t {{ d.send(:initialize) }},
          t {{ d.send(:initialize, b: 2) }},
          t {{ begin; d.send(:initialize, a: 2); rescue FrozenError => e; e.receiver.equal?(d); end }},
          d.to_h,
          t {{ s.send(:initialize, 2) }},
          t {{ s.send(:initialize, 1, 2, 3) }},
          t {{ begin; s.send(:initialize, 2); rescue FrozenError => e; e.receiver.equal?(s); end }},
          s.to_a,
          D.new(a: 3).with(a: 4).to_h,
          S.new(5).tap {{ |x| x.send(:initialize, 6) }}.to_a,
        ]
        "##
    ));
}

#[test]
fn errno_of_a_core_raised_system_call_error() {
    run_test_once(
        r##"
        def e
          yield
        rescue SystemCallError => x
          [x.class.to_s, x.errno.is_a?(Integer), x.errno == x.class::Errno]
        end
        r, = IO.pipe
        [
          e { File.open("/nonexistent/file") },
          e { Process.waitpid(999999) },
          e { Process.kill(0, 999999) },
          e { Dir.rmdir("/nonexistent/dir") },
          e { IO.new(999) },
          e { r.read_nonblock(1) },
          e { raise Errno::EAGAIN, "x" },
          e { raise SystemCallError.new("x", 3) },
          Errno::ENOENT.allocate.errno,
        ]
        "##,
    );
}

#[test]
fn id2ref_immediates_and_deprecation_category() {
    run_test_once(&format!(
        r##"
        {CAP}
        objs = [0, -3, 1 << 40, 1.5, -0.25, :foo, :"bar baz", nil, true, false]
        res = []
        res << cap {{ res << objs.map {{ |o| ObjectSpace._id2ref(o.object_id) }} }}
        Warning[:deprecated] = true
        res << cap {{ ObjectSpace._id2ref(1) }}
        Warning[:deprecated] = false
        s = "str"
        res << ObjectSpace._id2ref(s.object_id).equal?(s)
        res << t {{ ObjectSpace._id2ref(:foo) }}
        res << t {{ ObjectSpace._id2ref(12) }}
        res << t {{ ObjectSpace._id2ref(44) }}
        res << t {{ ObjectSpace._id2ref(2**70) }}
        res
        "##
    ));
}

#[test]
fn ractor_new_warns_once() {
    run_test_once(&format!(
        r##"
        {CAP}
        res = []
        res << cap {{ res << t {{ Ractor.new }} }}
        res << cap {{ Ractor.new {{ 1 }}.value }}
        res << cap {{ Ractor.new {{ 2 }}.value }}
        res
        "##
    ));
}

#[test]
fn gc_latest_gc_info() {
    run_test_once(&format!(
        r##"
        {CAP}
        h = {{ a: 1 }}
        GC.start
        GC.start
        info = GC.latest_gc_info
        [
          info.keys,
          info.values_at(:major_by, :need_major_by, :gc_by, :have_finalizer, :immediate_sweep, :state),
          GC.latest_gc_info(:gc_by),
          GC.latest_gc_info(nil).keys == info.keys,
          GC.latest_gc_info(h).equal?(h),
          h.keys.first(2),
          GC.method(:latest_gc_info).arity,
          t {{ GC.latest_gc_info("state") }},
          t {{ GC.latest_gc_info(:unknown) }},
          t {{ GC.latest_gc_info(1, 2) }},
        ]
        "##
    ));
}

#[test]
fn readpartial_reads_at_most_maxlen_from_the_fd() {
    run_test_once(
        r##"
        res = []
        r, w = IO.pipe
        w.write "xyz"
        res << r.readpartial(1)
        r.ungetc("B")
        res << r.readpartial(10)
        res << r.readpartial(10)
        r, w = IO.pipe
        w.write "xyz"
        r.readpartial(1)
        res << r.wait_readable(0).class
        w.close
        res << r.read
        res << (r.readpartial(1) rescue $!.class)
        buf = +"buf"
        r, w = IO.pipe
        w << "abcdef"
        res << r.readpartial(2, buf) << buf << r.readpartial(100)
        res
        "##,
    );
}

#[test]
fn autoload_of_a_bare_feature_is_not_pending_during_its_require() {
    run_test_once(
        r##"
        require "tmpdir"
        res = []
        Dir.mktmpdir do |dir|
          File.write("#{dir}/i2113_feat.rb", <<~RUBY)
            $res << [Object.const_defined?(:I2113Feat), Object.autoload?(:I2113Feat)]
            require "i2113_sub"
            class I2113Feat; end
          RUBY
          File.write("#{dir}/i2113_sub.rb", <<~RUBY)
            $res << [Object.const_defined?(:I2113Feat), Object.autoload?(:I2113Feat)]
          RUBY
          $LOAD_PATH.unshift dir
          $res = res
          autoload :I2113Feat, "i2113_feat"
          res << Object.autoload?(:I2113Feat)
          require "i2113_feat"
          res << Object.autoload?(:I2113Feat) << I2113Feat.name
          $LOAD_PATH.delete(dir)
        end
        # A feature the stub tree provides.
        autoload :IPSocket, "socket"
        module Kernel
          alias __i2113_require require
          def require(f)
            $res << [f, Object.const_defined?(:IPSocket), Object.autoload?(:IPSocket)] if f == "socket.so"
            __i2113_require(f)
          end
        end
        require "socket"
        res << Object.autoload?(:IPSocket) << IPSocket.name
        res
        "##,
    );
}

fn monoruby_w(args: &[&str]) -> String {
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .arg("--disable=gems")
        .arg("-w")
        .args(args)
        .output()
        .expect("failed to spawn monoruby");
    assert!(out.status.success(), "{out:?}");
    String::from_utf8_lossy(&out.stderr).into_owned()
}

#[test]
fn unused_variable_warning() {
    let path = std::env::temp_dir().join(format!("i2078_unused_{}.rb", std::process::id()));
    std::fs::write(
        &path,
        "x = 1\ndef m\n  y = 1\n  _z = 2\n  w = 3\n  w\nend\n1.times { |i| q = i }\n",
    )
    .unwrap();
    let file = path.to_str().unwrap();
    let err = monoruby_w(&[file]);
    let _ = std::fs::remove_file(&path);
    // In prism's order, as CRuby prints them: the scopes as they close.
    let expected = [
        format!("{file}:3: warning: assigned but unused variable - y"),
        format!("{file}:8: warning: assigned but unused variable - q"),
        format!("{file}:1: warning: assigned but unused variable - x"),
    ];
    let got: Vec<&str> = err
        .lines()
        .filter(|l| l.contains("assigned but unused"))
        .collect();
    assert_eq!(got, expected, "stderr: {err}");
    // `-e`: only a variable below the top level.
    let err = monoruby_w(&["-e", "xx = 1; def m; yy = 1; end"]);
    assert_eq!(err, "-e:1: warning: assigned but unused variable - yy\n");
    // Not without `-w`.
    let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .args(["--disable=gems", "-e", "def m; yy = 1; end"])
        .output()
        .unwrap();
    assert!(out.stderr.is_empty(), "{out:?}");
}
