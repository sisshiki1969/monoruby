//! A second batch of small CRuby incompatibilities.
//!
//! - **`ENV.merge!(ENV)`** (#2157) skips ENV itself; `ENV.replace(ENV)`
//!   returns at once.
//! - **`File::SEPARATOR` & co.** (#2162) are frozen, and `Separator` is the
//!   same object as `SEPARATOR`.
//! - **A lookup in an empty Hash** (#2127) does not hash the key.
//! - **`IO.popen` with an Integer mode** (#2145).
//! - **`Array#grep(regexp)`** (#2142) matches a String in any encoding, as
//!   `Regexp#match?` does.
//! - **Array builtins that run Ruby** (#2172) keep the values they hold
//!   rooted: `grep`, `min` / `max` / `minmax` with a block, `Array#hash`.
//! - **"global variable not initialized"** (#2160) names the reading line
//!   and goes through `Warning.warn`.
//! - **`trace_var` / `untrace_var`** (#2159).
//! - **`format`'s errors** for mixed named / numbered / unnumbered
//!   references (#2144).
//! - **File predicates given an IO** (#2129) `fstat` its descriptor.
//! - **`-i`** (#2174) leaves the last output as `$stdout`; an open File is
//!   flushed at exit; a refilled `ARGV` restarts the walk.
//! - **`{0.0 => 1, -0.0 => 2}`** (#2165) gets no duplicated-key warning.

use monoruby::tests::*;

const T: &str = r##"
  def t
    yield.inspect
  rescue Exception => e
    "#{e.class}: #{e.message}"
  end
"##;

#[test]
fn env_merge_and_replace_with_itself() {
    run_test_once(
        r##"
        ENV["MRV_I2157"] = "s"
        n = 0
        r1 = ENV.merge!(ENV) { |k, o, v| n += 1; v }.equal?(ENV)
        r2 = ENV.update(ENV, { "MRV_I2157" => "t" }) { |k, o, v| nil }.equal?(ENV)
        v = ENV["MRV_I2157"]
        r3 = ENV.replace(ENV).equal?(ENV)
        r = [n, r1, r2, v, r3, ENV["MRV_I2157"], ENV.key?("PATH")]
        ENV.delete("MRV_I2157")
        r
        "##,
    );
}

#[test]
fn file_constants_are_frozen() {
    run_test_once(&format!(
        r##"
        {T}
        [
          [File::SEPARATOR, File::Separator, File::PATH_SEPARATOR, File::NULL,
           File::Constants::NULL].map(&:frozen?),
          File::Separator.equal?(File::SEPARATOR),
          t {{ File::SEPARATOR << "x" }},
          File::SEPARATOR,
          File::ALT_SEPARATOR,
        ]
        "##
    ));
}

#[test]
fn empty_hash_lookup_does_not_hash_the_key() {
    run_test(
        r##"
        class Bad; def hash = raise("boom"); end
        b = Bad.new
        [->{ {}.include?(b) }, ->{ {}[b] }, ->{ {}.fetch(b, 1) },
         ->{ {}.delete(b) }, ->{ {}.key?(b) }, ->{ {}.dig(b) },
         ->{ {}.values_at(b) }, ->{ Hash.new(5)[b] }, ->{ {1 => 2}[b] }].map do |f|
          f.call
        rescue => e
          e.message
        end
        "##,
    );
}

#[test]
fn popen_with_an_integer_mode() {
    run_test_once(&format!(
        r##"
        {T}
        [
          t {{ IO.popen("echo hi", File::RDONLY, &:read) }},
          t {{ IO.popen("cat > /dev/null", File::WRONLY) {{ |io| io.write("x") }} }},
          t {{ IO.popen("cat", File::RDWR) {{ |io| io.write("x"); io.close_write; io.read }} }},
          t {{ IO.popen("echo hi", mode: File::RDONLY, &:read) }},
          t {{ IO.popen("echo hi", File::RDONLY, mode: "r", &:read) }},
        ]
        "##
    ));
}

#[test]
fn grep_regexp_matches_any_encoding() {
    run_test_once(&format!(
        r##"
        {T}
        o = Object.new
        def o.to_str = "abz"
        [
          t {{ ["MR\xFFK".b].grep(/\AMR/n) }},
          t {{ ["MR\xFFK".b].grep(/MR/) }},
          t {{ ["\xFF".b].grep(/\xFF/n) }},
          t {{ ["MR\xFFK"].grep(/MR/) }},
          t {{ ["MR\xFFK".b].grep_v(/MR/) }},
          t {{ [:abc, "abd", 1, nil, Object.new].grep(/ab/) }},
          t {{ [o].grep(/ab/).size }},
          t {{ ["é", "e"].grep(/é/) }},
          t {{ ["a".encode("UTF-16LE")].grep(/a/) }},
        ]
        "##
    ));
}

#[test]
fn array_builtins_root_values_across_ruby() {
    run_test_once(
        r##"
        def churn = (GC.start; $junk = Array.new(3000) { |i| "junk#{i}" })
        res = []
        class Pat
          def initialize(a) = @a = a
          def ===(x)
            if x.equal?(@a[-1])
              (0...@a.size - 1).each { |i| @a[i] = nil }
              churn
            end
            true
          end
        end
        a = (1..5).map { |i| "s#{i}" * 3 }
        res << a.grep(Pat.new(a))
        [:max, :min].each do |m|
          a = (1..10).map { |i| "v#{i}" * 3 }
          first = true
          res << a.send(m) { |x, y|
            if first
              first = false
              a.clear
              churn
            end
            x <=> y
          }
        end
        a = (1..10).map { |i| format("v%02d", i) * 3 }
        n = 0
        res << a.minmax { |x, y|
          n += 1
          if n == 5
            a.map!(&:dup)
            churn
          end
          x <=> y
        }
        class Popper
          def initialize(a) = @a = a
          def hash
            @a.pop(@a.size - 1) if @a.size > 1
            churn
            1
          end
        end
        a = []
        a << Popper.new(a)
        5.times { |i| a << "s#{i}" * 3 }
        res << a.hash.is_a?(Integer)
        class Cmp
          attr_reader :s
          def initialize(s, a) = (@s = s; @a = a)
          def <=>(o)
            @a.clear
            churn
            s <=> o.s
          end
        end
        a = []
        3.times { |i| a << Cmp.new("c#{i}" * 3, a) }
        res << a.max.s
        res
        "##,
    );
}

#[test]
fn uninitialized_gvar_warning_has_a_location() {
    run_test_once(
        r##"
        verbose, $VERBOSE = $VERBOSE, true
        log = []
        Warning.singleton_class.alias_method(:__i2160_warn, :warn)
        Warning.define_singleton_method(:warn) { |m, *| log << m }
        begin
          $_i2160_a
          def i2160_m = $_i2160_b
          12.times { i2160_m }
          [1].map { $_i2160_c }
        ensure
          Warning.singleton_class.alias_method(:warn, :__i2160_warn)
          $VERBOSE = verbose
        end
        log = log.grep(/not initialized/)
        lines = log.map { |m| m[/\A.+:(\d+): warning: global variable '\$_i2160_.' not initialized\n\z/, 1]&.to_i }
        [log.size, lines.all?, lines.compact.uniq.map { _1 - lines[0] }]
        "##,
    );
}

#[test]
fn trace_var_declares_any_name() {
    run_test_once(&format!(
        r##"
        {T}
        res = []
        res << t {{ trace_var(:$i2159_new) {{ }} }}
        res << global_variables.include?(:$i2159_new)
        res << defined?($i2159_new)
        res << t {{ untrace_var(:$i2159_never_seen) }}
        res << t {{ trace_var(:i2159_foo) {{ }} }}
        res << global_variables.include?(:i2159_foo)
        res << t {{ untrace_var(:i2159_foo).size }}
        res << t {{ untrace_var(:i2159_foo) }}
        res << t {{ trace_var(:"/errinfo") {{ |x| res << [:hook, x] }} }}
        begin; raise "x"; rescue; end
        res << t {{ untrace_var(:"/errinfo").size }}
        res << t {{ untrace_var(:"/i2159_nothing") }}
        trace_var(:$i2159_v) {{ |v| res << [:set, v] }}
        $i2159_v = 5
        res << defined?($i2159_v) << $i2159_v
        res << 10.times.map {{ $i2159_v += 1 }}.last
        untrace_var(:$i2159_v)
        res
        "##
    ));
}

#[test]
fn format_mixed_reference_errors() {
    run_test_once(&format!(
        r##"
        {T}
        [
          t {{ "%d %{{a}}" % [1, {{a: 1}}] }},
          t {{ "%d %<a>d" % [1, {{a: 1}}] }},
          t {{ "%{{a}} %d" % [{{a: 1}}] }},
          t {{ "%<a>d %d" % [{{a: 1}}] }},
          t {{ "%{{a}} %1$d" % [{{a: 1}}] }},
          t {{ "%<a>d %1$d" % [{{a: 1}}] }},
          t {{ "%<a>{{b}}" % [{{a: 1, b: 2}}] }},
          t {{ "%<a><b>d" % [{{a: 1, b: 2}}] }},
          t {{ "%1$d %{{a}}" % [{{a: 1}}] }},
          t {{ "%<a>d %<b>d %{{a}}" % [{{a: 1, b: 2}}] }},
          t {{ "%{{a}} %*d" % [{{a: 1}}] }},
          t {{ "%{{a}} %z" % [{{a: 1}}] }},
          t {{ "%<a>5.2<b>f" % [{{a: 1, b: 2}}] }},
          t {{ format("%-5<a>s|", a: "x") }},
          t {{ "%d %d %{{a}}" % [1, 2, {{a: 1}}] }},
          t {{ "%1$d %d" % [1, 2] }},
          t {{ "%d %1$d" % [1, 2] }},
        ]
        "##
    ));
}

#[test]
fn file_predicates_stat_an_io() {
    run_test_once(&format!(
        r##"
        {T}
        f = File.open("/dev/null"); f.close
        g = File.open("/dev/null")
        d = Dir.open("/tmp")
        o = Object.new
        def o.to_io = 1
        o2 = Object.new
        def o2.to_io = $stdin
        r = [
          t {{ File.exist?(f) }},
          t {{ File.file?(f) }},
          t {{ File.size(f) }},
          t {{ File.size?(f) }},
          t {{ File.zero?(f) }},
          t {{ File.directory?(f) }},
          t {{ File.identical?(IO.allocate, IO.allocate) }},
          t {{ test(?e, IO.allocate) }},
          t {{ [File.exist?(g), File.file?(g), File.size(g), File.directory?(g),
               File.identical?(g, "/dev/null")] }},
          t {{ File.directory?(d) }},
          t {{ File.exist?(o) }},
          t {{ File.exist?(o2) }},
          t {{ File.exist?(nil) }},
          t {{ [File.exist?("/nonexistent"), File.directory?("/tmp"), File.size?("/nonexistent")] }},
        ]
        g.close
        d.close
        r
        "##
    ));
}

fn monoruby(args: &[&str]) -> std::process::Output {
    std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
        .env_remove("RUBYOPT")
        .arg("--disable=gems")
        .args(args)
        .output()
        .expect("failed to spawn monoruby")
}

#[test]
fn inplace_edit_leaves_the_last_output_as_stdout() {
    let dir = std::env::temp_dir().join(format!("i2174_{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    let f1 = dir.join("f1.txt");
    let f2 = dir.join("f2.txt");
    let f3 = dir.join("f3.txt");
    std::fs::write(&f1, "a\nb\n").unwrap();
    std::fs::write(&f2, "c\n").unwrap();
    std::fs::write(&f3, "d\n").unwrap();
    let script = format!(
        "ARGF.each_line {{ |l| print l.upcase }}\n\
         first = $stdout\n\
         STDERR.puts [$stdout.path, $stdout.equal?(STDOUT), $stdout.closed?].inspect\n\
         ARGV.replace([{f3:?}])\n\
         ARGF.each_line {{ |l| print l * 2 }}\n\
         STDERR.puts [first.closed?, $stdout.path == {f3:?}].inspect\n\
         puts \"AFTER\"\n",
        f3 = f3.to_str().unwrap()
    );
    let out = monoruby(&[
        "-i",
        "-e",
        &script,
        f1.to_str().unwrap(),
        f2.to_str().unwrap(),
    ]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(
        String::from_utf8_lossy(&out.stderr),
        format!("[{:?}, false, false]\n[true, true]\n", f2.to_str().unwrap())
    );
    assert!(out.stdout.is_empty(), "{out:?}");
    assert_eq!(std::fs::read_to_string(&f1).unwrap(), "A\nB\n");
    assert_eq!(std::fs::read_to_string(&f2).unwrap(), "C\n");
    assert_eq!(std::fs::read_to_string(&f3).unwrap(), "d\nd\nAFTER\n");
    // A File nobody closed is flushed at exit.
    let f4 = dir.join("f4.txt");
    let out = monoruby(&[
        "-e",
        &format!(
            "f = File.open({:?}, 'w'); f.print 'hi'",
            f4.to_str().unwrap()
        ),
    ]);
    assert!(out.status.success(), "{out:?}");
    assert_eq!(std::fs::read_to_string(&f4).unwrap(), "hi");
    let _ = std::fs::remove_dir_all(&dir);
}

#[test]
fn zero_float_keys_of_either_sign_are_not_duplicates() {
    let path = std::env::temp_dir().join(format!("i2165_{}.rb", std::process::id()));
    std::fs::write(
        &path,
        "p({-0.0 => 5, 0.0 => 6})\n\
         p({0.0 => 5, -0.0 => 6})\n\
         p({0.0 => 5, 0.0 => 6})\n\
         p({-0.0 => 5, -0.0 => 6})\n\
         p({0.0 => 1, -0.0 => 2, 0.0 => 3})\n\
         def m(**k) = k\n\
         p m(**{0.0 => 1, -0.0 => 2})\n\
         p({0e0 => 1, -0.0 => 2, 1 => {0.0 => 1, -0.0 => 2}})\n",
    )
    .unwrap();
    let file = path.to_str().unwrap();
    let out = monoruby(&["-w", file]);
    let _ = std::fs::remove_file(&path);
    assert!(out.status.success(), "{out:?}");
    let err = String::from_utf8_lossy(&out.stderr);
    let got: Vec<&str> = err.lines().collect();
    assert_eq!(
        got,
        [
            format!("{file}:3: warning: key 0.0 is duplicated and overwritten on line 3"),
            format!("{file}:4: warning: key -0.0 is duplicated and overwritten on line 4"),
            format!("{file}:5: warning: key 0.0 is duplicated and overwritten on line 5"),
        ],
        "stderr: {err}"
    );
}
