//! `Process.initgroups` (#2241) and the bare `Errno` messages of the
//! other process calls CRuby raises with `rb_sys_fail(0)`; the line
//! counter of the file `ARGF` reads (#2262); a `#hash` that comes back
//! to the key being hashed (#2226); `f(**h)` passing a copy of `h`
//! without hashing its keys again (#2200); and the methods Array takes
//! from Enumerable in CRuby, which see an overridden `each`, against
//! the ones it has of its own, which do not (#2219).

use monoruby::tests::*;
use std::process::Command;

#[test]
fn initgroups_converts_its_arguments() {
    // Only the paths that raise before initgroups(3) runs: as root the
    // call would succeed and change the test process's groups.
    run_test_once(
        r##"
        r = [Process.respond_to?(:initgroups), Process.method(:initgroups).arity]
        t = ->(&b) { begin; b.call; rescue => e; [e.class, e.message]; end }
        r << t.() { Process.initgroups(nil, nil) }
        r << t.() { Process.initgroups("root", nil) }
        r << t.() { Process.initgroups(nil, 0) }
        r << t.() { Process.initgroups(:root, 0) }
        r << t.() { Process.initgroups("a\0b", 0) }
        r << t.() { Process.initgroups("root", "no_such_group_2241") }
        r << t.() { Process.initgroups("root", "a\0b") }
        r << t.() { Process.initgroups("root", 2**40) }
        r << t.() { Process.initgroups("root", Float::NAN) }
        r << t.() { Process.initgroups("root") }
        r
        "##,
    );
}

#[test]
fn initgroups_refused() {
    // Not root: initgroups(3) is refused, and the error is the bare errno.
    run_test_once_live(
        r##"
        if Process.euid == 0
          :root
        else
          o = Object.new
          def o.to_str = "nobody"
          begin
            Process.initgroups(o, 1.5)
          rescue SystemCallError => e
            [e.class, e.message]
          end
        end
        "##,
    );
}

#[test]
#[cfg(target_os = "linux")]
fn process_calls_raise_the_bare_errno() {
    run_test_once(
        r##"
        t = ->(&b) { begin; b.call; rescue SystemCallError => e; [e.class, e.message]; end }
        [
          t.() { Process.getpgid(0x7ffffff0) },
          t.() { Process.setpgid(0x7ffffff0, 0) },
          t.() { Process.getsid(0x7ffffff0) },
          t.() { Process.getpriority(Process::PRIO_PROCESS, 0x7ffffff0) },
          t.() { Process.setpriority(Process::PRIO_PROCESS, 0x7ffffff0, 0) },
          t.() { Process.getrlimit(9999) },
        ]
        "##,
    );
}

#[test]
fn argf_object_counts_lines_in_the_file() {
    run_test_once(
        r##"
        require "tmpdir"
        r = []
        Dir.mktmpdir do |d|
          l = File.join(d, "l.txt"); File.write(l, "abcdef\nxyz")
          m = File.join(d, "m.txt"); File.write(m, "one\ntwo\nthree\n")
          a = ARGF.class.new(l, m)
          $. = 0
          a.gets(2); r << [a.lineno, a.file.lineno, $.]
          a.gets; r << [a.lineno, a.file.lineno, $.]
          a.gets; r << [a.lineno, a.file.lineno, $.]
          a.gets; r << [a.lineno, a.file.lineno, $.]
          a.rewind; r << [a.lineno, a.file.lineno, $.]
          a.gets(1); r << [a.lineno, a.file.lineno, $.]
          a.rewind; r << [a.lineno, a.file.lineno, $.]
          r << a.readlines.size << [a.lineno, $.]
          b = ARGF.class.new(m)
          b.each_line(2).first(2); r << [b.lineno, b.file.lineno]
        end
        r
        "##,
    );
}

/// The process's own `ARGF`, over files named on the command line, on
/// monoruby and on CRuby.
#[test]
fn argf_counts_lines_in_the_file() {
    let dir = std::env::temp_dir().join(format!("monoruby-argf-2262-{}", std::process::id()));
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("l.txt"), "abcdef\nxyz").unwrap();
    std::fs::write(dir.join("m.txt"), "one\ntwo\nthree\n").unwrap();
    std::fs::write(
        dir.join("t.rb"),
        r#"
s = ->(tag) { p [tag, ARGF.lineno, ARGF.file.lineno, $.] }
ARGF.gets(2); s.(:cut)
ARGF.gets; s.(:rest)
ARGF.gets; s.(:next_file)
f = ARGF.file
ARGF.file.gets; s.(:file_gets)
ARGF.file.rewind; s.(:file_rewind)
ARGF.gets; ARGF.gets(1); s.(:gets)
ARGF.rewind; s.(:rewind)
p f.closed?
ARGF.each_line { |l| s.(l) }
p ARGF.lineno, $.
"#,
    )
    .unwrap();
    let run = |mut cmd: Command| {
        let out = cmd
            .current_dir(&dir)
            .env_remove("RUBYOPT")
            .args(["t.rb", "l.txt", "m.txt"])
            .output()
            .expect("spawn");
        format!(
            "{}{}",
            String::from_utf8_lossy(&out.stdout),
            String::from_utf8_lossy(&out.stderr)
        )
    };
    let expected = run(Command::new(monoruby::tests::ruby_path()));
    let actual = run(Command::new(env!("CARGO_BIN_EXE_monoruby")));
    let _ = std::fs::remove_dir_all(&dir);
    assert_eq!(expected, actual);
}

#[test]
fn hash_that_comes_back_to_its_key_stops_at_the_outermost() {
    run_test_once(
        r##"
        $n = 0
        class K
          attr_writer :h
          def inspect = "K"
          def hash
            $n += 1
            @h.delete(self) if @h
            1
          end
        end
        res = []
        t = ->(&b) { $n = 0; r = begin; b.call; rescue Exception => e; "#{e.class}: #{e.message}"; end; res << [r, $n] }
        mk = -> { k = K.new; h = { k => :v }; k.h = h; [k, h] }
        t.() { k, h = mk.(); [[0, k].uniq, h] }
        t.() { k, h = mk.(); [h.key?(k), h] }
        t.() { k, h = mk.(); h[k] }
        t.() { k, h = mk.(); h[k] = 2; h.size }
        t.() { k, h = mk.(); [h.delete(k), h] }
        t.() { k, h = mk.(); [k] | [0] }
        t.() { k, h = mk.(); [k] & [k] }
        t.() { k, h = mk.(); [k] - [0] }
        t.() { k, h = mk.(); [k].tally }
        t.() { k, h = mk.(); s = Set[0]; s.add(k); s.size }
        t.() { k, h = mk.(); Set[0].include?(k) }
        class R
          attr_writer :h
          def hash; $n += 1; @h.rehash if @h; 2; end
        end
        t.() { k = R.new; h = { k => 1 }; k.h = h; h[k] }
        # The throw unwinds the method in between, through its `ensure`
        # and past its `rescue`.
        class E
          def hash; $n += 1; begin; [self].hash; ensure; $n += 100; end; end
        end
        t.() { E.new.hash.class }
        t.() { [E.new].hash.class }
        t.() { {E.new => 1}.size }
        class Rs
          def hash; $n += 1; begin; [self].hash; rescue Exception; $n += 1000; end; 5; end
        end
        t.() { {Rs.new => 1}.size }
        a = []; a << a
        res << [a.hash == [a].hash, a.hash == [[a]].hash]
        h = {}; h[1] = h
        res << [h.hash == {1 => h}.hash, h.hash == {1 => {1 => h}}.hash]
        s = Struct.new(:x).new; s.x = s
        res << [s.hash == s.class.new(s).hash, [s].hash == s.hash]
        st = Struct.new(:x)
        res << [[st.new("a")].hash == [st.new("b")].hash, {st.new("a") => 1}.key?(st.new("a"))]
        res.inspect
        "##,
    );
}

#[test]
fn kwsplat_alone_passes_a_copy() {
    run_test(
        r##"
        $n = 0
        class V
          def hash = ($n += 1; 1)
          def eql?(o) = equal?(o)
          def inspect = "V"
        end
        def kw(**o) = o
        def kwa(a: 0, **o) = [a, o]
        def pos(o) = o
        def r2(*a) = a[0]
        def fwd(...) = kw(...)
        def fwd2(*, **) = kw(**)
        class P; def m(**o) = o; end
        class C < P; def m(**o) = super; end
        class I; attr_reader :o; def initialize(**o) = @o = o; end
        hsub = Class.new(Hash)
        h = hsub[V.new => 1, a: 2].compare_by_identity
        h.default = 7
        h.instance_variable_set(:@iv, 3)
        def h.sing = 1
        res = []
        [
          -> { kw(**h) },
          -> { kwa(**h)[1] },
          -> { pos(**h) },
          -> { r2(**h) },
          -> { send(:kw, **h) },
          -> { method(:kw).call(**h) },
          -> { proc { |**o| o }.call(**h) },
          -> { fwd(**h) },
          -> { fwd2(**h) },
          -> { C.new.m(**h) },
          -> { I.new(**h).o },
        ].each do |t|
          $n = 0
          r = t.()
          res << [$n, r.equal?(h), r.class == hsub, r.compare_by_identity?, r.default,
                  r.instance_variables, r.singleton_methods, r.frozen?, r.size]
        end
        $n = 0
        res << kwa(**h)[0] << kw(**h, b: 1).size << $n
        dp = Hash.new { :dp }; dp[:a] = 1
        res << kw(**dp)[:zz] << pos(**dp)[:zz]
        e = {}; e.default = 4
        res << kw(**e).default << r2(**e).nil? << kw(**{a: 1}.freeze).frozen?
        def fw(*a) = a.last
        ruby2_keywords :fw
        f = fw(**h)
        res << Hash.ruby2_keywords_hash?(f) << Hash.ruby2_keywords_hash?(kw(**f)) << f.default
        res
        "##,
    );
}

#[test]
fn array_subclass_each_reaches_the_enumerable_methods() {
    run_test_once(
        r##"
        class B < Array
          def each = yield(:e)
        end
        b = B.new([1, 2, 3])
        r = []
        r << b.partition { true } << b.flat_map { |x| [x] } << b.collect_concat { |x| [x] }
        a = []; b.each_with_index { |x, i| a << [x, i] }; r << a
        r << b.inject(:+) << b.inject { |s, x| [s, x] } << b.reduce(:+) << b.inject(0) { |s, x| [s, x] }
        r << b.filter_map { |x| x } << b.find_all { true } << b.grep(Symbol) << b.grep(Integer) { _1 }
        r << b.group_by { 1 } << b.sort_by { 1 } << b.tally << b.tally({})
        r << b.each_with_index.to_a << b.partition.size << b.flat_map.size << b.filter_map.size
        r << b.find_all.size
        # Array's own: the storage, whatever `each` says.
        r << b.select { true } << b.filter { true } << b.cycle(1).to_a << b.combination(1).to_a
        r << B.new([[3, 4]]).assoc(3) << B.new([[3, 4]]).rassoc(4)
        r << b.none? << b.none?(Symbol) << b.none? { _1 == 2 } << b.one? << b.one?(Integer) << b.one? { _1 == 2 }
        r << b.take_while { true } << b.take_while { _1 < 2 } << b.take_while.size
        r << Set.new(b).to_a << Set.new(b) { _1 * 2 }.to_a << b.to_set.to_a
        # And with `each` left alone, the same methods walk the storage.
        c = [1, 2, 3, nil]
        r << c.partition(&:nil?) << c.find_all(&:nil?) << c.none? << c.one?(nil) << c.take_while { _1 }
        r << c.each_with_index.to_a << c.filter_map { _1 } << c.flat_map { [_1] } << c.tally
        r << c.compact.inject(:+) << c.compact.sort_by { -_1 } << c.group_by(&:nil?) << c.grep(Integer)
        d = [3, 4]; r << d.take_while { d << 5 if d.size < 4; true }
        r << [].one? << [nil, false].one? << [1, nil].one? << [1, 2].none? { _1 > 2 }
        r.inspect
        "##,
    );
}
