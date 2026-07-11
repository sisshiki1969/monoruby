extern crate monoruby;
use monoruby::tests::*;

#[test]
fn read_only_gvar_message() {
    run_test_once(
        r#"
        res = []
        [proc { $: = [] }, proc { $" = [] }, proc { $-I = [] },
         proc { $? = 1 }, proc { $FILENAME = "x" }, proc { $< = 1 },
         proc { $-a = 1 }, proc { $-l = 1 }, proc { $-p = 1 }].each do |blk|
          begin
            blk.call
            res << "no error"
          rescue NameError => e
            res << e.message
          end
        end
        res
        "#,
    );
}

#[test]
fn record_separator_validation() {
    run_test_once(
        r#"
        res = []
        begin; $/ = 1; rescue TypeError => e; res << e.message; end
        begin; $, = :sym; rescue TypeError => e; res << e.message; end
        begin; $; = 1.0; rescue TypeError => e; res << e.message; end
        $; = /,/
        res << $;.class
        $; = nil
        begin; $\ = 42; rescue TypeError => e; res << e.message; end
        $/ = "abc"
        res << $/.frozen? << $/ << $-0
        $-0 = "xyz"
        res << $/
        $/ = "\n"
        str = Class.new(String).new("sub")
        $/ = str
        res << $/.instance_of?(String) << ($/ == "sub")
        $/ = "\n"
        s = +"zz"
        $\ = s
        res << $\.equal?(s)
        $\ = nil
        res
        "#,
    );
}

#[test]
fn last_line_number_coerces_with_to_int() {
    run_test_once(
        r#"
        res = [$.]
        $. = 123.5
        res << $.
        obj = Object.new
        def obj.to_int = 321
        $. = obj
        res << $.
        begin; $. = Object.new; rescue TypeError; res << "TypeError"; end
        $. = 0
        res
        "#,
    );
}

#[test]
fn stdout_must_have_write_method() {
    run_test_once(
        r#"
        res = []
        begin; $stdout = nil; rescue TypeError => e; res << e.message; end
        begin; $stderr = 42; rescue TypeError => e; res << e.message; end
        res
        "#,
    );
}

#[test]
fn verbose_normalizes_truthy_to_true() {
    run_test_once(
        r#"
        saved = $VERBOSE
        $VERBOSE = 1
        res = [$VERBOSE]
        $VERBOSE = nil
        res << $VERBOSE
        $VERBOSE = false
        res << $VERBOSE
        $VERBOSE = saved
        res
        "#,
    );
}

#[test]
fn debug_flag_alias() {
    run_test_once(
        r#"
        res = [$DEBUG, $-d]
        $DEBUG = true
        res << $-d
        $DEBUG = false
        res
        "#,
    );
}

#[test]
fn match_data_assignment() {
    run_test_once(
        r#"
        res = []
        begin; $~ = Object.new; rescue TypeError => e; res << e.message; end
        begin; $~ = 1; rescue TypeError => e; res << e.message; end
        "foo" =~ /(f)oo/
        m = $~
        "bar" =~ /(b)ar/
        res << $1
        $~ = m
        res << $1 << $& << $~.equal?(m)
        $~ = nil
        res << $~ << $1
        res
        "#,
    );
}

#[test]
fn errinfo_backtrace_gvar() {
    run_test_once(
        r#"
        res = [defined?($@), $@]
        begin
          $@ = ["x"]
        rescue ArgumentError => e
          res << e.message
        end
        begin
          raise "oops"
        rescue => err
          res << $@.instance_of?(Array)
          $@ = ["custom"]
          res << err.backtrace
          $@ = "single"
          res << err.backtrace
          begin; $@ = :sym; rescue TypeError; res << "TypeError"; end
          begin; $@ = [nil]; rescue TypeError; res << "TypeError2"; end
          begin; $@ = [["nested"]]; rescue TypeError; res << "TypeError3"; end
          $@ = nil
          res << err.backtrace
        end
        res
        "#,
    );
}

#[test]
fn load_path_aliases_share_identity() {
    run_test_once(
        r#"
        [$:.equal?($LOAD_PATH), $:.equal?($-I)]
        "#,
    );
}

#[test]
fn gvar_warnings() {
    // Deprecation warnings for the separator globals and $= are gated
    // on Warning[:deprecated] (or $VERBOSE == true); the
    // uninitialized-global warning on $VERBOSE only. `||=` lazy
    // initialization never warns. Warnings are routed through
    // Kernel#warn / Warning.warn, so a captured $stderr sees them.
    run_test_once(
        r#"
        def cap
          saved = $stderr
          buf = +""
          io = Object.new
          io.define_singleton_method(:write) { |*a| a.each { |x| buf << x.to_s } }
          $stderr = io
          yield
          buf
        ensure
          $stderr = saved
        end
        res = []
        Warning[:deprecated] = true
        res << cap { $/ = "x"; $/ = "\n" }.scan(/warning: [^\n]+/)
        res << cap { $, = "y"; $, = nil }.scan(/warning: [^\n]+/)
        res << cap { $; = "z"; $; = nil }.scan(/warning: [^\n]+/)
        res << cap { a = $= }.scan(/no longer effective[^\n]*/)
        res << cap { $= = 1 }.scan(/no longer effective[^\n]*/)
        Warning[:deprecated] = false
        res << cap { $\ = "w"; $\ = nil }.empty?
        saved_verbose = $VERBOSE
        $VERBOSE = true
        res << cap { $completely_uninit_gvar_zz }.scan(/warning: [^\n]+/)
        res << cap { $lazy_init_gvar_zz ||= 5 }.empty?
        $VERBOSE = saved_verbose
        res
        "#,
    );
}

#[test]
fn match_globals_inherit_subject_encoding() {
    run_test_once(
        r#"
        res = []
        "abc".dup.force_encoding(Encoding::EUC_JP) =~ /b/
        res << $~[0].encoding.to_s
        res << $&.encoding.to_s
        res << $`.encoding.to_s
        res << $'.encoding.to_s
        "abc".dup.force_encoding(Encoding::ISO_8859_1) =~ /a/
        res << $`.encoding.to_s
        res << $'.encoding.to_s
        /(b)(c)?/ =~ "abc".dup.force_encoding(Encoding::EUC_JP)
        res << $1.encoding.to_s
        res << $+.encoding.to_s
        res << Regexp.last_match(1).encoding.to_s
        /b/ === "abc".dup.force_encoding(Encoding::EUC_JP)
        res << $&.encoding.to_s
        res
        "#,
    );
}

#[test]
fn regexp_match_backref_identity() {
    run_test_once(
        r#"
        md = /foo/.match 'foo'
        res = [md.is_a?(MatchData), $~.equal?(md)]
        /bar/ =~ 'bar'
        res << $~.equal?(md) << $~.is_a?(MatchData)
        res
        "#,
    );
}

#[test]
fn stdio_external_encoding() {
    run_test_once(
        r#"
        [STDOUT.external_encoding, STDERR.external_encoding,
         STDOUT.internal_encoding, STDERR.internal_encoding].map(&:inspect)
        "#,
    );
}

#[test]
fn parse_time_warnings() {
    run_test_once(
        r#"
        require "stringio"
        res = []
        capture = proc do |verbose, code|
          orig_err, orig_v = $stderr, $VERBOSE
          $stderr = StringIO.new
          $VERBOSE = verbose
          begin
            eval(code)
          ensure
            out = $stderr.string
            $stderr = orig_err
            $VERBOSE = orig_v
          end
          out
        end
        res << (capture.call(true, "case 1\nwhen 2\n :foo\nwhen 2\n :bar\nend") =~ /warning: 'when' clause on line \d+ duplicates 'when' clause on line \d+ and is ignored/ ? "warned" : "silent")
        res << (capture.call(false, "case 1\nwhen 2\n :foo\nwhen 2\n :bar\nend") == "" ? "silent" : "warned")
        res << (capture.call(true, "defined?(Object.to_s); 42") =~ /warning: possibly useless use of defined\? in void context/ ? "warned" : "silent")
        res << (capture.call(false, "defined?(Object.to_s); 42") == "" ? "silent" : "warned")
        res << (capture.call(true, "$. = 0\n1.times { |i| i if 4..5 }") =~ /warning: integer literal in flip-flop/ ? "warned" : "silent")
        res << (capture.call(false, "$. = 0\n1.times { |i| i if 4..5 }") == "" ? "silent" : "warned")
        res
        "#,
    );
}

#[test]
fn shared_substring_snapshot_window() {
    // Regression: `string_snapshot` on a CoW shared substring (as
    // handed out by $' / $N / MatchData#[]) must present exactly the
    // substring's window, not the root's whole buffer — String#scan
    // and String#gsub-with-block snapshot their receiver.
    run_test_once(
        r#"
        s = "HEADER-LINE\nmiddle\ntail-line\n" + "pad" * 20
        s =~ /middle\n/
        post = $'
        res = [post.gsub(/^(.+)$/) { "X" + $1 }]
        res << post.scan(/\w+/)
        res << post.sub(/tail/) { "T" }
        md = /middle\n/.match(s)
        res << md.post_match.gsub(/^(.+)$/) { "Y" + $1 }
        res << ("if " + "@c" + post.gsub(/^(.+)$/) { "  " + $1 } + "end\n")
        res
        "#,
    );
}

#[test]
fn errinfo_fiber_local_and_read_only() {
    // `$!` is backed by the per-fiber Executor: an exception rescued
    // (and suspended) inside a Fiber must not leak into the parent
    // fiber's `$!`, and assigning `$!` raises NameError.
    run_test_once(
        r#"
        res = []
        Fiber.new do
          raise "hi"
        rescue
          Fiber.yield
        end.resume
        res << $!
        Fiber.new do
          raise "yo"
        rescue
          Fiber.yield
        end.resume
        res << $@
        begin
          eval("$! = []")
        rescue NameError => e
          res << e.message
        end
        res << ($! ? "leaked" : "clean")
        res
        "#,
    );
}

#[test]
fn errinfo_rescue_save_restore() {
    // The bytecodegen `$!` save/restore protocol (now routed through the
    // internal errinfo hook) still restores the outer exception when a
    // nested rescue completes, when a rescue exits via `return`, and
    // around `ensure` bodies; bare `raise` re-raises `$!`.
    run_test(
        r#"
        res = []
        def f(res)
          begin
            raise "x"
          rescue
            return 1
          ensure
            res << $!.class.to_s
          end
        end
        f(res)
        res << $!.inspect
        begin
          raise "outer"
        rescue
          begin
            raise "inner"
          rescue
          end
          res << $!.message
        end
        begin
          begin
            raise "again"
          rescue
            raise
          end
        rescue => e
          res << e.message
        end
        res
        "#,
    );
}

#[test]
fn lastline_set_by_stringio_gets() {
    // StringIO#gets / #readline publish `$_` to their caller's method
    // scope (CRuby sets it via rb_lastline_set from C), including nil at
    // EOF, and a read inside a block lands on the enclosing method scope.
    run_test_once(
        r#"
        require 'stringio'
        res = []
        sio = StringIO.new("foo\nbar\n", "r")
        res << sio.gets
        res << $_
        res << sio.gets
        res << $_
        res << sio.gets
        res << $_
        obj = Object.new
        def obj.run; yield; end
        sio2 = StringIO.new("a\nb\n", "r")
        obj.run { sio2.gets }
        res << $_
        sio3 = StringIO.new("x\n", "r")
        sio3.readline
        res << $_
        res
        "#,
    );
}

#[test]
fn set_backtrace_accepts_locations() {
    // CRuby 3.4+: `Exception#set_backtrace` (and `$@ =`) accept an Array
    // of `Thread::Backtrace::Location`, stored as their string forms;
    // mixed / invalid arrays raise TypeError with CRuby's message.
    run_test_once(
        r#"
        def probe = caller_locations
        locs = probe
        e = RuntimeError.new("x")
        e.set_backtrace(locs)
        res = [e.backtrace == locs.map(&:to_s)]
        res << (e.backtrace_locations.map(&:lineno) == locs.map(&:lineno))
        begin
          e.set_backtrace(["a", :b])
        rescue TypeError => te
          res << te.message
        end
        begin
          raise "z"
        rescue
          $@ = locs
          res << ($!.backtrace == locs.map(&:to_s))
        end
        res
        "#,
    );
}

#[test]
fn errinfo_cleared_on_non_local_return() {
    // A non-local exit (`return` from a block, `break`) leaving a
    // frame suspended inside a rescue clause restores `$!` to the
    // region-entry save at runtime; a `break` that *resumes* inside
    // the rescue clause keeps the caught exception in `$!`.
    run_test_once(
        r#"
        res = []
        obj = Class.new do
          def method_missing(*args)
            yield
          end
          def bar
            e = StandardError.new 'foo'
            begin
              raise e
            rescue
              foo(e) { return }
            end
          end
        end.new
        obj.bar
        res << $!
        def foo2
          [1].each do
            begin
              raise StandardError.new('err')
            rescue => e
              return
            end
          end
        end
        foo2
        res << $!
        def brk
          [1].each do
            begin
              raise "b"
            rescue
              break
            end
          end
        end
        brk
        res << $!
        def keep
          begin
            raise "k"
          rescue
            [1].each { break }
            return $!.message
          end
        end
        res << keep
        res << $!
        res
        "#,
    );
}

#[test]
fn dollar_zero_sets_process_title_and_pid() {
    // `$0 =` rewrites the process's argv area (Linux) so `ps` shows
    // the new title, `$$` is the process id, and the default
    // $LOAD_PATH tail carries @gem_prelude_index from sitelibdir on.
    run_test_once(
        r#"
        res = []
        res << ($$ == Process.pid)
        title = "mrb-title-test-#{$$}"
        $0 = title
        res << $0
        # The argv rewrite is Linux-only (/proc); on other platforms
        # compare a constant so monoruby and CRuby still agree.
        res << (RUBY_PLATFORM.include?("linux") ? `ps -ocommand= -p#{$$}`.include?(title) : true)
        begin
          eval("$$ = 1")
        rescue NameError => e
          res << e.message
        end
        require "rbconfig"
        idx = $:.index(RbConfig::CONFIG['sitelibdir'])
        res << !idx.nil?
        res << $:[idx..-1].all? { |p| p.instance_variable_defined?(:@gem_prelude_index) }
        res << $:[0...idx].all? { |p| !p.instance_variable_defined?(:@gem_prelude_index) }
        res.map(&:to_s).join("|").sub(title, "TITLE")
        "#,
    );
}
