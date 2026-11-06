//! What ruby-bench's application benchmarks (shipit, ruby-lsp, liquid-c,
//! graphql-native) ran into, each pinned against CRuby.

extern crate monoruby;
use monoruby::tests::*;

/// The `$~` a block sees inside `scan` / `gsub` / `sub`, and the one
/// `String#=~` leaves, names its Regexp: `$~[:name]` and `$~.names`
/// (ActionView's dependency tracker reads `Regexp.last_match[:dynamic]`
/// inside a `scan` block).
#[test]
fn match_data_in_blocks_names_its_regexp() {
    run_test(
        r#"
        r = []
        "ab ab".scan(/(?<x>a)(?<y>b)/) { |a, b| r << [:scan, $~.names, $~[:y], $~.pre_match] }
        "ab ab".gsub(/(?<x>a)b/) { r << [:gsub, $~[:x], $~.regexp.source]; "w" }
        "ab ab".sub(/(?<x>a)b/) { r << [:sub, $~[:x]]; "w" }
        "ab ab".gsub!(/(?<x>a)b/) { r << [:gsub!, $~[:x]]; "w" }
        "ab ab".gsub("ab") { r << [:gsub_str, $~.names, $~.regexp.source]; "" }
        "ab ab".scan("ab") { r << [:scan_str, $~.regexp.source] }
        "ab ab".scan(/(?<x>a)b/) { "q" =~ /(?<y>q)/ }
        r << [:after_scan, $~.names]
        "ab ab".gsub(/(?<x>a)b/) { "q" =~ /(?<y>q)/; "w" }
        r << [:after_gsub, $~.names]
        "ab" =~ /(?<x>a)/
        r << [:match_op, $~.names, $~[:x]]
        re = /(?<z>b)/
        "ab" =~ re
        r << [:match_op_var, $~.names, $~[:z]]
        r
        "#,
    );
}

/// `String#to_s` / `#to_str` of a subclass instance is a plain String
/// copy, made without calling the subclass's methods: SafeBuffer's `[]`
/// answers a SafeBuffer, so building the copy with `self[0, length]` made
/// SafeBuffer#gsub (`to_str.gsub`) recurse until the stack overflowed.
#[test]
fn string_subclass_to_str_is_a_plain_string() {
    run_test(
        r#"
        class SbStr < String
          def [](*a) = SbStr.new(super)
          def gsub(*a, &b) = to_str.gsub(*a, &b)
        end
        s = SbStr.new("héllo")
        t = s.to_str
        b = SbStr.new("\xff".b)
        f = SbStr.new("abc").freeze
        [t.class, t, t.encoding.name, t.equal?(s), s.gsub("l", "L"),
         b.to_s.encoding.name, b.to_s.valid_encoding?, f.to_s.frozen?, f.to_s.class]
        "#,
    );
}

/// `super` in a `define_method` body searches under the name the body
/// was called as, also when JIT code turned `send(:name, ...)` into a
/// direct call of the target (the body was entered from the `send` call
/// site), or looked a Symbol, String or splatted name up at run time.
/// active_model_serializers defines `_render_option_json` and
/// `_render_with_renderer_json` from one block; Rails calls the second
/// through `send`.
#[test]
fn define_method_super_through_inlined_send() {
    run_test(
        r#"
        module SupBase
          def sup_b(x, o) = [:base_b, x]
          def sup_go(o)
            a = [o[:m], 5, o]
            [send(:sup_b, o[:j], o), __send__(o[:m], 3, o), send(*a), send(o[:s], 7, o),
             send(*[o[:s], 9, o])]
          end
        end
        module SupSer
          def sup_build(r) = r.odd? ? nil : r
          [:sup_a, :sup_b].each do |m|
            define_method m do |r, o|
              s = sup_build(r)
              s ? super(s, o) : super(r, o)
            end
          end
        end
        class SupC
          include SupBase
          include SupSer
        end
        c = SupC.new
        res = nil
        50.times { |i| res = [c.sup_go({j: i, m: :sup_b, s: "sup_b"}), c.sup_b(i, {})] }
        res
        "#,
    );
}

/// A unit whose specialized callee calls `super` is salvaged when the
/// class version moves for an unrelated reason (an `extend`, which Loofah
/// does to every node it scrubs) instead of being recompiled each time —
/// shipit filled the JIT's code space that way — and still sees a real
/// redefinition of the `super` target.
#[test]
fn super_in_inlined_callee_survives_unrelated_extend() {
    run_test(
        r#"
        class SupSalvA; def foo(x) = x + 1; end
        class SupSalvB < SupSalvA; def foo(x) = super(x) * 2; end
        module SupSalvM; def m = 1; end
        def sup_salv_run(b) = b.foo(3)
        b = SupSalvB.new
        r = []
        30.times { r << sup_salv_run(b) }
        30.times { Object.new.extend(SupSalvM); r << sup_salv_run(b) }
        class SupSalvA; def foo(x) = x + 10; end
        30.times { r << sup_salv_run(b) }
        class SupSalvA; def foo(x) = x + 1; end
        r.tally
        "#,
    );
}

/// `dup` / `clone` keep every instance variable of an object that got a
/// singleton class before some of them were set: those live past the
/// real class's layout, in the singleton class's ivar table
/// (state_machines' `Machine#initialize_copy` read `nil` for them).
#[test]
fn dup_and_clone_keep_ivars_set_after_singleton_class() {
    run_test(
        r#"
        class DupIvK
          def initialize; @a = 1; end
        end
        o = DupIvK.new
        o.singleton_class
        o.instance_variable_set(:@b, 2)
        o.instance_variable_set(:@c, 3)
        d = o.dup
        c = o.clone
        d.instance_variable_set(:@e, 5)
        [d.instance_variables, d.instance_variable_get(:@c), d.instance_variable_get(:@e),
         c.instance_variables, c.instance_variable_get(:@b), c.singleton_class == o.singleton_class]
        "#,
    );
}

/// `MonitorMixin#synchronize` is defined once, in Ruby, so its
/// `source_location` is known (redis 4.8 computes the stdlib path from
/// it at load time). It is `builtins/monitor.rb`'s, so the path differs
/// from CRuby's.
#[test]
fn monitor_mixin_has_a_source_location() {
    run_test_once(
        r#"
        require "monitor"
        loc = MonitorMixin.instance_method(:synchronize).source_location
        [loc.nil?, loc[1].is_a?(Integer)]
        "#,
    );
}

/// `PTY.spawn` / `PTY.open` (shipit runs its commands on a pty):
/// the child's stdout is the slave, the reader sees its output and then
/// `Errno::EIO` on Linux once it exits, and both ends are ttys.
#[test]
fn pty_spawn_and_open() {
    run_test_once_live(
        r#"
        require "pty"
        r, w, pid = PTY.spawn("printf", "hi\\n")
        out = +""
        err = nil
        begin
          loop { out << r.readpartial(100) }
        rescue Errno::EIO, EOFError => e
          err = e.class
        end
        Process.wait(pid)
        status = $?.exitstatus
        ttys = PTY.open { |m, s| [m.tty?, s.tty?, s.path.start_with?("/dev/"), m.sync, s.sync] }
        sh = nil
        ret = PTY.spawn("sh", "-c", "tty -s && echo tty") { |rr, ww, pp|
          s = +""
          begin; loop { s << rr.readpartial(100) }; rescue Errno::EIO, EOFError; end
          sh = s
        }
        [ret, out, err, status, r.tty?, w.tty?, ttys, sh, PTY::ChildExited.ancestors.include?(RuntimeError)]
        "#,
    );
}

/// `PTY.spawn` takes spawn's redirects too, and one listed ahead of the
/// pty's own (`3 => w` here) leaves the slave on the child's stdin,
/// stdout and stderr: the child reaches both the pty and the pipe.
#[test]
fn pty_spawn_with_an_extra_redirect() {
    run_test_once_live(
        r#"
        require "pty"
        r, w = IO.pipe
        rr, ww, pid = PTY.spawn("sh", "-c", "echo pty; echo fd3 >&3", 3 => w)
        w.close
        out = +""
        begin; loop { out << rr.readpartial(100) }; rescue Errno::EIO, EOFError; end
        Process.wait(pid)
        [out, r.read, $?.exitstatus]
        "#,
    );
}

/// `tty?` asks isatty(3) for a File (a pty's slave is one, `/dev/null`
/// is not) and answers false for a pipe; `readpartial` on a pipe returns
/// what is there and raises EOFError at its end.
#[test]
fn io_tty_and_readpartial_on_files_and_pipes() {
    run_test_once(
        r#"
        io = IO.popen(["echo", "hi"])
        r = [io.tty?, io.readpartial(10)]
        begin
          io.readpartial(10)
        rescue EOFError => e
          r << e.class
        end
        io.close
        File.open("/dev/null") { |f| r << f.tty? }
        r
        "#,
    );
}
