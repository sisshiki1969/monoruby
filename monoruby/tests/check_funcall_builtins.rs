extern crate monoruby;
use monoruby::tests::*;

// Issue #1985: the builtins written in Ruby asked `obj.respond_to?(:to_x)`
// before converting, which never consults a `method_missing` that answers
// the conversion. They now go through the `rb_check_funcall` helpers
// (`__check_ary`, `__check_str`, `__to_int`, `__num2dbl`,
// `__check_funcall`, ...), as CRuby's C does.
//
// Each section runs its sites against four objects: one answering the
// conversion from `method_missing` alone (`MM`), one that claims it with
// `respond_to_missing?` and answers the wrong type (`Bad`), an `MM`
// answering nil, and one that does not convert (`Plain`). The sites
// where CRuby uses `rb_respond_to` instead (`each`, `call`, `to_open`,
// `succ`) are pinned too: `method_missing` alone is not enough there.
#[test]
fn builtins_written_in_ruby_convert_through_check_funcall() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        require "tmpdir"
        prev_report = Thread.report_on_exception
        Thread.report_on_exception = false
        def t; r = yield; r.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message.gsub(/0x\h+/, "0x")}"; end
        class MM
          def initialize(n, v) = (@n = n; @v = v)
          def method_missing(n, *a) = n == @n ? @v : super
          def inspect = "#<MM #{@n}>"
        end
        # claims to respond (respond_to_missing?) but the answer is of the wrong type
        class Bad
          def initialize(n, v) = (@n = n; @v = v)
          def respond_to_missing?(n, p) = n == @n
          def method_missing(n, *a) = n == @n ? @v : super
          def inspect = "#<Bad #{@n}>"
        end
        class Plain; def inspect = "#<Plain>"; end
        S = Struct.new(:a, :b)
        D = Data.define(:a, :b)
        Q = [Plain.new]
        def variants(n, good, bad = :wrong)
          [MM.new(n, good), Bad.new(n, bad), MM.new(n, nil), Plain.new]
        end
        variants(:to_ary, [1, 2]).each do |ary|
          row "-- to_ary #{ary.inspect}"
          row t { [[3, 4]].each_entry.zip(ary) }
          row t { {a: 1}.to_h { |k, v| ary } }
          row t { [[1, 2]].each_entry.flat_map { |x| ary } }
          row t { [1, 2, 3].lazy.zip(ary).to_a }
          row t { [[1, 2]].each_entry.to_h { |x| ary } }
          row t { S.new(1, 2).to_h { |k, v| ary } }
          row t { D.new(a: 1, b: 2).to_h { |k, v| ary } }
          row t { [1, 2].lazy.flat_map { |x| ary }.to_a }
          row t { [ary].each_entry.to_h }
          row t { [[1, 2]].to_h { |x| ary } }
          row t { [1].each_entry.zip(ary) { |a| } }
          row t { [ary].each_entry.flat_map { |x| x } }
        end
        variants(:to_str, "b").each do |str|
          row "-- to_str #{str.inspect}"
          row t { ("a".."c").include?(str) }
          row t { "a".upto(str).to_a }
          row t { ["a", "b", "c"].each_entry.grep(/b/) + [str].each_entry.grep(/b/) }
          row t { [str, "x"].each_entry.grep_v(/b/) }
          row t { S.new(1, 2).deconstruct_keys([str]) }
          row t { D.new(a: 1, b: 2).to_h.keys }
          row t { D.new(str => 1, a: 1) rescue "#{$!.class}: #{$!.message}" }
          row t { Thread.current[str] = 1; Thread.current[:b] }
          row t { th = Thread.new { }; th.name = str; th.join; th.name }
          row t { th = Thread.new { Thread.current.thread_variable_set(str, 3); Thread.current.thread_variable_get(:b) }; th.value }
          row t { Dir.mktmpdir { |d| Dir.new(MM.new(:to_str, d)).close; :ok } }
          row t { File.dirname(str) }
          row t { SystemCallError.new(str, 2).message }
          row t { Errno::ENOENT.new(str).message }
          row t { $stdout.dup.tap { |io| io.close }; IO.pipe { |r, w| w.write("xbyb"); w.close; r.gets(str) } }
          row t { e = RuntimeError.new("m"); def e.detailed_message(**) = MM.new(:to_str, "dm"); e.full_message(highlight: false).lines.first.chomp[-4..] }
        end
        variants(:to_sym, :deprecated).each do |sym|
          row "-- to_sym #{sym.inspect}"
          row t { warn("w", category: sym) }
        end
        [MM.new(:to_str, "+"), Bad.new(:to_str, 1), MM.new(:to_str, nil), Plain.new].each do |op|
          row "-- op #{op.inspect}"
          row t { (1..3).inject(op) }
          row t { (1..3).inject(10, op) }
          row t { [1, 2, 3].each_entry.inject(op) }
          row t { [1, 2, 3].each_entry.reduce(10, op) }
        end
        variants(:to_int, 2).each do |int|
          row "-- to_int #{int.inspect}"
          row t { {a: [1, [2]]}.flatten(int) }
          row t { 5.round(int) }
          row t { 15.round(MM.new(:to_int, -1)) }
          row t { 5.allbits?(int) }
          row t { 5.anybits?(int) }
          row t { 5.nobits?(int) }
          row t { Integer.sqrt(int) }
          row t { Integer.try_convert(int) }
          row t { File.dirname("/a/b/c/d", int) }
          row t { [1, 2, 3, 4].lazy.take(int).to_a }
          row t { [1, 2, 3, 4].lazy.drop(int).to_a }
          row t { [1, 2, 3, 4].lazy.first(int) }
          row t { S.new(1, 2).deconstruct_keys([int]) rescue "#{$!.class}: #{$!.message}" }
          row t { Thread::SizedQueue.new(int).max }
          row t { q = Thread::SizedQueue.new(1); q.max = int; q.max }
          row t { Thread.current.priority = int }
          row t { SystemCallError.new("x", int).errno }
          row t { IO.pipe { |r, w| w.putc(MM.new(:to_int, 65)); w.close; r.read } }
          row t { $stderr.dup.tap { |io| io.close }; IO.pipe { |r, w| w.write("abcdef"); w.close; r.gets(int) } }
          row t { [3, 1, 2].each_entry.first(int) }
          row t { [3, 1, 2].each_entry.take(int) }
          row t { [3, 1, 2].each_entry.min(int) }
          row t { [3, 1, 2].each_entry.each_slice(int).to_a }
          row t { [3, 1, 2].each_entry.each_cons(int).to_a }
          row t { [3, 1, 2].each_entry.cycle(int).to_a.size }
          row t { ENV.to_a.size > 0 && [1, 2, 3].each_entry.drop(int) }
          row t { Thread::Queue.new.tap { |q| 3.times { |i| q << i } }.then { |q| [q.num_waiting] } }
          row t { (1..10).step(int).to_a }
          row t { 1.step(10, int).to_a }
          row t { ObjectSpace.respond_to?(:each_object) && [1, 2, 3, 4].each_entry.each_with_index(int).to_a rescue "#{$!.class}" }
        end
        variants(:to_f, 0.01).each do |f|
          row "-- to_f #{f.inspect}"
          row t { Queue.new.tap { |q| q.push(1) }.pop(timeout: f) }
          row t { Thread::SizedQueue.new(1).tap { |q| q.push(1) }.pop(timeout: f) }
          row t { Thread::SizedQueue.new(2).push(1, timeout: f).class }
          row t { 5.coerce(f) }
          row t { 5.fdiv(f).class }
          row t { m = Mutex.new; m.lock; m.sleep(f).class }
          row t { ConditionVariable.new.then { |cv| m = Mutex.new; m.synchronize { cv.wait(m, f) }.class } }
          row t { Thread.new { }.join(f).class }
        end
        variants(:divmod, [0, 0.0]).each do |d|
          row "-- divmod #{d.inspect}"
          row t { sleep(d) }
          row t { IO.select(nil, nil, nil, d) }
        end
        variants(:exception, RuntimeError.new("x")).each do |ex|
          row "-- exception #{ex.inspect}"
          row t { raise ex }
          row t { raise ex, "msg" }
          row t { Thread.new { sleep }.tap { |th| Thread.pass until th.status == "sleep"; th.raise(ex) }.join }
          row t { Fiber.new { Fiber.yield }.tap(&:resume).raise(ex) }
        end
        variants(:to_hash, {a: 1}).each do |h|
          row "-- to_hash #{h.inspect}"
          row t { [:a, :b].each_entry.tally(h) }
          row t { [:a, :b].tally(h) }
          row t { [:a].each_entry.tally_by(h) { |x| x } }
        end
        variants(:each, [1, 2].each).each do |e|
          row "-- each #{e.inspect}"
          row t { [[3, 4]].each_entry.zip(e) }
          row t { [1, 2, 3].lazy.zip(e).first(1) }
          row t { Set.new(e).size }
        end
        variants(:to_path, "/tmp").each do |pa|
          row "-- to_path #{pa.inspect}"
          row t { File.dirname(pa) }
          row t { Dir.new(pa).close }
          row t { open(pa, "r") { |f| f.class } }
          row t { File.basename(pa) }
        end
        variants(:coerce, [10.0, 2.0]).each do |c|
          row "-- coerce #{c.inspect}"
          row t { 7.div(c) }
          row t { 7.fdiv(c) }
          row t { 7.remainder(c) }
          row t { 7.0.remainder(c) }
        end
        variants(:size, 7).each do |sz|
          row "-- size"
        end
        class EnumMM
          include Enumerable
          def each; yield 1; yield 2; end
          def method_missing(n, *a) = n == :size ? 42 : super
        end
        class EnumPlain; include Enumerable; def each; yield 1; yield 2; end; end
        [EnumMM.new, EnumPlain.new].each do |e|
          row "-- size #{e.class}"
          row t { e.each_slice(2).size }
          row t { e.each_with_index.size }
          row t { e.each_with_object([]).size }
          row t { e.map.size }
          row t { e.flat_map.size }
          row t { e.min_by.size }
          row t { e.max_by.size }
          row t { e.group_by.size }
          row t { e.sort_by.size }
          row t { e.reject.size }
          row t { e.each_cons(1).size }
          row t { e.partition.size }
          row t { e.minmax_by.size }
          row t { e.each_entry.size }
          row t { e.reverse_each.size }
          row t { e.cycle.size }
          row t { e.cycle(2).size }
          row t { e.lazy.size }
          row t { e.select.size }
          row t { e.find_index.size }
          row t { e.lazy.map { }.size }
          row t { (e.each_entry + [1]).size }
        end
        variants(:dig, :dug).each do |d|
          row "-- dig #{d.inspect}"
          row t { [d].dig(0, 1) }
          row t { {a: d}.dig(:a, 1) }
          row t { S.new(d, 1).dig(:a, 1) }
        end
        variants(:call, :called).each do |c|
          row "-- call #{c.inspect}"
          row t { (proc { |x| x } >> c).call(1) }
          row t { (proc { |x| x } << c).call(1) }
          row t { (1.method(:+) >> c).call(1) }
          row t { (1.method(:+) << c).call(1) }
          row t { ObjectSpace.define_finalizer(Object.new, c).class }
          row t { set_trace_func(c) rescue "#{$!.class}: #{$!.message}" }
        end
        variants(:write, 3).each do |w|
          row "-- write #{w.inspect}"
          row t { printf(w, "%d", 1) }
        end
        variants(:to_open, :opened).each do |o|
          row "-- to_open #{o.inspect}"
          row t { open(o) }
        end
        variants(:succ, 1).each do |s|
          row "-- succ #{s.inspect}"
        end
        variants(:to_a, [1, 2]).each do |a|
          row "-- to_a #{a.inspect}"
          row t { Thread::Queue.new(a).size }
        end
        $o
        "##,
    );
}

// `Struct#deconstruct_keys` reads a key that is neither a Symbol nor a
// String with NUM2LONG (`rb_struct_pos`); `Data#deconstruct_keys` takes
// Symbols and what `rb_check_id` accepts.
#[test]
fn deconstruct_keys_key_conversion() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; r = yield; r.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message.gsub(/0x\h+/, "0x")}"; end
        class MM
          def initialize(n, v) = (@n = n; @v = v)
          def method_missing(n, *a) = n == @n ? @v : super
          def inspect = "#<MM #{@n}>"
        end
        class TS; def to_str = "a"; end
        class TI; def to_int = 1; end
        D = Data.define(:a, :b)
        S = Struct.new(:a, :b)
        d = D.new(a: 1, b: 2); s = S.new(1, 2)
        [0, 1, -1, 5, TS.new, TI.new, MM.new(:to_str, "a"), MM.new(:to_int, 0), 1.5, nil, "a", :b, "zz"].each do |k|
          row t { d.deconstruct_keys([k]) }
          row t { s.deconstruct_keys([k]) }
        end
        $o
        "##,
    );
}

// `Integer#remainder` / `Float#remainder` are `int_remainder` /
// `num_remainder`: an operand that is not a Numeric is coerced first
// (through `rb_check_funcall`), and an infinite Float divisor of the
// other sign answers the receiver.
#[test]
fn remainder_follows_num_remainder() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; r = yield; r.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
        class Co; def coerce(o) = [10.0, 3.0]; end
        class Plain; end
        [3, -3, 2.5, -2.5, 2**70, -(2**70), Rational(7, 2), Co.new, Plain.new, nil, "a", 0, 0.0, Float::INFINITY, -Float::INFINITY, Float::NAN].each do |y|
          [7, -7, 7.5, -7.5, 2**70 + 1, 0, 0.0].each do |x|
            row "#{x.inspect}.remainder(#{y.class}) #{t { x.remainder(y) }}"
          end
        end
        $o
        "##,
    );
}

// `warn(category:)` is `rb_to_symbol_type` then `Warning[category]`, which
// refuses an unknown category before the messages are looked at;
// `printf` takes any first argument that is not a String as the stream.
#[test]
fn warn_category_and_printf_stream() {
    run_test2(
        r##"
        # The rows are collected instead of printed: the harness compares
        # the value of the whole script.
        $o = []
        def row(*a) = a.each { |x| $o << x.to_s }
        def rowp(*a) = a.each { |x| $o << x.inspect }
        def t; r = yield; r.inspect; rescue Exception => e; "#{e.class}: #{e.message}"; end
        rowp t { warn("w", category: :wrong) }
        rowp t { Warning[:wrong] }
        rowp t { warn("w", category: "deprecated") }
        rowp t { warn(category: :wrong) }
        rowp t { $VERBOSE = nil; warn("w", category: :wrong) }
        rowp t { printf(nil) }
        rowp t { printf() }
        rowp t { printf(nil, "a") }
        $o
        "##,
    );
}
