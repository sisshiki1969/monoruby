//! Method lookup made *for a call site* versus one made *by a builtin*
//! (#1998, #2029).
//!
//! A refinement reaches the call sites written in the scope that
//! activated it and nothing else. The methods a builtin calls on its
//! operands (`<=>` under `sort`, `coerce` under `1 + x`, `to_ary` under a
//! destructuring, `inspect` under `p`) are CRuby's `rb_funcall`, which no
//! `using` reaches; and the call sites inside monoruby's Ruby-written
//! core (`Array#each`'s own `size`) are C there.

extern crate monoruby;
use monoruby::tests::*;

/// How often a probe runs: past the JIT thresholds of a test build, so
/// the interpreter and the compiled code both answer.
fn runs() -> usize {
    if cfg!(feature = "gc-stress") { 2 } else { 20 }
}

/// The reproduction of #1998.
#[test]
fn builtin_operand_calls_see_no_refinement() {
    run_test_once(
        r##"
        class R1998; attr_reader :v; def initialize(v) = @v = v; end
        module M1998
          refine R1998 do
            def <=>(o) = v <=> o.v
            def coerce(o) = [1, 2]
            def to_ary = [1, 2]
            def to_a = [9]
            def to_s = "refined"
            def inspect = "refined-inspect"
            def =~(o) = true
            def succ = R1998.new(v + 1)
          end
        end
        using M1998
        r = R1998.new(2); r1 = R1998.new(1)
        t = ->(&b) { b.call rescue $!.class }
        [
          t.() { [r, r1].sort.map(&:v) },
          t.() { [r, r1].min.v },
          t.() { [r, r1].sort_by { |x| x }.map(&:v) },
          t.() { 1 + r },
          t.() { [r].sum(0) },
          t.() { "x" =~ r },
          t.() { (r1..r).to_a.size },
          t.() { a, b = r; [a, b] == [1, 2] },
          t.() { [*r] == [9] },
          t.() { [r].inspect == "[refined-inspect]" },
          t.() { Struct.new(:a).new(r).inspect == "#<struct a=refined-inspect>" },
          # Written in the scope, these do see it.
          "#{r}", r.to_s, r.inspect, (r <=> r1), (r =~ 1),
        ]
        "##,
    );
}

/// ~240 probes of the same kind, each run repeatedly: conversions,
/// comparisons, coercions, `inspect` / `to_s`, block-argument and
/// pattern destructuring, `send` and its relatives, on an object whose
/// class has all of those refined.
#[test]
fn funcall_probes() {
    let body = include_str!("refinements/funcall_probes.rb");
    run_test_once(&format!("N1998 = {}\n{body}", runs()));
}

/// The reproductions of #2029: the Ruby-written core's own call sites.
#[test]
fn core_call_sites_see_no_refinement() {
    run_test_once(
        r##"
        module R2029
          refine Array do
            def size(*) = :refined_size
            def empty? = :refined_empty
          end
          refine Integer do
            def zero? = :refined_zero
            def succ = :refined_succ
          end
        end
        using R2029
        t = ->(&b) { b.call rescue $!.class }
        res = []
        20.times do
          res << [
            t.() { p 1 },
            t.() { [1, 2].map { |e| e } },
            t.() { [1, 2].each_slice(1).to_a },
            t.() { [1, 2].cycle(1).to_a },
            t.() { [1, 2].combination(1).to_a },
            t.() { [1, 2].each_with_index.to_a },
            t.() { 1.upto(3).to_a },
            # The scope's own call sites.
            [1, 2].size, [].empty?, 0.zero?, 1.succ,
          ]
        end
        res.uniq
        "##,
    );
}

/// The names [`core_call_sites_poisoned`] refines to return `:poison`.
#[rustfmt::skip]
const POISON_NAMES: &[&str] = &[
    "+", "-", "*", "/", "%", "**", "==", "!=", "<", "<=", ">", ">=", "<=>", "===", "=~",
    "[]", "[]=", "<<", ">>", "!", "-@", "+@", "~", "&", "|", "^", "size", "length",
    "empty?", "each", "each_with_index", "each_pair", "each_key", "each_value",
    "each_entry", "each_slice", "each_cons", "each_char", "each_byte", "each_line", "map",
    "collect", "flat_map", "select", "filter", "reject", "find", "detect", "include?",
    "member?", "first", "last", "push", "pop", "shift", "unshift", "append", "prepend",
    "concat", "dup", "clone", "freeze", "frozen?", "to_a", "to_ary", "to_h", "to_hash",
    "to_s", "to_str", "to_sym", "to_i", "to_int", "to_f", "to_r", "to_c", "to_proc",
    "to_enum", "enum_for", "inspect", "hash", "eql?", "equal?", "nil?", "is_a?",
    "kind_of?", "instance_of?", "respond_to?", "class", "zero?", "positive?", "negative?",
    "succ", "pred", "next", "abs", "min", "max", "key?", "has_key?", "fetch", "keys",
    "values", "delete", "store", "merge", "update", "replace", "clear", "compact",
    "flatten", "reverse", "sort", "sort_by", "uniq", "join", "slice", "take", "drop",
    "count", "sum", "inject", "reduce", "times", "upto", "downto", "step", "call", "yield",
    "arity", "lambda?", "curry", "begin", "end", "exclude_end?", "cover?", "rewind",
    "peek", "with_index", "with_object", "lazy", "force", "zip", "any?", "all?", "none?",
    "one?", "tally", "group_by", "partition", "min_by", "max_by", "minmax", "entries",
    "block_given?", "raise", "loop", "lambda", "proc", "tap", "then", "itself", "send",
    "__send__", "public_send", "method", "instance_variable_get", "instance_variable_set",
    "object_id", "__id__", "divmod", "div", "modulo", "fdiv", "floor", "ceil", "round",
    "truncate", "integer?", "finite?", "infinite?", "nan?", "coerce", "even?", "odd?",
    "bytesize", "getbyte", "setbyte", "encoding", "force_encoding", "ascii_only?",
    "start_with?", "end_with?", "index", "rindex", "sub", "gsub", "tr", "split", "chars",
    "bytes", "lines", "strip", "chomp", "upcase", "downcase", "ord", "chr", "unpack",
    "pack", "format", "sprintf", "Integer", "Float", "Array", "String", "Rational",
    "Complex", "Hash", "name", "instance_exec", "instance_eval", "extend",
    "compare_by_identity", "default", "assoc", "dig", "values_at", "at", "insert", "fill",
    "rotate", "sample", "shuffle", "product", "combination", "permutation", "transpose",
    "bsearch", "cycle", "chunk_while", "slice_when", "each_with_object", "find_index",
    "take_while", "drop_while", "filter_map", "write", "read", "puts", "print", "p", "pp",
    "flush", "key", "value", "members", "to_set", "add", "delete_if", "keep_if", "select!",
    "reject!", "map!", "collect!", "sort!", "uniq!", "compact!", "flatten!", "reverse!",
    "shuffle!", "real", "imaginary", "numerator", "denominator", "rationalize",
    "bit_length", "digits", "pow", "gcd", "lcm", "between?", "clamp", "exception",
    "message", "backtrace", "full_message", "set_backtrace", "method_missing",
    "respond_to_missing?",
];

/// The classes and modules it refines them for.
#[rustfmt::skip]
const POISON_CLASSES: &[&str] = &[
    "Object", "Array", "Hash", "Integer", "Float", "String", "Symbol", "Range", "NilClass",
    "TrueClass", "FalseClass", "Proc", "Enumerator", "Comparable", "Enumerable", "Kernel",
    "Rational", "Struct",
];

/// One scope of [`core_call_sites_poisoned`]: the names its probes write
/// themselves (left unrefined), and the probes.
struct PoisonBatch {
    written: &'static [&'static str],
    probes: &'static [&'static str],
}

#[rustfmt::skip]
const POISON_BATCHES: &[PoisonBatch] = &[
    PoisonBatch {
        written: &[
            "!", "-", "-@", "/", ">", "begin", "call", "class", "combination", "divmod",
            "drop_while", "dup", "each", "each_cons", "end", "entries", "first", "lazy",
            "map", "max", "message", "prepend", "slice_when", "step", "sum", "to_a",
            "to_proc", "uniq", "values_at", "with_object", "|",
        ],
        probes: &[
            r#"[1, 2].map { |e| e }"#,
            r#"[1, 2, 3].combination(2).to_a"#,
            r#"[1, 2, 3].values_at(0, 2, 5)"#,
            r#"[1, 2, 4].slice_when { |a, b| true }.to_a"#,
            r#"[2, 1].lazy.drop_while { |e| nil }.to_a"#,
            r#"[2, 1].each.with_object(:a).to_a"#,
            r#"1.0.step(2.0, 0.5).to_a"#,
            r#"5.polar"#,
            r#"(1r/2).divmod(1)"#,
            r#"(1..3).max(2)"#,
            r#"(1..3).each_cons(2).to_a"#,
            r#"(1..3).entries"#,
            r#"(1..3).uniq"#,
            r#""abc".dup.prepend("d", "e")"#,
            r#":abc.to_proc.call("x") rescue $!.class"#,
            r#"(begin; fail "x"; rescue => e; e.message; end)"#,
            r#"(begin; [1].first(-1); rescue => e; e.message; end)"#,
            r#"[1, 2].sum(0.0)"#,
            r#"File.dirname("/a/b")"#,
        ],
    },
    PoisonBatch {
        written: &[
            "*", "+", "+@", ">", ">>", "[]", "begin", "call", "chunk_while", "clamp",
            "each_cons", "end", "fetch", "inject", "lazy", "map", "message", "method",
            "min_by", "partition", "size", "step", "sum", "times", "to_a", "uniq",
            "with_index", "|",
        ],
        probes: &[
            r#"[1, 2].reverse_each.to_a"#,
            r#"[1, 2].repeated_permutation(2).to_a"#,
            r#"[1, 2, 3].each_cons(2).to_a"#,
            r#"[2, 1.5].sum"#,
            r#"[2, 1].lazy.uniq.to_a"#,
            r#"[2, 1].map.with_index { |e, i| i }"#,
            r#"5.ceildiv(2)"#,
            r#"5.clamp(1, 3)"#,
            r#"(1..3).reverse_each.to_a"#,
            r#"(1.0..2.0).step(0.5).to_a"#,
            r#"(1..3).inject(:+)"#,
            r#"(1..3).min_by { |i| i }"#,
            r#"(1..3).chunk_while { |a, b| true }.to_a"#,
            r#""abc".partition("b")"#,
            r#"1.step(3).to_a"#,
            r#"(begin; [].fetch(1); rescue => e; e.message; end)"#,
            r#"(1.method(:+) >> 2.method(:*)).call(3)"#,
            r#"[[1, 2]].map { |a, b| b }"#,
            r#"1.times.size"#,
        ],
    },
    PoisonBatch {
        written: &[
            "%", "/", ">", ">=", "Integer", "[]", "[]=", "any?", "begin", "bsearch",
            "curry", "each_byte", "end", "first", "force", "lazy", "lcm", "message",
            "none?", "numerator", "proc", "reject", "select", "step", "take", "to_a", "|",
        ],
        probes: &[
            r#"[1, 2, 3].bsearch_index { |x| x >= 2 }"#,
            r#"[1, 2, 3].fetch_values(0, 2)"#,
            r#"[1, 2].none?"#,
            r#"[2, 1].lazy.select { |e| e }.first(1)"#,
            r#"[2, 1].lazy.force"#,
            r#"Enumerator.produce(1) { |x| x }.first(2)"#,
            r#"6.lcm(4)"#,
            r#"1.5.numerator"#,
            r#"(1..3).reject { |i| i }"#,
            r#"(1..3).to_a"#,
            r#"(1..3).take(2)"#,
            r#"(1..3).any? { |i| i }"#,
            r#"test(?e, "/")"#,
            r#""abc".each_byte { |b| b }"#,
            r#"1.step(3, 0.5).to_a"#,
            r#"(begin; Integer("z"); rescue => e; e.message; end)"#,
            r#"proc { |a, b| [a, b] }.curry[1][2]"#,
            r#"[1, 2].bsearch { |x| x >= 1 }"#,
            r#""%s" % [[1, 2]]"#,
        ],
    },
    PoisonBatch {
        written: &[
            "&", "==", ">", "assoc", "begin", "clamp", "cycle", "each_char", "end",
            "filter_map", "first", "inspect", "lazy", "map", "message", "size", "step",
            "sum", "take", "to_a", "to_r", "to_s", "upto", "|",
        ],
        probes: &[
            r#"[1, 2].filter_map { |e| e }"#,
            r#"[[1, 2], [3, 4]].assoc(3)"#,
            r#"[2, 1].minmax_by { |e| e }"#,
            r#"[2, 1].lazy.take(1).to_a"#,
            r#"[2, 1].chain([3]).size"#,
            r#"1.upto(3).to_a"#,
            r#"6.to_r"#,
            r#"1.5.clamp(1, 2)"#,
            r#"(1..3).sum"#,
            r#"(1..3).size"#,
            r#"(1..).lazy.map { |i| i }.first(2)"#,
            r#"(1..3).cycle(1).to_a"#,
            r#"nil.inspect"#,
            r#""abc".each_char.to_a"#,
            r#"1.step(3) == 1.step(3)"#,
            r#"(begin; def self.mm(a) = a; mm; rescue => e; e.message; end)"#,
            r#"[1, 2].to_s"#,
            r#"[1, 2].map(&:to_s)"#,
        ],
    },
];

/// Every core class gets a refinement that makes some 280 method names
/// (`size`, `each`, `+`, `==`, `respond_to?`, `raise`, `block_given?`, …)
/// return `:poison`, activated in the scope a batch of core-library calls
/// is written in. None of those calls may notice: what `Array#each` or
/// `Enumerable#each_slice` call on the way is the library's business.
#[test]
fn core_call_sites_poisoned() {
    let mut code = format!(
        r##"
        $res2029 = []
        module T2029
          def self.t(label)
            rs = []
            {}.times do
              r = begin
                yield
              rescue Exception => e
                e.class
              end
              rs << r.inspect
            end
            $res2029 << [label, rs.uniq]
          end
        end
        "##,
        runs()
    );
    for (i, batch) in POISON_BATCHES.iter().enumerate() {
        code += &format!("module Poison2029_{i}\n");
        for class in POISON_CLASSES {
            code += &format!("refine {class} do\n");
            for name in POISON_NAMES.iter().filter(|n| !batch.written.contains(n)) {
                code += &format!("def {name}(*a, **k, &b); :poison; end\n");
            }
            code += "end\n";
        }
        code += &format!("end\nmodule G2029_{i}\nusing Poison2029_{i}\n");
        for probe in batch.probes {
            code += &format!("T2029.t({probe:?}) {{ {probe} }}\n");
        }
        code += "end\n";
    }
    code += "$res2029\n";
    run_test_once(&code);
}

/// `Foo.new(...)` written at a call site is an allocation followed by a
/// call of `initialize` from that site (CRuby 4.0's `opt_new`), so the
/// scope's refinement of `initialize` applies — but not when a block is
/// passed, and not to `Class#new` reached any other way.
#[test]
fn new_calls_the_refined_initialize_of_its_site() {
    run_test_once(
        r##"
        class FooNew1998
          attr_reader :v
          def initialize(v = 0, k: 0) = @v = [:orig, v, k]
        end
        class BarNew1998 < FooNew1998; end
        module MNew1998
          refine FooNew1998 do
            def initialize(v = 0, k: 0, &b) = @v = [:refined, v, k, b&.call]
          end
          refine Array do
            def initialize(*) = raise("not reached")
          end
        end
        def plain_new1998 = FooNew1998.new(1).v
        res = [plain_new1998]
        using MNew1998
        def hot_new1998(n) = FooNew1998.new(n).v
        k = FooNew1998
        30.times do |i|
          res << [
            FooNew1998.new(1).v,
            FooNew1998.new(1, k: 2).v,
            FooNew1998.new(1, k: 2) { 5 }.v,
            BarNew1998.new(3).v,
            FooNew1998.send(:new, 1).v,
            FooNew1998.public_send(:new, 1).v,
            FooNew1998.method(:new).call(1).v,
            [1, 2].map { |x| FooNew1998.new(x).v },
            [1, 2].map(&FooNew1998.method(:new)).map(&:v),
            hot_new1998(7),
            plain_new1998,
            k.new(8).v,
            FooNew1998.new(*[1]).v,
            Class.new(FooNew1998).new(4).v,
            Array.new(2, 0),
          ]
        end
        res.uniq
        "##,
    );
}

/// An operator written in the scope finds the scope's refinement of it,
/// for the receivers the VM otherwise answers without a lookup too.
#[test]
fn refined_operators_at_the_site() {
    run_test_once(
        r##"
        $log1998 = []
        module POp1998
          refine Symbol do
            def ==(o) = ($log1998 << [:sym_eq, o]; true)
            def +(o) = :plus
            def <(o) = :lt
          end
          refine Integer do
            def ==(o) = :int_eq
            def +(o) = :int_plus
          end
          refine Array do
            def size = :psize
            def [](*) = :pidx
          end
          refine Struct do
            def deconstruct = [7, 8]
          end
        end
        using POp1998
        s3 = Struct.new(:a, :b)
        res = []
        20.times do
          x = :a
          d = [1, 2]
          res << [
            :a == 2, x == 2, :a.==(2), :a != 2, :a + 1, :a < 1, 1 == 2, 1 + 2,
            d.size == 2, ((d.size == 2) && 5), (d.size == 2 ? 1 : 2),
            # A pattern's own `size`, `==` and `[]` are call sites of the
            # scope it is written in.
            (case s3.new(1, 2); in [a, b] then [a, b]; end),
            [:a, :b].include?(:b), [1, 2].index(2), [[1, 2]].flatten,
          ]
        end
        [res.uniq, $log1998.uniq]
        "##,
    );
}

/// What CRuby calls with `CALL_PUBLIC`. `public_send` and `&:sym` look
/// the method up as a call written in the nearest Ruby frame is: under
/// that scope's refinements, public methods only. `rb_funcallv_public`
/// (`inject(:sym)`) asks the class first and the refinements only when
/// the class has no public method of that name — and its nearest Ruby
/// frame is the receiver's own `each` under `Enumerable#inject`.
#[test]
fn public_funcalls() {
    run_test_once(
        r##"
        class BasePub1998; def sup(o = nil) = :plain_sup; end
        class RPub1998 < BasePub1998
          def own(o = nil) = :plain_own
          private def priv(o = nil) = :plain_priv
          protected def prot(o = nil) = :plain_prot
          private def add(o) = 1
        end
        class EnPub1998; include Enumerable; def each; yield RPub1998.new; yield RPub1998.new; end; end
        module MPub1998
          refine RPub1998 do
            def own(o = nil) = :ref_own
            def sup(o = nil) = :ref_sup
            def only(o = nil) = :ref_only
            def priv(o = nil) = :ref_priv
            def prot(o = nil) = :ref_prot
          end
        end
        using MPub1998
        r = RPub1998.new
        t = ->(&b) { b.call rescue $!.class }
        res = []
        20.times do
          %i[own sup only priv prot add].each do |m|
            res << [
              m,
              t.() { [r, 1].inject(m) },
              t.() { [1].inject(r, m) },
              t.() { EnPub1998.new.inject(m) },
              t.() { EnPub1998.new.inject(r, m) },
              t.() { r.public_send(m) },
              t.() { r.send(m) },
              t.() { r.__send__(m) },
              t.() { [r].map(&m) },
              t.() { r.respond_to?(m) },
              t.() { r.method(m).call },
              t.() { m.to_proc.call(r) },
              t.() { r.instance_eval { public_send(m) } },
            ]
          end
          res << [t.() { [1, 2].inject(:puts) }, t.() { defined?(r.only) }, t.() { defined?(r.nope) }]
        end
        res.uniq
        "##,
    );
}

/// The C API's `rb_check_funcall` / `rb_check_convert_type` probe as the
/// call that follows them is made: a private conversion method counts,
/// a refined one does not.
#[test]
fn capi_check_funcall_is_refinement_blind() {
    run_test_once(
        r##"
        require "stringio"
        class RCapi1998; end
        class PCapi1998
          private
          def to_ary = [1, 2]
          def to_str = "x"
        end
        module MCapi1998
          refine RCapi1998 do
            def to_ary = [1, 2]
            def to_s = "refined"
            def to_str = "refined"
          end
        end
        using MCapi1998
        t = ->(&b) { b.call rescue $!.class }
        [
          t.() { s = StringIO.new; s.puts(RCapi1998.new); s.string.sub(/0x\h+/, "XX") },
          t.() { s = StringIO.new; s.print(RCapi1998.new); s.string.sub(/0x\h+/, "XX") },
          t.() { s = StringIO.new; s.write(RCapi1998.new); s.string.sub(/0x\h+/, "XX") },
          t.() { s = StringIO.new("abc"); s.ungetc(RCapi1998.new); s.string },
          t.() { s = StringIO.new; s.puts(PCapi1998.new); s.string },
          t.() { s = StringIO.new("abc"); s.ungetc(PCapi1998.new); s.string },
        ]
        "##,
    );
}
