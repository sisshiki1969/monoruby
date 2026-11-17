//! `$/` keeping the assigned object, a module method bound to a receiver
//! that does not include the module, `Time` field ranges and hour 24,
//! Complex arithmetic over parts that are not native numbers, Array
//! growth the allocator refuses, and the tail of `rb_check_funcall`
//! (#2051, #2056, #2055, #2043, #2032, #2054, part of #2033).
//!
//! - **`$/` / `$-0`** (#2051). As `deprecated_rs_setter`: a String equal
//!   to the default `"\n"` is replaced by the default separator object,
//!   a frozen plain String without instance variables is kept as it is,
//!   and anything else is copied and frozen.
//! - **`UnboundMethod#bind`** (#2056). A method of a module bound to a
//!   receiver whose class does not include it is inspected by its owner
//!   (`#<Method: MiM#mm() …>`), and two such Methods are not `==`.
//!   `Method#to_proc.inspect` has no source location.
//! - **`Time`** (#2055). Hour 24 is midnight of the next day when the
//!   minutes and seconds are zero; a field out of range names itself
//!   (`mon out of range`, …), and one that does not fit `obj2ubits`'
//!   bit width is `argument out of range`.
//! - **`Complex`** (#2043). `+ - * **` keep parts that are Rationals
//!   (or any other non-native Numeric) by dispatching on the parts;
//!   `Complex(str, x)` parses a String on either side; `fdiv` goes
//!   through `f_divide` like `/`.
//! - **Array growth** (#2032). A store, splice, `insert` or `fill` that
//!   would need more memory than the allocator gives is NoMemoryError
//!   (or IndexError / ArgumentError at the Array size limit), not an
//!   abort; `product` without a block is limited by the Array size
//!   only, and not at all with one.
//! - **`rb_check_funcall`** (#2054). A `respond_to?` that claims a
//!   method which does not exist is "does not answer", a
//!   `respond_to_missing?` that refuses stops before `method_missing`
//!   whatever `respond_to?` said. (The C API side is in
//!   `tests/capi/capi_value.rb`.)
//! - **Uncaught-exception report** (#2033). A `detailed_message` that
//!   raises, or answers a non-String or an empty String, is reported as
//!   the bare class name; a cause whose `detailed_message` raises ends
//!   the report.

use monoruby::tests::*;

const HELPER: &str = r##"
  t = ->(&b) { begin; b.call.inspect.gsub(/0x\h+/, "0x"); rescue Exception => e; "#{e.class}: #{e.message}".gsub(/0x\h+/, "0x"); end }
"##;

fn probe(prelude: &str, exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t.() {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n{prelude}\n[{list}]"));
}

/// The examples of #2051.
#[test]
fn rs_setter_keeps_or_copies() {
    run_test_once(
        r####"
        verbose, $VERBOSE = $VERBOSE, nil
        out = []
        default = $/
        begin
          str = "\n".dup.freeze
          $/ = str
          out << [$/.equal?(str), $/.equal?(default), $/.frozen?, $/ == "\n"]
          $-0 = str
          out << $-0.equal?(default)
          s2 = "abc".freeze
          $/ = s2
          out << $/.equal?(s2)
          s3 = +"abc"
          $/ = s3
          out << [$/.equal?(s3), $/.frozen?, $/]
          s4 = +"\n"
          $/ = s4
          out << [$/.equal?(s4), $/.equal?(default), $/.frozen?]
          klass = Class.new(String)
          s5 = klass.new("\n").freeze
          $/ = s5
          out << [$/.equal?(s5), $/.equal?(default), $/.class]
          s6 = klass.new("x").freeze
          $/ = s6
          out << [$/.equal?(s6), $/.class, $/]
          s7 = "y".dup; s7.instance_variable_set(:@a, 1); s7.freeze
          $/ = s7
          out << [$/.equal?(s7), $/.instance_variables, $/]
          $/ = nil
          out << $/
          out << (begin; $/ = 1; rescue => e; [e.class, e.message]; end)
          out << (begin; $/ = "\n".encode("UTF-16LE"); [$/.encoding.to_s, $/.bytes]; rescue => e; [e.class, e.message]; end)
          $/ = "\n"
          out << [$/.equal?(default), $/.encoding.to_s]
        ensure
          $/ = "\n"
          $VERBOSE = verbose
        end
        out
        "####,
    );
}

/// The examples of #2056.
#[test]
fn module_method_bound_outside_the_module() {
    probe(
        r####"
        module MiM; def mm = 1; end
        class MiC; end
        class MiD; include MiM; end
        class MiE < MiD; end
        loc = ->(s) { s.sub(/ \S+:\d+>\z/, " LOC>") }
        "####,
        &[
            "loc.(MiM.instance_method(:mm).bind(MiC.new).inspect)",
            "loc.(MiM.instance_method(:mm).bind(3).inspect)",
            "loc.(MiM.instance_method(:mm).bind(MiD.new).inspect)",
            "loc.(MiM.instance_method(:mm).bind(MiE.new).inspect)",
            "loc.(MiD.instance_method(:mm).bind(MiE.new).inspect)",
            "loc.(MiE.new.method(:mm).inspect)",
            "o = MiC.new; def o.x = 1; loc.(MiM.instance_method(:mm).bind(o).inspect)",
            "loc.(MiM.instance_method(:mm).bind(MiC).inspect)",
            "loc.(MiM.instance_method(:mm).bind(MiM).inspect)",
            "loc.(MiM.instance_method(:mm).bind(MiC.new).unbind.inspect)",
            "m = MiM.instance_method(:mm).bind(MiC.new); [m.owner, m.receiver.class, m.call, m.name, m.arity]",
            // Bound through an include class or not: never equal.
            "o = MiD.new; c = MiC.new; u = MiM.instance_method(:mm); [u.bind(o) == u.bind(o), u.bind(o) == o.method(:mm), u.bind(c) == u.bind(o), u.bind(o).eql?(o.method(:mm))]",
            "o = MiD.new; u = MiM.instance_method(:mm); { u.bind(o) => 1 }[o.method(:mm)]",
            // `Method#to_proc` is a lambda without a source location.
            r#"pr = MiD.new.method(:mm).to_proc; [pr.inspect.sub(/0x\h+/, "0x"), pr.lambda?, pr.call]"#,
            r#"1.method(:+).to_proc.inspect.sub(/0x\h+/, "0x")"#,
        ],
    );
}

/// The examples of #2055: hour 24, and which field a range error names.
#[test]
fn time_hour_24_and_field_ranges() {
    probe(
        r####"
        f = ->(tm) { [tm.year, tm.mon, tm.day, tm.hour, tm.min, tm.sec, tm.nsec, tm.utc_offset] }
        "####,
        &[
            "f.(Time.utc(2024, 12, 31, 24))",
            "f.(Time.utc(2024, 1, 31, 24))",
            "f.(Time.utc(2024, 2, 29, 24))",
            "f.(Time.gm(2024, 12, 31, 24, 0, 0))",
            "f.(Time.utc(2024, 12, 31, 24, 0, 0, 0))",
            "f.(Time.new(2024, 12, 31, 24, 0, 0, \"+09:00\"))",
            "f.(Time.new(2024, 12, 31, 24, 0, 0, in: \"+09:00\"))",
            "f.(Time.new(2024, 12, 31, 24, 0, 0, \"Z\"))",
            "f.(Time.new(\"2024-12-31 24:00:00\", in: \"UTC\"))",
            "f.(Time.new(\"2024-12-31T24:00:00Z\"))",
            "f.(Time.utc(0, 0, 24, 31, 12, 2024, nil, nil, nil, nil))",
            "f.(Time.utc(2024, 12, 31, \"24\"))",
            "f.(Time.utc(9999, 12, 31, 24))",
            // Hour 24 with anything after it.
            "Time.utc(2024, 12, 31, 24, 0, 1)",
            "Time.utc(2024, 12, 31, 24, 1, 0)",
            "Time.utc(2024, 12, 31, 24, 0, 0, 1)",
            "Time.utc(2024, 12, 31, 24, 0, 0.5)",
            "Time.new(2024, 12, 31, 24, 1, 0, \"+09:00\")",
            "Time.new(2024, 12, 31, 24, 0, 1, \"+09:00\")",
            "Time.new(\"2024-12-31 24:00:01\", in: \"UTC\")",
            "Time.utc(1, 0, 24, 31, 12, 2024, nil, nil, nil, nil)",
            // Each field by its name, and past the bit width.
            "Time.utc(2024, 12, 31, 25)",
            "Time.utc(2024, 12, 31, 32)",
            "Time.utc(2024, 12, 31, -1)",
            "Time.utc(2024, 12, 31, 23, 60)",
            "Time.utc(2024, 12, 31, 23, 64)",
            "Time.utc(2024, 12, 31, 23, 59, 61)",
            "Time.utc(2024, 12, 31, 23, 59, 64)",
            "Time.utc(2024, 12, 32)",
            "Time.utc(2024, 12, 0)",
            "Time.utc(2024, 13, 1)",
            "Time.utc(2024, 16, 1)",
            "Time.utc(2024, 0, 1)",
            "Time.utc(2024, 1, 1, 0, 0, 0, 1_000_000)",
            "Time.utc(2024, 1, 1, 0, 0, 0, -1)",
            "Time.utc(2024, \"foo\", 1)",
            "Time.utc(2024, \" 3 \", 1).mon",
            "Time.new(2024, 13, 1, 0, 0, 0, \"+09:00\")",
            "Time.new(2024, 12, 31, 23, 60, 0, \"+09:00\")",
            "f.(Time.utc(2024, 12, 31, 23, 59, 60))",
            "f.(Time.utc(2024, 2, 31))",
        ],
    );
}

/// The examples of #2043: parts that are not native numbers survive the
/// arithmetic, Strings convert in `Complex()`, `fdiv` divides the parts.
#[test]
fn complex_parts_strings_fdiv() {
    probe(
        "",
        &[
            "Complex(1r/3, 1r/2) + 1",
            "Complex(1r/3, 1r/2) * 2",
            "Complex(1r/3, 0) - Complex(1, 2)",
            "Complex(1r/3, 1r/2) * Complex(1r/3, 1r/2)",
            "Complex(1r/3, 1r/2) ** 2",
            "Complex(1r/3, 1r/2) ** 0",
            "Complex(1r/3, 1r/2) ** 1",
            "Complex(1r/3, 1r/2) ** -2",
            "Complex(1, 2) ** -2",
            "Complex(1, 2) ** -1",
            "Complex(1r/3, 0) ** 3",
            "Complex(0, 1r/3) ** 3",
            "Complex(1r/3, 1r/3) ** 5",
            "Complex(1r/3, -1r/3) ** 4",
            "Complex(2, 0.0) ** 3",
            "Complex(0.0, 2) ** 3",
            "Complex(1, 2) ** Complex(2r, 0)",
            "Complex(1, 2) ** Complex(2, 0)",
            "Complex(1, 2) ** 2r",
            "(1r/3) + Complex(1, 2)",
            "(1r/3) * Complex(1, 2)",
            "(1r/3) - Complex(1, 2)",
            "1 * Complex(1r/3, 0)",
            "1 + Complex(1r/3, 0)",
            "1 - Complex(1r/3, 0)",
            "2.5 * Complex(1r/3, 1r/2)",
            "Complex(1r/3, 1r/2) - 2.5",
            "Complex(1r/3, 1r/2) * (2**70)",
            "(2**70) + Complex(1r/3, 1r/2)",
            "Complex(1, 2) * 3",
            "Complex(1.5, 2) * 3",
            "3 * Complex(1.5, 2)",
            "Complex(1, 2) + 3.0",
            "Complex(1, 2) - 3",
            "3 - Complex(1, 2)",
            "[Complex(1r/3, 1r/2).abs, Complex(1r/3, 0).abs, Complex(0, -1r/3).abs, Complex(1r/3, 0.0).abs, Complex(0.0, 1r/3).abs]",
            "[Complex(1r/3, 1r/2).abs2, Complex(1r/3, 1r/2).conj, Complex(1, 1r/2).conjugate, Complex(1, 2).conj]",
            // Complex(String, …)
            "Complex(\"1\", 0)",
            "Complex(\"1\", \"2\")",
            "Complex(\"1+2i\", \"3\")",
            "Complex(\"1\", 2.5)",
            "Complex(1, \"2i\")",
            "Complex(\"x\", 1)",
            "Complex(\"1\", nil)",
            "Complex(\"1/3\", \"2/5\")",
            "Complex(\"1.5\", 1)",
            "Complex(\"1x\", 1, exception: false)",
            "Complex(1, \"1x\", exception: false)",
            // fdiv
            "Complex(1, 2).fdiv(Complex(2, 0))",
            "Complex(1, 2).fdiv(1r/3)",
            "Complex(1, 2).fdiv(2)",
            "Complex(1, 2).fdiv(2.0)",
            "Complex(1, 2).fdiv(Complex(1, 1))",
            "Complex(1, 2).fdiv(\"a\")",
            "Complex(1, 2).fdiv(0)",
            "Complex(1r/3, 2).fdiv(Complex(3, 4))",
            "Complex(1, 2).fdiv(nil)",
            "Complex(1.0, 2).fdiv(Complex(0, 0)).to_s",
            "Complex(10**30, 2).fdiv(10**25)",
            "Complex(1, 2) / Complex(3, 4)",
            "Complex(1, 2).quo(2)",
        ],
    );
}

/// The examples of #2032. The sizes refused are ones no machine has
/// memory for (2**40 elements and up).
#[test]
fn array_growth_refused() {
    probe(
        "",
        &[
            "a = [1]; a[2**40] = 1; a.size",
            "a = [1]; a[2**60 - 1] = 1; a.size",
            "a = [1]; a[2**60] = 1; a.size",
            "a = [1]; a[2**62] = 1; a.size",
            "a = [1]; a.insert(2**40, 1); a.size",
            "a = [1]; a.insert(2**40, 1, 2); a.size",
            "a = [1]; a.insert(2**61, 1); a.size",
            "a = [1]; a[2**40, 1] = [1]; a.size",
            "a = [1]; a[2**40, 1] = 5; a.size",
            "a = [1]; a[2**61, 1] = [1]; a.size",
            "a = [1]; a[2**60 - 1, 0] = [1, 2]; a.size",
            "a = [1]; a[2**40..] = [1]; a.size",
            "[1].fill(0, 0, 2**40).size",
            "Array.new(3).fill(0, 2**40, 5).size",
            "[1].fill(0, 0, 2**61).size",
            "[1].fill(0, 2**61, 1).size",
            "[1].fill(2**40, 5) { 0 }.size",
            // The receiver is untouched by a refused store.
            "a = [1, 2]; (a[2**40] = 1) rescue 0; (a.insert(2**61, 1)) rescue 0; (a[2**40, 1] = [1]) rescue 0; a",
            "e = (([1][2**40] = 1) rescue $!); [e.class, e.class.superclass, e.message]",
            // Small growth is as before.
            "a = []; a[5] = 1; a",
            "a = [1]; a[3, 0] = [8, 9]; a",
            "a = [1]; a[3, 0] = 8; a",
            "a = [1]; a.insert(3, 8, 9); a",
            "a = [1, 2]; a.fill(7, 1, 3); a",
            // product
            "[1, 2].product([1] * 2**20, [1] * 2**20, [1] * 2**20).size",
            "[1, 2].product([1] * 2**20, [1] * 2**20, [1] * 2**22).size",
            "[1, 2].product([1] * 2**21, [1] * 2**21, [1] * 2**21, []).size",
            "[1, 2].product([], [1] * 2**21, [1] * 2**21, [1] * 2**21).size",
            "a = (1..1001).to_a; a.product(a).size",
            "n = 0; a = (1..1100).to_a; r = a.product(a) { n += 1 }; [n, r.equal?(a)]",
            "[1, 2].product([3, 4], [5])",
        ],
    );
}

/// A store compiled by the JIT takes the same check.
#[test]
fn array_growth_refused_in_compiled_code() {
    run_test(
        r####"
        def store(a, i) = (a[i] = 1)
        out = []
        a = []
        30.times { |i| store(a, i) }
        out << a.size
        out << (begin; store(a, 2**40); rescue NoMemoryError => e; e.message; end)
        out << (begin; store(a, 2**60); rescue IndexError => e; e.message; end)
        out << a.size
        out
        "####,
    );
}

/// The Ruby-level conversions built on `rb_check_funcall` (#2054): what
/// `respond_to?`, `respond_to_missing?` and `method_missing` each decide.
#[test]
fn check_funcall_claims_without_a_method() {
    probe(
        r####"
        class CfA; def respond_to?(n, a = false) = true; end
        class CfB; def respond_to?(n, a = false) = true; def respond_to_missing?(n, a) = false; def method_missing(n, *a) = ($log << [:mm, n]; 42); end
        class CfC; def respond_to?(n, a = false) = true; def respond_to_missing?(n, a) = ($log << [:rtm, n, a]; true); def method_missing(n, *a) = ($log << [:mm, n]; n == :to_ary ? [7] : 42); end
        class CfE; def respond_to_missing?(n, a) = true; end
        class CfG; def respond_to?(n, a = false) = true; def method_missing(n, *a) = raise(NoMethodError, "boom"); end
        class CfH; def method_missing(n, *a) = raise(NoMethodError, "boom"); end
        row = ->(k) {
          $log = []
          o = k.new
          r = [t.() { Array(o).size }, t.() { [o].flatten.size }, t.() { String(o).class }, t.() { 1 + o }, t.() { [1] + o }, t.() { "a" + o }]
          r << $log.uniq
        }
        "####,
        &[
            "row.(CfA)",
            "row.(CfB)",
            "row.(CfC)",
            "row.(CfE)",
            "row.(CfG)",
            "row.(CfH)",
        ],
    );
}

/// The report of an uncaught exception whose `detailed_message` does not
/// give a message (#2033). Spawns the binary, since the in-process
/// harness never prints the report. The expected texts are CRuby 4.0.9's.
#[test]
fn uncaught_report_without_a_detailed_message() {
    let cases = [
        (
            r#"class E < StandardError; def detailed_message(**) = raise("boom"); end; raise E, "x""#,
            "-e:1:in '<main>': E\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(**) = 42; end; raise E, "x""#,
            "-e:1:in '<main>': E\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(**) = ""; end; raise E, "x""#,
            "-e:1:in '<main>': E\n",
        ),
        (
            r#"module M; class E < StandardError; def detailed_message(**) = nil; end; end; raise M::E, "c""#,
            "-e:1:in '<main>': M::E\n",
        ),
        // A cause that raises ends the report; one that answers a
        // non-String is its class name.
        (
            r#"class E < StandardError; def detailed_message(**) = raise("boom"); end; begin; raise E, "c"; rescue; raise "x"; end"#,
            "-e:1:in '<main>': x (RuntimeError)\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(**) = raise("boom"); end; begin; begin; raise "c0"; rescue; raise E, "c"; end; rescue; raise "x"; end"#,
            "-e:1:in '<main>': x (RuntimeError)\n",
        ),
        (
            r#"class E < StandardError; def detailed_message(**) = 42; end; begin; raise E, "c"; rescue; raise "x"; end"#,
            "-e:1:in '<main>': x (RuntimeError)\n-e:1:in '<main>': E\n",
        ),
    ];
    for (script, expected) in cases {
        let out = std::process::Command::new(env!("CARGO_BIN_EXE_monoruby"))
            .env_remove("RUBYOPT")
            .env_remove("RUBYLIB")
            .args(["--disable=gems", "-e", script])
            .output()
            .expect("spawn monoruby");
        assert_eq!(String::from_utf8_lossy(&out.stderr), expected, "{script}");
    }
}

/// `Struct#dig` with a String that names no member makes no Symbol
/// (#2033).
#[test]
fn struct_dig_does_not_intern() {
    probe(
        "S2033 = Struct.new(:a, :b)",
        &[
            r#"s = S2033.new(1, [2]); (s.dig("zq2033dig") rescue 0); Symbol.all_symbols.map(&:to_s).grep(/zq2033dig/)"#,
            r#"s = S2033.new(1, [2]); [s.dig("a"), s.dig(:b, 0), s.dig("b", 0), s.dig(1, 0)]"#,
            r#"S2033.new(1, 2).dig("nope")"#,
            r#"S2033.new(1, 2).dig(:nope)"#,
            r#"S2033.new(1, 2).dig(5)"#,
            r#"S2033.new(1, 2).dig(1.5)"#,
        ],
    );
}
