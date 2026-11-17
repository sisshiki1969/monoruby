//! Leftovers of the name-argument and allocation work (#2025).
//!
//! - A request for more memory than there is raises NoMemoryError, or
//!   the ArgumentError CRuby has for a size it refuses outright, rather
//!   than aborting the process.
//! - `Exception#full_message` of an exception that was never raised
//!   shows where `full_message` was called.
//! - `format` / `sprintf` / `printf` with no arguments.
//! - A name argument that is a String no Symbol exists for
//!   (`rb_check_id`): the NameError carries the String as `#name`, and
//!   no Symbol is made for it. The names below appear nowhere else, so
//!   that nothing has interned them.

use monoruby::tests::*;

const HELPER: &str = r####"
def t; yield.inspect.gsub(/[^ "\\\[]+:\d+:in /, "F:in "); rescue Exception => e; "#{e.class}: #{e.message}"; end
def fm(e) = e.full_message(highlight: false)
"####;

fn probe(exprs: &[&str]) {
    let list = exprs
        .iter()
        .map(|s| format!("t {{ {s} }}"))
        .collect::<Vec<_>>()
        .join(",\n");
    run_test_once(&format!("{HELPER}\n[{list}]"));
}

/// Sizes no machine has: every one is an error both implementations
/// report alike.
#[test]
fn allocation_limits() {
    probe(&[
        r####"Array.new(2**60 - 1)"####,
        r####"Array.new(2**60)"####,
        r####"Array.new(2**61)"####,
        r####"Array.new(2**60 - 1, 1)"####,
        r####"Array.new(2**60 - 1) { 1 }"####,
        r####"Array.new(2**60) { 1 }"####,
        r####""abc" * 2**61"####,
        r####""abc" * 2**62"####,
        r####""a" * (2**63 - 1)"####,
        r####""a".ljust(2**62).size"####,
        r####""a".center(2**62).size"####,
        r####""a".rjust(2**62).size"####,
        r####""%#{2**40}d" % 1"####,
        r####"[1] * 2**59"####,
        r####"[1, 2] * 2**62"####,
        r####"Array.new(-1)"####,
        r####"[Array.new(3), Array.new(2, 1), Array.new(2) { _1 }, "ab" * 3, "a".ljust(3), "%5d" % 1]"####,
        r####"String.new(capacity: 2**62).size"####,
        r####"[1].cycle.first(2**60)"####,
    ]);
}

/// The position line is the caller of `full_message` (the path and
/// line are normalized: the harness runs the two from different files).
#[test]
fn full_message_of_an_unraised_exception() {
    probe(&[
        r####"RuntimeError.new("x").full_message(highlight: false)"####,
        r####"RuntimeError.new("x").full_message(highlight: false, order: :bottom)"####,
        r####"RuntimeError.new("x").full_message(highlight: true)"####,
        r####"e = RuntimeError.new("x"); e.set_backtrace([]); e.full_message(highlight: false)"####,
        r####"e = RuntimeError.new("x"); e.set_backtrace(["a:1"]); e.full_message(highlight: false)"####,
        r####"e = RuntimeError.new("x\ny"); e.full_message(highlight: false)"####,
        r####"e = RuntimeError.new(""); e.full_message(highlight: false)"####,
        r####"fm(RuntimeError.new("x"))"####,
        r####"RuntimeError.new("x").method(:full_message).call(highlight: false)"####,
        r####"RuntimeError.new("x").send(:full_message, highlight: false)"####,
        r####"[RuntimeError.new("x")].map { |e| e.full_message(highlight: false) }"####,
        r####"RuntimeError.new("x").detailed_message(highlight: false)"####,
        r####"c = Class.new(RuntimeError) { alias fm2 full_message }; c.new("x").fm2(highlight: false).sub(/#<Class:0x\h+>/, "C")"####,
        r####"e = (raise "x" rescue $!); e.full_message(highlight: false)"####,
        r####"e = RuntimeError.new("x"); e.set_backtrace(nil); e.full_message(highlight: false)"####,
    ]);
}

/// `format` and its relatives need a format string.
#[test]
fn format_without_arguments() {
    probe(&[
        r####"format"####,
        r####"sprintf"####,
        r####"$stdout.printf"####,
        r####"Kernel.format"####,
        r####"[method(:format).arity, method(:sprintf).arity, method(:printf).arity, $stdout.method(:printf).arity]"####,
        r####"format("%d", 1)"####,
        r####"$stderr.printf(1)"####,
        r####"require "stringio"; StringIO.new.printf"####,
    ]);
}

/// `NameError#name` for each reflection method, and the Symbols that
/// exist afterwards.
#[test]
fn name_of_a_string_that_is_no_symbol() {
    probe(&[
        r####"begin; Object.send(:remove_const, :Zq1972a); rescue NameError => e; [e.name.class, e.name]; end"####,
        r####"begin; Object.send(:remove_const, "Zq1972b"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_get("Zq1972c"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_get("Zq1972c2::Zq1972c3"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.new.instance_variable_get("@zq1972e"); rescue NameError => e; [e.name.class, e.name]; end"####,
        r####"begin; Object.class_variable_get("@@zq1972f"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.new.send("zq1972g"); rescue NameError => e; [e.class, e.name.class, e.name, e.message.sub(/0x\h+/, ""), e.args]; end"####,
        r####"begin; Object.new.public_send("zq1972h"); rescue NameError => e; [e.class, e.name.class, e.name, e.message.sub(/0x\h+/, ""), e.args]; end"####,
        r####"begin; Object.new.__send__("zq1972i", 1, 2); rescue NameError => e; [e.class, e.name.class, e.name, e.args, e.private_call?]; end"####,
        r####"begin; Object.new.method("zq1972j"); rescue NameError => e; [e.class, e.name.class, e.name, e.message.sub(/0x\h+/, "")]; end"####,
        r####"begin; Object.instance_method("zq1972k"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:remove_method, "zq1972l"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:undef_method, "zq1972m"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:alias_method, :a, "zq1972n"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:public, "zq1972o"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:remove_class_variable, "@@zq1972p"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.new.remove_instance_variable("@zq1972q"); rescue NameError => e; [e.class, e.name.class, e.name, e.message.sub(/0x\h+/, "")]; end"####,
        r####"begin; Object.new.public_method("zq1972r"); rescue NameError => e; [e.class, e.name.class, e.name, e.message.sub(/0x\h+/, "")]; end"####,
        r####"begin; Object.const_source_location("Zq1972t"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.public_instance_method("zq1972u"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:private_constant, "Zq1972v"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Object.send(:deprecate_constant, "Zq1972w"); rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; Struct.new(:a).new(1)["zq1972x"]; rescue NameError => e; [e.class, e.name.class, e.name, e.message]; end"####,
        r####"begin; 1.send("zq1972y", 3); rescue NameError => e; [e.class, e.name.class, e.name, e.message, e.args, e.receiver]; end"####,
        r####"begin; Class.new { def respond_to_missing?(*) = false }.new.send("zq1972aa", 3); rescue NameError => e; [e.class, e.name.class, e.name]; end"####,
        r####"begin; BasicObject.new.__send__("zq1972ab", 3); rescue NameError => e; [e.class, e.name.class, e.name]; end"####,
        r####"begin; Object.new.instance_eval { send("zq1972ac") }; rescue NameError => e; [e.class, e.name.class, e.name, e.private_call?]; end"####,
        r####"begin; Object.new.public_send(:zq1972ad); rescue NameError => e; [e.class, e.name.class, e.name, e.private_call?]; end"####,
        r####"begin; Object.const_get("zq1972ae"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_defined?("Zq1972af"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_get("Zq1972ag", false); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Comparable.const_get("Zq1972ah"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_get("Object::Zq1972ai"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"begin; Object.const_get("Comparable::Zq1972aj"); rescue NameError => e; [e.name.class, e.name, e.message, e.receiver]; end"####,
        r####"Symbol.all_symbols.map(&:to_s).grep(/zq1972/i).sort"####,
    ]);
}

/// The same name arguments when the Symbol does exist: a String is as
/// good as the Symbol, whatever its encoding says about ASCII bytes.
#[test]
fn string_names_of_existing_symbols() {
    run_test_once(
        r####"
        o = Object.new; o.instance_variable_set(:@zq2025iv, 3)
        class K2025; @@zq2025cv = 4; ZQ2025C = 5; def zq2025m = 6; end
        s = Struct.new(:zq2025a, :b).new(1, 2)
        r = [
          o.instance_variable_get("@zq2025iv"), o.instance_variable_defined?("@zq2025iv"),
          o.remove_instance_variable("@zq2025iv"),
          K2025.class_variable_get("@@zq2025cv"), K2025.class_variable_defined?("@@zq2025cv"),
          K2025.const_get("ZQ2025C"), K2025.const_defined?("ZQ2025C"),
          K2025.instance_method("zq2025m").name, K2025.public_instance_method("zq2025m").name,
          K2025.method_defined?("zq2025m"), K2025.new.send("zq2025m"),
          K2025.new.public_send("zq2025m"), K2025.new.__send__("zq2025m"),
          K2025.new.send("zq2025m".encode("Shift_JIS")),
          K2025.send(:alias_method, "zq2025n", "zq2025m"), K2025.send(:private, "zq2025n"),
          K2025.private_method_defined?("zq2025n"), K2025.send(:public, ["zq2025n"]),
          K2025.public_method_defined?("zq2025n"),
          K2025.send(:remove_method, "zq2025n") == K2025, K2025.method_defined?("zq2025n"),
          K2025.send(:undef_method, "zq2025m") == K2025,
          K2025.send(:remove_class_variable, "@@zq2025cv"), K2025.send(:remove_const, "ZQ2025C"),
          s["zq2025a"], s["b"], (s["zq2025a"] = 7), s.to_a,
          Module.new { def zq2025f = 1; module_function "zq2025f" }.zq2025f,
        ]
        r
        "####,
    );
}

/// Queries that fail quietly make no Symbol either, and the errors of
/// the remaining name arguments carry the String.
#[test]
fn unknown_string_names_in_queries_and_setters() {
    run_test_once(
        r####"
        e = ->(&b) { begin; b.call; rescue NameError => x; [x.class, x.name, x.message.gsub(/0x\h+/, ""), x.receiver.class.to_s[0, 8]]; end }
        s = Struct.new(:a).new(1)
        r = [
          Object.method_defined?("zq2025q1"), Object.private_method_defined?("zq2025q2"),
          Object.public_method_defined?("zq2025q3"), Object.protected_method_defined?("zq2025q4"),
          Object.new.instance_variable_get("@zq2025q5"),
          e.() { Module.new.send(:module_function, "zq2025q6") },
          e.() { Class.new.send(:private_class_method, "zq2025q7") },
          e.() { Class.new.send(:private, ["zq2025q8"]) },
          e.() { Class.new.send(:protected, "zq2025q9") },
          e.() { Comparable.send(:undef_method, "zq2025q10") },
          e.() { Object.singleton_class.send(:undef_method, "zq2025q11") },
          e.() { Comparable.instance_method("zq2025q12") },
          e.() { Comparable.send(:remove_method, "zq2025q13") },
          e.() { s["zq2025q14"] },
          e.() { s["zq2025q15"] = 1 },
        ]
        r << Symbol.all_symbols.map(&:to_s).grep(/zq2025q/)
        "####,
    );
}

/// `send` of an unknown String name from compiled code: the inlined
/// `send` resolves the name through the same check. A receiver with its
/// own `method_missing` is handed the name as a Symbol; without one the
/// NoMethodError carries the String, the arguments, and whether that
/// `send` was written as a function call.
#[test]
fn send_of_an_unknown_string_name_compiled() {
    run_test_once(
        r####"
        class MM2025; def method_missing(n, *a) = [n, a]; end
        def s1(o) = o.send("zq2025s1", 2)
        def s2 = send("zq2025s2", 2)
        def s3(*a) = 1.send(*a)
        def s4(o) = o.public_send("zq2025s4")
        def s5(o) = o.__send__("zq2025s5", 1, 2)
        def s6(o) = o.send("zq2025s6", 3)
        # Enough calls for the callers to be compiled with `send` inlined.
        d = ->(x) { [x.class, x.name, x.args, x.private_call?, x.receiver.class, x.message.gsub(/0x\h+/, "")] }
        r = []
        300.times { r << (s1(Object.new) rescue d.($!)) }
        300.times { r << (s2 rescue d.($!)) }
        300.times { r << (s3("zq2025s3", 4) rescue d.($!)) }
        300.times { r << (s4(Object.new) rescue d.($!)) }
        300.times { r << (s5(BasicObject.new) rescue [$!.name, $!.args, $!.private_call?]) }
        300.times { r << s6(MM2025.new) }
        r.uniq
        "####,
    );
}
