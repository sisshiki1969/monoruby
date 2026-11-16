extern crate monoruby;
use monoruby::tests::*;

// Issue #1929: with `to_s` redefined on a core class, builtins that CRuby
// runs on the object itself went through the redefinition, and builtins
// that CRuby converts through `to_s` did not.

#[test]
fn symbol_methods_run_on_the_name() {
    // CRuby's `sym_upcase`, `sym_aref`, `sym_match`, … work on
    // `rb_sym2str(sym)`: neither `Symbol#to_s` nor the String method of
    // the same name is called.
    run_test_once(
        r##"
        class Symbol; def to_s = "SYM"; end
        class String
          def upcase(*) = "XX"
          def length = 98
          def [](*) = "idx"
          def =~(o) = :strmatch
          def match?(*) = :strmatchp
          def casecmp(o) = :cc
          def succ = "SUCC"
          def encoding = :enc
          def to_sym = :TOSYM
          def empty? = :emp
        end
        [
          :abc[1], :abc[0, 2], :abc.slice(1..), :abc.upcase, :Abc.downcase,
          :abc.capitalize, :aBc.swapcase, :abc.succ, :az.next,
          (:abc =~ /b/), :abc.match?(/b/), :abc.match(/b/).to_a,
          :abc.encoding.to_s, :abc.size, :abc.length, :abc.empty?, :"".empty?,
          :abc.casecmp(:ABC), :abc.casecmp?(:ABC), :abc.casecmp("abc"),
          :abc.start_with?("a"), :abc.end_with?("c"), :abc.id2name.size > 0,
          :abc.name, :abc.inspect, :abc <=> :abd,
        ]
        "##,
    );
}

#[test]
fn symbol_string_methods() {
    // The same methods, nothing redefined: results, encodings, options,
    // arities, aliases and `$~`.
    run_test(
        r##"
        $~ = nil
        res = []
        res << [:abc.size, :"".empty?, :a.empty?, :"äö".size, :"äö".length,
                :abc.encoding.to_s, :"äö".encoding.to_s]
        res << [:abc.upcase, :Abc.downcase, :abc.capitalize, :aBc.swapcase,
                :"äö".upcase, :"äö".upcase(:ascii), :ß.upcase,
                :"ß".downcase(:fold)]
        res << [:a.succ, :az.next, :zz.succ, :"a9".succ]
        res << [:abc.casecmp(:ABC), :abc.casecmp(:abd), :abc.casecmp("abc"),
                :abc.casecmp(1), :"äö".casecmp?(:"ÄÖ"), :abc.casecmp?(:ABC),
                :abc.casecmp?("ABC"), :abc.casecmp?(nil)]
        res << [:abc.match?(/b/), :abc.match?("b"), :abc.match?(/b/, 2), $~]
        res << [:abc.match(/b/).to_a, $~.to_a, :abc.match("c", 1).to_a,
                :abc.match(/b/) { |m| m[0] * 2 }, :abc.match(/z/), $~]
        res << [:abc =~ /c/, $~[0], :abc =~ /z/, $~]
        res << [:abc[0], :abc[1, 2], :abc[1..], :abc["bc"], :abc[/(b)(c)/, 2],
                $1, :abc[10], :abc.slice(-1)]
        res << %i[size length empty? encoding upcase downcase capitalize
                  swapcase succ next casecmp casecmp? match? match =~ []
                  slice].map { |m| Symbol.instance_method(m).arity }
        res << [Symbol.instance_method(:size) == Symbol.instance_method(:length),
                Symbol.instance_method(:succ) == Symbol.instance_method(:next),
                Symbol.instance_method(:[]) == Symbol.instance_method(:slice)]
        res << [:abc.upcase.frozen?, :abc[0].frozen?]
        b = "\xff\xfe".b.to_sym
        res << [b.size, b.encoding.to_s, b.empty?, b.upcase == b]
        e = "\xa4\xa2".force_encoding("EUC-JP").to_sym
        res << [e.size, e.encoding.to_s, e.upcase.encoding.to_s, e[0].encoding.to_s]
        u = "ab".encode("UTF-16LE").to_sym
        res << [u.size, u.encoding.to_s, u.upcase == "AB".encode("UTF-16LE").to_sym]
        res << [(:a.casecmp rescue $!.class), (:a.upcase(1, 2, 3) rescue $!.class)]
        res
        "##,
    );
}

#[test]
fn puts_converts_nil_through_to_s() {
    // `rb_io_puts` converts with `rb_obj_as_string`, `nil` included.
    run_test_once(
        r##"
        class NilClass; def to_s = "NIL"; end
        r, w = IO.pipe
        w.puts(nil)
        w.puts([nil, [nil]])
        w.puts(nil, 1)
        w.close
        out = r.read
        r.close
        out
        "##,
    );
    run_test_once(
        r##"
        r, w = IO.pipe
        w.puts(nil)
        w.puts([nil, [nil]])
        w.close
        out = r.read
        r.close
        out
        "##,
    );
    // A `to_s` that does not answer a String is replaced by the default
    // rendering of the object.
    run_test_once(
        r##"
        class NilClass; def to_s = 42; end
        r, w = IO.pipe
        w.puts(nil)
        w.close
        out = r.read
        r.close
        out
        "##,
    );
}

#[test]
fn rational_to_s_formats_its_parts_through_to_s() {
    run_test_once(
        r##"
        res = [Rational(1, 2).to_s, Rational(1, 2).to_s.encoding.to_s, "#{Rational(1, 2)}"]
        class Integer; def to_s(*) = "INT"; end
        res << [Rational(1, 2).to_s, Rational(-3, 1).to_s, Rational(2**70, 3).to_s,
                Rational(1, 2).inspect, "#{Rational(1, 2)}", "#{Complex(1, 2)}",
                Complex(Rational(1, 2), 3).to_s, Rational(1, 2).to_s.frozen?]
        class Integer; def to_s(*) = 42; end
        res << Rational(1, 2).to_s << "#{Rational(1, 2)}" << Rational(1, 2).to_s.encoding.to_s
        class Integer; undef_method :to_s; end
        res << (Rational(1, 2).to_s rescue $!.class)
        res
        "##,
    );
}

#[test]
fn name_error_messages_show_the_name_through_symbol_to_s() {
    // CRuby formats the name in the message as a Symbol, so through
    // `Symbol#to_s`.
    run_test_once(
        r##"
        class Symbol; def to_s = "SYM"; end
        class NameErrK
          def sup = super
          protected def prot = 1
          private def priv = 1
        end
        o = NameErrK.new
        res = []
        res << (nil.foo rescue [$!.class, $!.message, $!.name])
        res << (1.foo(2) rescue [$!.class, $!.message, $!.name, $!.args])
        res << (o.prot rescue [$!.class, $!.message])
        res << (o.priv rescue [$!.class, $!.message])
        res << (o.sup rescue [$!.class, $!.message])
        res << (1.send(:nope) rescue $!.message)
        res << (1.public_send(:nope) rescue $!.message)
        res << (:nope.to_proc.call(1) rescue $!.message)
        res << (1.method(:nope) rescue [$!.class, $!.message])
        res << (eval("zzz") rescue [$!.class, $!.message])
        # Not through the Symbol: an exception made by hand, a KeyError.
        res << NoMethodError.new("undefined method 'foo' for x", :foo).message
        res << ({a: 1}.fetch(:zz) rescue $!.message)
        res
        "##,
    );
    // A `to_s` that itself runs into a NameError, one with a non-ASCII
    // answer, and one that does not answer a String.
    run_test_once(
        r##"
        res = []
        class Symbol; def to_s = (zzz rescue "inner"); end
        res << (nil.foo rescue $!.message)
        class Symbol; def to_s = "été"; end
        res << (nil.foo rescue [$!.message, $!.message.encoding.to_s, $!.inspect])
        class Symbol; def to_s = 42; end
        res << (nil.foo rescue $!.message.sub(/0x\h+/, "XX"))
        res
        "##,
    );
}
