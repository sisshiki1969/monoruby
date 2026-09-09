extern crate monoruby;
use monoruby::tests::*;

// Psych: the psych 5.3.1 gem's Ruby half (gem/psych) over
// `src/builtins/yaml.rs` (libyaml-safer, a port of libyaml 0.2.5) in
// place of the C extension. Every expectation is the CRuby oracle's:
// the parse events, the node tree, `load` / `safe_load` /
// `unsafe_load`, the scalar scanner's types, `dump` byte for byte
// (quoting, folding, indentation, tags), the error classes and
// positions, and the event-level API (`Psych::Parser` with a
// `TreeBuilder`, `Psych::Emitter`), which rubocop and Rails use.

#[test]
fn psych_loads_like_cruby() {
    run_test_once(
        r##"
        require "yaml"
        r = []
        r << Psych::VERSION << Psych::LIBYAML_VERSION
        r << YAML.load("a: 1\nb: [1, 2]\nc: {d: e}\n")
        r << YAML.unsafe_load("default: &d\n  a: 1\nx:\n  <<: *d\n  b: 2\n")
        r << YAML.load("- 1\n- 2.5\n- true\n- ~\n- 2024-01-02\n- :sym\n- 'str'\n- \"dq\\n\"\n- |\n  lit\n  eral\n- >\n  fol\n  ded\n", permitted_classes: [Symbol, Date]).map { |v| v.is_a?(Date) ? v.to_s : v }
        r << YAML.load("x: 0o17\ny: 0x1f\nz: 1_000\nw: 1e3\nv: .inf\nu: -.NaN\nt: 0b101\ns: 1:30\n").transform_values { |v| [v.class, v.to_s] }
        r << YAML.load("- 2024-01-02 03:04:05 +09:00\n- 2024-01-02\n- 2024-01-02 03:04:05.123456 Z\n", permitted_classes: [Time, Date]).map { |v| [v.class, v.to_s] }
        r << YAML.load("!!str 1") << YAML.load("!!int '1'") << YAML.load("a: !!float 1") << YAML.load("--- !!binary aGVsbG8=\n")
        r << YAML.load("--- >-\n  folded\n  text\n\n  para\n") << YAML.load("--- |+\n  keep\n\n")
        r << YAML.load("? [a, b]\n: 1\n", permitted_classes: []) rescue r << $!.class
        r << YAML.load("empty:\nnull: ~\ntilde: '~'\n")
        r << YAML.load_stream("--- 1\n--- 2\n") << YAML.load_stream("a: 1\n---\nb: 2\n")
        r << YAML.safe_load("--- :x\n", permitted_symbols: [:x], permitted_classes: [Symbol])
        r << (begin; YAML.safe_load("--- :x\n"); rescue Psych::DisallowedClass => e; e.message; end)
        r << (begin; YAML.load("--- !ruby/object:Foo\na: 1\n"); rescue Psych::DisallowedClass => e; e.message; end)
        r << (begin; YAML.safe_load("a: &x 1\nb: *x\n"); rescue Psych::AliasesNotEnabled => e; e.class.to_s; end)
        r << YAML.safe_load("a: &x 1\nb: *x\n", aliases: true)
        r << (begin; YAML.load("a: 1\nb"); rescue Psych::SyntaxError => e; [e.class.to_s, e.message, e.line, e.column, e.offset, e.problem, e.context, e.file]; end)
        r << (begin; YAML.load("- 1\n-2\n"); rescue Psych::SyntaxError => e; e.message; end)
        r << (begin; YAML.load("a: [1, 2"); rescue Psych::SyntaxError => e; e.message; end)
        r << (begin; YAML.load("a: 1\n b: 2"); rescue Psych::SyntaxError => e; e.message; end)
        r << (begin; YAML.load_file("/no/such/file.yml"); rescue SystemCallError => e; e.class.to_s; end)
        r << YAML.load("", fallback: :empty) << YAML.load("") << YAML.load("# only a comment\n")
        r << YAML.load("a: 1", symbolize_names: true) << YAML.load("a: 1", freeze: true).frozen?
        r << YAML.load("%YAML 1.1\n---\nyes: no\n") << YAML.load("---\n- yes\n- No\n- on\n- Off\n- y\n")
        r << YAML.load("a: \"\\u00e9\\t\\x41\"\n").transform_values(&:bytes)
        r << YAML.load("--- \"\\/\"\n").bytes
        r << YAML.unsafe_load("--- !ruby/range 1..3\n") << YAML.unsafe_load("--- !ruby/regexp /a.b/i\n").source
        r << YAML.unsafe_load("--- !ruby/sym foo\n") << YAML.unsafe_load("--- !ruby/string:String\nstr: x\n\"@iv\": 1\n").instance_variable_get(:@iv)
        r
        "##,
    );
}

#[test]
fn psych_dumps_like_cruby() {
    run_test_once(
        r##"
        require "yaml"
        r = []
        r << YAML.dump({"a" => 1, "b" => [1, 2, {"c" => nil}], :d => "e", "f" => "multi\nline", "g" => true, "h" => 1.5, "i" => "1", "j" => "", "k" => " lead", "l" => "yes"})
        r << YAML.dump("plain") << YAML.dump([]) << YAML.dump({}) << YAML.dump(nil) << YAML.dump(:sym) << YAML.dump(1..3)
        r << YAML.dump("with: colon") << YAML.dump("#hash") << YAML.dump("a: b") << YAML.dump("trail ") << YAML.dump("tab\there")
        r << YAML.dump("null") << YAML.dump("~") << YAML.dump("1.0") << YAML.dump("0x1f") << YAML.dump("2024-01-02") << YAML.dump("true")
        r << YAML.dump("- dash") << YAML.dump("[bracket") << YAML.dump("{brace") << YAML.dump("'quote'") << YAML.dump("\"dq\"") << YAML.dump("&amp") << YAML.dump("*star") << YAML.dump("!bang") << YAML.dump("%pct") << YAML.dump("@at") << YAML.dump("`tick") << YAML.dump("|pipe") << YAML.dump(">gt") << YAML.dump("?q")
        r << YAML.dump("line1\nline2\n") << YAML.dump("line1\nline2") << YAML.dump("\n") << YAML.dump("  indented\nsecond") << YAML.dump("x\n\n\ny")
        r << YAML.dump({"long" => "x " * 60}) << YAML.dump({"long" => "x " * 60}, line_width: -1) << YAML.dump({"long" => "x " * 60}, line_width: 20)
        r << YAML.dump([1, [2, [3]]], indentation: 4) << YAML.dump({"a" => {"b" => {"c" => 1}}}, indentation: 3)
        r << YAML.dump("a", version: [1, 1]) << YAML.dump([1, 2], header: true)
        r << YAML.dump(Time.utc(2024, 1, 2, 3, 4, 5)) << YAML.dump(Time.utc(2024, 1, 2, 3, 4, 5, 123456)) << YAML.dump(Date.new(2024, 1, 2))
        r << YAML.dump(1 << 70) << YAML.dump(-3) << YAML.dump(1.0 / 3) << YAML.dump(Float::INFINITY) << YAML.dump(-Float::INFINITY) << YAML.dump(Float::NAN) << YAML.dump(1e20) << YAML.dump(Rational(1, 3)) << YAML.dump(Complex(1, 2))
        r << YAML.dump({1 => "int key", nil => "nil key", [1, 2] => "array key", true => "bool key"})
        r << YAML.dump([[1, 2], {"a" => [3]}, [], {}, [nil], [""]])
        s = "shared"
        r << YAML.dump([s, s]) << YAML.dump([[1], [1]])
        h = {"self" => nil}; h["self"] = h
        r << YAML.dump(h)
        Point = Struct.new(:x, :y)
        r << YAML.dump(Point.new(1, 2)) << (YAML.unsafe_load(YAML.dump(Point.new(1, 2))) == Point.new(1, 2))
        class Foo; def initialize; @a = 1; @b = "two"; @c = [self.class]; end; attr_reader :a, :b; end
        r << YAML.dump(Foo.new).lines.first(3)
        f = YAML.unsafe_load(YAML.dump(Foo.new)); r << [f.class.to_s, f.a, f.b]
        r << YAML.dump(Exception.new("boom")) << YAML.dump(ArgumentError.new)
        e = YAML.unsafe_load(YAML.dump(RuntimeError.new("again"))); r << [e.class.to_s, e.message]
        r << YAML.dump(Set.new([1, 2])) << YAML.dump(/a.b/i) << YAML.dump(:"with space") << YAML.dump("é".b) << YAML.dump("\xff".b)
        r << YAML.dump_stream(1, 2) << YAML.dump_stream({"a" => 1}, [2])
        r << [1, "two", nil].to_yaml << {"k" => :v}.to_yaml << "str".to_yaml << 3.to_yaml
        r << YAML.dump("x", indentation: 1) << YAML.dump({"a" => [1]}, indentation: 9)
        r << YAML.dump("x" * 100, line_width: 0)
        r << YAML.dump({"a" => 1}, canonical: true) rescue r << $!.class
        r
        "##,
    );
}

#[test]
fn psych_event_api_like_cruby() {
    run_test_once(
        r##"
        require "yaml"
        require "stringio"
        r = []
        # The node tree, as rubocop's duplicate-key checker walks it.
        tree = Psych.parse_stream("a: 1\nb: &x [1, 2]\nc: *x\nd: |\n  lit\n")
        r << tree.class.to_s << tree.children.size
        doc = tree.children.first
        r << [doc.class.to_s, doc.version, doc.tag_directives, doc.implicit, doc.implicit_end]
        map = doc.root
        r << [map.class.to_s, map.anchor, map.tag, map.implicit, map.style, map.start_line, map.start_column, map.end_line, map.end_column]
        r << map.children.map { |n| [n.class.to_s.split("::").last, (n.respond_to?(:value) ? n.value : nil), n.anchor, n.tag, (n.respond_to?(:plain) ? [n.plain, n.quoted, n.style] : (n.respond_to?(:style) ? n.style : nil)), n.start_line, n.start_column, n.end_line, n.end_column] }
        r << tree.to_ruby << doc.to_ruby.class.to_s
        r << Psych.parse("--- !!str 1").to_ruby << Psych.parse("").class.to_s
        # Events straight from the parser, with their locations.
        events = []
        handler = Class.new(Psych::Handler) do
          define_method(:event_location) { |*a| events << [:loc, *a] }
          %i[start_stream end_stream start_document end_document alias scalar start_sequence end_sequence start_mapping end_mapping].each do |m|
            define_method(m) { |*a| events << [m, *a] }
          end
        end.new
        Psych::Parser.new(handler).parse("%YAML 1.1\n%TAG !e! tag:example.com,2000:\n--- !e!foo &a\n- 'q'\n- *a\n...\n")
        r << events
        # A TreeBuilder subclass finds duplicate keys (rubocop's checker).
        dups = []
        checker = Class.new(Psych::TreeBuilder) do
          define_method(:end_mapping) do
            node = super()
            seen = {}
            node.children.each_slice(2) { |k, _| dups << [seen[k.value].start_line, k.start_line, k.value] if seen[k.value]; seen[k.value] = k }
            node
          end
        end.new
        parser = Psych::Parser.new(checker)
        parser.parse("a: 1\nb: 2\na: 3\n", "cfg.yml")
        r << dups << checker.root.class.to_s
        r << (begin; Psych::Parser.new(Psych::TreeBuilder.new).parse("a: [", "cfg.yml"); rescue Psych::SyntaxError => e; [e.message, e.file]; end)
        # The emitter, event by event, and re-emitting a parsed tree.
        io = StringIO.new("".dup)
        em = Psych::Emitter.new(io)
        em.start_stream(Psych::Nodes::Stream::UTF8)
        em.start_document([1, 1], [["!e!", "tag:example.com,2000:"]], false)
        em.start_mapping(nil, nil, true, Psych::Nodes::Mapping::BLOCK)
        em.scalar("key", nil, nil, true, false, Psych::Nodes::Scalar::PLAIN)
        em.start_sequence("s", "!e!foo", false, Psych::Nodes::Sequence::FLOW)
        em.scalar("one", nil, nil, true, false, Psych::Nodes::Scalar::ANY)
        em.scalar("two", nil, nil, false, true, Psych::Nodes::Scalar::DOUBLE_QUOTED)
        em.end_sequence
        em.scalar("ref", nil, nil, true, false, Psych::Nodes::Scalar::PLAIN)
        em.alias("s")
        em.scalar("lit", nil, nil, true, false, Psych::Nodes::Scalar::PLAIN)
        em.scalar("a\nb\n", nil, nil, true, false, Psych::Nodes::Scalar::LITERAL)
        em.end_mapping
        em.end_document(true)
        em.end_stream
        r << io.string
        r << Psych.parse_stream("a: [1, {b: c}]\n").yaml << Psych.parse_stream("x: &y 1\nz: *y\n").to_yaml
        r << Psych.parse_stream("a: 1").yaml(nil, indentation: 4, line_width: 10)
        # A document emitted outside a stream: libyaml's state machine
        # rejects it (psych raises the emitter's message).
        r << (begin; Psych.parse("a: 1").to_yaml; rescue RuntimeError => e; e.message; end)
        r << (begin; em = Psych::Emitter.new(StringIO.new("".dup)); em.start_stream(1); em.start_stream(1); rescue RuntimeError => e; e.message; end)
        em2 = Psych::Emitter.new(StringIO.new("".dup), Psych::Handler::DumperOptions.new.tap { |o| o.indentation = 5; o.canonical = true; o.line_width = 40 })
        r << [em2.indentation, em2.canonical, em2.line_width]
        r << (begin; Psych::Emitter.new(StringIO.new("".dup)).start_document(1, [], false); rescue TypeError => e; e.message; end)
        r << (begin; Psych::Emitter.new(StringIO.new("".dup)).start_document([], [["x"]], false); rescue RuntimeError => e; e.message; end)
        r << Psych.libyaml_version
        r
        "##,
    );
}
