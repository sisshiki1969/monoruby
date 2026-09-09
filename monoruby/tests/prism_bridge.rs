extern crate monoruby;
use monoruby::tests::*;

// The Ruby-level `Prism` module (`gem/prism`: the prism 1.9.0 gem's Ruby
// half over `src/builtins/prism.rs`, which serializes with the libprism
// monoruby links) and the Ripper it provides (`Prism::Translation::Ripper`).
// Every expectation is the CRuby oracle's. The oracle runs CRuby with
// `--disable=gems`, i.e. its *default* prism (1.8.1 on Ruby 4.0.2), so the
// comparisons stay to what has not changed between that and the vendored
// 1.9.0 (`Prism.lex_compat` gained `on_sp` tokens in between; the Ripper
// test covers it against CRuby's own Ripper instead).

#[test]
fn prism_parses_lexes_and_queries() {
    run_test_once(
        r##"
        require "prism"
        require "tmpdir"
        r = []
        res = Prism.parse("a = 1 + 2\nputs a # done")
        r << res.class.name << res.success? << res.value.class.name
        r << res.value.statements.body.map { |n| n.class.name }
        r << res.comments.map(&:slice) << res.value.location.start_line
        call = res.value.statements.body.last
        r << call.name << call.arguments.arguments.first.class.name << call.message_loc.slice
        r << Prism.lex("x = 1").value.map { |tok, _| [tok.type, tok.value] }
        r << Prism.parse_success?("def") << Prism.parse_failure?("def")
        r << Prism.parse("def").errors.map(&:message)
        r << Prism.parse("x = 1\nx.foo(2) { |y| y }", line: 10).value.statements.body.last.location.start_line
        r << [Prism::StringQuery.local?("foo"), Prism::StringQuery.local?("Foo"), Prism::StringQuery.constant?("Foo"), Prism::StringQuery.method_name?("foo?"), Prism::StringQuery.method_name?("1a")]
        r << Prism.parse_comments("# one\n1 # two\n=begin\nthree\n=end\n").map { |c| [c.class.name, c.slice] }
        r << Prism.dump("1").bytesize << Prism.dump("1")[0, 5]
        r << Prism.parse_lex("1 + 2").value.last.map { |tok, _| tok.type }
        r << Prism.parse("é = 1").value.statements.body.first.name
        r << Prism.parse("x", scopes: [[:x]]).value.statements.body.first.class.name
        r << Prism.parse("x").value.statements.body.first.class.name
        r << Prism.parse("foo 1, *a, k: 2, &b").value.statements.body.first.arguments.arguments.map { |n| n.class.name }
        r << Prism.parse("if x then 1 else 2 end").value.statements.body.first.class.name
        path = File.join(Dir.tmpdir, "monoruby_prism_bridge_#{$$}.rb")
        File.write(path, "class C\n  def m(x) = x * 2\nend\n")
        f = Prism.parse_file(path)
        r << f.value.statements.body.first.class.name << f.source.lines.size << Prism.parse_file_success?(path) << Prism.lex_file(path).value.size
        r << Prism.parse_file_comments(path).size
        File.delete(path)
        r << (begin; Prism.parse_file(Dir.tmpdir); rescue SystemCallError => e; e.class.name; end)
        r << (begin; Prism.parse_file(File.join(Dir.tmpdir, "no-such-file-#{$$}")); rescue SystemCallError => e; e.class.name; end)
        r
        "##,
    );
}

#[test]
fn prism_visitors_and_translations_load() {
    // The pure-Ruby layers over the tree: a Visitor walk, the pattern
    // matcher, `Prism::Translation::Ripper` (§ below) and the node
    // inspector all run on the deserialized tree.
    run_test_once(
        r##"
        require "prism"
        r = []
        names = []
        visitor = Class.new(Prism::Visitor) do
          define_method(:visit_call_node) { |node| names << node.name; super(node) }
        end
        Prism.parse("foo(bar(1)); baz").value.accept(visitor.new)
        r << names
        r << Prism.parse("1 + 2").value.inspect.lines.size
        r << Prism::Pattern.new("CallNode[name: :foo]").scan(Prism.parse("foo; bar; foo").value).map { |n| n.location.start_offset }
        r << Prism.parse("x = 1").value.statements.body.first.child_nodes.map { |n| n&.class&.name }
        r
        "##,
    );
}

#[test]
fn ripper_is_prisms_translation() {
    // `require "ripper"` answers `Prism::Translation::Ripper`: the class
    // API, the event tables, S-expressions, tokens, and the `on_*`
    // subclass protocol Rails' render/annotation parsers rely on.
    run_test_once(
        r##"
        require "ripper"
        r = []
        r << Ripper.sexp("1 + a") << Ripper.sexp_raw("[1]") << Ripper.sexp("def m(x) = x")
        r << Ripper.sexp("foo(1, k: 2) { |y| y }")
        r << Ripper.lex("x = 1").map { |pos, type, tok, state| [pos, type, tok, state.to_s] }
        r << Ripper.tokenize("a.b(1)")
        r << Ripper::PARSER_EVENT_TABLE[:binary] << Ripper::PARSER_EVENT_TABLE.size << Ripper::SCANNER_EVENTS.include?(:int)
        r << Ripper::PARSER_EVENTS.include?(:command) << Ripper::EVENTS.size
        parser = Class.new(Ripper) do
          Ripper::PARSER_EVENTS.each do |event|
            module_eval("begin; undef on_#{event}; rescue NameError; end\ndef on_#{event}(*args); [:#{event}, *args]; end")
          end
          Ripper::SCANNER_EVENTS.each do |event|
            module_eval("def on_#{event}(tok); [:@#{event}, tok, [lineno, column]]; end")
          end
        end
        r << parser.new("render 'x', y: 1").parse
        calls = []
        finder = Class.new(Ripper) do
          define_method(:on_fcall) { |name| calls << [:fcall, name]; nil }
          define_method(:on_command) { |name, *| calls << [:command, name]; nil }
        end
        finder.new("foo(1)\nbar 2\nbaz.qux").parse
        r << calls
        r << Ripper.new("x").filename
        r << (Ripper.sexp("def") == nil) << Ripper.new("def").tap(&:parse).error?
        r << Ripper.lex_state_name(Ripper::Lexer::State.new(1).to_int) rescue r << $!.class.name
        r
        "##,
    );
}
