//! Nokogiri on monoruby (`src/builtins/nokogiri/` over the bundled
//! libxml2, the gem's Ruby half under `gem/nokogiri/`), compared with
//! CRuby's nokogiri gem: each script runs under the `monoruby` binary and
//! under the host CRuby (with rubygems, so not through the snapshot
//! oracle) and the outputs must match byte for byte. Skips when the host
//! ruby has no nokogiri (CI installs it).

extern crate monoruby;
use monoruby::tests::ruby_path;
use std::process::Command;

fn gem_available(name: &str) -> bool {
    Command::new(ruby_path())
        .args(["-e", &format!("require '{name}'")])
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

fn run(mut cmd: Command) -> String {
    let out = cmd
        .env("LC_ALL", "C.UTF-8")
        .env_remove("RUBYOPT")
        .env_remove("RUBYLIB")
        .output()
        .expect("failed to spawn");
    assert!(
        out.status.success(),
        "{:?} exited with {:?}\nstderr: {}",
        cmd.get_program(),
        out.status,
        String::from_utf8_lossy(&out.stderr)
    );
    String::from_utf8_lossy(&out.stdout).trim().to_string()
}

/// Run `script` (which `require`s nokogiri and prints) under both and
/// compare.
fn compare(script: &str) {
    if !gem_available("nokogiri") {
        eprintln!("skipped: the nokogiri gem is not installed for the host ruby");
        return;
    }
    let script = format!("require 'nokogiri'\n{script}");
    let mut ruby = Command::new(ruby_path());
    ruby.args(["-E", "UTF-8", "-e", &script]);
    let expected = run(ruby);
    let mut mono = Command::new(env!("CARGO_BIN_EXE_monoruby"));
    mono.args(["-e", &script]);
    let got = run(mono);
    eprintln!("ruby:\n{expected}\nmonoruby:\n{got}");
    assert!(!expected.is_empty(), "the script printed nothing");
    assert_eq!(expected, got, "output differs from CRuby");
}

#[test]
fn nokogiri_xml_parse_and_traverse() {
    compare(
        r##"
        doc = Nokogiri::XML("<root><a id='1'>hello</a><b class='x y'>world</b><c/></root>")
        p [doc.class, doc.root.name, doc.root.children.map(&:name),
         doc.root.element_children.map(&:name), doc.root.first_element_child.name,
         doc.root.last_element_child.name, doc.at_css("a").next_element.name,
         doc.at_css("b").previous_element.name, doc.at_css("b").parent.name,
         doc.at_css("c").children.length, doc.at_css("a").child.text,
         doc.at_css("a").text, doc.root.text, doc.encoding, doc.version, doc.url,
         doc.at_css("a").node_type, doc.at_css("a").child.node_type, doc.root.line,
         doc.at_css("b").path, doc.errors, doc.root.document.equal?(doc),
         doc.at_css("a").equal?(doc.at_css("a")), doc.at_css("a").blank?,
         doc.at_css("c").child, doc.root.namespace, doc.at_css("a").ancestors.map(&:name),
         doc.at_css("a").element?, doc.at_css("a").child.text?, doc.root.children.first.class,
         doc.at_css("a").key?("id"), doc.at_css("a").key?("nope"), doc.at_css("a").attribute("id").class,
         doc.at_css("a").attribute("id").value, doc.at_css("a").attribute("id").name,
         doc.at_css("b").attribute_nodes.map(&:to_xml), doc.at_css("a").line]
        "##,
    );
}

#[test]
fn nokogiri_xpath_and_css() {
    compare(
        r##"
        doc = Nokogiri::XML(<<~XML)
          <root xmlns="http://d" xmlns:x="http://x">
            <a id="1" class="one two">hello</a>
            <x:a id="2">ns</x:a>
            <b><c>deep</c></b>
          </root>
        XML
        p [doc.xpath("//d:a", "d" => "http://d").map(&:text),
         doc.xpath("//x:a").map(&:text),
         doc.xpath("//*[local-name()='a']").length,
         doc.css("a").map(&:text), doc.css("root > a.two").map { |n| n["id"] },
         doc.css("b c").map(&:text), doc.at_css("c").text,
         doc.xpath("count(//*)"), doc.xpath("string(//d:b)", "d" => "http://d"),
         doc.xpath("boolean(//nothing)"), doc.xpath("//nothing").class,
         doc.xpath("//nothing").length, doc.at_xpath("//x:a")["id"],
         doc.root.namespace_definitions.map { |ns| [ns.prefix, ns.href] },
         doc.at_xpath("//x:a").namespace.prefix, doc.root.namespace_scopes.size,
         doc.css("a").map(&:path), (doc.css("a") | doc.css("c")).map(&:name),
         (doc.css("*") - doc.css("a")).map(&:name), (doc.css("a") & doc.css("*")).length,
         doc.css("*")[1..2].map(&:name), doc.css("*")[-1].name, doc.css("*")[1, 2].length,
         doc.css("*").include?(doc.at_css("c")), doc.css("a").to_a.map(&:class),
         doc.css("*").map(&:name), doc.xpath("//@id").map(&:value), doc.xpath("//namespace::*").map(&:href).sort,
         doc.at_css("a").css("*").length, doc.root.at_xpath("d:b/d:c", "d" => "http://d").text,
         doc.root.xpath("./d:a", "d" => "http://d").length]
        "##,
    );
}

#[test]
fn nokogiri_xpath_errors() {
    compare(
        r##"
        doc = Nokogiri::XML("<r/>")
        r = []
        begin
          doc.xpath("//[")
        rescue Nokogiri::XML::XPath::SyntaxError => e
          r << [e.class, e.message]
        end
        begin
          doc.xpath("//nope:x")
        rescue Nokogiri::XML::XPath::SyntaxError => e
          r << [e.class, e.message]
        end
        p r
        "##,
    );
}

#[test]
fn nokogiri_serialization() {
    compare(
        r##"
        doc = Nokogiri::XML("<root><a id='1'>hello &amp; é</a><b><![CDATA[x<y]]></b><!-- c --><?pi data?></root>")
        p [doc.to_xml, doc.root.to_xml, doc.to_s, doc.root.to_s, doc.to_xml(indent: 4),
         doc.to_xml(save_with: Nokogiri::XML::Node::SaveOptions::AS_XML),
         doc.at_css("a").serialize, doc.root.to_xml(encoding: "ISO-8859-1"),
         doc.to_xml(encoding: "ISO-8859-1").encoding, doc.at_css("b").child.class,
         doc.at_css("b").child.to_xml, doc.root.children[2].class, doc.root.children[2].to_xml,
         doc.root.children[3].class, doc.root.children[3].to_xml, doc.root.inner_html,
         doc.at_css("a").inner_html, doc.at_css("a").content, doc.at_css("a").to_xml.encoding,
         doc.to_xml(encoding: "ISO-8859-1").bytes.size, doc.root.to_html, doc.to_xhtml]
        "##,
    );
}

#[test]
fn nokogiri_editing() {
    compare(
        r##"
        doc = Nokogiri::XML("<root><a>1</a><b>2</b></root>")
        r = []
        doc.root << Nokogiri::XML::Node.new("c", doc)
        doc.at_css("a")["id"] = "x"
        doc.at_css("a")["id"] = "y"
        doc.at_css("b").content = "two & three"
        doc.at_css("c").add_child(Nokogiri::XML::Text.new("t", doc))
        doc.at_css("c").add_child(Nokogiri::XML::Comment.new(doc, "cm"))
        doc.at_css("c").add_child(Nokogiri::XML::CDATA.new(doc, "cd"))
        doc.at_css("c").add_child(Nokogiri::XML::ProcessingInstruction.new(doc, "p", "i"))
        r << doc.to_xml
        doc.at_css("a").add_next_sibling("<d>after a</d>")
        doc.at_css("b").add_previous_sibling(Nokogiri::XML::Node.new("e", doc))
        doc.at_css("b").replace("<f>replaced b</f>")
        r << doc.to_xml
        doc.at_css("d").unlink
        doc.at_css("a").remove_attribute("id")
        r << doc.to_xml
        doc.at_css("c").name = "renamed"
        doc.at_css("renamed").children.unlink
        r << doc.to_xml
        doc.root.add_namespace_definition("ns", "http://ns")
        n = Nokogiri::XML::Node.new("ns:item", doc)
        doc.root << n
        r << [n.namespace&.prefix, doc.to_xml]
        r << doc.root.children.map(&:name)
        e = doc.at_css("e")
        e.unlink
        r << [e.parent, e.document.equal?(doc), e.to_xml]
        doc.root.children.first.after("<g/>")
        r << doc.root.element_children.map(&:name)
        a = Nokogiri::XML::Attr.new(doc, "k")
        a.value = "v<>"
        r << a.to_xml
        r << Nokogiri::XML::Document.new.to_xml
        d = Nokogiri::XML::Document.new
        d.root = Nokogiri::XML::Node.new("top", d)
        d.root["a"] = "b"
        r << [d.to_xml, d.root.name]
        other = Nokogiri::XML("<o><moved x='1'>m</moved></o>")
        d.root << other.at_css("moved")
        r << [d.to_xml, other.to_xml]
        d.root.children.first.before("t1")
        d.root.add_child("t2")
        r << d.to_xml
        r << d.root.children.map(&:class)
        p r
        "##,
    );
}

#[test]
fn nokogiri_syntax_errors() {
    compare(
        r##"
        r = []
        bad = Nokogiri::XML("<root><a></root>")
        r << bad.errors.map { |e| [e.class, e.to_s, e.message, e.line, e.column, e.level, e.domain, e.code, e.fatal?, e.error?, e.warning?, e.file] }
        r << bad.to_xml
        begin
          Nokogiri::XML("<root><a></root>") { |c| c.strict }
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message, e.line, e.column]
        end
        begin
          Nokogiri::XML("<root><a></root><b>") { |c| c.strict }
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message]
        end
        r << Nokogiri::XML("").errors.size
        begin
          Nokogiri::XML("") { |c| c.strict }
        rescue Nokogiri::XML::SyntaxError => e
          r << e.message
        end
        p r
        "##,
    );
}

#[test]
fn nokogiri_html4_documents_and_fragments() {
    compare(
        r##"
        require "stringio"
        html = "<html><head><title>T</title></head><body><p class='a'>Hello <b>w</b></p><p>2</p></body></html>"
        doc = Nokogiri::HTML4(html)
        r = [doc.class, doc.title, doc.css("p").map(&:text), doc.at_css("p.a b").text, doc.to_html]
        frag = Nokogiri::HTML4.fragment("<div>x<span>y</span></div>tail")
        r << [frag.class, frag.children.map(&:name), frag.to_html]
        n = doc.at_css("p")
        n.add_child("<i>added</i>")
        n.inner_html = "<u>replaced</u>"
        r << n.to_html
        x = Nokogiri::XML::DocumentFragment.parse("<a>1</a><b>2</b>")
        r << [x.children.map(&:name), x.to_xml]
        r << Nokogiri::XML(StringIO.new("<r><x/></r>")).root.children.map(&:name)
        r << Nokogiri.parse("<html><body><p>auto</p></body></html>").class
        bad = Nokogiri::HTML4("<p><b>unclosed")
        r << [bad.errors.size, bad.to_html]
        begin
          Nokogiri::HTML4("<p>x</p>") { |c| c.strict }
        rescue => e
          r << [e.class, e.message]
        end
        r << Nokogiri::HTML4::Document.new.to_html
        r << Nokogiri::HTML4("<p>&eacute;&amp;&nbsp;</p>").at_css("p").text
        r << Nokogiri::HTML4::NamedCharacters["eacute"]
        r << doc.xpath("//p[@class='a']/b/text()").map(&:to_s)
        r << doc.at_css("p").attributes.keys
        r << Nokogiri::HTML4::Document.parse("<p>x", nil, "UTF-8").encoding
        r << Nokogiri::HTML4("<div><p>x<p>y</div>").css("p").map(&:to_html)
        p r
        "##,
    );
}

#[test]
fn nokogiri_builder_and_document_new() {
    compare(
        r##"
        b = Nokogiri::XML::Builder.new(encoding: "UTF-8") do |xml|
          xml.root(attr: "v") { xml.child "text"; xml.empty; xml.cdata "c<d" }
        end
        h = Nokogiri::HTML4::Builder.new { |x| x.html { x.body { x.p.warn! "hi" } } }
        p [b.to_xml, b.doc.root.name, h.to_html]
        "##,
    );
}

#[test]
fn nokogiri_node_misc_natives() {
    compare(
        r##"
        doc = Nokogiri::XML(<<~XML)
          <root xmlns:p="http://p" xml:lang="en">
            <a p:k="pk" k="plain" id="1">one</a>
            <b>two</b><c>three</c>
          </root>
        XML
        a, b, c = doc.css("a, b, c").to_a
        r = []
        r << [b.next_sibling.name, b.next.name, c.previous_sibling.name, c.previous.name, a.previous_sibling.class,
         a.next_sibling.class, doc.root.next_sibling, a.child.next_sibling]
        r << [a.pointer_id == doc.at_css("a").pointer_id, a.pointer_id == b.pointer_id, a.pointer_id.class, a.data_ptr?]
        r << [a.lang, doc.root.lang, b.lang]
        b.lang = "fr"
        r << [b.lang, b.to_xml]
        r << [a <=> b, b <=> a, a <=> a, (a <=> doc.root), c.line]
        c.line = 12
        r << c.line
        c.line = 70000
        r << c.line
        r << [a.attribute_with_ns("k", "http://p")&.value, a.attribute_with_ns("k", nil)&.value,
         a.attribute_with_ns("nope", "http://p"), a.namespaced_key?("k", "http://p"), a.namespaced_key?("k", nil),
         a.namespaced_key?("k", "http://q"), a["p:k"], a["k"], a["q:k"], a[nil], a["missing"]]
        t = a.attribute("k").children.first
        a["k"] = "changed"
        r << [t.class, t.to_s, a["k"], a.attribute("k").children.size]
        r << [a.encode_special_chars("a < b & \"c\""), doc.root.encode_special_chars("é")]
        attr = a.attribute("id")
        ns = attr.add_namespace_definition("x", "http://x")
        r << [ns.class, ns.prefix, ns.href, a.to_xml, attr.namespace&.prefix]
        d = Nokogiri::XML::Node.new("d", doc)
        doc.root << d
        dns = d.add_namespace_definition(nil, "http://default")
        r << [dns.prefix, dns.href, d.namespace&.href, d.to_xml]
        d.namespace = doc.root.namespace_definitions.find { |n| n.prefix == "p" }
        r << [d.namespace.prefix, d.to_xml]
        d.namespace = nil
        r << [d.namespace, d.to_xml, d.default_namespace = "http://dd", d.namespace.href]
        r << [a.text.to_i, a.attribute("id").node_type, a.attribute("id").parent.name, a.attribute("id").document.equal?(doc)]
        r << [doc.internal_subset, doc.external_subset, doc.root.internal_subset]
        dtd = doc.create_internal_subset("root", "-//X//DTD//EN", "http://x/root.dtd")
        r << [dtd.class, dtd.name, dtd.external_id, dtd.system_id, dtd.node_type, doc.internal_subset.equal?(dtd),
         doc.root.internal_subset.class, doc.to_xml]
        begin
          doc.create_internal_subset("root", nil, nil)
        rescue => e
          r << [e.class, e.message]
        end
        e = Nokogiri::XML(<<~XML)
          <!DOCTYPE r PUBLIC "-//R//DTD R//EN" "http://r/r.dtd" [
            <!ELEMENT r (#PCDATA | s | t)*>
            <!ELEMENT s (t, (u | v)+)>
            <!ELEMENT t EMPTY>
            <!ELEMENT u ANY>
            <!ELEMENT v (#PCDATA)>
            <!ATTLIST r x CDATA #IMPLIED>
            <!ATTLIST r y (one | two) "one">
            <!ATTLIST s z ID #REQUIRED>
            <!ENTITY ent "entity value">
            <!ENTITY ext SYSTEM "http://r/ext.xml">
            <!NOTATION gif PUBLIC "-//GIF//" "gif.exe">
            <!ENTITY pic SYSTEM "a.gif" NDATA gif>
          ]>
          <r>&ent;<![CDATA[c]]><s z="i1"><t/><v>v</v></s></r>
        XML
        sub = e.internal_subset
        r << [sub.class, sub.children.map(&:class), sub.children.map(&:name),
         e.root.children.map(&:class), e.root.children.first.name, e.root.children.first.to_xml, e.to_xml,
         e.children.map(&:class), sub.children.map(&:to_xml), sub.external_id, sub.system_id, sub.name,
         sub.html_dtd?, sub.keys.sort, sub.to_a.map { |k, v| [k, v.class] }.sort]
        r << [sub.entities.keys.sort, sub.entities.values.map(&:class).uniq, sub.elements.keys.sort, sub.attributes.keys.sort,
         sub.notations.keys, sub.notations["gif"].class, sub.notations["gif"].to_a, e.external_subset]
        ent = sub.entities["ent"]
        r << [ent.class, ent.name, ent.content, ent.original_content, ent.entity_type, ent.external_id, ent.system_id,
         ent.to_s, ent.inspect =~ /EntityDecl/ ? :ok : ent.inspect]
        r << [sub.entities["ext"].entity_type, sub.entities["ext"].system_id, sub.entities["ext"].content,
         sub.entities["pic"].entity_type, sub.entities["pic"].content, sub.entities["pic"].system_id]
        el = sub.elements["s"]
        r << [el.class, el.name, el.element_type, el.prefix, el.content.class, el.content.type, el.content.occur,
         el.content.name, el.content.prefix, el.content.children.map(&:name), el.content.children.map(&:type),
         el.content.children.last.children.map { |c| [c.name, c.type, c.occur] }, el.content.document.equal?(e),
         el.content.inspect =~ /ElementContent/ ? :ok : el.content.inspect]
        r << [sub.elements["t"].element_type, sub.elements["t"].content, sub.elements["u"].element_type,
         sub.elements["r"].content.occur, sub.elements["r"].content.type, sub.elements["v"].content.type]
        at = sub.attributes["y"]
        r << [at.class, at.name, at.attribute_type, at.default, at.enumeration, sub.attributes["x"].attribute_type,
         sub.attributes["x"].default, sub.attributes["x"].enumeration, sub.attributes["z"].attribute_type, at.to_s]
        r << sub.validate(e).map(&:to_s)
        bad = Nokogiri::XML("<r><w/></r>")
        r << sub.validate(bad).map(&:to_s)
        r << [sub.validate(bad).map(&:class).uniq, sub.validate(bad).first.level]
        ne = Nokogiri::XML("<!DOCTYPE r><r/>")
        r << [ne.internal_subset.entities, ne.internal_subset.elements, ne.internal_subset.attributes, ne.internal_subset.notations,
         ne.internal_subset.external_id, ne.internal_subset.system_id]
        d2 = Nokogiri::XML("<!DOCTYPE r><r/>")
        made = d2.create_entity("foo", Nokogiri::XML::EntityDecl::INTERNAL_GENERAL, nil, nil, "bar")
        r << [made.class, made.name, made.content, made.entity_type, d2.internal_subset.entities.keys, d2.to_xml]
        made2 = Nokogiri::XML::EntityDecl.new("ext", d2, Nokogiri::XML::EntityDecl::EXTERNAL_GENERAL_PARSED, nil, "http://x/e.xml")
        r << [made2.entity_type, made2.system_id, made2.external_id, made2.content, d2.create_entity("only").content]
        [-> { d2.create_entity("foo", 1, nil, nil, "again") }, -> { Nokogiri::XML("<r/>").create_entity("nodtd") },
         -> { d2.create_entity }, -> { d2.create_entity("x", "notatype") }].each do |l|
          begin
            r << l.call.class
          rescue => ex
            r << [ex.class, ex.message]
          end
        end
        r << [Nokogiri::XML::Text.new("t", doc).class, Nokogiri::XML::Node.new("n", doc.root).parent,
         Nokogiri::XML::Comment.new(doc.root, "x").to_xml, Nokogiri::XML::CDATA.new(doc.root, "y").to_xml]
        [-> { Nokogiri::XML::Node.new("n", "notadoc") }, -> { Nokogiri::XML::Text.new(1, doc) },
         -> { Nokogiri::XML::Text.new("s", 1) }, -> { Nokogiri::XML::Comment.new(1, "s") },
         -> { Nokogiri::XML::Comment.new(doc, 1) }, -> { Nokogiri::XML::CDATA.new(1, "s") },
         -> { Nokogiri::XML::Attr.new(doc.root, "k") }, -> { doc.root.add_child(1) },
         -> { doc.root.add_child(doc.root) }, -> { doc.root.parent.add_child(doc.root) }].each do |l|
          begin
            l.call
            r << :ok
          rescue => ex
            r << [ex.class, ex.message]
          end
        end
        p r
        "##,
    );
}

#[test]
fn nokogiri_document_misc_natives() {
    compare(
        r##"
        r = []
        doc = Nokogiri::XML("<old><x/></old>")
        old = doc.root
        doc.root = Nokogiri::XML::Node.new("new", doc)
        r << [doc.root.name, old.parent, old.to_xml, doc.to_xml]
        other = Nokogiri::XML("<foreign a='1'><y/></foreign>")
        doc.root = other.root
        r << [doc.root.name, doc.root.equal?(other.root), other.root.name, doc.to_xml, other.to_xml]
        doc.root = nil
        r << [doc.root, doc.to_xml]
        begin
          doc.root = "not a node"
        rescue => e
          r << [e.class, e.message]
        end
        r << doc.encoding
        doc.encoding = "ISO-8859-1"
        doc.encoding = "UTF-8"
        r << [doc.encoding, doc.to_xml]
        h = Nokogiri::HTML4::Document.new("http://example.com/", "-//W3C//DTD HTML 4.01//EN")
        r << [h.type, h.to_html, h.url, h.internal_subset&.external_id, Nokogiri::HTML4::Document.new.type]
        begin
          Nokogiri::HTML4("<p><b>unclosed & more") { |c| c.strict }
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message]
        end
        begin
          Nokogiri::XML::Document.parse("<r/>", nil, "NOPE-ENCODING")
          r << :ok
        rescue => e
          r << [e.class, e.message]
        end
        r << Nokogiri::XML("<r/>", "file:///doc.xml").url
        r << Nokogiri::XML::Document.new("1.1").version
        r << Nokogiri::XML("<a><b/></a>") { |c| c.noblanks }.root.children.size
        frag = doc.fragment("<q>1</q>")
        r << [frag.document.equal?(doc), frag.to_xml]
        p r
        "##,
    );
}

#[test]
fn nokogiri_node_set_and_xpath_natives() {
    compare(
        r##"
        doc = Nokogiri::XML("<r xmlns:n='http://n'><a/><b/><c/><n:d/></r>")
        r = []
        set = doc.css("a, b, c")
        a, b, c = set.to_a
        r << [set.delete(b)&.name, set.length, set.delete(b), set.map(&:name)]
        set.push(b)
        r << [set.map(&:name), set.include?(b), set.push(b).length]
        copy = set.dup
        r << [copy.class, copy.length, copy.map(&:name), copy.document.equal?(doc)]
        r << [set[5], set[-5], set[1..].map(&:name), set[1..-1].map(&:name), set[..1].map(&:name), set[-2..].map(&:name),
         set[1...2].map(&:name), set[4..5], set[3..].length, set[-9..], set[1, 0].length, set[1, 9].length,
         set[3, 1].length, set[4, 1], set[-1, 1].map(&:name), set[1, -1]]
        begin
          set["1"]
        rescue => e
          r << e.class
        end
        empty = Nokogiri::XML::NodeSet.new(doc)
        r << [empty.length, empty.to_a, (empty | set).length, (set & empty).length, (set - empty).length, empty[0]]
        ns = doc.xpath("//namespace::*")
        r << [ns.map(&:class), ns.map(&:prefix), ns.length, ns.include?(ns[0]), ns.delete(ns[0])&.href, ns.length]
        ns = nil
        GC.start
        r << doc.xpath("//namespace::n").map(&:href)
        ctx = Nokogiri::XML::XPathContext.new(doc.root)
        ctx.register_ns("m", "http://n")
        ctx.register_variable("v", "b")
        r << [ctx.evaluate("count(//m:d)"), ctx.evaluate("//*[name()=$v]").map(&:name), ctx.evaluate("name(.)")]
        ctx.node = doc.at_css("a")
        r << [ctx.evaluate("name(.)"), ctx.evaluate("name(..)"), ctx.evaluate("count(../*)"), ctx.evaluate("boolean(../c)")]
        begin
          ctx.evaluate("//*[")
        rescue Nokogiri::XML::XPath::SyntaxError => e
          r << [e.class, e.message]
        end
        begin
          ctx.evaluate("undefined-fn()")
        rescue Nokogiri::XML::XPath::SyntaxError => e
          r << [e.class, e.message]
        end
        r << doc.xpath("//*[@id=$id]", nil, id: "x").length
        r << doc.xpath("//*[local-name()=$n]", nil, n: "d").length
        del = doc.css("a, c")
        del.unlink
        r << [doc.root.children.map(&:name), del.map(&:parent)]
        p r
        "##,
    );
}

#[test]
fn nokogiri_encoding_handler() {
    compare(
        r##"
        h = Nokogiri::EncodingHandler
        r = [h["UTF-8"].class, h["UTF-8"].name, h["utf-8"].name, h["ISO-8859-1"].name, h["nonexistent-enc"], h["ASCII"]&.name]
        r << h.alias("UTF-8", "MY-UTF-8")
        r << h["MY-UTF-8"].name
        r << h.delete("MY-UTF-8")
        r << [h["MY-UTF-8"], h.delete("MY-UTF-8"), h.delete("never-existed")]
        h.alias("ISO-8859-1", "LATIN-A")
        r << h["LATIN-A"].name
        r << h.clear_aliases!.equal?(h)
        r << h["LATIN-A"]
        r << [Nokogiri::HTML4::EntityLookup.new.get("amp").value, Nokogiri::HTML4::NamedCharacters["nope"],
         Nokogiri::HTML4::NamedCharacters.get("lt").name]
        p r
        "##,
    );
}

#[test]
fn nokogiri_io_parsing_and_writing() {
    compare(
        r##"
        require "stringio"
        r = []
        html = "<html><head><title>T</title></head><body><p class='a'>Hello <b>w</b></p></body></html>"
        opts = Nokogiri::XML::ParseOptions::DEFAULT_HTML
        d = Nokogiri::HTML4::Document.read_io(StringIO.new(html), nil, "UTF-8", opts)
        r << [d.class, d.title, d.encoding, d.errors.size, d.at_css("p").text]
        d = Nokogiri::HTML4(StringIO.new(html), nil, "UTF-8")
        r << [d.class, d.title, d.css("b").map(&:text)]
        d = Nokogiri::HTML4(StringIO.new(html), "http://x/y.html", "UTF-8")
        r << [d.url]
        r << Nokogiri::HTML4::Document.read_io(StringIO.new("<p><b>unclosed"), nil, "UTF-8", opts).errors.map(&:to_s)
        begin
          Nokogiri::HTML4::Document.read_io(StringIO.new("<p><b>unclosed & more"), nil, "UTF-8", Nokogiri::XML::ParseOptions.new(opts).strict.to_i)
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message]
        end
        class Reader
          def initialize(chunks) = @chunks = chunks
          def read(n) = @chunks.shift
        end
        class Boom < Reader
          def read(n) = raise(IOError, "boom in read")
        end
        class Found < Reader
          def encoding_found = Nokogiri::HTML4::EncodingReader::EncodingFound.new("Shift_JIS")
        end
        r << Nokogiri::XML::Document.read_io(Reader.new(["<r>", "<a/>", "</r>", nil]), nil, nil, Nokogiri::XML::ParseOptions::DEFAULT_XML).root.children.map(&:name)
        r << Nokogiri::HTML4::Document.read_io(Reader.new(["<p>a", "b</p>", nil]), nil, "UTF-8", opts).at_css("p").text
        [-> { Nokogiri::XML::Document.read_io(Boom.new([]), nil, nil, Nokogiri::XML::ParseOptions::DEFAULT_XML) },
         -> { Nokogiri::HTML4::Document.read_io(Boom.new([]), nil, "UTF-8", opts) },
         -> { Nokogiri::HTML4::Document.read_io(Found.new(["<p>x</p>", nil]), nil, nil, opts) },
         -> { Nokogiri::XML::Document.read_io(Reader.new([nil]), nil, nil, Nokogiri::XML::ParseOptions::DEFAULT_XML) },
         -> { Nokogiri::HTML4::Document.read_io(Reader.new([nil]), nil, "UTF-8", Nokogiri::XML::ParseOptions.new(opts).strict.to_i) }].each do |l|
          begin
            r << l.call.class
          rescue => e
            r << [e.class, e.message]
          end
        end
        doc = Nokogiri::XML("<r><a>é</a></r>")
        class Sink
          attr_reader :chunks
          def initialize = @chunks = []
          def write(s) = (@chunks << [s.encoding.name, s]; s.bytesize)
        end
        s = Sink.new
        doc.write_to(s)
        r << s.chunks
        s = Sink.new
        doc.write_xml_to(s, encoding: "ISO-8859-1")
        r << s.chunks.map { |e, c| [e, c.bytes] }
        io = StringIO.new
        io.set_encoding("ISO-8859-1")
        doc.write_to(io, encoding: "ISO-8859-1")
        r << [io.string.encoding.name, io.string.bytes.size]
        class BadSink
          def write(s) = raise(IOError, "boom in write")
        end
        begin
          doc.write_to(BadSink.new)
          r << :no_raise
        rescue IOError => e
          r << [e.class, e.message]
        end
        r << doc.at_css("a").send(:dump_html)
        r << Nokogiri::HTML4("<p>x<br>y</p>").at_css("p").send(:dump_html)
        p r
        "##,
    );
}

#[test]
fn nokogiri_reparenting_edge_cases() {
    compare(
        r##"
        r = []
        doc = Nokogiri::XML("<r><a>one</a>middle<b>two</b>tail</r>")
        a = doc.at_css("a")
        a.replace(Nokogiri::XML::Node.new("z", doc))
        r << [doc.to_xml, a.parent, doc.at_css("z").name]
        doc.at_css("b").replace(Nokogiri::XML::Text.new("TEXT", doc))
        r << [doc.to_xml, doc.root.children.map(&:class), doc.root.children.map(&:text)]
        doc = Nokogiri::XML("<r>x<a/>y</r>")
        t = doc.root.children[1]
        t.replace("Q")
        r << [doc.to_xml, doc.root.children.size, doc.root.children.map(&:text)]
        doc = Nokogiri::XML("<r>x<a/>y</r>")
        doc.at_css("a").replace(doc.create_text_node("Q"))
        r << [doc.to_xml, doc.root.children.size]
        doc = Nokogiri::XML("<r>x</r>")
        doc.root.children.first.replace("<n/>")
        r << doc.to_xml
        doc = Nokogiri::XML("<r>x</r>")
        doc.root.children.first.replace(Nokogiri::XML::Node.new("m", doc))
        r << doc.to_xml
        doc = Nokogiri::XML("<r><a k='v'>t</a></r>")
        attr = doc.at_css("a").attribute("k")
        attr.add_child(Nokogiri::XML::Text.new("+more", doc))
        r << [doc.to_xml, attr.value, attr.children.size]
        begin
          attr.add_child(Nokogiri::XML::Node.new("e", doc))
        rescue => e
          r << [e.class, e.message]
        end
        begin
          doc.add_child(Nokogiri::XML::Attr.new(doc, "q"))
        rescue => e
          r << [e.class, e.message]
        end
        ns_doc = Nokogiri::XML(<<~XML)
          <root xmlns="http://default" xmlns:p="http://p" xmlns:q="http://q">
            <p:item p:attr="1" plain="2"><p:sub>s</p:sub><q:other/></p:item>
            <holder/>
          </root>
        XML
        item = ns_doc.at_xpath("//p:item", "p" => "http://p")
        ns_doc.at_css("holder") << item
        r << [ns_doc.to_xml, item.namespace.prefix, item.at_xpath("p:sub", "p" => "http://p").namespace.href]
        moved = Nokogiri::XML("<x xmlns:p='http://p' xmlns:z='http://z'><p:leaf p:a='1' z:b='2'><z:deep/></p:leaf></x>").at_xpath("//p:leaf", "p" => "http://p")
        ns_doc.at_css("holder") << moved
        r << [ns_doc.to_xml, moved.namespace_definitions.map(&:prefix), moved.namespace.href, moved.attribute_nodes.map { |x| x.namespace&.href }]
        def_doc = Nokogiri::XML("<d xmlns='http://default'><child>c</child></d>")
        ns_doc.at_css("holder") << def_doc.at_xpath("//d:child", "d" => "http://default")
        r << [ns_doc.to_xml, ns_doc.at_css("holder").children.last.namespace&.prefix]
        other_default = Nokogiri::XML("<o xmlns='http://other'><kid/></o>")
        ns_doc.at_css("holder") << other_default.at_xpath("//o:kid", "o" => "http://other")
        r << ns_doc.to_xml
        ns_doc.namespace_inheritance = true
        plain = Nokogiri::XML::Node.new("inherits", ns_doc)
        ns_doc.root << plain
        r << [plain.namespace&.href, plain.to_xml]
        n = Nokogiri::XML::Node.new("p:named", ns_doc)
        n["q:attr"] = "x"
        ns_doc.root << n
        r << [n.namespace&.prefix, n.attribute_nodes.map { |x| [x.name, x.namespace&.prefix] }, n.to_xml]
        frag = Nokogiri::XML::DocumentFragment.parse("<f1/><f2/>")
        r << [frag.to_xml, frag.children.map(&:name)]
        fdoc = Nokogiri::XML("<r/>")
        f = fdoc.fragment("<a>1</a><b/>")
        begin
          fdoc.fragment("<a><b></a>")
          r << [:frag_errors, fdoc.errors.map(&:to_s)]
        rescue => e
          r << [e.class, e.message]
        end
        r << [fdoc.root.parse("<x><y>").map(&:name), fdoc.errors.map(&:to_s)]
        begin
          fdoc.root.parse("<x>", Nokogiri::XML::ParseOptions::STRICT)
          r << :ok
        rescue => e
          r << [e.class, e.message]
        end
        fdoc.root << f
        r << [fdoc.to_xml, f.children.size]
        empty = Nokogiri::XML::Document.new
        ef = empty.fragment("<top>1</top>")
        r << [ef.to_xml, empty.root]
        bad = Nokogiri::XML::Document.new
        r << bad.fragment("<a></b>").children.map(&:name)
        r << bad.errors.map(&:to_s)
        r << bad.root
        h = Nokogiri::HTML4("<div><p>x</p></div>")
        h.at_css("p").add_child("<span>1</span>frag<i>2</i>")
        r << h.at_css("p").children.map(&:name)
        r << h.at_css("div").inner_html
        html_frag = Nokogiri::HTML4.fragment("<p>1<p>2")
        r << [html_frag.children.map(&:name), html_frag.to_html]
        begin
          doc.at_css("a").add_child(Nokogiri::XML::Comment.new(doc, "c")).parent.name.then { |x| r << x }
        rescue => e
          r << e.class
        end
        r << doc.xpath("//*[nokogiri-builtin:local-name-is('a')]").map(&:name)
        r << doc.xpath("//*[nokogiri-builtin:css-class(@k, 'v')]").map(&:name)
        [-> { doc.xpath("//*[nokogiri-builtin:local-name-is()]") }, -> { doc.xpath("//*[nokogiri-builtin:local-name-is('a', 'b')]") },
         -> { doc.xpath("//*[nokogiri-builtin:css-class(@k)]") }, -> { doc.xpath("//*[nokogiri-builtin:css-class(@k, 'v', 'w')]") }].each do |l|
          begin
            r << l.call.length
          rescue => e
            r << [e.class, e.message]
          end
        end
        orphan_doc = Nokogiri::XML("<r/>")
        orphan = Nokogiri::XML::Node.new("orphan", orphan_doc)
        orphan2 = Nokogiri::XML::Text.new("txt", orphan_doc)
        held = orphan_doc.root
        orphan_doc = nil
        orphan = nil
        orphan2 = nil
        GC.start
        r << held.document.root.name
        held = nil
        GC.start
        r << Nokogiri::XML("<z/>").root.name
        p r
        "##,
    );
}

#[test]
fn nokogiri_node_identity_across_gc() {
    // Every node wraps into one Ruby object that the document keeps alive;
    // unlinked nodes stay owned by their document.
    compare(
        r##"
        xml = "<root>" + (1..50).map { |i| "<item id='#{i}'><name>n#{i}</name></item>" }.join + "</root>"
        keep = []
        sum = 0
        300.times do |i|
          doc = Nokogiri::XML(xml)
          items = doc.xpath("//item")
          sum += items.length
          n = doc.at_css("item[id='7'] name")
          keep << n if i % 50 == 0
          doc.root.add_child(Nokogiri::XML::Node.new("extra", doc))
          items.first.unlink
          GC.start if i % 100 == 0
        end
        GC.start
        a = Nokogiri::XML("<r><x/></r>")
        p [sum, keep.map(&:text).uniq, keep.map { |n| n.document.root.name }.uniq,
         a.at_css("x").equal?(a.root.children.first), a.at_css("x").equal?(a.xpath("//x")[0])]
        "##,
    );
}
