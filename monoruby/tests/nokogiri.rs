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

/// A `SAX::Document` that records every event.
const SAX_RECORDER: &str = r##"
        class Recorder < Nokogiri::XML::SAX::Document
          attr_reader :events
          def initialize; super; @events = []; end
          def xmldecl(v, e, s) = @events << [:xmldecl, v, e, s]
          def start_document = @events << [:start_document]
          def end_document = @events << [:end_document]
          def start_element(name, attrs = []) = @events << [:start_element, name, attrs]
          def end_element(name) = @events << [:end_element, name]
          def start_element_namespace(name, attrs = [], prefix = nil, uri = nil, ns = [])
            @events << [:start_element_namespace, name, attrs.map(&:to_a), prefix, uri, ns]
            super
          end
          def end_element_namespace(name, prefix = nil, uri = nil)
            @events << [:end_element_namespace, name, prefix, uri]
            super
          end
          def characters(s) = @events << [:characters, s]
          def comment(s) = @events << [:comment, s]
          def cdata_block(s) = @events << [:cdata_block, s]
          def processing_instruction(n, c) = @events << [:pi, n, c]
          def reference(n, c) = @events << [:reference, n, c]
          def warning(s) = @events << [:warning, s]
          def error(s) = @events << [:error, s]
        end
"##;

#[test]
fn nokogiri_sax_parser() {
    compare(&format!(
        r##"
        {SAX_RECORDER}
        xml = <<~XML
          <?xml version="1.0" encoding="UTF-8" standalone="yes"?>
          <!DOCTYPE root [ <!ENTITY ent "entity text"> ]>
          <root xmlns="http://d" xmlns:p="http://p" p:a="1" b="2"><!-- c -->
            <p:child>text &amp; &ent;<![CDATA[cd<>]]></p:child>
            <?pi data?><empty/>
          </root>
        XML
        r = []
        rec = Recorder.new
        parser = Nokogiri::XML::SAX::Parser.new(rec)
        parser.parse(xml)
        r << rec.events
        rec = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec).parse(StringIO.new(xml))
        r << rec.events.size
        rec2 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec2).parse_memory("<a x='1'>&lt;</a>")
        r << rec2.events
        rec3 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec3).parse("<a><b></a>")
        r << rec3.events
        rec4 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec4).parse("<a><b></a>") {{ |ctx| ctx.recovery = true; r << [ctx.recovery, ctx.replace_entities, ctx.line, ctx.column] }}
        r << rec4.events
        rec5 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec5).parse("<a>&ent;</a>") {{ |ctx| ctx.replace_entities = true; r << ctx.replace_entities }}
        r << rec5.events
        ctx = Nokogiri::XML::SAX::ParserContext.new("<a/>")
        r << [ctx.class, ctx.line, ctx.column, ctx.recovery, ctx.replace_entities]
        ctx = Nokogiri::XML::SAX::ParserContext.io(StringIO.new("<a/>"), Encoding::UTF_8)
        r << [ctx.class, ctx.line, ctx.column]
        rec6 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec6).parse_memory("<a>é</a>".encode("ISO-8859-1"), Encoding::ISO_8859_1)
        r << rec6.events
        rec7 = Recorder.new
        Nokogiri::XML::SAX::Parser.new(rec7, "UTF-8").parse_memory("<a>x</a>")
        r << rec7.events
        require "tempfile"
        Tempfile.create(["sax", ".xml"]) do |f|
          f.write("<f><g/></f>"); f.flush
          rec8 = Recorder.new
          Nokogiri::XML::SAX::Parser.new(rec8).parse_file(f.path)
          r << rec8.events
        end
        [-> {{ Nokogiri::XML::SAX::Parser.new.parse_memory("") }},
         -> {{ Nokogiri::XML::SAX::Parser.new.parse_memory(nil) }},
         -> {{ Nokogiri::XML::SAX::Parser.new.parse_memory(42) }},
         -> {{ Nokogiri::XML::SAX::ParserContext.memory("<a/>", 5) }},
         -> {{ Nokogiri::XML::SAX::ParserContext.io(Object.new) }},
         -> {{ Nokogiri::XML::SAX::ParserContext.new("<a/>").parse_with(Object.new) }},
         -> {{ Nokogiri::XML::SAX::Parser.new.parse_file("/nonexistent/x.xml") }},
         -> {{ Nokogiri::XML::SAX::Parser.new.parse_memory("<a/>", Encoding::UTF_8).class }},
         -> {{ Nokogiri::XML::SAX::Parser.new.parse_memory("<a/>", "NOT-AN-ENCODING") }},
         -> {{ Nokogiri::XML::SAX::ParserContext.memory("<a/>", Encoding.find("Big5")).class }}].each do |l|
          begin
            r << l.call
          rescue => e
            r << [e.class, e.message]
          end
        end
        class Boom < Nokogiri::XML::SAX::Document
          attr_reader :seen
          def initialize(at); super(); @at = at; @seen = []; end
          def start_element(name, attrs = []) = (@seen << name; raise ArgumentError, "boom at #{{name}}" if name == @at)
          def end_document = @seen << :end
        end
        b = Boom.new("b")
        begin
          Nokogiri::XML::SAX::Parser.new(b).parse("<a><b/><c/></a>")
          r << :no_raise
        rescue ArgumentError => e
          r << [e.class, e.message, b.seen]
        end
        b = Boom.new("b")
        begin
          Nokogiri::XML::SAX::Parser.new(b).parse(StringIO.new("<a><b/><c/></a>"))
        rescue ArgumentError => e
          r << [e.message, b.seen]
        end
        p r
        "##
    ));
}

#[test]
fn nokogiri_sax_push_parser() {
    compare(&format!(
        r##"
        {SAX_RECORDER}
        r = []
        rec = Recorder.new
        pp = Nokogiri::XML::SAX::PushParser.new(rec)
        r << [pp.options, pp.replace_entities]
        pp << "<?xml version='1.0'?><root xmlns:p='http://p'><p:a k='v'>te"
        pp << "xt</p:a><!-- c --><![CDATA[x]]>"
        pp.write("<b/>", false)
        pp.write("</root>")
        pp.finish
        r << rec.events
        rec = Recorder.new
        pp = Nokogiri::XML::SAX::PushParser.new(rec, "file.xml")
        pp.replace_entities = true
        pp.options = Nokogiri::XML::ParseOptions::RECOVER | Nokogiri::XML::ParseOptions::NOENT
        r << [pp.replace_entities, pp.options]
        pp << "<a><b></a>"
        pp.finish
        r << rec.events
        rec = Recorder.new
        pp = Nokogiri::XML::SAX::PushParser.new(rec)
        begin
          pp << "<a><b></a>"
          pp.finish
          r << :no_raise
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message, e.line, e.column]
        end
        r << rec.events
        rec = Recorder.new
        pp = Nokogiri::XML::SAX::PushParser.new(rec)
        pp << "<a>x"
        begin
          pp.write(nil, true)
          r << :no_raise
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message]
        end
        r << rec.events
        class Boom < Nokogiri::XML::SAX::Document
          attr_reader :seen
          def initialize; super; @seen = []; end
          def start_element(name, attrs = []) = (@seen << name; raise ArgumentError, "boom at #{{name}}" if name == "b")
          def end_document = @seen << :end
        end
        b = Boom.new
        pp = Nokogiri::XML::SAX::PushParser.new(b)
        pp << "<a>"
        begin
          pp << "<b/><c/>"
          r << :no_raise
        rescue ArgumentError => e
          r << [e.class, e.message, b.seen]
        end
        rec = Recorder.new
        hp = Nokogiri::HTML4::SAX::PushParser.new(rec)
        hp << "<html><body><p class='c'>Hello"
        hp << " <b>w</b>&amp;&eacute;</p><br>"
        hp.finish
        r << rec.events
        rec = Recorder.new
        Nokogiri::HTML4::SAX::PushParser.new(rec, nil, "ISO-8859-1").write("<p>\xe9</p>".b, true)
        r << rec.events
        begin
          Nokogiri::HTML4::SAX::PushParser.new(Recorder.new, nil, "NOT-AN-ENCODING")
        rescue => e
          r << [e.class, e.message]
        end
        rec = Recorder.new
        Nokogiri::HTML4::SAX::Parser.new(rec).parse("<html><head><meta charset='utf-8'></head><body><p>x</p></body></html>")
        r << rec.events
        rec = Recorder.new
        Nokogiri::HTML4::SAX::Parser.new(rec).parse(StringIO.new("<p>from io<p>two"))
        r << rec.events
        rec = Recorder.new
        Nokogiri::HTML4::SAX::Parser.new(rec).parse_memory("<p>mem</p>", Encoding::UTF_8)
        r << rec.events
        require "tempfile"
        Tempfile.create(["sax", ".html"]) do |f|
          f.write("<p>file</p>"); f.flush
          rec = Recorder.new
          Nokogiri::HTML4::SAX::Parser.new(rec).parse_file(f.path)
          r << rec.events
        end
        r << Nokogiri::HTML4::EncodingReader.detect_encoding("<html><head><meta http-equiv='Content-Type' content='text/html; charset=Shift_JIS'></head><body></body></html>")
        r << Nokogiri::HTML4::EncodingReader.detect_encoding("<html><body><p>none</p></body></html>")
        r << Nokogiri::HTML4::EncodingReader.detect_encoding("<?xml version='1.0' encoding='EUC-JP'?><html/>")
        d = Nokogiri::HTML4(StringIO.new("<html><head><meta charset='ISO-8859-1'></head><body><p>\xe9t\xe9</p></body></html>".b))
        r << [d.encoding, d.at_css("p").text, d.at_css("p").text.encoding.name]
        d = Nokogiri::HTML4(StringIO.new("<html><body><p>plain io</p></body></html>"))
        r << [d.encoding, d.at_css("p").text]
        d = Nokogiri::HTML4(StringIO.new(("<html><head><title>T</title></head><body>" + "<p>x</p>" * 2000 + "<meta charset='UTF-8'></body></html>")))
        r << [d.encoding, d.css("p").size, d.title]
        p r
        "##
    ));
}

#[test]
fn nokogiri_reader() {
    compare(
        r##"
        require "stringio"
        xml = <<~XML
          <?xml version="1.0" encoding="UTF-8"?>
          <!-- top -->
          <root xmlns="http://d" xmlns:p="http://p" xml:lang="en" p:a="1" b="2" xml:base="http://base/">
            <p:child id="c1">text &amp; more<![CDATA[cd]]><?pi data?></p:child>
            <empty/>
            <deep><x><y>z</y></x></deep>
          </root>
        XML
        r = []
        reader = Nokogiri::XML::Reader(xml)
        r << [reader.class, reader.source.class, reader.errors, reader.encoding, reader.state, reader.node_type, reader.name]
        reader.each do |node|
          r << [node.node_type, node.name, node.local_name, node.prefix, node.namespace_uri, node.depth, node.value,
           node.value?, node.attributes?, node.attribute_count, node.empty_element?, node.self_closing?, node.default?,
           node.lang, node.xml_version, node.state, node.base_uri]
          if node.node_type == Nokogiri::XML::Reader::TYPE_ELEMENT
            r << [node.attribute_hash, node.namespaces, node.attributes, node.attribute("id"), node.attribute("p:a"),
             node.attribute_at(0), node.attribute_at(9), node.attribute(nil), node.attribute_at(nil)]
            r << [node.inner_xml, node.outer_xml] if node.name == "deep" || node.name == "empty"
          end
        end
        r << [reader.state, reader.read, reader.encoding]
        io_reader = Nokogiri::XML::Reader(StringIO.new("<a><b>1</b><c x='y'/></a>"), "http://u/", "UTF-8")
        r << io_reader.map { |n| [n.name, n.node_type, n.depth, n.attribute_hash, n.base_uri] }
        r << [io_reader.encoding, io_reader.source.class]
        r << Nokogiri::XML::Reader.from_memory("<a>x</a>").map(&:name)
        r << Nokogiri::XML::Reader.from_io(StringIO.new("<a>x</a>"), nil, nil, 0).map(&:name)
        r << Nokogiri::XML::Reader.new("<a>x</a>", nil, "ISO-8859-1").tap(&:read).encoding
        bad = Nokogiri::XML::Reader("<root><a></root>")
        begin
          bad.each { |n| }
          r << :no_raise
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message, bad.errors.map(&:to_s), bad.errors.map(&:class).uniq]
        end
        rec = Nokogiri::XML::Reader("<root><a></root>") { |c| c.recover }
        begin
          r << rec.map(&:name)
        rescue Nokogiri::XML::SyntaxError => e
          r << [e.class, e.message]
        end
        r << rec.errors.map(&:to_s)
        ent = Nokogiri::XML::Reader("<!DOCTYPE r [<!ENTITY e 'v'><!ATTLIST r x CDATA 'dflt'>]><r>&e;</r>")
        r << ent.map { |n| [n.name, n.node_type, n.value, n.default?, n.attribute_hash, n.attribute_count] }
        ent2 = Nokogiri::XML::Reader("<!DOCTYPE r [<!ENTITY e 'v'>]><r>&e;</r>", nil, nil, Nokogiri::XML::ParseOptions::NOENT | Nokogiri::XML::ParseOptions::DTDATTR)
        r << ent2.map { |n| [n.name, n.node_type, n.value] }
        [-> { Nokogiri::XML::Reader(nil) }, -> { Nokogiri::XML::Reader.from_io(nil) }, -> { Nokogiri::XML::Reader.from_memory(nil) },
         -> { Nokogiri::XML::Reader.from_memory }, -> { Nokogiri::XML::Reader.from_memory("<a/>", 1) },
         -> { Nokogiri::XML::Reader("").map(&:name) }, -> { Nokogiri::XML::Reader("<a/>").attribute_at("x") },
         -> { Nokogiri::XML::Reader("<a/>").attribute(1) }].each do |l|
          begin
            r << l.call
          rescue => e
            r << [e.class, e.message]
          end
        end
        class Boom
          def read(n) = raise(IOError, "boom")
        end
        begin
          r << Nokogiri::XML::Reader.from_io(Boom.new).map(&:name)
        rescue => e
          r << [e.class, e.message]
        end
        rd = Nokogiri::XML::Reader("<r>" + "<i k='v'>t</i>" * 200 + "</r>")
        n = 0
        rd.each { |node| n += node.attribute_hash.size + node.namespaces.size; GC.start if n % 50 == 0 }
        r << n
        p r
        "##,
    );
}

#[test]
fn nokogiri_dup_and_xpath_handlers() {
    compare(
        r##"
        r = []
        doc = Nokogiri::XML("<r xmlns:p='http://p'><a id='1'><b>t</b></a><p:c k='v'/></r>")
        a = doc.at_css("a")
        d1 = a.dup
        r << [d1.class, d1.parent, d1.document.equal?(doc), d1.to_xml, d1.equal?(a), d1.children.size, d1["id"]]
        d0 = a.dup(0)
        r << [d0.to_xml, d0.children.size]
        other = Nokogiri::XML("<o/>")
        d2 = a.dup(1, other)
        r << [d2.document.equal?(other), d2.to_xml]
        other.root << d2
        r << other.to_xml
        r << [a.clone.to_xml, a.clone.equal?(a)]
        r << doc.at_css("b").children.first.dup.to_xml
        r << doc.at_css("a").attribute("id").dup.to_xml
        r << [doc.at_xpath("//p:c").dup.to_xml, doc.at_xpath("//p:c").dup.namespace&.prefix]
        doc.root << a.dup
        r << doc.to_xml
        dd = doc.dup
        r << [dd.class, dd.equal?(doc), dd.to_xml, dd.root.equal?(doc.root), dd.root.document.equal?(dd), dd.errors]
        dd.root << Nokogiri::XML::Node.new("added", dd)
        r << [doc.to_xml == dd.to_xml, dd.root.children.last.name]
        r << doc.dup(0).to_xml
        r << doc.clone.root.name
        h = Nokogiri::HTML4("<p>x</p>").dup
        r << [h.class, h.to_html, h.root.name]
        frag = Nokogiri::XML::DocumentFragment.parse("<x/><y/>")
        r << frag.dup.to_xml
        keep = (1..50).map { |i| doc.dup }
        GC.start
        r << [dd.root.children.map(&:name), keep.map { |d| d.root.name }.uniq]
        handler = Class.new {
          def regex(set, re) = set.find_all { |n| n.text =~ /#{re}/ }
          def upcase(s) = s.upcase
          def count_nodes(set) = set.length
          def half(n) = n / 2.0
          def big(*) = 2**70
          def yes(*) = true
          def no(*) = false
          def nothing(*) = nil
          def bad(*) = Object.new
          def boom(*) = raise(ArgumentError, "boom in handler")
          def echo(*args) = args.map(&:class).inspect
        }.new
        doc = Nokogiri::XML("<r><a>foo</a><a>bar</a><a>baz</a></r>")
        r << doc.xpath("//a[nokogiri:regex(., 'ba')]", handler).map(&:text)
        r << doc.xpath("nokogiri:upcase(string(//a))", handler)
        r << doc.xpath("nokogiri:count_nodes(//a)", handler)
        r << doc.xpath("nokogiri:half(7)", handler)
        r << doc.xpath("nokogiri:big()", handler)
        r << doc.xpath("//a[nokogiri:yes()]", handler).size
        r << doc.xpath("//a[nokogiri:no()]", handler).size
        r << doc.css("a:regex('^b')", handler).map(&:text)
        r << doc.xpath("nokogiri:regex(//a, 'z')", handler).map(&:text)
        r << doc.xpath("nokogiri:echo(1, 'two', true, //a, count(//a))", handler)
        r << doc.at_css("a").xpath("nokogiri:count_nodes(../a)", handler)
        [-> { doc.xpath("//a[nokogiri:nothing()]", handler) }, -> { doc.xpath("//a[nokogiri:bad()]", handler) },
         -> { doc.xpath("//a[nokogiri:boom()]", handler) }, -> { doc.xpath("//a[nokogiri:undefined()]", handler) },
         -> { doc.xpath("//a[nokogiri:regex(., 'x')]") }].each do |l|
          begin
            r << l.call.size
          rescue => e
            r << [e.class, e.message]
          end
        end
        r << doc.xpath("//a[nokogiri:regex(., 'o')]", handler).map(&:text)
        p r
        "##,
    );
}

#[test]
fn nokogiri_schema_and_relax_ng() {
    compare(
        r##"
        require "tempfile"
        r = []
        xsd = <<~XSD
          <xs:schema xmlns:xs="http://www.w3.org/2001/XMLSchema">
            <xs:element name="shiporder">
              <xs:complexType>
                <xs:sequence>
                  <xs:element name="orderperson" type="xs:string"/>
                  <xs:element name="item" maxOccurs="unbounded">
                    <xs:complexType>
                      <xs:sequence>
                        <xs:element name="title" type="xs:string"/>
                        <xs:element name="quantity" type="xs:positiveInteger"/>
                      </xs:sequence>
                    </xs:complexType>
                  </xs:element>
                </xs:sequence>
                <xs:attribute name="orderid" type="xs:string" use="required"/>
              </xs:complexType>
            </xs:element>
          </xs:schema>
        XSD
        good = "<shiporder orderid='1'><orderperson>A</orderperson><item><title>T</title><quantity>2</quantity></item></shiporder>"
        bad = "<shiporder><orderperson>A</orderperson><item><title>T</title><quantity>-2</quantity></item><extra/></shiporder>"
        schema = Nokogiri::XML::Schema(xsd)
        r << [schema.class, schema.errors, schema.parse_options.class, schema.parse_options.to_i]
        r << [schema.valid?(Nokogiri::XML(good)), schema.validate(Nokogiri::XML(good))]
        errors = schema.validate(Nokogiri::XML(bad))
        r << errors.map { |e| [e.class, e.message, e.line, e.column, e.level, e.domain, e.code, e.error?, e.path] }
        r << schema.valid?(Nokogiri::XML(bad))
        Tempfile.create(["doc", ".xml"]) do |f|
          f.write(good); f.flush
          r << schema.validate(f.path)
          f.rewind; f.truncate(0); f.write(bad); f.flush
          r << schema.validate(f.path).map(&:to_s)
          r << schema.valid?(f.path)
        end
        r << Nokogiri::XML::Schema.new(xsd, Nokogiri::XML::ParseOptions::DEFAULT_SCHEMA).parse_options.to_i
        r << Nokogiri::XML::Schema.new(xsd, Nokogiri::XML::ParseOptions.new(Nokogiri::XML::ParseOptions::NONET)).parse_options.class
        r << Nokogiri::XML::Schema.read_memory(xsd).class
        r << Nokogiri::XML::Schema.from_document(Nokogiri::XML(xsd)).class
        r << Nokogiri::XML::Schema.from_document(Nokogiri::XML(xsd), nil).parse_options.to_i
        blanks = Nokogiri::XML(xsd)
        blanks.root.children.each { |c| c.text? }
        r << Nokogiri::XML::Schema.from_document(blanks).valid?(Nokogiri::XML(good))
        [-> { Nokogiri::XML::Schema("<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'><xs:element name='a' type='xs:nope'/></xs:schema>") },
         -> { Nokogiri::XML::Schema("<not-a-schema/>") },
         -> { Nokogiri::XML::Schema("<xs:schema xmlns:xs='http://www.w3.org/2001/XMLSchema'><xs:import schemaLocation='http://example.com/x.xsd'/></xs:schema>").errors.map(&:to_s) },
         -> { Nokogiri::XML::Schema.from_document("<a/>") },
         -> { Nokogiri::XML::Schema.from_document(Nokogiri::XML(xsd).root).class },
         -> { schema.validate("not a file, not a doc") },
         -> { schema.validate(Nokogiri::XML("")) },
         -> { Nokogiri::XML::Schema.from_document }].each do |l|
          begin
            r << l.call
          rescue => e
            r << [e.class, e.message.lines.first.chomp]
          end
        end
        rng = <<~RNG
          <element name="addressBook" xmlns="http://relaxng.org/ns/structure/1.0">
            <zeroOrMore>
              <element name="card">
                <element name="name"><text/></element>
                <element name="email"><text/></element>
              </element>
            </zeroOrMore>
          </element>
        RNG
        relax = Nokogiri::XML::RelaxNG(rng)
        r << [relax.class, relax.class.superclass, relax.errors, relax.parse_options.to_i]
        r << relax.validate(Nokogiri::XML("<addressBook><card><name>n</name><email>e</email></card></addressBook>"))
        r << relax.validate(Nokogiri::XML("<addressBook><card><name>n</name><phone>p</phone></card></addressBook>")).map { |e| [e.class, e.message, e.line, e.domain] }
        r << relax.valid?(Nokogiri::XML("<other/>"))
        r << Nokogiri::XML::RelaxNG.read_memory(rng).class
        r << Nokogiri::XML::RelaxNG.from_document(Nokogiri::XML(rng), nil).class
        [-> { Nokogiri::XML::RelaxNG("<element xmlns='http://relaxng.org/ns/structure/1.0'><bogus/></element>") },
         -> { Nokogiri::XML::RelaxNG("<nope/>") },
         -> { Nokogiri::XML::RelaxNG.from_document(nil) },
         -> { relax.validate("/nonexistent") }].each do |l|
          begin
            r << l.call
          rescue => e
            r << [e.class, e.message.lines.first.chomp]
          end
        end
        keep = (1..30).map { Nokogiri::XML::Schema(xsd) }
        GC.start
        r << keep.map { |s| s.valid?(Nokogiri::XML(good)) }.uniq
        p r
        "##,
    );
}

#[test]
fn nokogiri_element_description() {
    compare(
        r##"
        r = []
        %w[a p br img table td html font center frame applet div span input nope].each do |tag|
          d = Nokogiri::HTML4::ElementDescription[tag]
          if d.nil?
            r << [tag, nil]
            next
          end
          r << [d.class, d.name, d.description, d.implied_start_tag?, d.implied_end_tag?, d.save_end_tag?, d.empty?,
           d.deprecated?, d.inline?, d.block?, d.sub_elements.size, d.sub_elements.first(3), d.default_sub_element,
           d.optional_attributes.size, d.optional_attributes.first(3), d.deprecated_attributes, d.required_attributes,
           d.to_s, d.inspect]
        end
        h = Nokogiri::HTML4("<html><body><p>x<br><img src='a'></p></body></html>")
        r << h.css("p, br, img").map { |n| [n.name, n.description&.name, n.description&.empty?] }
        r << Nokogiri::XML("<p/>").root.description
        begin
          Nokogiri::HTML4::ElementDescription[nil]
        rescue => e
          r << [e.class, e.message]
        end
        keep = (1..20).map { Nokogiri::HTML4::ElementDescription["p"] }
        GC.start
        r << keep.map(&:name).uniq
        p r
        "##,
    );
}

#[test]
fn nokogiri_html5() {
    compare(
        r##"
        require "stringio"
        r = []
        html = <<~HTML
          <!DOCTYPE html>
          <html lang="en"><head><meta charset="utf-8"><title>T &amp; T</title>
          <script>if (a < b) { x = "<y>"; }</script></head>
          <body class="a b">
            <p id="p1">Hello <b>world</b>&nbsp;&lt;3 "quoted" 'single'</p>
            <img src="a.png" alt="">
            <pre>
          keep
          </pre>
            <textarea>
          text</textarea>
            <ul><li>one<li>two</ul>
            <table><tr><td>cell</table>
            <svg viewBox="0 0 1 1"><circle r="1" xlink:href="#x"/><foreignObject><div>fo</div></foreignObject></svg>
            <math><mi>x</mi><annotation-xml encoding="text/html"><p>in ann</p></annotation-xml></math>
            <template><div>tpl</div></template>
            <!-- comment --><?pi data?>
            <custom-element data-x="1" xml:lang="en">c</custom-element>
          </body></html>
        HTML
        doc = Nokogiri::HTML5(html)
        r << [doc.class, doc.class.superclass, doc.encoding, doc.url, doc.quirks_mode, doc.errors, doc.xml?, doc.html?,
         doc.internal_subset&.name, doc.internal_subset&.external_id, doc.internal_subset&.system_id, doc.root.name]
        r << doc.to_html
        r << doc.to_html(preserve_newline: true)
        r << doc.serialize
        r << doc.to_xml
        r << doc.to_xhtml
        r << [doc.at_css("p").to_html, doc.at_css("p").inner_html, doc.at_css("pre").inner_html(preserve_newline: true),
         doc.at_css("pre").to_html, doc.at_css("pre").send(:prepend_newline?), doc.at_css("p").send(:prepend_newline?),
         doc.at_css("textarea").to_html(preserve_newline: true)]
        r << doc.css("svg, circle, foreignObject, math, mi, annotation-xml, custom-element").map { |n| [n.name, n.namespace&.href, n.namespace&.prefix, n.attribute_nodes.map { |a| [a.name, a.namespace&.href] }] }
        r << [doc.at_css("circle")["xlink:href"], doc.at_css("custom-element")["xml:lang"], doc.at_css("custom-element").to_html]
        r << doc.css("li, td, tr, tbody").map(&:name)
        r << doc.at_css("script").text
        r << doc.at_css("template").children.map(&:name)
        r << doc.at_css("title").text
        r << [doc.at_css("p").line, doc.at_css("svg").line, doc.at_css("custom-element").line]
        r << doc.xpath("//p/b").map(&:text)
        r << doc.css("p b").map(&:to_html)
        r << Nokogiri::HTML5("").to_html
        r << Nokogiri::HTML5(nil).to_html
        r << Nokogiri::HTML5("<p>x").to_html
        r << Nokogiri::HTML5("<!DOCTYPE html PUBLIC '-//W3C//DTD HTML 4.01//EN' 'http://www.w3.org/TR/html4/strict.dtd'><p>q").tap { |d| r << [d.quirks_mode, d.internal_subset.external_id, d.internal_subset.system_id] }.to_html
        r << Nokogiri::HTML5("<p>quirks").quirks_mode
        r << Nokogiri::HTML5("<!DOCTYPE html><p>no</p>").quirks_mode
        r << Nokogiri::HTML5("<!DOCTYPE html PUBLIC '-//W3C//DTD XHTML 1.0 Frameset//EN'><p>lim").quirks_mode
        errs = Nokogiri::HTML5("<html><body><p>x</b></p><table><p>z</table>", max_errors: 10)
        r << errs.errors.map { |e| [e.class, e.message, e.line, e.column, e.str1, e.domain, e.code, e.level, e.file] }
        r << Nokogiri::HTML5("<p></b>", "http://u/", max_errors: 1).errors.map { |e| [e.file, e.to_s] }
        r << Nokogiri::HTML5("<p></b></b>", max_parse_errors: 2).errors.size
        r << Nokogiri::HTML5("<p></b></b>").errors.size
        r << Nokogiri::HTML5::Document.parse(StringIO.new("<p>io</p>")).to_html
        r << Nokogiri::HTML5::Document.read_memory("<p>mem</p>", "u", "UTF-8").url
        r << Nokogiri::HTML5::Document.read_io(StringIO.new("<p>io2</p>"), nil, nil).at_css("p").text
        r << Nokogiri::HTML5("<p>\xe9t\xe9</p>".b, encoding: "ISO-8859-1").at_css("p").text
        r << Nokogiri::HTML5("<noscript><p>ns</p></noscript>").at_css("noscript").children.map(&:name)
        r << Nokogiri::HTML5("<noscript><p>ns</p></noscript>", parse_noscript_content_as_text: true).at_css("noscript").children.map(&:name)
        deep = "<div>" * 50 + "x" + "</div>" * 50
        r << Nokogiri::HTML5(deep, max_tree_depth: 100).css("div").size
        attrs = "<p " + (1..5).map { |i| "a#{i}='v'" }.join(" ") + ">x</p>"
        r << Nokogiri::HTML5(attrs, max_attributes: 5).at_css("p").attributes.size
        [-> { Nokogiri::HTML5(deep, max_tree_depth: 10) }, -> { Nokogiri::HTML5(attrs, max_attributes: 4) },
         -> { Nokogiri::HTML5("<p>", bogus_option: 1) }, -> { Nokogiri::HTML5(42) },
         -> { Nokogiri::Gumbo.parse("<p>", nil, Nokogiri::HTML5::Document) },
         -> { Nokogiri::Gumbo.parse(1, nil, Nokogiri::HTML5::Document, max_attributes: 1, max_errors: 1, max_tree_depth: 1) },
         -> { Nokogiri::HTML5(deep, max_tree_depth: -1).css("div").size }].each do |l|
          begin
            r << l.call
          rescue => e
            r << [e.class, e.message]
          end
        end
        frag = Nokogiri::HTML5.fragment("<p>one<p>two<b>b")
        r << [frag.class, frag.class.superclass, frag.document.class, frag.errors, frag.quirks_mode, frag.children.map(&:name), frag.to_html, frag.to_xml]
        r << Nokogiri::HTML5.fragment("<tr><td>x").to_html
        r << Nokogiri::HTML5.fragment("<tr><td>x", context: "table").to_html
        r << Nokogiri::HTML5.fragment("<tr><td>x", context: "html:tbody").to_html
        r << Nokogiri::HTML5.fragment("<circle/>", context: "svg").children.map { |n| [n.name, n.namespace&.href] }
        r << Nokogiri::HTML5.fragment("<mi>x</mi>", context: "math:math").children.map { |n| [n.name, n.namespace&.href] }
        r << Nokogiri::HTML5.fragment("<input><p>in form", context: "form").to_html
        r << Nokogiri::HTML5.fragment("<p>x</b>", max_errors: 5).errors.map { |e| [e.file, e.to_s] }
        r << Nokogiri::HTML5.fragment("", context: "p").to_html
        ctx_doc = Nokogiri::HTML5("<!DOCTYPE html><body><form><div id='d'></div></form><svg id='s'/><math><annotation-xml id='a' encoding='text/html'/></math>")
        r << Nokogiri::HTML5.fragment("<td>t<input>", context: ctx_doc.at_css("div")).to_html
        r << Nokogiri::HTML5.fragment("<circle/>", context: ctx_doc.at_css("svg")).children.map { |n| [n.name, n.namespace&.href] }
        r << Nokogiri::HTML5.fragment("<p>x", context: ctx_doc.at_css("annotation-xml")).children.map { |n| [n.name, n.namespace&.href] }
        r << ctx_doc.at_css("div").fragment("<span>frag<p>p").to_html
        r << Nokogiri::HTML5::DocumentFragment.new(ctx_doc, "<b>bold</b>", ctx_doc.at_css("div")).to_html
        r << Nokogiri::HTML5::DocumentFragment.new(ctx_doc).to_html
        q = Nokogiri::HTML5("<p>quirky")
        r << Nokogiri::HTML5.fragment("<table><p>x", context: q.at_css("p")).quirks_mode
        r << Nokogiri::HTML5.fragment("<table><p>x", context: ctx_doc.at_css("div")).quirks_mode
        [-> { Nokogiri::HTML5.fragment("<p>", context: "bogus:p") }, -> { Nokogiri::HTML5.fragment("<p>", context: "a:b:c") },
         -> { Nokogiri::HTML5.fragment("<p>", context: Nokogiri::XML("<r xmlns='http://other'/>").root) },
         -> { Nokogiri::HTML5.fragment(deep, max_tree_depth: 10) }, -> { Nokogiri::HTML5.fragment("<p>", nope: 1) }].each do |l|
          begin
            r << l.call.to_html
          rescue => e
            r << [e.class, e.message]
          end
        end
        d = Nokogiri::HTML5("<!DOCTYPE html><body><div id='x'></div>")
        d.at_css("div").add_child("<p>added<span>s</span>")
        d.at_css("div").inner_html = "<i>replaced</i>"
        d.at_css("div") << Nokogiri::XML::Node.new("custom-tag", d)
        d.at_css("div")["data-q"] = "a\"b&c<d>"
        r << [d.to_html, d.at_css("div").children.map(&:class)]
        b = Nokogiri::HTML5::Builder.new { |x| x.html { x.body { x.p.cls!("k") { x.text("t<>"); x.br } } } }
        r << b.to_html
        r << Nokogiri::HTML5("<p>x</p>").to_html(encoding: "ISO-8859-1").encoding.name
        r << Nokogiri::HTML5("<p>café ☃</p>").to_html(encoding: "ISO-8859-1")
        r << Nokogiri::HTML5("<p>x</p>").at_css("p").to_html(save_with: Nokogiri::XML::Node::SaveOptions::AS_XML)
        keep = (1..30).map { Nokogiri::HTML5(html) }
        GC.start
        r << keep.map { |k| k.css("*").size }.uniq
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
