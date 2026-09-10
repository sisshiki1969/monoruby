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
